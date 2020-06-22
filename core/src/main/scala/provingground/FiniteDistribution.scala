package provingground

//import scala.language.implicitConversions

// import Collections._

import spire.algebra._

import spire.implicits._

import upickle.default._
import provingground.scalahott.IntTypes.Fin
import cats.effect.syntax.`package`.all

//import LinearStructure._

//import scala.collection.parallel.immutable.ParVector

object FiniteDistribution {
  val random = new scala.util.Random

  implicit def rw[A: ReadWriter]: ReadWriter[FiniteDistribution[A]] =
    readwriter[Vector[Weighted[A]]].bimap(
      _.pmf,
      FiniteDistribution(_)
    )

  def apply[T](pmf: Iterable[Weighted[T]]): FiniteDistribution[T] =
    FiniteDistribution(pmf.toVector)

  def apply[T](pairs: (T, Double)*): FiniteDistribution[T] = FiniteDistribution(
    pairs.map { case (x, p) => Weighted(x, p) }.toVector
  )

  def entropy[T](fd: FiniteDistribution[T]): Double =
    (fd.supp map ((x) => fd(x)) map ((p) => -p * math.log(p))).sum

  def collect[T](fd: FiniteDistribution[Option[T]]): FiniteDistribution[T] =
    FiniteDistribution(
      fd.pmf.filter(wo => wo.elem.isDefined).map((wo) => wo.map(_.get)))

  def uniform[A](s: Iterable[A]): FiniteDistribution[A] = {
    val prob = 1.0 / s.size
    val pmf  = s map (Weighted(_, prob))
    FiniteDistribution(pmf)
  }

  def reSample[A](fd: FiniteDistribution[A], size: Double, allowEmpty: Boolean = false) : FiniteDistribution[A] = 
  {
    val support = fd.support.filter(x => fd(x) < random.nextDouble * size)
    val newFD = FiniteDistribution(support.map(x => Weighted(x, fd(x) * random.nextDouble))).safeNormalized
    if (allowEmpty || support.nonEmpty)  newFD else 
      {
        require(fd.support.nonEmpty, "cannot get non-empty sample from an empty distribution")
        reSample(fd, size, allowEmpty)
      }

  }

  def rawUnif[A](s: Iterable[A]): FiniteDistribution[A] =
    FiniteDistribution(s map (Weighted(_, 1.0)))

  def unif[A](as: A*): FiniteDistribution[A] = uniform(as.toSet)

  def empty[T]: FiniteDistribution[T] =
    FiniteDistribution[T](Vector(): Vector[Weighted[T]]) //Empty[T]

  def linearCombination[T](
      terms: Seq[(Double, FiniteDistribution[T])]): FiniteDistribution[T] = {
    val scaled = for ((a, d) <- terms) yield d * a
    scaled.foldRight(empty[T])(_ ++ _)
  }

  def invFlatMap[S, T](f: S => FiniteDistribution[T],
                       support: Iterable[S]): FiniteDistribution[T] = {
    val dists = support map ((s: S) => f(s))
    dists.foldRight(FiniteDistribution.empty[T])(_ ++ _)
  }

}

/**
  * Finite distributions, often supposed to be probability distributions,
  * but may also be tangents to this or intermediates.
  *
  * @param pmf probability mass function, may have same object split.
  *
  *
  */
case class FiniteDistribution[T](pmf: Vector[Weighted[T]])
    extends AnyVal
    with ProbabilityDistribution[T] {

  /**
    * objects with positive probability (or bounded below by a threshhold)
    */
  def posmf(t: Double = 0.0): Vector[Weighted[T]] = pmf filter (_.weight > t)

  /**
    * total of the positive weights
    */
  def postotal(t: Double = 0.0): Double =
    (posmf(t) map (_.weight)).sum

  /**
    * weight of the label.
    */
  def apply(label: T): Double = getsum(label)

  /**
    * l1-norm
    */
  def norm: Double = (pmf map (_.weight.abs)).sum

  def flatten: FiniteDistribution[T] =
    FiniteDistribution(Weighted.flatten(pmf))

  def toMap: Map[T, Double] =
    Weighted.flatten(pmf).map { case Weighted(x, p) => x -> p }.toMap

  def restrict(s: Set[T]) : FiniteDistribution[T] = 
      FiniteDistribution(pmf.filter{case Weighted(x, _) => s.contains(x)})

  def sort: FiniteDistribution[T] =
    FiniteDistribution(pmf.sortBy((wt) => 1 - wt.weight))

  def supp: Vector[T] = (Weighted.flatten(pmf) map (_.elem)).toVector

  def support: Set[T] = supp.toSet

  //  lazy val decryFlat = FiniteDistribution(pmf, true, epsilon)

  def getsum(label: T): Double =
    (pmf filter (_.elem == label) map (_.weight)).sum

  def normalized(t: Double = 0.0): FiniteDistribution[T] = {
    val sc = 1.0 / postotal(t)
    FiniteDistribution(posmf(t) map (_.scale(sc)))
  }

  def prunedPMF(epsilon: Double): Vector[Weighted[T]] =
    flatten.pmf filter ((x) => math.abs(x.weight) > epsilon)

  def pruneMap[S](f: => (T => S), epsilon: Double): FiniteDistribution[S] = {
    if (prunedPMF(epsilon).isEmpty) FiniteDistribution.empty[S]
    else FiniteDistribution(prunedPMF(epsilon)) map f
  }

  def filter(p: T => Boolean) =
    FiniteDistribution(pmf filter (wt => p(wt.elem)))

  def purge(epsilon: Double): FiniteDistribution[T] =
    filter((t: T) => apply(t) > epsilon)

  def inRatioBall(that: FiniteDistribution[T], ratio: Double) : Boolean = 
    (that.support == support) && {
      support.forall(t => math.max(this(t)/ that(t), that(t)/ this(t)) < ratio)
    }

  def *(sc: Double): FiniteDistribution[T] =
    FiniteDistribution(pmf map (_.scale(sc)))

  def ++(that: FiniteDistribution[T]): FiniteDistribution[T] = {
    FiniteDistribution(pmf ++ that.pmf)
  }

  def --(that: FiniteDistribution[T]): FiniteDistribution[T] =
    (this ++ (that * (-1))).flatten

  def +(elem: T, prob: Double): FiniteDistribution[T] =
    this ++ FiniteDistribution(Vector(Weighted(elem, prob)))

  override def map[S](f: T => S): FiniteDistribution[S] = {
    val newpmf = for (Weighted(elem, wt) <- pmf) yield Weighted(f(elem), wt)
    FiniteDistribution(newpmf)
  }

  def mapOpt[S](f: T => Option[S]): FiniteDistribution[S] = {
    val newpmf = for (Weighted(elem, wt) <- pmf;
                      felem <- f(elem)) yield Weighted(felem, wt)
    FiniteDistribution(newpmf)
  }

  def collect[S](f: PartialFunction[T, S]): FiniteDistribution[S] = mapOpt(f.lift)

  def zip[S](that: FiniteDistribution[S]): FiniteDistribution[(T, S)] = {
    val newpmf =
      for {
        Weighted(x1, p1) <- pmf
        Weighted(x2, p2) <- that.pmf
      } yield Weighted((x1, x2), p1 * p2)

    FiniteDistribution(newpmf)
  }

  def invmap[S](f: S => T, support: Iterable[S]): FiniteDistribution[S] = {
    val mem   = memo
    def memFn = (x: T) => mem.getOrElse(x, 0.0)
    val pmf   = support.toVector map ((s: S) => Weighted(s, memFn(f(s))))
    FiniteDistribution(pmf)
  }

  def invmapOpt[S](f: S => Option[T],
                   support: Iterable[S]): FiniteDistribution[S] = {
    val mem                = memo
    def memFn: T => Double = (x: T) => mem.getOrElse(x, 0.0)
    val pmf =
      support.toVector map
        ((s: S) => Weighted(s, f(s).map(memFn).getOrElse(0)))
    FiniteDistribution(pmf)
  }

  def flatMap[S](f: T => FiniteDistribution[S]): FiniteDistribution[S] = {
    val newpmf =
      for (Weighted(x, p) <- pmf;
           Weighted(y, q) <- f(x).pmf) yield Weighted(y, p * q)
    FiniteDistribution(newpmf)
  }

  def pickle: List[(String, Double)] =
    flatten.map((t: T) => t.toString).pmf.toList.map((w) => (w.elem, w.weight))

  def innerProduct(that: FiniteDistribution[T]): Double =
    (for (l <- supp) yield this(l) * that(l)).sum

  def dot(that: FiniteDistribution[T]): Double = innerProduct(that)

  private def preMemo: Map[T, Double] = {
    (for (Weighted(x, p) <- pmf) yield x -> p).toMap
  }

  def memo: Map[T, Double] = flatten.preMemo

  def total: Double = (pmf map { case Weighted(x, p) => p }).sum

  /**
    * normalize if possible, otherwise empty.
    */
  def safeNormalized: FiniteDistribution[T] =
    if (total == 0) FiniteDistribution.empty[T] else *(1 / total)

  import FiniteDistribution.random

  /**
    * next instance of a random variable with the given distribution
    */
  def next: T = Weighted.pick(posmf(), random.nextDouble * postotal())

  override def toString: String = {
    val sortedpmf = pmf.sortBy(1 - _.weight)
    val terms = (for (Weighted(elem, wt) <- sortedpmf)
      yield elem.toString + " : " + wt.toString + ", ").foldLeft("")(_ + _)
    "[" + terms.dropRight(2) + "]"
  }

  override def conditioned(p: T => Boolean): FiniteDistribution[T] = {
    val filtered = filter(p)
    if (filtered.total > 0) filtered.normalized()
    else FiniteDistribution.empty[T]
  }

  override def condMap[S](f: T => Option[S]): FiniteDistribution[S] = {
    val image = mapOpt(f)
    if (image.norm > 0) image.normalized() else FiniteDistribution.empty[S]
  }

  def entropy(elem: T): Double = -math.log(apply(elem)) / math.log(2)

  def entropyView: Vector[Weighted[String]] =
    supp.map((x) => Weighted(x.toString, entropy(x))).sortBy(_.weight)

  def entropyVec: Vector[Weighted[T]] =
    supp.map((x) => Weighted(x, entropy(x))).sortBy(_.weight)

  def pmfVec : Vector[(T, Double)] = pmf.map{case Weighted(elem, weight) => elem -> weight}.sortBy(- _._2)

  def split(groups: Int): Map[Int, FiniteDistribution[T]] = {
    val rand = new scala.util.Random

    pmf.groupBy ((_) => rand.nextInt(groups - 1)).view.mapValues { x =>
      FiniteDistribution(x)
    }.toMap
  }

  def expectation(implicit ls: VectorSpace[T, Double]): T = {
    val wtdelems = for (Weighted(a, p) <- pmf) yield p *: a
    wtdelems.foldRight(ls.zero)(_ + _)
  }

  def integral(f: T => Double): Double =
    (pmf map { case Weighted(x, p) => p * f(x) }).sum

  /**
    * entropy feedback for the finite distribution to move in the direction of the base distribution,
    * however values outside support are ignored.
    * warning: should come after ++ to ensure implementation choice.
    *
    * @param baseweights base weights
    */
  def rawfeedback(baseweights: T => Double,
                  damp: Double = 0.1,
                  strictness: Double = 1.0): FiniteDistribution[T] = {
    val weights = (t: T) => math.pow(baseweights(t), strictness)
    val rawdiff = for (elem <- supp)
      yield Weighted(elem, weights(elem) / (weights(elem) * damp + apply(elem)))
    val shift = rawdiff.map(_.weight).sum / rawdiff.size
    val normaldiff = for (Weighted(pres, prob) <- rawdiff)
      yield Weighted(pres, prob - shift)
    FiniteDistribution(normaldiff)
  }

  /**
    * entropy feedback for the finite distribution to move in the direction of the base distribution,
    * however values outside support are ignored.
    * smoothed to ensure at most proportional to the target probability
    * warning: should come after ++ to ensure implementation choice.
    *
    * @param baseweights base weights
    */
  def smoothedFeedback(baseweights: T => Double,
                       damp: Double = 0.1,
                       strictness: Double = 1.0): FiniteDistribution[T] = {
    val weights = (t: T) => math.pow(baseweights(t), strictness)
    val rawdiff = for (elem <- supp)
      yield
        Weighted(elem,
                 weights(elem) * weights(elem) /
                   (weights(elem) * damp + apply(elem)))
    val shift = rawdiff.map(_.weight).sum / (rawdiff.size)
    val normaldiff = for (Weighted(pres, prob) <- rawdiff)
      yield Weighted(pres, prob - shift)
    FiniteDistribution(normaldiff)
  }

  def klDivergence(that: FiniteDistribution[T]): Double =
    (for {
      Weighted(x, p) <- pmf
    } yield p * math.log(p / that(x))).sum

  /**
    * gradient w.r.t. inner product scaled by presentation weights,
    * perpendicular to the gradient (w.r.t. same inner product) of the "total weight" function.
    */
  def KLfeedback(baseweights: T => Double,
                 strictness: Double = 1.0): FiniteDistribution[T] = {
    val weights   = (t: T) => math.pow(baseweights(t), strictness)
    val rawdiff   = for (elem <- supp) yield Weighted(elem, 1.0 / apply(elem))
    val innerprod = rawdiff.map((x) => 1.0 / x.weight).sum // Sum(1/q))
    val normsq    = rawdiff.map((x) => 1.0 / weights(x.elem)).sum // Sum (1/p)
    val normaldiff = for (Weighted(pres, prob) <- rawdiff)
      yield Weighted(pres, prob - ((1 / weights(pres)) * innerprod / normsq))
    FiniteDistribution(normaldiff)
  }
}
