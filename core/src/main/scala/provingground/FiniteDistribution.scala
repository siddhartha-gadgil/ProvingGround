package provingground

//import scala.language.implicitConversions

// import Collections._

import spire.algebra._
// import spire.math._
import spire.implicits._

//import LinearStructure._

//import scala.collection.parallel.immutable.ParVector

/**
  * Finite distributions, often supposed to be probability distributions, but may also be tangents to this or intermediates.
  *
  * @param pmf probability mass function, may have same object split.
  *
  * @param epsilon cutoff below which some methods ignore objects. should be very small to allow for split objects.
  */
object FiniteDistribution {
  val random = new scala.util.Random

  // choose default implementation
//  def apply[T](pmf: Traversable[Weighted[T]], epsilon: Double = 0.0) : FiniteDistribution[T] = FiniteDistributionSet(Weighted.flatten(pmf.toSeq).toSet, epsilon)
  def apply[T](pmf: Traversable[Weighted[T]]): FiniteDistribution[T] =
    FiniteDistribution(pmf.toVector)

  def entropy[T](fd: FiniteDistribution[T]) =
    (fd.supp map ((x) => fd(x)) map ((p) => -p * math.log(p))).sum

  def collect[T](fd: FiniteDistribution[Option[T]]) =
    FiniteDistribution(
      fd.pmf.filter(wo => !(wo.elem.isEmpty)).map((wo) => wo.map(_.get)))



  def uniform[A](s: Traversable[A]): FiniteDistribution[A] = {
    val prob = 1.0 / s.size
    val pmf  = (s map (Weighted(_, prob)))
    FiniteDistribution(pmf)
  }

  def rawUnif[A](s: Traversable[A]) =
    FiniteDistribution(s map ((Weighted(_, 1.0))))

  def unif[A](as: A*) = uniform(as.toSet)

  def empty[T]: FiniteDistribution[T] =
    FiniteDistribution[T](Vector(): Vector[Weighted[T]]) //Empty[T]

  def linearCombination[T](terms: Seq[(Double, FiniteDistribution[T])]) = {
    val scaled = for ((a, d) <- terms) yield d * a
    (scaled :\ (empty[T]))(_ ++ _)
  }

  def invFlatMap[S, T](f: S => FiniteDistribution[T],
                       support: Traversable[S]) = {
    val dists = support map ((s: S) => f(s))
    (dists :\ FiniteDistribution.empty[T])(_ ++ _)
  }

}

case class FiniteDistribution[T](pmf: Vector[Weighted[T]])
    extends AnyVal
    with ProbabilityDistribution[T] {

  /**
    * objects with positive probability (or bounded below by a threshhold)
    */
  def posmf(t: Double = 0.0) = pmf filter (_.weight > t)

  /**
    * total of the positive weights
    */
  def postotal(t: Double = 0.0): Double =
    ((posmf(t) map (_.weight))).sum

  /**
    * weight of the label.
    */
  def apply(label: T) = getsum(label)

  /**
    * l^1-norm
    */
  def norm: Double = (pmf map (_.weight.abs)).sum

  def flatten: FiniteDistribution[T] =
    FiniteDistribution(Weighted.flatten(pmf))

  def sort: FiniteDistribution[T] =
    FiniteDistribution(pmf.sortBy((wt) => 1 - wt.weight))

  def supp: Vector[T] = (Weighted.flatten(pmf) map (_.elem)).toVector

  def support = supp.toSet

//  lazy val decryFlat = FiniteDistribution(pmf, true, epsilon)

  def getsum(label: T) = (pmf filter (_.elem == label) map (_.weight)).sum

  def normalized(t: Double = 0.0): FiniteDistribution[T] = {
    val sc = 1.0 / postotal(t)
    FiniteDistribution(posmf(t) map (_.scale(sc)))
  }

  def prunedPMF(epsilon: Double) =
    flatten.pmf filter ((x) => math.abs(x.weight) > epsilon)

  def pruneMap[S](f: => (T => S), epsilon: Double) = {
    if (prunedPMF(epsilon).isEmpty) FiniteDistribution.empty[S]
    else FiniteDistribution(prunedPMF(epsilon)) map (f)
  }

  def filter(p: T => Boolean) =
    FiniteDistribution(pmf filter (wt => p(wt.elem)))

  def purge(epsilon: Double) = filter((t: T) => (apply(t) > epsilon))

  def *(sc: Double): FiniteDistribution[T] =
    FiniteDistribution(pmf map (_.scale(sc)))

  def ++(that: FiniteDistribution[T]): FiniteDistribution[T] = {
    FiniteDistribution(pmf ++ that.pmf)
  }

  def --(that: FiniteDistribution[T]) = (this ++ (that * (-1))).flatten

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

  def invmap[S](f: S => T, support: Traversable[S]): FiniteDistribution[S] = {
    val mem   = memo
    def memFn = (x: T) => mem.get(x).getOrElse(0.0)
    val pmf   = support.toVector map ((s: S) => Weighted(s, memFn(f(s))))
    FiniteDistribution(pmf)
  }

  def invmapOpt[S](f: S => Option[T],
                   support: Traversable[S]): FiniteDistribution[S] = {
    val mem   = memo
    def memFn = (x: T) => mem.get(x).getOrElse(0.0)
    val pmf =
      support.toVector map
        ((s: S) => Weighted(s, f(s).map(memFn).getOrElse(0)))
    FiniteDistribution(pmf)
  }

  def flatMap[S](f: T => FiniteDistribution[S]) = {
    // implicit val ls = FiniteDistribution.FiniteDistVec[S]
    // val distdist    = map(f)
    // distdist.expectation
    val newpmf =
      for (
        Weighted(x, p) <- pmf;
        Weighted(y, q) <- (f(x).pmf)
      ) yield Weighted(y, p * q)
    FiniteDistribution(newpmf)
  }

  def pickle =
    flatten.map((t: T) => t.toString).pmf.toList.map((w) => (w.elem, w.weight))

  def innerProduct(that: FiniteDistribution[T]) =
    (for (l <- supp) yield this(l) * that(l)).sum

  def dot(that: FiniteDistribution[T]) = innerProduct(that)

  private def preMemo: Map[T, Double] = {
    (for (Weighted(x, p) <- pmf) yield (x -> p)).toMap
  }

  def memo: Map[T, Double] = flatten.preMemo

  def total = (pmf map { case Weighted(x, p) => p }).sum

  import FiniteDistribution.random

  /**
    * next instance of a random variable with the given distribution
    */
  def next = Weighted.pick(posmf(), random.nextDouble * postotal())

  override def toString = {
    val sortedpmf = pmf.toSeq.sortBy(1 - _.weight)
    val terms = (for (Weighted(elem, wt) <- sortedpmf)
      yield (elem.toString + " : " + wt.toString + ", ")).foldLeft("")(_ + _)
    "[" + terms.dropRight(2) + "]"
  }

  override def conditioned(p: T => Boolean): FiniteDistribution[T] = {
    val filtered = filter(p)
    if (filtered.total > 0) filtered.normalized()
    else FiniteDistribution.empty[T]
  }

  override def condMap[S](f: T => Option[S]) = {
    val image = mapOpt(f)
    if (image.norm > 0) image.normalized() else FiniteDistribution.empty[S]
  }

  def entropy(elem: T) = -math.log(apply(elem)) / math.log(2)

  def entropyView =
    supp.map((x) => Weighted(x.toString, entropy(x))).sortBy(_.weight)

  def entropyVec =
    supp.map((x) => Weighted(x, entropy(x))).sortBy(_.weight)

  def split(groups: Int) = {
    val rand = new scala.util.Random

    pmf groupBy ((_) => rand.nextInt(groups - 1)) mapValues { x =>
      FiniteDistribution(x)
    }
  }

  def expectation(implicit ls: VectorSpace[T, Double]): T = {
    val wtdelems = for (Weighted(a, p) <- pmf) yield (p *: a)
    (wtdelems :\ ls.zero)(_ + _)
  }

  def integral(f: T => Double) =
    (pmf map { case Weighted(x, p) => p * f(x) }).sum

  /**
    * entropy feedback for the finite distribution to move in the direction of the base distribution,
    * however values outside support are ignored.
    * warning: should come after ++ to ensure implementation choice.
    *
    * @param baseweights
    */
  def rawfeedback(baseweights: T => Double,
                  damp: Double = 0.1,
                  strictness: Double = 1.0) = {
    val weights = (t: T) => math.pow(baseweights(t), strictness)
    val rawdiff = for (elem <- supp)
      yield
        (Weighted(elem, weights(elem) / (weights(elem) * damp + apply(elem))))
    val shift = rawdiff.map(_.weight).sum / (rawdiff.size)
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
    * @param baseweights
    */
  def smoothedFeedback(baseweights: T => Double,
                       damp: Double = 0.1,
                       strictness: Double = 1.0) = {
    val weights = (t: T) => math.pow(baseweights(t), strictness)
    val rawdiff = for (elem <- supp)
      yield
        (Weighted(elem,
                  weights(elem) * weights(elem) /
                    (weights(elem) * damp + apply(elem))))
    val shift = rawdiff.map(_.weight).sum / (rawdiff.size)
    val normaldiff = for (Weighted(pres, prob) <- rawdiff)
      yield Weighted(pres, prob - shift)
    FiniteDistribution(normaldiff)
  }

  /**
    * gradient w.r.t. inner product scaled by presentation weights,
    * perpendicular to the gradient (w.r.t. same inner product) of the "total weight" function.
    */
  def KLfeedback(baseweights: T => Double, strictness: Double = 1.0) = {
    val weights   = (t: T) => math.pow(baseweights(t), strictness)
    val rawdiff   = for (elem <- supp) yield (Weighted(elem, 1.0 / apply(elem)))
    val innerprod = rawdiff.map((x) => 1.0 / x.weight).sum // Sum(1/q))
    val normsq    = rawdiff.map((x) => 1.0 / weights(x.elem)).sum // Sum (1/p)
    val normaldiff = for (Weighted(pres, prob) <- rawdiff)
      yield Weighted(pres, prob - ((1 / weights(pres)) * innerprod / normsq))
    FiniteDistribution(normaldiff)
  }
}
