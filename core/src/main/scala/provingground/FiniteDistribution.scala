package provingground

import scala.annotation._
import scala.util._

import scala.language.implicitConversions

import Collections._

/**
 * Finite distributions, often supposed to be probability distributions, but may also be tangents to this or intermediates.
 *
 * @param pmf probability mass function, may have same object split.
 *
 * @param epsilon cutoff below which some methods ignore objects. should be very small to allow for split objects.
 */
sealed trait FiniteDistribution[T] extends ProbabilityDistribution[T] with LabelledVector[T]{
  val pmf: Traversable[Weighted[T]]

  val injective: Boolean

  val epsilon: Double

  /**
   * flatten distribution collating elements.
   */
  def flatten : FiniteDistribution[T]

  /**
   * add together all probabilities for
   */
  def getsum(label : T): Double

  def filter(p : T => Boolean): FiniteDistribution[T]


  /**
   * normalized so all probabilities are positive and the total is 1.
   */
  def normalized(t : Double = 0.0) : FiniteDistribution[T]

  /**
   * add another distribution without normalizing
   */
  def ++(that: FiniteDistribution[T]) : FiniteDistribution[T]

  /**
   * scale the distribution
   */
  def *(sc: Double) : FiniteDistribution[T]




  lazy val elems = pmf map (_.elem)

  /**
   * support of the distribution.
   */
  lazy val support = (pmf filter (_.weight > epsilon) map (_.elem)).toSet


  /**
   * A more efficient support
   */
  lazy val supp = if (injective) elems else flatten.elems

  /**
   * l^1-norm
   */
  lazy val norm = (pmf.toSeq map (_.weight.abs)).sum

  /**
   * next instance of a random variable with the given distribution
   */
  def next = Weighted.pick(pmf, random.nextDouble)

  /**
   * get weight.
   */
  def get(label: T) = Some(getsum(label))



  /**
   * weight of the label.
   */
  def apply(label: T) = getsum(label)

  /**
   * distribution as set with collation
   */
  def flatdist = supp map ((l) => Weighted(l, getsum(l)))





  /**
   * probability of elements satisfying a predicate
   */
  def prob(p: T => Boolean) = {
    (supp filter p map getsum).sum
  }

  /**
   * weights of the quotient distribution, for feedback.
   * Note that we cannot use equality in the `quotient' distribution as quotienting actually involves picking a set.
   */
  def weight(equiv: (T, T) => Boolean)(t: T) = (supp filter (equiv(t, _)) map getsum).sum

  /**
   * objects with positive probability (or bounded below by a threshhold)
   */
  def posmf(t : Double = 0.0) = flatdist filter (_.weight > t)

  /**
   * total of the positive weights
   */
  def postotal(t : Double = 0.0) = ((posmf(t).toSeq map (_.weight))).sum ensuring (_ > 0)


  /**
   * add weighted element without normalizing
   */
  def +(elem: T , prob : Double) =
    this ++ FiniteDistribution(Set(Weighted(elem, prob)))


  /**
   * subtract distribution
   */
  def --(that: FiniteDistribution[T]) = this ++ (that * (-1))

  /**
   * map distribution without normalizing.
   */
  def map[S](f: T => S) : FiniteDistribution[S]

  def mapOpt[S](f: T => Option[S]) : FiniteDistribution[S]

  def invmap[S](f: S => T, support: Traversable[S]): FiniteDistribution[S]

  def invmapOpt[S](f: S => Option[T], support: Traversable[S]): FiniteDistribution[S]

  def expectation(implicit ls : LinearStructure[T]) : T = {
    val wtdelems = for (Weighted(a, p) <- pmf) yield ls.mult(p, a)
    (wtdelems :\ ls.zero)(ls.sum)
  }

  def flatMap[S](f: T => FiniteDistribution[S]) = {
    implicit val ls = FiniteDistribution.FiniteDistVec[S]
    val distdist = map(f)
    distdist.expectation
  }



  def purge(epsilon: Double) = filter((t: T) => (apply(t) > epsilon))

  /**
   * entropy feedback for the finite distribution to move in the direction of the base distribution,
   * however values outside support are ignored.
   *
   * @param baseweights
   */
  def feedback(baseweights: T => Double, damp : Double = 0.1) ={
    val rawdiff = for (elem <- supp) yield (Weighted(elem, baseweights(elem)/(baseweights(elem)* damp + apply(elem))))
    val shift = rawdiff.map(_.weight).sum/(rawdiff.size)
    val normaldiff = for (Weighted(pres, prob)<-rawdiff) yield Weighted(pres, prob - shift)
    FiniteDistribution(normaldiff, epsilon)
  }

  override def toString = {
    val sortedpmf = pmf.toSeq.sortBy(1 - _.weight)
    val terms = (for (Weighted(elem, wt) <- sortedpmf) yield (elem.toString + " : "+ wt.toString+ ", ")).foldLeft("")(_+_)
    "[" + terms.dropRight(2) + "]"
  }

  def entropy(elem: T) = -math.log(apply(elem))/math.log(2)

  def entropyView = supp.toList.map((x)=>Weighted(x.toString, entropy(x))).sortBy(_.weight)
}


case class FiniteDistributionSet[T](pmf: Set[Weighted[T]], epsilon: Double = 0.0) extends FiniteDistribution[T]{
  val injective = true

  def flatten  = this

  def getsum(label : T) = (pmf.toSeq filter (_.elem == label) map (_.weight)).sum

  def normalized(t : Double = 0.0) = FiniteDistributionSet((posmf(t) map (_.scale(1.0/postotal(t)))).toSet)

  def filter(p : T => Boolean) = FiniteDistributionSet(pmf filter (wt => p(wt.elem)))

  def *(sc: Double) = FiniteDistributionSet(pmf map (_.scale(sc)))

  def ++(that: FiniteDistribution[T]) = {
    val combined = (for (k <- support union that.support) yield Weighted(k, apply(k) + that(k)))
    FiniteDistributionSet(combined, epsilon)
  }


  override def map[S](f: T => S) = {
    val newpmf = for (Weighted(elem, wt) <- pmf.toSeq) yield Weighted(f(elem), wt)
    FiniteDistributionSet(Weighted.flatten(newpmf).toSet, epsilon)
  }

  def mapOpt[S](f: T => Option[S]) = {
    val newpmf = for (Weighted(elem, wt) <- pmf.toSeq;
      felem <- f(elem)) yield Weighted(felem, wt)
    FiniteDistributionSet(Weighted.flatten(newpmf).toSet, epsilon)
  }

  def invmap[S](f: S => T, support: Traversable[S]) = {
    val pmf = support.toSet map ((s: S) => Weighted(s, apply(f(s))))
    FiniteDistributionSet(pmf)
  }

  def invmapOpt[S](f: S => Option[T], support: Traversable[S]) = {
    val pmf = support.toSet map ((s: S) => Weighted(s, f(s).map(apply).getOrElse(0)))
    FiniteDistributionSet(pmf)
  }

  /** quotient by an equivalence relation
   *
   */
  def quotient(equiv: (T, T) => Boolean) = {
    val supp = transversal(support.toList, equiv)
    val quotpmf = for (x <- supp) yield Weighted(x, prob(equiv(x, _)))
    FiniteDistributionSet(quotpmf.toSet, epsilon)
  }
}

object FiniteDistribution{
  def apply[T](pmf: Traversable[Weighted[T]], epsilon: Double = 0.0) : FiniteDistribution[T] = FiniteDistributionSet(pmf.toSet, epsilon)

  def uniform[A](s: Traversable[A]) = {
    val prob = 1.0/s.size
    val pmf = (s map (Weighted(_, prob))).toSet
    FiniteDistribution(pmf)
  }

  def unif[A](as: A*) = uniform(as.toSet)

  def empty[T] : FiniteDistribution[T] = Empty[T]

  case class Empty[T]() extends FiniteDistribution[T]{
    val pmf: Traversable[Weighted[T]] = Traversable.empty

  val injective: Boolean = true

  val epsilon: Double = 0

  /**
   * flatten distribution collating elements.
   */
  def flatten : FiniteDistribution[T] = this

  /**
   * add together all probabilities for
   */
  def getsum(label : T): Double = 0

  def filter(p : T => Boolean): FiniteDistribution[T] = this


  /**
   * normalized so all probabilities are positive and the total is 1.
   */
  def normalized(t : Double = 0.0) : FiniteDistribution[T] =
    {assert(false, "can only normailze non-empty distributions")
      ???
    }

  /**
   * add another distribution without normalizing
   */
  def ++(that: FiniteDistribution[T]) : FiniteDistribution[T] = that

  /**
   * scale the distribution
   */
  def *(sc: Double) : FiniteDistribution[T] = this

  /** As seen from class Empty, the missing signatures are as follows.
   *  *  For convenience, these are usable as stub implementations.  */
  def invmap[S](f: S => T,support: Traversable[S]): provingground.FiniteDistribution[S] = empty

  def invmapOpt[S](f: S => Option[T],support: Traversable[S]): provingground.FiniteDistribution[S] = empty

  def map[S](f: T => S): provingground.FiniteDistribution[S] = empty

  def mapOpt[S](f: T => Option[S]): provingground.FiniteDistribution[S] = empty


  }

  def linearCombination[T](terms: Seq[(Double, FiniteDistribution[T])]) = {
    val scaled = for ((a, d) <- terms) yield d * a
    (scaled :\ (empty[T]))(_ ++ _)
  }

  def invFlatMap[S, T](f: S => FiniteDistribution[T], support: Traversable[S]) = {
    val dists = support map ((s: S) => f(s))
    (dists :\ FiniteDistribution.empty[T])(_ ++ _)
  }

  implicit def FiniteDistVec[T] = LinearStructure[FiniteDistribution[T]](FiniteDistribution.empty, _++_, (w, d) => d * w)
}
