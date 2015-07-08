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
trait FiniteDistribution[T] extends ProbabilityDistribution[T] with LabelledVector[T]{
  val pmf: Traversable[Weighted[T]]
  
  lazy val elems = pmf map (_.elem)

  val injective: Boolean

  val epsilon: Double

  /**
   * support of the distribution.
   */
  lazy val support = (pmf filter (_.weight > epsilon) map (_.elem)).toSet


  /**
   * l^1-norm
   */
  lazy val norm = (pmf.toSeq map (_.weight.abs)).sum

  /**
   * next instance of a random variable with the given distribution
   */
  def next = Weighted.pick(pmf.toSeq, random.nextDouble)

  /**
   * get weight, not collapsing, unsafe.
   */
  @deprecated("use getsum or apply", "1/8/2014")
  def get(label: T) = pmf find (_.elem == label) map (_.weight)

  /**
   * add together all probabilities for
   */
  def getsum(label : T) = (pmf.toSeq filter (_.elem == label) map (_.weight)).sum

  /**
   * weight of the label.
   */
  def apply(label: T) = getsum(label)

  /**
   * distribution as set with collation
   */
  def flatdist = support map ((l) => Weighted(l, getsum(l)))

  /**
   * flatten distribution collating elements.
   */
  def flatten : FiniteDistribution[T]



  /**
   * probability of elements satisfying a predicate
   */
  def prob(p: T => Boolean) = {
    (support filter p map getsum).sum
  }

  /**
   * weights of the quotient distribution, for feedback.
   * Note that we cannot use equality in the `quotient' distribution as quotienting actually involves picking a set.
   */
  def weight(equiv: (T, T) => Boolean)(t: T) = (support filter (equiv(t, _)) map getsum).sum

  /**
   * objects with positive probability (or bounded below by a threshhold)
   */
  def posmf(t : Double = 0.0) = flatdist filter (_.weight > t)

  /**
   * total of the positive weights
   */
  def postotal(t : Double = 0.0) = ((posmf(t).toSeq map (_.weight))).sum ensuring (_ > 0)

  /**
   * normalized so all probabilities are positive and the total is 1.
   */
  def normalized(t : Double = 0.0) : FiniteDistribution[T]

  /**
   * scale the distribution
   */
  def *(sc: Double) : FiniteDistribution[T]

  /**
   * add weighted element without normalizing
   */
  def +(elem: T , prob : Double) =
    this ++ FiniteDistribution.fromWts(elem -> prob)

  /**
   * add another distribution without normalizing
   */
  def ++(that: FiniteDistribution[T]) : FiniteDistribution[T]

  /**
   * subtract distribution
   */
  def --(that: FiniteDistribution[T]) : FiniteDistribution[T]

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

  def filter(p : T => Boolean): FiniteDistribution[T]

  def purge(epsilon: Double) = filter((t: T) => (apply(t) > epsilon))

  /**
   * entropy feedback for the finite distribution to move in the direction of the base distribution,
   * however values outside support are ignored.
   *
   * @param baseweights
   */
  def feedback(baseweights: T => Double, damp : Double = 0.1) ={
    val rawdiff = for (elem <- support) yield (Weighted(elem, baseweights(elem)/(baseweights(elem)* damp + apply(elem))))
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

  def entropyView = flatten.support.toList.map((x)=>Weighted(x.toString, entropy(x))).sortBy(_.weight)
}


case class FiniteDistributionSet[T](pmf: Set[Weighted[T]], epsilon: Double = 0.0) extends FiniteDistribution[T]{
  val injective = true

  def flatten  = FiniteDistributionSet(flatdist)

  def normalized(t : Double = 0.0) = FiniteDistributionSet((posmf(t) map (_.scale(1.0/postotal(t)))).toSet)

  def filter(p : T => Boolean) = FiniteDistributionSet(pmf filter (wt => p(wt.elem)))

  def *(sc: Double) = FiniteDistributionSet(pmf map (_.scale(sc)))

  def ++(that: FiniteDistribution[T]) = {
    val combined = (for (k <- support union that.support) yield Weighted(k, apply(k) + that(k)))
    FiniteDistributionSet(combined, epsilon)
  }

  def --(that: FiniteDistribution[T]) = {
    val combined = (for (k <- support union that.support) yield Weighted(k, apply(k) - that(k)))
    FiniteDistributionSet(combined)
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

  def empty[T] = FiniteDistribution[T]((Set.empty : Set[Weighted[T]]))

  def linearCombination[T](terms: Seq[(Double, FiniteDistribution[T])]) = {
    val scaled = for ((a, d) <- terms) yield d * a
    (empty[T] /: scaled)(_ ++ _)
  }

/*  @deprecated("use fromWts", "tmp")
  def apply[T](tws : (T, Double)*) : FiniteDistribution[T] = {
    FiniteDistribution(
        Weighted.flatten(
            tws.toSeq map ((tw : (T, Double)) => Weighted(tw._1, tw._2))
            ).toSet
            )
  }*/

  def fromWts[T](tws : (T, Double)*) : FiniteDistribution[T] = {
    FiniteDistribution(
        Weighted.flatten(
            tws.toSeq map ((tw : (T, Double)) => Weighted(tw._1, tw._2))
            ).toSet
            )
  }

  implicit def FiniteDistVec[T] = LinearStructure[FiniteDistribution[T]](FiniteDistribution.empty, _++_, (w, d) => d * w)
}
