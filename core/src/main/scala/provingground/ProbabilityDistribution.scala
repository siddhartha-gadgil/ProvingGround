package provingground

import scala.util._

import scala.concurrent._

import scala.concurrent.ExecutionContext.Implicits.global

/**
  * A probability distribution, from which we can pick values at random (the only abstract method).
  * We can obtain a random variable from this, which is an iterator.
  */
trait ProbabilityDistribution[A] extends Any { pd =>

  /**
    * the next random value
    */
  def next: A

  def map[B](f: A => B): ProbabilityDistribution[B] =
    ProbabilityDistribution.Mapped(this, f)

  def flatMap[B](
      f: A => ProbabilityDistribution[B]): ProbabilityDistribution[B] =
    ProbabilityDistribution.FlatMapped(this, f)

  def randomVariable: Iterator[A] = new Iterator[A] {
    def hasNext = true

    def next = pd.next
  }

  /**
    * Find element satisfying predicate, timing out after given number of loops.
    */
  @annotation.tailrec
  private def find(p: A => Boolean, maxloops: Long): Option[A] =
    if (maxloops < 1) None
    else {
      val a = next
      if (p(a)) Some(a) else find(p, maxloops - 1)
    }

  /**
    * Find element satisfying predicate, runs concurrently, returning future
    */
  def findFut(p: A => Boolean, maxloops: Long) = Future(find(p, maxloops))

  /**
    * returns i.i.d. sample
    */
  def sample(n: Int) =
    FiniteDistribution.uniform((1 to n).toVector map ((_) => next)).flatten

  /**
    * mix in weighted distributions;
    * the mixed in distributions are called by name, so can depend on this distribution.
    */
  def <++>(components: => Vector[Weighted[ProbabilityDistribution[A]]]) =
    new ProbabilityDistribution.Mixture(this, components)

  /**
    * generates from the mixed in distribution with probability _weight_,
    * otherwise defaults to this distribution;
    * as the mixed in distribution is called by name, it may depend on the present one.
    */
  def <+>(mixin: => ProbabilityDistribution[A], weight: Double) =
    new ProbabilityDistribution.Mixin(this, mixin, weight)

  /**
    * generates from the mixed in optional valued distribution with probability `weight`,
    * otherwise, or if the optional returns None, defaults to this distribution;
    * the mixed in distribution is call by name, so may depend on this distribution.
    */
  def <+?>(mixin: => ProbabilityDistribution[Option[A]], weight: Double) =
    new ProbabilityDistribution.MixinOpt(this, mixin, weight)

/*  def |++|(components: => Seq[(ProbabilityDistribution[A], Double)]) =
    new ProbabilityDistribution.Mixture(this, components.toVector map ((xy) => Weighted(xy._1, xy._2)))*/
}

object ProbabilityDistribution {
  val rand = new Random

  @annotation.tailrec
  def chooseOpt[A](
      value: Double,
      pmf: Traversable[Weighted[A]]
  ): Option[A] =
    if (pmf.isEmpty) None
    else if (value < pmf.head.weight) Some(pmf.head.elem)
    else chooseOpt(pmf.head.weight - value, pmf.tail)

  class Mixture[A](base: ProbabilityDistribution[A],
                   components: => Vector[Weighted[ProbabilityDistribution[A]]])
      extends ProbabilityDistribution[A] {

    lazy val first = base

    lazy val rest = components

    lazy val dists = first +: (rest map (_.elem))

    lazy val qs = rest map (_.weight)

    lazy val ps = (1.0 - qs.sum) +: qs

    def next =
      chooseOpt(rand.nextDouble, components) map (_.next) getOrElse (base.next)
  }

  class Mixin[A](base: ProbabilityDistribution[A],
                 mixin: => ProbabilityDistribution[A],
                 weight: Double)
      extends ProbabilityDistribution[A] {
    lazy val first = base

    lazy val second = mixin

    lazy val q = weight

    def next =
      if (rand.nextDouble < weight) mixin.next else base.next
  }

  class MixinOpt[A](base: ProbabilityDistribution[A],
                    mixin: => ProbabilityDistribution[Option[A]],
                    weight: Double)
      extends ProbabilityDistribution[A] {

        lazy val first = base

        lazy val second = mixin

        lazy val q = weight

    def next =
      if (rand.nextDouble < weight) mixin.next.getOrElse(base.next)
      else base.next
  }

  case class Mapped[A, B](base: ProbabilityDistribution[A], f: A => B)
      extends ProbabilityDistribution[B] {
    def next = f(base.next)
  }

  case class FlatMapped[A, B](
      base: ProbabilityDistribution[A], f: A => ProbabilityDistribution[B])
      extends ProbabilityDistribution[B] {
    def next = f(base.next).next
  }
}
