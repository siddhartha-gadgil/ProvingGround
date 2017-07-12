package provingground

import scala.util._

import scala.concurrent._

import scala.concurrent.ExecutionContext.Implicits.global

import spire.algebra._
// import spire.math._
import spire.implicits._

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

  def product[B](that: ProbabilityDistribution[B]) =
    ProbabilityDistribution.Product(this, that)

  def fibProduct[Q, B](quot: A => Q, fibers: Q => ProbabilityDistribution[B]) =
    ProbabilityDistribution.FiberProduct(this, quot, fibers)

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
  // def sample(n: Int) =
  //   FiniteDistribution.uniform((1 to n).toVector map ((_) => next)).flatten

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
    new ProbabilityDistribution.Mixin(pd, mixin, weight)

  /**
    * generates from the mixed in optional valued distribution with probability `weight`,
    * otherwise, or if the optional returns None, defaults to this distribution;
    * the mixed in distribution is call by name, so may depend on this distribution.
    */
  def <+?>(mixin: => ProbabilityDistribution[Option[A]], weight: Double) =
    new ProbabilityDistribution.MixinOpt(this, mixin, weight)

  def conditioned(p: A => Boolean): ProbabilityDistribution[A] =
    ProbabilityDistribution.Conditioned(this, p)

  def condMap[B](f: A => Option[B]): ProbabilityDistribution[B] =
    ProbabilityDistribution.CondMapped(this, f)

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

    lazy val weightedDists = dists.zip(ps)

    override def conditioned(p: A => Boolean) =
      new Mixture(base.conditioned(p), components map {
        case Weighted(d, w) => Weighted(d.conditioned(p), w)
      })

    override def condMap[B](f: A => Option[B]) =
      new Mixture(base.condMap(f), components map {
        case Weighted(d, w) => Weighted(d.condMap(f), w)
      })

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

    lazy val p = 1 - q

    override def conditioned(p: A => Boolean) =
      base.conditioned(p) <+> (mixin.conditioned(p), weight)

    override def condMap[B](p: A => Option[B]) =
      base.condMap(p) <+> (mixin.condMap(p), weight)

    def next =
      if (rand.nextDouble < weight) mixin.next else base.next
  }

  class MixinOpt[A](base: ProbabilityDistribution[A],
                    mixin: => ProbabilityDistribution[Option[A]],
                    weight: Double)
      extends ProbabilityDistribution[A] {

    lazy val first = base

    lazy val second = mixin

    val q = weight

    val p = 1 - q

    override def conditioned(p: A => Boolean) =
      base.conditioned(p) <+?>
        (mixin.conditioned((oa) => oa.map(p).getOrElse(false)), weight)

    override def condMap[B](f: A => Option[B]) =
      base.condMap(f) <+?>
        (mixin.condMap((oa) => oa.map(f)), weight)

    def next =
      if (rand.nextDouble < weight) mixin.next.getOrElse(base.next)
      else base.next
  }

  case class Mapped[A, B](base: ProbabilityDistribution[A], f: A => B)
      extends ProbabilityDistribution[B] {
    def next = f(base.next)
  }

  case class FlatMapped[A, B](base: ProbabilityDistribution[A],
                              f: A => ProbabilityDistribution[B])
      extends ProbabilityDistribution[B] {
    def next = f(base.next).next
  }

  case class FiberProduct[A, Q, B](base: ProbabilityDistribution[A],
                                   quotient: A => Q,
                                   fibers: Q => ProbabilityDistribution[B])
      extends ProbabilityDistribution[(A, B)] {
    def next = {
      val a = base.next
      (a, fibers(quotient(a)).next)
    }
  }

  case class Product[A, B](first: ProbabilityDistribution[A],
                           second: ProbabilityDistribution[B])
      extends ProbabilityDistribution[(A, B)] {
    def next = (first.next, second.next)
  }

  case class Conditioned[A](base: ProbabilityDistribution[A], p: A => Boolean)
      extends ProbabilityDistribution[A] {
    def next: A = {
      val x = base.next
      if (p(x)) x else next // Warning: unsafe, if there are no elements satisfying the condition this hangs.
    }
  }

  // The distributions below have total measure different from 1

  case class CondMapped[A, B](base: ProbabilityDistribution[A],
                              f: A => Option[B])
      extends ProbabilityDistribution[B] {
    def next: B = {
      f(base.next)
        .getOrElse(next) // Warning: unsafe, if there are no elements satisfying the condition this hangs.
    }
  }

  case class Scaled[A](base: ProbabilityDistribution[A], scale: Double)
      extends ProbabilityDistribution[A] {
    def next = base.next
  }

  case class Sum[A](first: ProbabilityDistribution[A],
                    second: ProbabilityDistribution[A])
      extends ProbabilityDistribution[A] {
    def next = if (rand.nextDouble < 0.5) first.next else second.next
  }

  case class Flattened[A](base: ProbabilityDistribution[Option[A]])
      extends ProbabilityDistribution[A] {
    def next: A = {
      val x = base.next
      x.getOrElse(next) // Warning: unsafe, if there are no elements satisfying the condition this hangs.
    }
  }

  implicit def vs[T]: VectorSpace[ProbabilityDistribution[T], Double] =
    new VectorSpace[ProbabilityDistribution[T], Double] {
      def negate(x: ProbabilityDistribution[T]) =
        Scaled(x, -1)

      val zero = FiniteDistribution.empty[T]

      def plus(x: ProbabilityDistribution[T], y: ProbabilityDistribution[T]) =
        Sum(x, y)

      def timesl(r: Double, x: ProbabilityDistribution[T]) = Scaled(x, r)

      implicit def scalar: Field[Double] = Field[Double]
    }

}
