package provingground

import scala.util._

import scala.concurrent._

import scala.concurrent.ExecutionContext.Implicits.global

trait ProbabilityDistribution[A] extends Any {
  def next: A  
  
  def map[B](f: A =>B) : ProbabilityDistribution[B] = 
    ProbabilityDistribution.Mapped(this, f)
  
  def flatMap[B](f: A => ProbabilityDistribution[B]) : ProbabilityDistribution[B] = 
    ProbabilityDistribution.FlatMapped(this, f)  
    
  def hasNext: Boolean = true
  
  @annotation.tailrec
  private def findRaw(p: A => Boolean) : A =
  {
    val a = next
    if (p(a)) a else findRaw(p)
  }
  
  def find(p: A => Boolean) = Future(findRaw(p))
  
  def pick = next
  
  def sample(n: Int) = 
    FiniteDistribution.uniform(
        (1 to n).toVector map ((_) => next)).flatten
  
  def <++>(components: => Vector[Weighted[ProbabilityDistribution[A]]]) =
    new ProbabilityDistribution.Mixture(this, components)
 
  def <+>(mixin : => ProbabilityDistribution[A], weight: Double) =
    new ProbabilityDistribution.Mixin(this, mixin, weight)
  
  def <+?>(mixin : => ProbabilityDistribution[Option[A]], weight: Double) =
    new ProbabilityDistribution.MixinOpt(this, mixin, weight)
  
  def |++|(components: => Seq[(ProbabilityDistribution[A], Double)]) =
    new ProbabilityDistribution.Mixture(this, components.toVector map ((xy) => Weighted(xy._1, xy._2)))
}

object ProbabilityDistribution{
  val rand = new Random
  
  @annotation.tailrec
  def chooseOpt[A](
      value : Double, 
      pmf: Traversable[Weighted[A]] 
      ) : Option[A] =
    if (pmf.isEmpty) None 
    else 
      if (value< pmf.head.weight) Some(pmf.head.elem)
      else chooseOpt (pmf.head.weight - value, pmf.tail)
  
  class Mixture[A](
      base: ProbabilityDistribution[A], 
      components: => Vector[Weighted[ProbabilityDistribution[A]]]) extends ProbabilityDistribution[A]{
        def next = 
          chooseOpt(rand.nextDouble, components) map (_.next) getOrElse (base.next)
      }
  
  class Mixin[A](
      base: ProbabilityDistribution[A], 
      mixin: => ProbabilityDistribution[A],
      weight: Double) extends ProbabilityDistribution[A]{
    def next = 
      if (rand.nextDouble < weight) mixin.next else base.next
  }
  
  class MixinOpt[A](
      base: ProbabilityDistribution[A], 
      mixin: => ProbabilityDistribution[Option[A]],
      weight: Double) extends ProbabilityDistribution[A]{
    def next = 
      if (rand.nextDouble < weight) mixin.next.getOrElse(next) 
      else base.next
  }
  
  case class Mapped[A, B](
      base: ProbabilityDistribution[A], 
      f: A => B) extends ProbabilityDistribution[B]{
    def next = f(base.next)
  }
  
  case class FlatMapped[A, B](
      base: ProbabilityDistribution[A], 
      f: A => ProbabilityDistribution[B]) extends ProbabilityDistribution[B]{
    def next = f(base.next).next
  }
}