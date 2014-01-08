package provingGround

import scala.annotation._
import scala.util._

object Collections{
    trait InfSeq[T]{
      val head: T
      val tail: InfSeq[T]
        
      @tailrec final def apply(n: Int): T = {require(n>=0); if (n==0) head else tail.apply(n-1)}
        
      @tailrec final def drop(n: Int): InfSeq[T] = if (n<1) this else tail.drop(n-1)
        
      def take(n: Int) = for (i<-0 to (n-1)) yield (apply(i))
        
      def #::(head: T) :InfSeq[T] = InfSeq.cons(head, this)
      
      def map[S](f: T =>S): InfSeq[S] = InfSeq.cons(f(head), tail.map(f))
      
      /*
       * This is a diagonal, not a true flatMap, so works only in some cases: approximations, independent random variables.
       */
      def flatMap[S](f: T => InfSeq[S]): InfSeq[S] = InfSeq.diagonal(map(f))
        
    }
    
    object InfSeq{
        class fromFunction[T](seq: => (Int => T)) extends InfSeq[T]{
        	lazy val head = seq(0)
            lazy val tail = new fromFunction((n: Int) => seq(n+1))
        }        
        
        def cons[T](head: T, tail: => InfSeq[T]) = {
            def seq(n: Int) = if (n<1) head else tail(n-1)
            new fromFunction(seq)
        }
        
        def apply[T](seq: => (Int => T)) = new fromFunction(seq)
        
        def diagonal[T](seq: InfSeq[InfSeq[T]]): InfSeq[T] = cons(seq.head.head, diagonal(seq.tail.tail))
    }
    
    trait ApproxSeq[T] extends InfSeq[T]{
      val stable: Boolean
      
      val tail: ApproxSeq[T]
      
      override def #::(head: T) = ApproxSeq.cons(head, this)
      
      override def map[S](f: T =>S): ApproxSeq[S] = ApproxSeq.cons(f(head), tail.map(f))
    }

    object ApproxSeq{
      case class Const[T](t: T) extends ApproxSeq[T]{
        lazy val head = t
        val stable = true
        
        lazy val tail = this
      }
      
      class Prepend[T](h:  => T, t: => ApproxSeq[T], val stable: Boolean = false) extends ApproxSeq[T]{
        lazy val head = h
        lazy val tail = t
        
      }
      
      def cons[T](h: => T, t: => ApproxSeq[T], stable: Boolean = false) = new Prepend(h, t, stable)
      
      case class Dyn[T](init: T, dyn: T=> T, halt: T => Boolean) extends ApproxSeq[T]{
        lazy val head = init
        
        lazy val stable = halt(init)
        
        lazy val tail : ApproxSeq[T] = if (stable) Const(init) else Dyn(dyn(init), dyn, halt)
      }
      
      def diagonal[T](seq: ApproxSeq[ApproxSeq[T]]): ApproxSeq[T] = cons(seq.head.head, diagonal(seq.tail.tail), seq.stable && seq.head.stable)
    }
    

    trait ProbabilityDistribution[T]{self => 
      def next: T 
      
      def iid : InfSeq[T] = InfSeq((_ : Int) => next)
      
      def map[S](f: T => S) = ProbabilityDistribution(f(next))
    }
    
    object ProbabilityDistribution{
      def apply[T](nxt: => T) = new ProbabilityDistribution[T]{
        def next = nxt
      }
    }
    
    class Uniform extends ProbabilityDistribution[Double]{
      lazy val rand = new Random
      
      def next = rand.nextDouble
    }
    
    case class Weighted[T](elem: T, weight: Double)
    
    object Weighted{
      @tailrec final def pick[T](dist: IndexedSeq[Weighted[T]], t: Double): T = if (t - dist.head.weight <0) dist.head.elem 
    		  	else pick(dist.tail, t- dist.head.weight)  
    }
    
    class FiniteDistribution[T](pmf: IndexedSeq[Weighted[T]]) extends ProbabilityDistribution[T]{
      lazy val rand = new Random
      
      def next = Weighted.pick(pmf, rand.nextDouble)
    }
}