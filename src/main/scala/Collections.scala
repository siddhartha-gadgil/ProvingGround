package provingGround

import scala.annotation._
import scala.util._

import scala.language.implicitConversions

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
      
      def flatMap[S](f: T => ProbabilityDistribution[S]) = ProbabilityDistribution(f(next).next)
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
    
    case class Weighted[T](elem: T, weight: Double){
      def scale(s: Double) = Weighted(elem, weight * s)
    }
    
    object Weighted{
      @tailrec final def pick[T](dist: Seq[Weighted[T]], t: Double): T = if (t - dist.head.weight <0) dist.head.elem 
    		  	else pick(dist.tail, t- dist.head.weight) 
      def sumWeigths[T](seq: Seq[Weighted[T]]) = seq.map(_.weight).sum  	
      
      private def gather[T](seq: Seq[Weighted[T]]) = Weighted(seq.head.elem, Weighted.sumWeigths(seq))
  
      def flatten[T](seq: Seq[Weighted[T]]) = seq groupBy (_.elem) map (_._2) map (gather(_))
      
      def combine[T](seqs: Seq[Weighted[T]]*) = flatten(seqs.flatten)
    }
    
    class FiniteDistribution[T](pmf: Seq[Weighted[T]]) extends ProbabilityDistribution[T] with LabelledArray[T, Double]{    
      lazy val support = pmf map (_.elem)
        
      lazy val rand = new Random
      
      def next = Weighted.pick(pmf, rand.nextDouble)
      
      def get(label: T) = pmf find (_.elem == label) map (_.weight)
    
      private def posmf = pmf filter (_.weight > 0)
      
      private def postotal = ((posmf map (_.weight)) :\ 0.0)(_ + _) ensuring (_ > 0)
      
      def normalized = new FiniteDistribution(posmf map (_.scale(1.0/postotal)))
    }
    
    class LinearStructure[A](sum: (A, A) => A, mult : (Double, A) => A)
    
    trait LabelledArray[L,T] extends Traversable[T]{
      val support: Traversable[L]
      
      def foreach[U](f: T => U): Unit = support.map(get(_).get).foreach(f)
      
      def get(label: L): Option[T]
        
      def apply(label: L)(implicit zero: T)  = get(label).getOrElse(zero)
    }
    
    implicit val zero: Double = 0
    
    case class ArrayMap[L, T](coords: Map[L, T]) extends LabelledArray[L, T]{
      lazy val support = coords.keys  
        
      def get(label: L) = coords.get(label)
    }
    
    implicit class Shift[B](val shift: (B, B, Double) => B)
    
    def update[B](init: B, tangent: B, epsilon: Double)(implicit s: Shift[B]) = s.shift(init, tangent, epsilon)
    
    trait DiffBleFunction[A, B] extends (A => B){self =>
    	def grad(a: A) : B => A   
    	
    	/**
    	 * Composition f *: g is f(g(_))
    	 */
    	def *:[C](that: DiffBleFunction[B, C]) = andThen(that)
    	
    	def andThen[C](that: DiffBleFunction[B, C]): DiffBleFunction[A, C] = DiffBleFunction((a: A) => that(this(a)))(
    													(a: A) => 
    	  													(c: C) =>
    	  													  grad(a)(that.grad(this(a))(c)))
    	}
    
    object DiffBleFunction{
      def apply[A, B](f: A => B)(grd: A => (B => A)) = new DiffBleFunction[A, B]{
        def apply(a: A) = f(a)
        
        def grad(a: A) = grd(a)
      }
    }
    
    
    trait LearningSystem[I, P, O] extends DiffBleFunction[(I, P), O]{
        def apply(inp: I, param: P): O = this.apply((inp,param))
        
        def update(feedback: O, inp: I, param: P, epsilon: Double)(implicit s: Shift[P]) = s.shift(param, grad(inp, param)(feedback)._2, epsilon)

//        def stack[Q, X](that: LearningSystem[O, Q, X]) = asLearner(this andThen (that)
    }
    
    
    implicit def asLearner[I, P, O](f: DiffBleFunction[(I, P), O]): LearningSystem[I, P, O] = new LearningSystem[I, P, O]{
    	def apply(a: (I, P)) = f(a)
    	
    	def grad(a: (I, P)) = f.grad(a)
    }
    
}

