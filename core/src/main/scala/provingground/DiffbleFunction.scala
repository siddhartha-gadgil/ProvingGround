package provingground

import scala.annotation._
import scala.util._

import scala.language.implicitConversions 

import provingground.Collections._

	trait DiffbleFunction[A, B]{self =>
      def apply(a: A): B
    
    	def grad(a: A) : B => A   
    	 
    	/**
    	 * Composition f *: g is f(g(_))
    	 */
    	def *:[C](that: => DiffbleFunction[B, C]) = andthen(that)
    	
    	def andthen[C](that: => DiffbleFunction[B, C]): DiffbleFunction[A, C] = DiffbleFunction((a: A) => that(this(a)))(
    													(a: A) => 
    	  													(c: C) =>
    	  													  grad(a)(that.grad(this(a))(c)))
    	/**
    	 * Conjugate that by this.
    	 */
    	def ^:(that: B => B) = (a : A) => grad(a)(that(apply(a)))
    	
    	/**
    	 * post-compose by the gradient of this, for instance for a feedback.
    	 */
    	def **:(that: A => B) = (a : A) => grad(a)(that(a))
    	
    	def oplus[C, D](that : DiffbleFunction[C, D]) = {
    		def func(ac: (A, C)) = (this(ac._1), that(ac._2))
    	
    		def grad(ac: (A, C))(bd : (B, D)) = (self.grad(ac._1)(bd._1), that.grad(ac._2)(bd._2))
    	
      	DiffbleFunction(func)(grad)
      }
	}

    
    
    object DiffbleFunction{
      def apply[A, B](f: => A => B)(grd: => A => (B => A)) = new DiffbleFunction[A, B]{
        def apply(a: A) = f(a)
        
        def grad(a: A) = grd(a)
      }
      
      def selfadj[A](f: => A => A) = apply(f)((x: A) => (v: A) => f(v)) 
      
      def id[A] = apply((a: A) => a)((a: A) => (b: A) => b)
      
      def incl1[A, B](implicit lsB: LinearStructure[B]) = apply((a: A) => (a, lsB.zero))((a: A) => (x: (A, B)) => x._1)
      
      def proj1[A, B](implicit lsB: LinearStructure[B]) = apply((x: (A, B)) => x._1)((x) => (a) => (a, lsB.zero))
      
      def incl2[A, B](implicit lsA: LinearStructure[A]) = apply((b: B) => (lsA.zero, b))((b: B) => (x: (A, B)) => x._2)
      
      def proj2[A, B](implicit lsA: LinearStructure[A]) = apply((x: (A, B)) => x._2)((x) => (b) => (lsA.zero, b))
      
      def block[A : LinearStructure, 
        B : LinearStructure, 
        C : LinearStructure, 
        D : LinearStructure](f : DiffbleFunction[A, C], g: DiffbleFunction[B, D]) = {
            val add = vsum[DiffbleFunction[(A, B), (C, D)]]
            
            val p1 = proj1[A, B]
            val p2 = proj2[A, B]
            
            val i1 = incl1[C, D]
            val i2 = incl2[C, D]
            
            add(p1 andthen f andthen i1, p2 andthen g andthen i2)
      }
      
      def scprod[V](implicit ls: LinearStructure[V], ip: InnerProduct[V]) = {
        def fn(av: (Double, V)) = ls.mult(av._1, av._2)
        
        def grad(av: (Double, V))(w: V) = (ip.dot(av._2, w), ls.mult(av._1, w))
        
        DiffbleFunction[(Double, V), V](fn)(grad)
      }
      
      /**
       * raise a function to 2^(n -1) wrt composition, so for n = 0 we get identity and n = 1 gives f.
       */
      def repsquare[A : LinearStructure](f: DiffbleFunction[A, A]): Int => DiffbleFunction[A, A] = {
        case 0 => id[A]
        case 1 => f
        case n if n<0 => 
          vzero[DiffbleFunction[A, A]]
        case n =>
          {val rs = repsquare(f)
          rs(n-1) andthen (rs(n-1))
          } 
      }
      
        /**
       * Iterate a differentiable function.
      */
      @tailrec def iterateDiffble[X](fn:  DiffbleFunction[X, X], n: Int, accum:  DiffbleFunction[X, X] = id[X]):  DiffbleFunction[X, X] = {
          if (n<1) accum else iterateDiffble(fn, n-1, accum andthen (fn :  DiffbleFunction[X, X]) )
        }
      
      def iterate[A](f: DiffbleFunction[A, A]) : Int => DiffbleFunction[A, A] = (n) => iterateDiffble(f, n)
      
      def mixinIsle[A : LinearStructure](f: DiffbleFunction[A, A], 
          isle : DiffbleFunction[A, A] => DiffbleFunction[A, A],
          normalize: DiffbleFunction[A, A] = id[A]) = {
        val g = iterate(f)  
        def h(m : Int) : DiffbleFunction[A, A] = m match {
        	case 0 => id[A]
        	case n if n<0 => 
        		vzero[DiffbleFunction[A, A]]
        	case n if n >0 => 
        		val dsum  = vsum[DiffbleFunction[A, A]]
        		dsum(g(n), isle(h(n -1))) andthen normalize
        }
        h _
      }
      
      /**
       * Big sum, with terms (via support) in general depending on the argument.
       */
      def bigsum[A, B](fns: A => Traversable[DiffbleFunction[A, B]])(implicit lsA: LinearStructure[A], lsB: LinearStructure[B]) = {
        def func(a: A) = {
          val terms = for (f <- fns(a)) yield f(a) 
          val zero = vzero[B]
          val sum = vsum[B]
          (terms :\ zero)(sum)
        }
        
        def grad(a: A)(b: B) = {
          val terms = for (f <- fns(a)) yield f.grad(a)(b) 
          val zero = vzero[A]
          val sum = vsum[A]
          (terms :\ zero)(sum)
        }
        
        DiffbleFunction(func)(grad)
      }
      
      def diagonal[A](implicit lsA : LinearStructure[A]) ={
        def func(a: A) = (a, a)
        
        def grad(a: A)(v : (A, A)) = lsA.sum(v._1, v._2)
        
        DiffbleFunction(func)(grad)
      }
      
      implicit def diffFnLS[A : LinearStructure, B : LinearStructure]
      : LinearStructure[DiffbleFunction[A, B]] = {
        def sum(fst: DiffbleFunction[A, B], scnd: DiffbleFunction[A, B]) = {
          val addB = vsum[B]
        
          val addA = vsum[A]
        
          def fn(a: A) = addB(fst(a), scnd(a))
        
          def grad(a: A)(b: B) = addA(fst.grad(a)(b), scnd.grad(a)(b))
        
          DiffbleFunction(fn)(grad)
      }
      
      def scprod(sc: Double, vect: DiffbleFunction[A, B]) = {
        val prodB = vprod[B]
        
        val prodA = vprod[A]
        
        def fn(a: A) = prodB(sc, vect(a))
        
        def grad(a: A)(b: B) = prodA(sc, vect.grad(a)(b))
        
        DiffbleFunction(fn)(grad)
      }
      
      def zero = {
        val zeroA = vzero[A]
        
        val zeroB = vzero[B]
        
        DiffbleFunction((a: A) => zeroB)((a: A) => (b: B) => zeroA)
      }
      
      LinearStructure(zero, sum, scprod)
    }
      
      
      
      val hyptan = apply[Double, Double]((arg: Double) => math.tanh(arg))((arg : Double) => (y: Double) => y/(math.cosh(y) * math.cosh(y)))
    }
