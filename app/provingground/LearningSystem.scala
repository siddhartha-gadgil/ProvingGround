package provingground

import scala.annotation._
import scala.util._

import scala.language.implicitConversions 

import provingground.Collections._

object LearningSystem{
   
	trait DiffbleFunction[A, B] extends (A => B){self =>
    	def grad(a: A) : B => A   
    	 
    	/**
    	 * Composition f *: g is f(g(_))
    	 */
    	def *:[C](that: DiffbleFunction[B, C]) = andThen(that)
    	
    	def andThen[C](that: => DiffbleFunction[B, C]): DiffbleFunction[A, C] = DiffbleFunction((a: A) => that(this(a)))(
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
	}

    
    
    object DiffbleFunction{
      def apply[A, B](f: => A => B)(grd: => A => (B => A)) = new DiffbleFunction[A, B]{
        def apply(a: A) = f(a)
        
        def grad(a: A) = grd(a)
      }
      
      def id[A] = apply((a: A) => a)((a: A) => (b: A) => b)
      
      def incl1[A, B](implicit lsB: LinearStructure[B]) = apply((a: A) => (a, lsB.zero))((a: A) => (x: (A, B)) => x._1)
      
      def proj1[A, B](implicit lsB: LinearStructure[B]) = apply((x: (A, B)) => x._1)((x) => (a) => (a, lsB.zero))
      
      def incl2[A, B](implicit lsA: LinearStructure[A]) = apply((b: B) => (lsA.zero, b))((b: B) => (x: (A, B)) => x._2)
      
      def proj2[A, B](implicit lsA: LinearStructure[A]) = apply((x: (A, B)) => x._2)((x) => (b) => (lsA.zero, b))
      
      def scprod[V](implicit ls: LinearStructure[V], ip: InnerProduct[V]) = {
        def fn(av: (Double, V)) = ls.mult(av._1, av._2)
        
        def grad(av: (Double, V))(w: V) = (ip.dot(av._2, w), ls.mult(av._1, w))
        
        DiffbleFunction[(Double, V), V](fn)(grad)
      }
      
      def repsquare[A](f: DiffbleFunction[A, A])(implicit ls: LinearStructure[A]): Int => DiffbleFunction[A, A] = {
        case 0 => id[A]
        case n if n<0 => 
          vzero[DiffbleFunction[A, A]]
        case n => 
          repsquare(f)(ls)(n-1)
            
      }
      
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
      
      
      val hyptan = apply[Double, Double]((arg: Double) => math.tanh(arg))((arg : Double) => (y: Double) => y/(math.cosh(y) * math.cosh(y)))
    }
    
    implicit def diffFnLS[A, B](
        implicit lsA : LinearStructure[A], lsB: LinearStructure[B]) : LinearStructure[DiffbleFunction[A, B]] = {
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
    
    
    trait LearningSystem[I, P, O] extends DiffbleFunction[(I, P), O]{
        def apply(inp: I, param: P): O = this.apply((inp,param))
        
        def update(feedback: O, inp: I, param: P, epsilon: Double)(implicit s: Shift[P]) = s(param, grad(inp, param)(feedback)._2, epsilon)

        def stack[Q, X](that: LearningSystem[O, Q, X]) = {
            
            def fwd(inp: I, pq: (P, Q)) = that(this(inp, pq._1), pq._2) 
            
            def bck(inp: I, pq: (P, Q))(x: X) = {
              val p = pq._1
              val q= pq._2
              val midval : O = this(inp, p) 
              val thatdiff  = that.grad((midval, q))(x)
              val odiff = thatdiff._1
              val thisdiff = grad((inp, p))(odiff)
              (thisdiff._1, (thisdiff._2, thatdiff._2))
              
            }
            
            LearningSystem(fwd, bck)
        }
    }
  
      object LearningSystem{
      def aggregate[L, I, P, O](edge: LearningSystem[I, P, O], base: Traversable[L])(implicit zero: O,ls: LinearStructure[O]) ={
        def fwd(inps: ArrayMap[L, I], params: ArrayMap[L, P]) ={
          val terms = for (k <-inps.coords.keys; in <-inps.get(k); p <- params.get(k)) yield edge(in, p)
          terms.foldLeft(zero)(ls.sum(_,_))
        }
        
        def bck(inps: ArrayMap[L, I], params: ArrayMap[L, P])(o: O) ={
          val inpmap = (for (k <-inps.coords.keys; in <-inps.get(k); p <- params.get(k)) yield (k -> edge.grad(in, p)(o)._1)).toMap
          
          val parammap = (for (k <-inps.coords.keys; in <-inps.get(k); p <- params.get(k)) yield (k -> edge.grad(in, p)(o)._2)).toMap
          
          (ArrayMap(inpmap, inps.supp), ArrayMap(parammap, params.supp))
        }
        
        LearningSystem(fwd, bck)
      }
      
      def apply[I, P, O](fwd: (I, P) => O, bck: (I, P) => O =>(I, P))={
        def forward(ip: (I, P)) = fwd(ip._1, ip._2)
        
        def back(ip : (I, P))(out: O): (I, P) = back(ip._1, ip._2)(out)
        
        asLearner(DiffbleFunction[(I,P), O](forward)(back))
      } 
      
      def edge[I](f: DiffbleFunction[I, Double]) ={
        def fwd(inp: I, wt: Double) = wt * f(inp)
        
        def bck(inp: I, wt: Double)(o: Double) = (f.grad(inp)(wt * o), fwd(inp, wt))
        
        LearningSystem[I, Double, Double](fwd, bck)
      }
      
      def collect[I, P, O, E](comps: Map[E, LearningSystem[I, P, O]])(implicit zi: I, zp: P, zo: O, ls: LinearStructure[I]) ={
        val exits = comps.keys
        
        def fwd(inp: I, param: ArrayMap[E, P]) = ArrayMap((exits map ((k) => (k ->comps(k)(inp,param(k))))).toMap)
        
        def bck(inp: I, param: ArrayMap[E, P])(out: ArrayMap[E, O]) ={
          val parammap = (for (e <- exits) yield (e -> comps(e).grad(inp, param(e))(out(e))._2)).toMap
          
          val inpterms = for (e <- exits) yield comps(e).grad(inp, param(e))(out(e))._1

          ((zi /: inpterms)(ls.sum(_,_)), ArrayMap(parammap))
        }
        
        LearningSystem[I, ArrayMap[E, P], ArrayMap[E, O]](fwd, bck)
      }
      
      def ANN[D, C](f: DiffbleFunction[Double, Double], dom: Traversable[D], codom: Traversable[C], inc: (D, C) => Boolean) ={
        val ed = edge(f)
        
        def toexit(y: C) = aggregate(ed, dom filter (inc(_, y)))
        
        val exitMap = (for (y<- codom) yield (y -> toexit(y))).toMap
        
        collect(exitMap)
      }
      
      def tuner[P, O](evol: DiffbleFunction[P, O]) ={
        def fwd: (Unit, P) => O = {(_, p) => evol(p)}
        def back(u: Unit, param: P)(err: O) = ({}, evol.grad(param)(err))
        LearningSystem(fwd, back)
      }
            
    }
    
    
    implicit def asLearner[I, P, O](f: DiffbleFunction[(I, P), O]): LearningSystem[I, P, O] = new LearningSystem[I, P, O]{
    	def apply(a: (I, P)) = f(a)
    	
    	def grad(a: (I, P)) = f.grad(a)
    }
    
    def learn[I, P, O](learner: LearningSystem[I, P, O], feedback: (O, O) => O, epsilon: Double)(implicit s: Shift[P]): (I, P, O) => P ={(inp, param, target) =>
      learner.update(feedback(target, learner(inp, param)), inp, param, epsilon)}
}