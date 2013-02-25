package provingGround

import Stream._
import Structures._

/** Generic Evolver built from base by adding spans and weights */
object StackedEvolver{
  /** Function for evolving from given dynamics */
  def evolveFn[A,B](gens:B, init:A, nxt:(B, A, Int)=> A, n: Int): A = {
    if (n < 1) init else nxt(gens, evolveFn(gens, init, nxt, n-1), n)
  }
      
	/** Stream from generators given dynamics */
  def evolve[A,B](gens:B, init:A, nxt:(B, A, Int)=> A): Stream[A]={
    (from (0)) map (evolveFn( gens, init, nxt, _))
    }

  /** Base evolver, provides methods but has trivial dynamics */
  trait BaseEvolver[A]{
  	/** Generators */
    val gens: Stream[A]
    /** Weight function : mixed in by critics*/
    def weight(a: A, n: Int): Int = 0
    /** Initial set; should be overridden for efficiency if generators are a finite set.*/ 
    val init: Set[A] = (gens take 1).toSet

    /** Overridden for mixing in dynamics safely */
    def mixinSpan(S: Set[A], n: Int): Set[A] = Set.empty 
           
    private def nxt(gens: Stream[A], S: Set[A], n: Int) = {
      val genSet: Set[A] = ((gens take n).toSet union S) filter (weight(_, n)<=n)
       S union mixinSpan(genSet, n)
      }

    /** Increasing stream of sets from the dynamics*/
    val setStream: Stream[Set[A]] = evolve(gens, init, nxt)

		/** Disjoint stream of sets from the dynamics */
    val newSetStream: Stream[Set[A]] = (setStream, setStream.tail).zipped.map(_ -- _)

		/** Single stream from the dynamics; 
			* 
			* Warning: May loop forever if elements are sought beyond orbit of generators.
			*/
    val flow: Stream[A] = newSetStream.flatten

    /** Safe way to take the elements obtained by n iterations of the dynamics 
    	*
    	* Note: the result is usually not n elements
    	*/
    def take(n: Int): Stream[A] = (newSetStream take n).flatten  
    }

	/** Critics assign scores to elements */
	trait Critic[A]{
		def score(a: A, n: Int): Int
	}

	trait AddCritic[A] extends BaseEvolver[A] with Critic[A]{
		override def weight(a:A, n: Int) = super.weight(a, n) + score(a, n)
	}

	/** Spans to mixin dynamics */
  trait Span[A]{
  	/** the span to be mixed in */
    def span(S: Set[A], n: Int): Set[A]
  }
  
  /** Add new spans to Evolver */
  trait AddinEvolver[A] extends BaseEvolver[A] with Span[A]{
    override def mixinSpan(S: Set[A], n: Int): Set[A] = super.mixinSpan(S, n) union span(S, n)
  }

  /** Span of a partial product */
  trait SpanProd[A] extends Span[A]{
    val prod: PartialFunction[(A,A), A]
    def span(S: Set[A], n: Int): Set[A] = setProd(S, S) collect (prod)
  }

  /** Union of spans */
  trait SpanUnion[A] extends Span[A]{
    val spans: Seq[Span[A]]
    def span(S: Set[A], n: Int) = spans.toSet flatMap ((s: Span[A]) => s.span(S, n))
  }

  type Flow[A] = PartialFunction[A, Stream[A]]
  /** Span of a flow : streams from some elements */
  trait SpanFlow[A] extends Span[A]{
    val flow: Flow[A]
  	def span(S: Set[A], n: Int): Set[A] = (S collect flow) flatMap (_ take n)
  }

	/** Span of a partial product giving sets*/
  trait SpanMultiProd[A] extends Span[A]{
    val multiProd: PartialFunction[(A,A), Set[A]]
    def span(S: Set[A], n: Int): Set[A] = (setProd(S,S) collect (multiProd)) reduce (_ union _)      
  }

  /** Span of a stream */
  trait SpanStream[A] extends Span[A]{
    val stream: Stream[A]
    def span(S: Set[A], n: Int) = (stream take n).toSet
  }
  
  /** Span of a Groupoid*/
  trait SpanGroupoid[A<: Groupoid[A]] extends Span[A]{
    def span(S: Set[A], n: Int) = Groupoid.pairClosure(S, S)
  }
  
  /** Evolver for a Groupoid */
  class GroupoidEvolver[A <: Groupoid[A]](val gens: Stream[A]) extends BaseEvolver[A] with SpanGroupoid[A]
}

  
  






