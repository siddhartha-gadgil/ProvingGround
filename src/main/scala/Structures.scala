package provingGround

/** Effective structures, collections and Utility functions for these */
object Structures{

	/** Flips co-ordinates of a tuple */
  def flip[A](r: (A, A)): (A,A) = (r._2, r._1)

  /** Cartesian product of Sets */
  def setProd[A, B](s: Set[A], t: Set[B]): Set[(A,B)] = (for (x<-s ; y<-t) yield (x,y)).toSet

  /** Product of a List of Sets */
  def listProd[A](ls: List[Set[A]]): Set[List[A]] = if (ls.isEmpty) Set(List()) else {
    for (s <- ls.head; t <- listProd(ls.tail)) yield (s :: t)
  }
  
  /** Product of a set of sets */
  def indexSetProd[A](ss: Set[Set[A]]): Set[Set[A]] = if (ss.isEmpty) Set(Set()) else {
    for (s <- ss.head; t <- indexSetProd(ss.tail)) yield (t + s)
  }
  
  /** Returns fixed point of a function. Loops forever if there is none */
  def fixedPoint[A](x: A, f: A=>A): A = if (f(x)==x) x else fixedPoint(f(x), f)

  /** Returns all maps from S to T as PartialFunctions */
  def allMaps[A,B](S: Set[A], T: Set[B]): Set[PartialFunction[A, B]] = {
    if (S.isEmpty) Set((Map.empty: PartialFunction[A,B])) else {
      (for (t <- T; f <- allMaps(S.tail, T)) yield (Map(S.head -> t) orElse f)).toSet 
    }
  }

  /** Returns f^{-1}(b) for a partial function f*/
  def invSet[A,B](b: B, dom: Set[A], f: PartialFunction[A,B]) = dom filter(f.lift(_)== Some(b))

  /** Returns {(a,c) : (a,b), (b,c) in S} */
  def edgePairs[A](S: Set[(A,A)]): Set[(A,A)] = {
     (for ((a, b) <- S; (c,d)<- S; if b==c) yield (a,c)).toSet
      }
      
  /** Returns transitive closure for the relation corresponding to S */
  def transClosure[A](S: Set[(A,A)]): Set[(A,A)] = fixedPoint(S, (T:Set[(A,A)]) => T union edgePairs(T))
  /** Symmetrize a set of pairs */
  def symSet[A](S: Set[(A,A)]) = S union (S map flip)

	/** Partition corresponding to the set of an equivalence relation */
	def partitionSet[A](relSet: Set[(A,A)]): Set[Set[A]] = if (relSet.isEmpty) Set.empty else {
			val dom = relSet map (_._1)
      val head=dom.head
      val tailSet = relSet filter ((ab: (A, A)) => (ab._1 != head) && (ab._2 != head))
      val tailPartition = partitionSet(tailSet)
      val (withHead, rest) = tailPartition partition (_ contains head)
      val headClass = Set(head) union (withHead reduce (_ union _))
      rest union Set(headClass)
      }
  
  /** Directed graph */
  trait DiGraph[V,E]{
  	/** Vertex set */
  	val vertices: Set[V]
  	/** Edge set */ 
  	val edges: Set[E]
  	/** Initial Vertex */
  	def i(e: E):V
  	/** Terminal vertex */
  	def t(e: E):V
  	/** Edges beginning with a vertex in S */
  	def edgesFrom(S: Set[V]) = edges filter (S contains i(_))
  	/** Terminal vertices of edges beginning with a vertex in S*/
  	def verticesFrom(S: Set[V]) = edgesFrom(S) map (t) 
  	
  	private def expandFrom(S: Set[V]) = S union verticesFrom(S)
  	/** The closure of S under directed adjacency */
  	def closure(S: Set[V]) = fixedPoint(S, expandFrom)
  }

  /** Graph: defined in terms of pairs of edges with an orientation reversing operation */  
  trait Graph[V, E] extends DiGraph[V, E]{
  	/** Orientation reversing on an edge */
  	def bar(e: E): E    
  	/** Pairs of vertices bounding an ege */
  	lazy val edgesSet: Set[(V,V)] = (edges map ((e: E) => (i(e), t(e)))) union (edges map ((e: E) => (t(e), i(e))))  
 		/** Adjacency as an equivalence relation */
  	lazy val adj = new EquivRelation(edgesSet union (vertices map ((v: V) => (v,v))))
  }

  /** Edge with endpoints in V*/
  trait Edge[V]{
    def i: V
    def t: V
    }

 
  /** Covering space for a graph */
  case class Cover[V, E](base: Graph[V,E], total: Graph[V, E], p: E => E){
  	/**Lift of edge e starting at v */
    def liftEdge(v: V, e: E): E = {
      def isLift(up: E): Boolean = (p(up) == e) && (total.i(up)==v)
      (total.edges find (isLift(_))).get
      }
    /**Lift of a sequence of edges starting at v */
    def lift(v: V, es: List[E]): List[E]= es match{
      case List() => List()
      case List(x) => List(liftEdge(v, x))
      case xs: List[E] => liftEdge(v, xs.head) :: lift(total.t(liftEdge(v, xs.head)), xs.tail)
      }
    }

  /** Simple graph given by vertex set and set of adjacent pairs of vertices */
  case class SimpleGraph[V](val vertices: Set[V], edgeSet: Set[(V,V)]) extends Graph[V, (V,V)]{
      val edges = edgeSet union (edgeSet map (bar))
      def i(e: (V,V))= e._1
      def t(e: (V,V))= e._2
      def bar(e: (V,V)): (V,V)= (e._2, e._1)
      override lazy val edgesSet = edgeSet
      }

  /** Binary Relation */
  class Relation[A](val relSet : Set[(A,A)]){
  	/** Domain */
    lazy val dom = relSet map (_._1)
    /** Codomain */
    lazy val codom = relSet map (_._2)
    
    /** Corresponding Boolean relation */ 
    def rel(a: A, b: A) = relSet contains (a,b)

		/** Symmetrized relation */
    lazy val sym = new SymRelation[A](relSet union (relSet map flip))
 
    /** Set for Transitive closure of the relation */
    lazy val transSet: Set[(A,A)] = transClosure(relSet)
    
    /** Transitive closure*/
    lazy val * = new Relation(transSet)
    
    /** Equivalence relation spanned */
    lazy val equiv = new EquivRelation(transClosure(symSet(relSet)))
    /** Adjacent to a vertex*/
    def adj(a: A)(e: (A,A)): Boolean = (e._1 == a || e._2 == a)
		/** Not adjacent to a vertex */
    def notAdj(a: A)(e: (A,A)): Boolean = !adj(a)(e)
    /** Delete a vertex and adjacent edges */
    def delete(a: A): Set[(A,A)] = relSet filter (notAdj(a)(_))
    } 

   /** Symmetric relation */
   class SymRelation[A](relSet : Set[(A,A)]) extends Relation[A](relSet: Set[(A,A)]){
    override lazy val equiv = new EquivRelation(transSet)
    }

   /** Equivalence relation */
   class EquivRelation[A](relSet : Set[(A,A)]) extends SymRelation[A](relSet: Set[(A,A)]){
   	/** Partition corresponding to an equivalence relation; should avoid creating relation in recursion */
    lazy val partition: Set[Set[A]] = partitionSet(relSet)
      }
    

  /** Triangulated surface */
  trait Surface[V,E] extends Graph[V,E]{
  	/** Faces */
    val faces: Set[List[E]]
    /** Euler characteristic */    
    val euler = vertices.size - edges.size + faces.size 
  }

  /** (partly ideal) triangulation of punctured surface */
  trait PuncturedSurface[V, E] extends Surface[V, E]{
  	/** ideal vertices */
    def ideal(v: V): Boolean
    }
    
  
 
  /** Words of length n in generators as lists */
	def words[A](gen: Set[A], n: Int): Set[List[A]] = {
     if (n<=1) gen map (List(_)) else {
      val prev:Set[List[A]] = words[A](gen, n-1)
			for (a<- gen; w <- prev) yield (a :: w)
    }  
  }
  
  
  private def wordSetNew[A](gen: Set[A], prev: Set[List[A]]):Set[List[A]] = {
    for (a <- gen; w <- prev) yield (a :: w)
      }
  
  /** Stream with nth entry words of length n in the generators */
  def wordSetStream[A](gen: Set[A]): Stream[Set[List[A]]]= {
    (gen map (List(_))) #:: (wordSetStream[A](gen) map (wordSetNew[A](gen,_)))
    }  

  /** Stream of words in the generators */
  def wordStream[A](gen: Set[A]):Stream[List[A]] = wordSetStream[A](gen).flatten
   
  /** Groupoid in terms of a partial product */
  trait Groupoid[A]{
  	/** The partial product */
    val  * : PartialFunction[A,A]
    /** Optional product */
    def mult(that: A): Option[A] = (*.lift)(that)

    
    }

    
    
  object Groupoid{
  	/** Closure of sets S and T under the partial multiplication */
    def pairClosure[A <: Groupoid[A]](S: Set[A], T:Set[A]) ={
			(for (s <-S; t <- T) yield (s mult t)).flatten
      }        
    


// Enumerating in a groupoid multiplying all that are posssible.
  /** Returns nth step of recursive span of generators of the groupoid*/  
  def span[A<:Groupoid[A]](gens: Stream[A], n: Int): Set[A] = {
      if (n<=1) (gens take n).toSet else {
        val prevSet:Set[A] = (span[A](gens, n-1)) union (gens take n).toSet
        pairClosure(prevSet, prevSet) union prevSet
        }
      }
      
  private def spanNext[A<:Groupoid[A]](gens: Stream[A], startSet: Set[A], n:Int =1) ={
    val prevSet:Set[A] = startSet union (gens take n).toSet
    pairClosure(prevSet, prevSet)
    }

	private def spanNew[A<:Groupoid[A]](gens: Stream[A], n: Int): Set[A] = {  
		if (n>1) span(gens, n) -- span(gens, n-1) else span(gens, 1)
	}
	
  /** Span of generators of a groupoid as a stream with repetition */              
  def stream[A<:Groupoid[A]](gens: Stream[A]): Stream[A]= (Stream.from (1)) flatMap (span(gens,_))
   
	/** Span of generators without repetition */
	def cleanStream[A<:Groupoid[A]](gens: Stream[A]): Stream[A]= (Stream.from (1)) flatMap (spanNew(gens,_))
  }
   
  trait Group[A]{
    val G: Set[A]
    def *(g: Group[A]) : Group[A]
    val e: Group[A]
    val inv: Group[A]
    
    def conj(g: Group[A]): Group[A] = this * g * (this.inv)
  }
  
  object Monoid{
    trait Struct[A]{
      val e: A
      def prod(a: A, b: A): A
    }
    
    trait Elem{
      def *(a: Elem): Elem
      }   
      
    case object Broken extends Elem{
      def *(a: Elem)= this
      }
      
    case class Element[A](elem: A, g: Struct[A]) extends Elem{
      def *(a: Elem): Elem = a match 
        {case Element(x, g) => Element(g.prod(this.elem, x), g)
        case _ => Broken
        } 
    }
  }
  
  object Group{
    trait Struct[A] extends Monoid.Struct[A]{
//      val e: A
//      def prod(a: A, b: A): A
      def inv(a: A): A
      
    }
    
    trait Elem extends Monoid.Elem{
//      def *(a: Elem): Elem
      def inv: Elem
      def ^(a: Elem) = a.inv * this * a
      }
     
    case object Broken extends Elem{
      def *(a: Monoid.Elem)= this
      def inv = this
      }
      

      
    
    case class Element[A](elem: A, g: Struct[A]) extends Elem{
      def *(a: Monoid.Elem): Elem = a match 
        {case Element(x, g) => Element(g.prod(this.elem, x), g)
        case _ => Broken
        }
      def inv = Element(g.inv(this.elem), g)
    }
  }  
}    