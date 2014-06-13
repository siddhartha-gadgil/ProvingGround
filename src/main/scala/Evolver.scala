package provingGround

import Stream._
import Structures._
import annotation.tailrec
import scala.util._
import scala.language.postfixOps
 
import akka.actor._
import akka.pattern.ask
import scala.concurrent._
import scala.concurrent.duration._

import provingGround.HoTT._

import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}

import ExecutionContext.Implicits.global

/** Generic Memoised Evolver built from base by adding spans and weights */
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
    /** Initial set*/ 
    val init: Set[A] = if (gens.hasDefiniteSize) gens.toSet else (gens take 1).toSet

    /** Overridden for mixing in dynamics safely */
    def mixinSpan(S: Set[A], n: Int): Set[A]  = Set.empty
           
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
  
  	/** Evolver without weights depending on local moves  */
  	class SimpleEvolver[A](val gens: Stream[A], span: A => Set[A]) extends BaseEvolver[A]{
  	  override def mixinSpan(S: Set[A], n: Int): Set[A]= S flatMap ((a:A) => span(a))
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

object GeneticEvolver{
  
  def listMap[A](f: A => A) ={
    (l: List[A]) => if (l.isEmpty) l else f(l.head) :: l
  }
  
  trait BinaryTree[A]{
    val root: A
  }
  
  case class AtomicTree[A](root: A) extends BinaryTree[A]
  
  case class RecTree[A](root: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]
  
  
  def treeMap[A](prod: (A, A) => A) ={
    def lr(left: BinaryTree[A], right: BinaryTree[A]) = RecTree(prod(left.root, right.root), left, right)
    lr(_, _)
  }
  
  @tailrec def pickByWeight[A](l : List[A], weight: A=> Int, n: Int): A ={
    if (weight(l.head) < n) l.head
    else pickByWeight(l.tail, weight, n-weight(l.head))
  }
  
  def pickRandByWeight[A](l : List[A], weight: A=> Int)(implicit rand: scala.util.Random): A ={
    val total = (l map (weight(_))).sum
    pickByWeight(l, weight, rand.nextInt(total))
  }
  
  def sampleByWeight[A](l: List[A], weight: A=> Int, n: Int)(implicit rand: Random) ={
    (for (i <- 0 until n) yield (pickRandByWeight(l, weight)(rand))).toList
  }
  
  @tailrec def sampleMaxSize[A](l: List[A], weight: A=> Int, size: A => Long, n: Long, soFar: Set[A] = Set.empty)(implicit rand: Random): Set[A] ={
    if (n<0) soFar
    else {val newElem = pickRandByWeight(l, weight)
    sampleMaxSize[A](l, weight, size, n - size(newElem), soFar + newElem)
    }
  }
  
  case class Markov[A](dyn: Set[A] => Set[A]){
    def apply(s: Set[A]) = dyn(s)
    
    def andThen(d: Markov[A]) = Markov(dyn andThen d.dyn)
    
    def ++(d: Markov[A]) = Markov((s: Set[A]) => dyn(s) union d.dyn(s))
    
    def circ(d: Markov[A]) = d andThen this
    
    def andThen(dmap: Set[A] => Set[A]) = Markov(dyn andThen dmap)
    
    def ++(dmap: Set[A] => Set[A]) = Markov((s: Set[A]) => dyn(s) union dmap(s))
    
    def circ(dmap: Set[A] => Set[A]) = Markov(dmap) andThen this
  }
  
  object Markov{
    def unary[A](f: A => Set[A]) = Markov((s: Set[A]) => s flatMap f)
    
    def prodSet[A](prodSet: (A, A) => Set[A]) ={
      Markov[A]((s: Set[A]) => (for (x<-s; y<-s) yield prodSet(x,y)).flatten)
    }
    
    def prod[A](prod: (A, A) => A) ={
      Markov[A]((s: Set[A]) => for (x<-s; y<-s) yield prod(x,y))
    }
    
    def prune[A](weight: A => Int, cutoff : Int) = {
      Markov((s: Set[A]) => s filter (weight(_) <= cutoff))
    }
      
    def pruneNumber[A](weight: A => Int, number: Int) ={
        Markov((s: Set[A]) => s.toList.sortBy(weight(_)).take(number).toSet)   
    }
    
    
    
    
    def pruneTotal[A](weight: A => Int, number: Int) ={
    	@tailrec def pruneListTotal(l: List[A], total: Int, headList: List[A] =List.empty): List[A] = {
    			if (total <0 || l.isEmpty) headList
    			else pruneListTotal(l.tail, total - weight(l.head), l.head :: headList)
    	}
      
        Markov((s: Set[A]) => pruneListTotal(s.toList.sortBy(weight(_)), number).toSet)   
    }
    
    def pruneTotalSize[A](weight: A => Int, number: Int, size: A => Int) ={
    	@tailrec def pruneListTotal(l: List[A], total: Int, headList: List[A] =List.empty): List[A] = {
    			if (total <0 || l.isEmpty) headList
    			else pruneListTotal(l.tail, total - size(l.head), l.head :: headList)
    	}
      
       Markov((s: Set[A]) => pruneListTotal(s.toList.sortBy(weight(_)), number).toSet)
    }
  }
  
  class MarkovEvolver[A](val base: Set[A], dynaBase: Markov[A], core: Set[A]= Set.empty) extends provingGround.Aware.Seeker[A, Set[A]]{

    def dyn(s: Set[A]) = dynaBase.dyn(s) union s
    
    def this(base: Set[A], dyn: Set[A] => Set[A]) = this(base, Markov(dyn), Set.empty: Set[A])
    
    def this(base: Set[A], dyn: Set[A] => Set[A], coreSet: Set[A]) = this(base, Markov(dyn), coreSet)
    
    @tailrec final def evolution(init: Set[A], n: Long): Set[A] = {
      if (n<1) init
      else evolution(dyn(init), n-1)
    }
    
    @tailrec final def findEvolve(init: Set[A], p: A => Boolean, n: Long): Option[A] ={
      if (n<0) None 
      else if (init exists p) init find p
      else findEvolve(dyn(init), p, n-1)
    }
    
    def find(p: A => Boolean, n: Int) = findEvolve(base, p, n)
    
    @tailrec final def seek(p: A => Boolean, n: Long, init: Set[A] = base): Either[Set[A], A] ={
      if (n<0) Left(init) 
      else if (init exists p) Right(init find p get)
      else seek(p, n-1, dyn(init))
    }
    
    def apply(n: Int) = evolution(base, n)
    
    def pruneByWeight(weight: A => Int, cutoff: Int) = new MarkovEvolver(base, dynaBase andThen Markov.prune(weight, cutoff), core)
    
    def pruneByNumber(weight: A => Int, number: Int) = new MarkovEvolver(base, dynaBase andThen Markov.pruneNumber(weight, number), core)
  }
}





object Evolver{
    trait Inbox[A] extends Outbox[A]{
        def get(n: Int): Set[A]
        def pull(n: Int): Set[A]
        
        
        
        def getAll: Set[A]
        def pullAll: Set[A]
    }
    
    class ListInbox[A] extends Inbox[A]{
        private var mem: List[A] = List()
        def get(n: Int) = (mem take n).toSet
        def pull(n: Int) = {
            val (head, tail) = mem splitAt n
            mem = tail
            head.toSet
        }
        
       
        def push = (a: A) => {mem = a:: mem; }
        
        def getAll = mem.toSet
        def pullAll = {val head = mem; mem = List(); head.toSet}
    }
    
   trait Outbox[A]{
    def push: A => Unit
    
    def pushAll(s: Traversable[A]) = s foreach (push(_))
    
    def pushTrav: PartialFunction[Traversable[A], Unit] = {case s : Traversable[A] => pushAll(s)}
    
    def bind(f : Future[Traversable[A]]) = f.onSuccess(pushTrav)
   }
   
   case class SimpleOutbox[A](push: A => Unit) extends Outbox[A]
    
   case class VanishBox[A]() extends Outbox[A]{
     def push: A => Unit = {_ => }
   }
   
   case class EmptyBox[A]() extends Inbox[A]{
     def get(n: Int): Set[A] = Set.empty
     def pull(n: Int): Set[A] = Set.empty
        
     def push: A => Unit = {_ => }   
        
     def getAll: Set[A] = Set.empty
     def pullAll: Set[A] = Set.empty
   }
   
   case object EvolverTyp extends SmallTyp
   
   class Gen(dyn: => (Set[Term] => Set[Term]), 		   		 
		   		state:  => Set[Term], 
		   		mapping:  Term => Term = {(x : Term) => x},
		   		outbox: Outbox[Term] = VanishBox[Term]) extends AtomicObj{
     
     object typ extends SmallTyp
     
     def nextState = dyn(state)
     def nextGen = new Gen(dyn, nextState, mapping, outbox)
     def nextSet = {
//         println(state)
//         println(nextChar(usedChars(state)))
//         println(nextState map (mapping(_)))
//         println(state filter (_.isInstanceOf[LogicalTyp]))
         nextState map (mapping(_))
     }
//     println(state)
//     println(nextChar(usedChars((state))))
   } 
   
   trait Evolver[A] extends DynSys[A]{
       val inbox: Inbox[A]
       
       val outbox: Outbox[A]
       
       def generate(S: Set[A]): Set[A]
       
       def purge(S: Set[A]): Set[A]
       
       def step: Set[A] => Set[A] = (S: Set[A]) => purge(generate(S) union inbox.pullAll)

       @tailrec final def getNow(S: Set[A], n: Int): Set[A] = if (n<1) S else getNow(step(S), n-1)
       
       def get(S: Set[A], n: Int) = Future(getNow(S, n))
       
       @tailrec final def seekNowWithState(S: Set[A], p: A => Boolean, n: Int): (Option[A], Set[A]) = {
         if (!(S find p).isEmpty) (S find p, S) 
         else if (n<1) (None, S)
         else seekNowWithState(step(S), p, n-1)
       }
       
       def seekNow(S: Set[A], p: A=> Boolean, n: Int) = seekNowWithState(S, p, n)._1
       
       def seek(S: Set[A], p: A => Boolean, n: Int) = Future(seekNow(S, p, n)) 
   }
   
   class BasicEvolver[A](gen: Set[A] => Set[A], val pur: Set[A]=> Set[A] = {(s : Set[A]) => s}) extends Evolver[A]{
     def generate(s: Set[A]) = gen(s)
     
     def purge(s: Set[A]) = pur(s)
     
     val inbox = EmptyBox[A]
     
     val outbox = VanishBox[A]
   }
   
   def fromGen(gen: Gen) = Set((gen.nextGen: Term)) union (gen.nextSet)
   
   def expandGen(obj: Term) = obj match {
     case gen: Gen => fromGen(gen)
     case ob: Term => Set(ob)
   }
   
   def expandGens(s: Set[Term]) = s flatMap (expandGen _)
   
   case class Dyn[A](step: Set[A] => Set[A]) extends DynSys[A]

	 trait DynSys[A] extends Function1[Set[A], Set[A]]{
		 def step: Set[A] => Set[A]

     def apply(s: Set[A]) = step(s)
     
     def union(thatStep: Set[A] => Set[A]) = Dyn((s: Set[A])=> step(s) union thatStep(s))
     
     def ++(thatStep: Set[A] => Set[A]) = union(thatStep)
     
     def andThen(thatStep: Set[A] => Set[A]) = Dyn((s: Set[A]) => thatStep(step(s)))
     
     def ||(thatStep: Set[A] => Set[A]) = andThen(thatStep)
     
     def mixin(thatStep: Set[A] => ( =>(Set[A] => Set[A])) => Set[A]) = {
		   Dyn((s: Set[A])=> step(s) union thatStep(s)(this))
		 }
              
   }
   
   object Dyn{
     def id[A] = Dyn((s: Set[A]) => s)
     
     def lift[A](f: A => A) = new Dyn((s: Set[A]) => (s map f))
     
     def plift[A](pf: PartialFunction[A,A]) = new Dyn((s: Set[A]) => (s collect pf))
     
     def pairs[A](pairing: PartialFunction[(A, A), A]) = Dyn(
       (s : Set[A]) => (for(x<- s; y<-s) yield (x,y)) collect pairing
     )
     
     def triples[A](tripling: PartialFunction[(A, A, A), A]) = {
       (s : Set[A]) => (for(x<- s; y<-s; z <-s) yield (x,y, z)) collect tripling
     }

	def apply[A](pf: PartialFunction[A, A]): Dyn[A] = Dyn((s: Set[A])=> s collect pf)
   }
   
   
	
   	object HottEvolvers{
	 
	  
     type Pairing = PartialFunction[(Term, Term),Term]
    
//	def Applications[W<: Term, V<: Typ[U], U<: Term]: Pairing = {
//	  case (f: FuncObj[_, _, _], x: Term) if f.dom == x.typ => f(x).get 
//	}

	
	def LogicalArrows[V <: Term : TypeTag]: Pairing = {
		case (dom: LogicalTyp, codom: Typ[_]) => FuncTyp[Term, Term](dom, codom)
		}


		
	def lambdaGen(x: Term, dynam: => (Set[Term] => Set[Term]), state: Set[Term]) = {
	  new Gen(dynam, state, lambda(x) _)
	}
	
	def lambdaGens(state: Set[Term])(dynam: => (Set[Term] => Set[Term])) ={
	  val newVarSym = nextChar(usedChars(state))
	  val gens: PartialFunction[Term, Term] = {
	    case typ: Typ[_] =>
	      val obj = typ.symbObj(newVarSym.toString)
	      lambdaGen(obj , dynam, state + obj)
	  }
	  state collect gens
	}
	
	val InferenceDyn = Dyn.id[Term] ++ Dyn.pairs(LogicalArrows) andThen (expandGens _) mixin (lambdaGens _) 
	
	val InferenceEvolver = new BasicEvolver(InferenceDyn)
   }
   

   
   
	object HottInnerEvolvers{
	  
	import provingGround.HoTTinner._

	type Pairing = PartialFunction[(AbsObj, AbsObj),AbsObj]
    
	def Applications[U<: AbsObj]: Pairing = {
	  case (f: AbsFunc[U], x: AbsObj) if f.domain == x.typ => f(x) 
	}
	
	def Arrows[U<: AbsObj, V <: AbsObj]: Pairing = {
	  case (dom: Typ[U], codom: EffectiveTyp[V]) => FuncTyp(dom, codom)
	}
	
	def LogicalArrows[V <: AbsObj]: Pairing = {
		case (dom: LogicalTyp, codom: EffectiveTyp[V]) => dom --> codom
		}

	def lambdaMap(x: AbsObj)(y: AbsObj) ={
		val dom= x.typ
		val codom = y.typ.asInstanceOf[EffectiveTyp[AbsObj]]
		val fnTyp = FuncTyp(dom, codom)
//		println(fnTyp.dom, dom, x, y, fnTyp.codom, codom);
		fnTyp.Lambda(x as fnTyp.dom, y)
	}
		
//	def lambdaGen(x: Term, dynam: => (Set[Term] => Set[Term]), state: Set[Term]) = {
//	  new Gen(dynam, state, lambdaMap(x) _)
//	}
	
//	def lambdaGens(state: Set[Term])(dynam: => (Set[Term] => Set[Term])) ={
//	  val newVarSym = nextChar(usedChars(state))
//	  val gens: PartialFunction[Term, Term] = {
//	    case typ: EffectiveTyp[_] =>
//	      val obj = typ.symbObj(newVarSym)
//	      lambdaGen(obj , dynam, state + obj)
//	  }
//	  state collect gens
//	}
	
//	val InferenceDyn = Dyn.id[Term] ++ Dyn.pairs(LogicalArrows) andThen (expandGens _) mixin (lambdaGens _) 
	
//	val InferenceEvolver = new BasicEvolver(InferenceDyn)
  }
   
   trait StatefulEvolver[A] extends Evolver[A]{
     val init: Set[A]
     
     var state = init
     
     def getShift(n: Int) = get(state, n) map {x => {state = x ; x}}
     
     def seekShift(p: A => Boolean, n: Int) = Future(seekNowWithState(state, p, n)) map {x => {state = x._2 ; x._1}}
   }
   
   
   case class SeekMemo[A](set: Set[A], pred: A => Boolean, soln: A){
     val problem = (set, pred)
   }
   
   trait MemoEvolver[A] extends StatefulEvolver[A]{
     private var memopad: Set[SeekMemo[A]] = Set.empty
     
     private def note(solution: SeekMemo[A]) = {memopad += solution}
     
     private def note(set: Set[A], p: A=> Boolean, soln: A) = {memopad += SeekMemo(set, p, soln)}
     
     def askNow(set: Set[A] = state, p: A => Boolean, n: Int): Option[A] = {
       val fromMemo = (memopad find (_.problem == (set, p)) map (_.soln)) 
       fromMemo orElse seekNow(set, p, n ).map (x => {note(set, p, x); x})
     }
     
     def askNowShift(p: A => Boolean, n: Int): Option[A] = {
       val fromMemo = (memopad find (_.problem == (state, p)) map (_.soln)) 
       fromMemo orElse {val result = seekNowWithState(state, p, n); state = result._2; result._1 map (x => {note(state, p, x); x})}
     }
     
     def ask(set: Set[A] = state, p: A => Boolean, n: Int, shift: Boolean = false) = Future(askNow(set, p, n))
     
     def askShift(p: A => Boolean, n: Int) = Future(askNowShift(p, n))
   }
   
   case class Atom[A](atom: A)
   
   case class Trav[A](trav: Traversable[A])
   
   case class GetQuery[A](set: Set[A], n: Int)
   
   case class SeekQuery[A](set: Set[A], p: A=> Boolean, n: Int)
   
   trait EvolverActor[A] extends Evolver[A] with Actor{
       val upstream: ActorRef
       
       def channel: Receive = {
         case GetQuery(set, n) => sender ! get(set.asInstanceOf[Set[A]], n)
         case SeekQuery(set, p, n) => sender ! seek(set.asInstanceOf[Set[A]], p, n)
        		 	     
         case Trav(trav: Traversable[A]) => inbox.pushAll(trav)
         case Atom(atom) => Try(inbox.push(atom.asInstanceOf[A]))
       }
       
       def receive = channel
       
       val outbox = SimpleOutbox((a: A) => upstream ! Atom(a))
   }
   
   case class GetShiftQuery(n: Int)
   
   case class AskQuery[A](set: Set[A] =Set.empty, p: A => Boolean, n: Int)
   
   case class AskShiftQuery[A](p: Function1[A, Boolean], n: Int)
   
   trait StatefulEvolverActor[A] extends EvolverActor[A] with MemoEvolver[A]{
     def asyncChannel : Receive = {
       case GetShiftQuery(n) => sender ! getShift(n)
       case AskQuery(set, p, n) => sender !  ask(set.asInstanceOf[Set[A]], p ,n)
       case AskShiftQuery(p: Function1[A, Boolean], n: Int) => sender ! askShift(p , n)
     }
     
     override def receive = channel orElse asyncChannel
   }
   
}





