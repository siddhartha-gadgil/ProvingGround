object HottInnerEvolvers{

import provingground.HoTTinner._

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
