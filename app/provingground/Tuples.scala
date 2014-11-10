package provingground
import HoTT._
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}

object Tuples {
	
	trait TermTuple[Head <: Term with Subs[Head], TermType <: Term with Subs[TermType]]{

	  val head: Head
	  
	  val term : TermType with Subs[TermType]
	  
	  def subs(x: Term, y: Term): TermTuple[Head, TermType]

	  def newhead(that: Head): TermTuple[Head, TermType]
	  
	  type LambdaType <: Term with Subs[LambdaType]
	  
	  def lm(target: Term) : LambdaType
	}
	
	case class Singleton[U <: Term with Subs[U] : TypeTag](head : U) extends TermTuple[U, U]{
	  
	  lazy val term = head
	  
	  type TailCons = U
	  
	  def apply(value: U) = value
	  
	  def subs(x: Term, y: Term) = Singleton(head.subs(x, y))
	  
	  def newhead(that: U) = Singleton(that)
	  
	  type LambdaType = FuncTerm[U, Term]
	  
	  def lm(target: Term) = lambda(term)(target)
	}
	
	
	case class PairCons[U <: Term with Subs[U] : TypeTag, V <: Term with Subs[V], W <: Term with Subs[W]](
	    head: U, tail: TermTuple[V, W]) extends TermTuple[U, PairObj[U, W]]{
	
	  lazy val term = PairObj(head, tail.term)
	  
	  def subs(x: Term, y: Term) = PairCons(head.subs(x, y), tail.subs(x, y))
	  
	  def newhead(that: U) = PairCons(that, tail)
	  
	  type LambdaType = FuncTerm[U, tail.LambdaType]
	  
	  def lm(target: Term) = lambda(head)(tail.lm(target))
	}

	
}