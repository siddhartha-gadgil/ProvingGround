package provingground.functionfinder

import provingground.HoTT._
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}
import ScalaRep._

object IntTypes {


  
    //An example - should change to use SimpleRep and SimpleConst
  object IntRep extends ScalaRep[Term, Long]{
    val typ = Z
    
    def apply(n: Long) = Zcnst(n)
    
    def unapply(u: Term) = u match {
      case Zcnst(n, _) => Some(n)
      case _ => None
    }

  }   
  
    // TODO make this tail recursive
  private def inducFn[U<: Term](f0 : U, g: Int => U => U) : Int => U = {
   case n if n > 0 => g(n)(inducFn(f0, g)(n -1))
   case 0 => f0
  }
  
  
  private def inducCurry[U <: Term: TypeTag]: U => (Int => U => U) => (Int => U) = {
    (f0: U) => g: (Int => U => U) => inducFn(f0, g)
  }
  
  private val indCurry = inducCurry[Term]
  
  private val n = dsl.i[Int](N)
  
  
  def recursion[U <: Term : TypeTag](u: Typ[U]) = {    
    val rep = u -->: (n -->: u -->: u) -->: (n -->: u)
    def induccurry: U => (Int => U => U) => (Int => U) = {
    (f0: U) => g: (Int => U => U) => inducFn(f0, g)
    }
    rep(induccurry)
  }
  
  val recN = depFunc(__, (u: Typ[Term]) => recursion(u))
  /*
  
  private def recRep[U <: Term : TypeTag](u: Typ[U]) = {
    u -->: (n -->: u -->: u) -->: (n -->: u)
  } 
  
  
  val recAllRep = __ ~>: ((u: Typ[Term]) => recRep(u))
 
  
  
  private val recAppl = recAllRep.apply _
  
  
  val recAll = recAllRep((u: Typ[Term]) => indCurry)
  
  */

  
  
	trait IntTyp extends SmallTyp{
	  def const(n: Long): Term 
	}
	
    case object Z extends SmallTyp
	
    
	trait IntCnst extends ConstTerm[Long]{
	  val value: Long
	}
	
	case class Zcnst(value : Long, typ: Typ[Term] = Z) extends IntCnst
	
	def zcnst(k: Long, typ: Typ[Term]) = typ match {
	  case Z => Zcnst(k)
	  case N => Zcnst(k.abs, typ)
	  case fin : Fin => Zcnst(k % fin.n, typ)
	}
	
	case class IntFn[U <: Term with Subs[U]: TypeTag](f: Long => U, codom : Typ[U], dom: Typ[Term] = Z) extends IntLkFn[U](f, codom){
	  
//	  val dom: provingground.HoTT.Typ[provingground.HoTT.Term] = Z	  
	  
	  def make(f: Long => U, codom : Typ[U]) = IntFn(f, codom, dom)
	  
	  def subs(x: provingground.HoTT.Term,y: provingground.HoTT.Term) = (x, y) match {
	    case (u, v: FuncTerm[Term,U]) if u == this => v
	    case _ => make((n: Long) => f(n).subs(x, y), codom)
	  }
	  
	}
	
	case object N extends SmallTyp
	
	case class Fin(n: Long) extends SmallTyp
	
	case class FinTyp(n: Term) extends SmallTyp
	
	val fin = IntFn(Fin(_), __)
	
	
	abstract class IntLkFn[U <: Term with Subs[U]](f: Long => U, codom : Typ[U])(implicit tag: TypeTag[U]) extends FuncTerm[Term, U]{
	  
	  val dom: Typ[Term]
	  
	  val typ = dom ->: codom
	  
	  def apply(n : Term) = n match {
	    case c: IntCnst => f(c.value)
	    case arg: Term => codom.symbObj(ApplnSym(this, arg))
	  }
	  
	  val domobjtpe: reflect.runtime.universe.Type = typeOf[Term]
	  
	  val codomobjtpe: reflect.runtime.universe.Type = typeOf[U]
	  
	  val depcodom: provingground.HoTT.Term => provingground.HoTT.Typ[U] = (t) => codom

	  def make(f: Long => U, codom : Typ[U]) : IntLkFn[U]
	  

	  
	}
	
	def sum(doma: Typ[Term], domb:Typ[Term], codom: Typ[Term]) = IntFn((a: Long) => 
	  {IntFn((b: Long) => zcnst(a+b, codom), codom, domb)}, 
	  domb ->: codom, doma)
	
	def lift(doma: Typ[Term], domb:Typ[Term], codom: Typ[Term])(binop: (Long, Long) => Long) = {
	  IntFn((a: Long) => 
	  	{IntFn((b: Long) => zcnst(binop(a, b), codom), codom, domb)}, 
	  	domb ->: codom, doma)
	}  
	  
	def lift[V <: Term with Subs[V] : TypeTag](doma: Typ[Term], domb:Typ[Term], codom: Typ[V])(binop: (Long, Long) => V) = {
	  IntFn((a: Long) => 
	  	{IntFn((b: Long) => binop(a, b), codom, domb)}, 
	  	domb ->: codom, doma)
	}
	
	def bigsum(n: Term)(f: FuncTerm[Term, Term])  =  {
	  assert (f.typ == fin(n) ->: Z) 
	}
}