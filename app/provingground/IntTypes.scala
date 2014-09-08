package provingground

import HoTT._
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}

object IntTypes {
	trait ConstTerm[T] extends Term{
	  val value: T
	  
	  type scalaType = T
	  
      def subs(x : Term, y: Term) = this
    }
	
	def extend[T, U <: Term : TypeTag](fn: T => U, functerm: FuncTerm[Term, U], codom: Typ[U]): Term => U = {
	  case c: ConstTerm[T] => fn(c.value)
	  case arg: Term => codom.symbObj(ApplnSym(functerm, arg))
	}
  
	trait IntTyp extends SmallTyp{
	  def const(n: Long): Term 
	}
	
	object Z extends SmallTyp
	
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
	  
	}
	
	object N extends SmallTyp
	
	case class Fin(n: Long) extends SmallTyp
	
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
	  
	  def subs(x: provingground.HoTT.Term,y: provingground.HoTT.Term) = (x, y) match {
	    case (u, v: FuncTerm[Term,U]) if u == this => v
	    case _ => make((n: Long) => f(n).subs(x, y), codom)
	  }
	  
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
}