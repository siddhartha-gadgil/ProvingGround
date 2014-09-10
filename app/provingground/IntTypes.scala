package provingground

import HoTT._
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}

object IntTypes {
  // Abstract code, to eventually move to another object.
  // should build combinators for scala representations. 
  trait ScalaRep[U <: Term, V]{
    val typ : Typ[U]
    
    type tpe = V
    
    def apply(v: V) : U
    
    def unapply(u: U) : Option[V]
  }
  
  //An example
  object IntRep extends ScalaRep[Term, Long]{
    val typ = Z
    
    def apply(n: Long) = Zcnst(n)
    
    def unapply(u: Term) = u match {
      case Zcnst(n, _) => Some(n)
      case _ => None
    }

  }
  
  case class FuncRep[U <: Term : TypeTag, V, X <: Term : TypeTag, Y](
      domrep: ScalaRep[U, V], codomrep: ScalaRep[X, Y]) extends ScalaRep[FuncTerm[U, X], V => Y]{
    val typ = domrep.typ ->: codomrep.typ
    
    def apply(f: V => Y) = ExtendedFunction(f, domrep, codomrep)
    
    def unapply(u: FuncTerm[U, X]) : Option[V => Y] = u match {
      case ext: ExtendedFunction[_, V, _, Y] if ext.domrep == domrep && ext.codomrep == codomrep => Some(ext.dfn)
      case _ => None
    }
  }
  
  case class SimpleConst[V](value: V, typ: Typ[Term]) extends ConstTerm[V]
  
  case class SimpleRep[V](typ: Typ[Term]) extends ScalaRep[Term, V]{
    def apply(v: V) = SimpleConst(v, typ)
    
    def unapply(u : Term) : Option[V] = u match  {
      case smp: SimpleConst[V] => Some(smp.value)
      case _ => None
    }
  }
  
  case class ExtendedFunction[U <: Term : TypeTag, V, X <: Term : TypeTag, Y](dfn: V => Y, 
      domrep: ScalaRep[U, V], codomrep: ScalaRep[X, Y]) extends FuncObj[U, X]{
    
	  val dom = domrep.typ
	  
	  val codom = codomrep.typ
	  
	  val typ = dom ->: codom
	  
	  def apply(u : U) = u match {
	    case domrep(v) => codomrep(dfn(v))
	    case _ => codom.symbObj(ApplnSym(this, u))
	  }
	  
	  val domobjtpe: reflect.runtime.universe.Type = typeOf[U]
	  
	  val codomobjtpe: reflect.runtime.universe.Type = typeOf[X]
	  
//	  val depcodom: provingground.HoTT.Term => provingground.HoTT.Typ[X] = (t) => codom

	  
	  def subs(x: provingground.HoTT.Term,y: provingground.HoTT.Term) = (x, y) match {
	    case (u, v: FuncObj[U ,X]) if u == this => v
	    case _ => this
	  }
    
  }
  
  
  
  
    case class ExtendedDepFunction[U <: Term : TypeTag, V, X <: Term : TypeTag, Y](dfn: V => Y, 
      domrep: ScalaRep[U, V], codomreps: V => ScalaRep[X, Y], fibers: TypFamily[U, X]) extends FuncTerm[U, X]{
    
	  val dom = domrep.typ
	  
	  val depcodom : U => Typ[X] = (arg : U) => fibers(arg)
	  
	  val typ = PiTyp(fibers)
	  
	  def apply(u : U) = u match {
	    case domrep(v) => codomreps(v)(dfn(v))
	    case arg => fibers(arg).symbObj(ApplnSym(this, arg))
	  }
	  
	  val domobjtpe: reflect.runtime.universe.Type = typeOf[Term]
	  
	  val codomobjtpe: reflect.runtime.universe.Type = typeOf[U]
	  

	  
	  def subs(x: provingground.HoTT.Term,y: provingground.HoTT.Term) = (x, y) match {
	    case (u, v: FuncTerm[U ,X]) if u == this => v
	    case _ => this
	  }
    
  }
  
  

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
	
	def bigsum(n: Term)(f: FuncTerm[Term, Term])  =  {
	  assert (f.typ == fin(n) ->: Z) 
	}
}