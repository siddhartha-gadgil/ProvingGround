package provingground

import HoTT._
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}

object IntTypes {

  trait ScalaRep[+U <: Term, V]{
    val typ : Typ[U]
    
    type tpe = V
    
    def apply(v: V) : U
    
    def unapply(u: Term) : Option[V]
    
    def ->:[W <: Term : TypeTag, X, UU >: U <: Term : TypeTag](that : ScalaRep[W, X]) = 
      FuncRep[W, X, UU, V](that, this)
    
    def ++[UU >: U <: Term with Subs[UU]: TypeTag, X <: Term with Subs[X]: TypeTag, Y](
        codrepfmly: V => ScalaRep[X, Y]) = SigmaRep[UU, V, X, Y](this, codrepfmly)
  }
  
  
  case class SimpleConst[V](value: V, typ: Typ[Term]) extends ConstTerm[V]{
    override def toString = value.toString
  }
  
  case class SimpleRep[V](typ: Typ[Term]) extends ScalaRep[Term, V]{
    def apply(v: V) = SimpleConst(v, typ)
    
    def unapply(u : Term) : Option[V] = u match  {
      case smp: SimpleConst[V] if smp.typ == typ => Some(smp.value)
      case _ => None
    }
  }
  
 
  
  case class FuncRep[U <: Term : TypeTag, V, X <: Term : TypeTag, Y](
      domrep: ScalaRep[U, V], codomrep: ScalaRep[X, Y]) extends ScalaRep[FuncTerm[U, X], V => Y]{
    val typ = domrep.typ ->: codomrep.typ
    
    def apply(f: V => Y) = ExtendedFunction(f, domrep, codomrep)
    
    def unapply(u: Term) : Option[V => Y] = u match {
      case ext: ExtendedFunction[_, V, _, Y] if ext.domrep == domrep && ext.codomrep == codomrep => Some(ext.dfn)
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
	  
	  
	  def subs(x: provingground.HoTT.Term,y: provingground.HoTT.Term) = (x, y) match {
	    case (u, v: FuncObj[U ,X]) if u == this => v
	    case _ => this
	  }
    
  }
  
  
  
  case class SimpleFuncRep[U <: Term : TypeTag, V, X <: Term : TypeTag](
      domrep: ScalaRep[U, V], codom: Typ[X]) extends ScalaRep[FuncTerm[U, X], V => X]{
    val typ = domrep.typ ->: codom
    
    def apply(f: V => X) = SimpleExtendedFunction(f, domrep, codom)
    
    def unapply(u: Term) : Option[V => X] = u match {
      case ext: SimpleExtendedFunction[_, V, X] if ext.domrep == domrep && ext.codom == codom => Some(ext.dfn)
      case _ => None
    }
  }
  
  case class SimpleExtendedFunction[U <: Term : TypeTag, V, X <: Term : TypeTag](dfn: V => X, 
      domrep: ScalaRep[U, V], codom: Typ[X]) extends FuncObj[U, X]{
    
	  val dom = domrep.typ
	  
	  
	  val typ = dom ->: codom
	  
	  def apply(u : U) = u match {
	    case domrep(v) => dfn(v)
	    case _ => codom.symbObj(ApplnSym(this, u))
	  }
	  
	  val domobjtpe: reflect.runtime.universe.Type = typeOf[U]
	  
	  val codomobjtpe: reflect.runtime.universe.Type = typeOf[X]
	  
	  
	  def subs(x: provingground.HoTT.Term,y: provingground.HoTT.Term) = (x, y) match {
	    case (u, v: FuncObj[U ,X]) if u == this => v
	    case _ => this
	  }
    
  }
  
  
  
  
  case class SigmaRep[U <: Term with Subs[U] : TypeTag, V, X <: Term with Subs[X]: TypeTag, Y](domrep: ScalaRep[U, V],
      codrepfmly: V => ScalaRep[X, Y]) extends ScalaRep[Term, (V, Y)]{

    val typ = SigmaTyp(fibers)
    
    val rep = SimpleFuncRep(domrep, __)
    
    val fibers = rep((v: V) => codrepfmly(v).typ)
    
    def apply(vy: (V, Y))  = DepPair(domrep(vy._1), codrepfmly(vy._1)(vy._2), fibers)
    
    def unapply(u: Term) = u match {
      case DepPair(domrep(v), vy, _) =>
        val codrep = codrepfmly(v)
        vy match {
          case codrep(y) => Some((v, y))
          case _ => None
        }
      case _ => None
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
  
    
    object dsl{
      def i[V](typ: Typ[Term]) = SimpleRep[V](typ)
      
      def s[U <: Term with Subs[U] : TypeTag, V, X <: Term with Subs[X]: TypeTag, Y](domrep: ScalaRep[U, V])(
      codrepfmly: V => ScalaRep[X, Y]) = SigmaRep(domrep, codrepfmly)
    }
    
   
    
    //An example - should change to use SimpleRep and SimpleConst
  object IntRep extends ScalaRep[Term, Long]{
    val typ = Z
    
    def apply(n: Long) = Zcnst(n)
    
    def unapply(u: Term) = u match {
      case Zcnst(n, _) => Some(n)
      case _ => None
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