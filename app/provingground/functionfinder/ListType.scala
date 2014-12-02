package provingground.functionfinder
import provingground.HoTT._
import ScalaRep._
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}

object ListType {
  case class ListTyp[U<: Term](elemTyp: Typ[U]) extends SmallTyp
  
  case class ListTerm[U <: Term](value: List[U], elemTyp: Typ[U]) extends ConstTerm[List[U]]{
    val typ = ListTyp(elemTyp)
  }
  
  case class ListRep[U<: Term](elemTyp: Typ[U]) extends ScalaRep[Term, List[U]]{
    val typ = ListTyp(elemTyp)
    
    def apply(l: List[U]) = ListTerm(l, elemTyp)
    
    def unapply(u: Term) = u match {
      case ListTerm(l, `elemTyp`) => Some(l.asInstanceOf[List[U]])
      case _ => None
    }
    
    def subs(x: Term, y: Term) = ListRep(elemTyp.subs(x, y))
  }
  
  def foldFunction[U <: Term with Subs[U]: TypeTag, V <: Term with Subs[V] : TypeTag](u: Typ[U], v: Typ[V]) = {
    val rep = ListRep(u) -->: v -->: (u -->: v -->: v) -->: v 
    val fld = (l: List[U]) => (init : V) => (op : U => V => V) => {
      def cop(u: U, v: V) = op(u)(v)
      (l :\ init)(cop)
    }
    rep(fld)
  }
  
  def lmapFunc[U <: Term with Subs[U] : TypeTag, V <: Term : TypeTag](u: Typ[U], v: Typ[V]) = {
    val rep = (u -->: v) -->: ListRep(u) -->: ListRep(v)
    rep((f: U => V) => (l: List[U]) => l map (f))
  }
  
  lazy val lmap = depFunc(__, (u: Typ[Term])=> depFunc(__, (v: Typ[Term]) => lmapFunc(u, v)))
  
  lazy val foldLeft = depFunc(__, (u: Typ[Term]) => depFunc(__, (v: Typ[Term]) => foldFunction(u, v)))
  
  def headOptFn[U <: Term : TypeTag](typ: Typ[U]) = {
    val rep = ListRep(typ) -->: MaybeTyp.MaybeRep(typ)
    rep ((l: List[U]) => l.headOption)
  }
  
  
  
  def tailFn[U <: Term : TypeTag](typ: Typ[U]) = {
    val rep = ListRep(typ) -->: ListRep(typ)
    rep((l: List[U]) => l drop 1)
  }
  
  lazy val headOpt = depFunc(__, (u: Typ[Term]) => headOptFn(u))

  lazy val tail = depFunc(__, (u: Typ[Term]) => tailFn(u))
}