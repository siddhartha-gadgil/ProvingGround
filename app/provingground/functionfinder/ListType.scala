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
  }
  
  def foldFunction[U <: Term : TypeTag, V <: Term : TypeTag](u: Typ[U], v: Typ[V]) = {
    val rep = ListRep(u) -->: v -->: (u -->: v -->: v) -->: v
    val fld = (l: List[U]) => (init : V) => (op : U => V => V) => {
      def cop(u: U, v: V) = op(u)(v)
      (l :\ init)(cop)
    }
    rep(fld)
  }
  
  def lmapFunc[U <: Term : TypeTag, V <: Term : TypeTag](u: Typ[U], v: Typ[V]) = {
    val rep = (u -->: v) -->: ListRep(u) -->: ListRep(v)
    rep((f: U => V) => (l: List[U]) => l map (f))
  }
  
  lazy val lmap = depFunc(__, (u: Typ[Term])=> depFunc(__, (v: Typ[Term]) => lmapFunc(u, v)))
  
  lazy val foldLeft = depFunc(__, (u: Typ[Term]) => depFunc(__, (v: Typ[Term]) => foldFunction(u, v)))
  
}