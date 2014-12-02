package provingground.functionfinder

import provingground.HoTT._
import ScalaRep._
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}

object MaybeTyp {
  case class MaybeTyp[U<: Term](tp: Typ[U]) extends SmallTyp
  
  case class Maybe[U <: Term](opt: Option[U], tp: Typ[U]) extends AtomicTerm{
    val typ = MaybeTyp(tp)
  }
  
  def get[U <: Term] : PartialFunction[Maybe[U], U] = {
    case Maybe(Some(x), _) => x
  }
  
  case class MaybeRep[U <: Term : TypeTag](typ: Typ[U]) extends ScalaRep[Term, Option[U]]{
    def apply (term: Option[U]) = Maybe(term, typ)
    
    def unapply(term: Term) = term match {
      case Maybe(Some(x), `typ`) => Some(Some(x.asInstanceOf[U]))
      case _ => None
    }
    
    def subs(x: Term, y: Term) = MaybeRep(typ.subs(x, y))
  }
}