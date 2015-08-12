package provingground

import scala.language.implicitConversions

import HoTT._

trait ScalaPolyRep[U <: Term with Subs[U], W]{
  def apply(typ: Typ[U])(elem : W) : Option[U]

  def unapply(term: U) : Option[W]

  def ||(that : ScalaPolyRep[U, W]) = OrScalaPolyRep(this, that)

  def subs(x: Term, y: Term) : ScalaPolyRep[U, W]
  }

import ScalaPolyRep._

case class ScalaRepWrap[U<: Term with Subs[U], W](rep: ScalaRep[U, W]) extends ScalaPolyRep[U, W]{
  def apply(typ: Typ[U])(elem: W) = if (typ == rep.typ) Some(rep(elem)) else None

  def unapply(term : U) = rep.unapply(term)

  def subs(x: Term, y: Term) : ScalaPolyRep[U, W] = ScalaRepWrap(rep.subs(x, y))
}

case class OrScalaPolyRep[U<: Term with Subs[U], W](first: ScalaPolyRep[U, W], second: ScalaPolyRep[U, W]) extends ScalaPolyRep[U, W]{
  def apply(typ : Typ[U])(elem : W) = first(typ)(elem) orElse (second(typ)(elem))

  def unapply(term: U) = first.unapply(term) orElse(second.unapply(term))

  def subs(x: Term, y: Term) : ScalaPolyRep[U, W] = OrScalaPolyRep(first.subs(x, y), second.subs(x, y))
}

case class FuncPolyRep[U <: Term with Subs[U], W, X <: Term with Subs[X], Y](domrep : ScalaPolyRep[U, W], codrep: ScalaPolyRep[X, Y]) extends ScalaPolyRep[Func[U, X], W => Y]{
  def apply(typ : Typ[Func[U, X]])(elem : W => Y) = typ match {
    case FuncTyp(dom : Typ[U], codom: Typ[X]) => Some(ExtendedFunction(elem, domrep, codrep, dom, codom))
    case _ => None
  }

  def unapply(term: Func[U, X]) = term match {
    case ExtendedFunction(dfn : (W => Y), domrep, codomrep, dom, codom)  => Some(dfn)
    case _ => None
  }

  def subs(x: Term, y: Term) = FuncPolyRep(domrep.subs(x, y), codrep.subs(x, y))
}

object ScalaPolyRep{
  implicit def poly[U<: Term with Subs[U], W](rep : ScalaRep[U, W]) : ScalaPolyRep[U, W] = ScalaRepWrap(rep)

    /**
   * Formal extension of a function given by a definition and representations for
   * domain and codomain.
   */
  case class ExtendedFunction[U <: Term with Subs[U], V, X <: Term with Subs[X], Y](
    dfn: V => Y,
    domrep: ScalaPolyRep[U, V], codomrep: ScalaPolyRep[X, Y], dom: Typ[U], codom: Typ[X]
  ) extends Func[U, X] {

    lazy val typ = dom ->: codom

    def newobj = typ.obj

    def act(u: U) = u match {
      case domrep(v) => codomrep(codom)(dfn(v)).getOrElse(codom.symbObj(ApplnSym(this, u)))
      case _ => codom.symbObj(ApplnSym(this, u))
    }

    // val domobjtpe: reflect.runtime.universe.Type = typeOf[U]

    // val codomobjtpe: reflect.runtime.universe.Type = typeOf[X]

    def subs(x: provingground.HoTT.Term, y: provingground.HoTT.Term) = (x, y) match {
      case (u, v: Func[U, X]) if u == this => v
      case _ => ExtendedFunction((v: V) => dfn(v), domrep.subs(x, y), codomrep.subs(x, y), dom.subs(x, y), codom.subs(x, y))
    }

  }


}
