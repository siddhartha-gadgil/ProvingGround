package provingground.functionfinder

import provingground._, HoTT._
import ScalaRep._
import scala.reflect.runtime.universe.{
  Try => UnivTry,
  Function => FunctionUniv,
  _
}
import ListType._
//import provingground.ScalaUniv._

object EnumType {
  case class EnumTyp[U <: Term with Subs[U]](elemTyp: Typ[U]) extends SmallTyp

  case class EnumTerm[U <: Term with Subs[U]](value: List[U], elemTyp: Typ[U])
      extends ConstTerm[List[U]] {
    val typ = EnumTyp(elemTyp)

    lazy val rep = EnumRep(elemTyp)
  }

  case class EnumRep[U <: Term with Subs[U]](elemTyp: Typ[U])
      extends ScalaRep[Term, EnumTerm[U]] {
    val typ = EnumTyp(elemTyp)

    def apply(v: EnumTerm[U]) = v

    def unapply(u: Term) = u match {
      case en: EnumTerm[_] if en.elemTyp == elemTyp =>
        Some(en.asInstanceOf[EnumTerm[U]])
      case _ => None
    }

    def subs(x: Term, y: Term) = EnumRep(elemTyp.subs(x, y))
  }

  def enumList[U <: Term with Subs[U]](elemTyp: Typ[U]) = {
    val rep = EnumRep(elemTyp) -->: ListRep(elemTyp)
    rep((en: EnumTerm[U]) => en.value)
  }

  val enumlist = lambda("u" :: Type)(enumList("u" :: Type))
}
