package provingground.functionfinder

import provingground.HoTT._
import ScalaRep._
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}
import ListType._

object EnumType {
  case class EnumTyp[U <: Term](elemTyp: Typ[U]) extends SmallTyp
  
  trait EnumTerm[U <: Term] extends ConstTerm[List[U]]{
    val elemTyp : Typ[U]
    
    val typ = EnumTyp(elemTyp)    
    
 //   val tpe: TypeTag[U]
  }
  
  case class EnumRep[U <: Term](elemTyp: Typ[U]) extends ScalaRep[Term, EnumTerm[U]]{
    val typ = EnumTyp(elemTyp)
    
    def apply(v: EnumTerm[U]) = v
    
    def unapply(u: Term) = u match {
      case en: EnumTerm[_] if en.elemTyp == elemTyp => Some(en.asInstanceOf[EnumTerm[U]])
      case _ => None
    }
  }
  
  def enumList[U <: Term](elemTyp: Typ[U]) = {
    val rep = EnumRep(elemTyp) -->: ListRep(elemTyp)
    rep((en: EnumTerm[U]) => en.value)
  }
  
  val enumlist = depFunc(__, enumList[Term])
}