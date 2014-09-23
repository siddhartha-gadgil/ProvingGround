package provingground.functionfinder
import provingground.HoTT._
import ScalaRep._
import IntTypes._
import EnumType._
import ListType._
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}

object BigOps {
  case class EnumFin(n: Long) extends EnumTerm[Term]{
    val elemTyp = Fin(n)
    
    val value = (0 to (n-1).toInt).toList map ((k: Int) => dsl.i[Long](Fin(n))(k))
  }
  
  def bigSumPolyFunc[U <: Term : TypeTag](typ: Typ[U], en: EnumTerm[U], f: U => Term) = {
	assert(typ == en.elemTyp)
	val lst = enumlist(en.elemTyp)(en)
	fold(typ)(N)(lst)(Nrep(0))(Nsum)
  }
  
  def bigSumFunc[U <: Term : TypeTag](typ: Typ[U]) = {
    val rep = EnumRep(typ) -->: (typ -->: N) -->: N
    rep((en: EnumTerm[U]) => (f : U => Term) => bigSumPolyFunc(typ, en, f))
  }
  
  val bigSum = depFunc(__, bigSumFunc[Term])
}