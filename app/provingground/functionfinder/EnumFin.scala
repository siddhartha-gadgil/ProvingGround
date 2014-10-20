package provingground.functionfinder
import provingground.HoTT._
import ScalaRep._
import IntTypes._
import EnumType._

object EnumFin {
  def enumFinFn(n: Long) = {
    val value = (1 to (n-1).toInt).toList map (Fin(n).rep(_))
    EnumTerm(value, Fin(n))
  }
  
  private val rep = N.rep ~~>: ((k : Long) => EnumRep(Fin(k)))
  
  val enumFin = rep(enumFinFn)
}