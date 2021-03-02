package provingground.scalahott
import provingground._
import ScalaRep._
import IntTypes._
import EnumType._

object EnumFin {
  def enumFinList(n: Long) = {
    (1 to (n - 1).toInt).toList map (Fin(n).rep(_))
  }

  def enumFinFn(n: Long) = EnumTerm(enumFinList(n), Fin(n))

  private val rep = N.rep ~~>: ((k: Long) => EnumRep(Fin(k)))

  val enumFin = rep(enumFinFn)
}
