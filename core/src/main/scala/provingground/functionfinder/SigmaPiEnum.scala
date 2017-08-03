package provingground.functionfinder

import provingground._, HoTT._
import EnumType._
import ListType._
import ScalaRep._

object SigmaPiEnum {
  private val A = "A" :: Type

  private val B = "B" :: A ->: Type

  private val a = "a" :: A

  /**
   * (a: A) -> (B(a) -> Sigma(B))
   */
  val resSigma = lambda(A)(
    lambda(B)(
      lambda(a)(B(a) ->: SigmaTyp(B))))

  private val allA = "allA" :: EnumTyp(A)

  private def fn(en: EnumTerm[Term]) = {
    val l = en.value map (B(_))
    (l :\ (PiDefn(B): Typ[Term]))(_ ->: _)
  }

  private val rep = EnumRep(A) -->: Type

  private val foldArrow = rep(fn)

  /**
   *  given enumeration, resolves Pi (for all)
   */
  val resPi = lambda(A)(
    lambda(B)(
      lambda(allA)(
        foldArrow(allA))))
}
