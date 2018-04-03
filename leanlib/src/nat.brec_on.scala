
package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
object nat$brec_on {
  val value = lambda("'s" :: FuncTyp("nat" :: Type, Type))(lambda("'t" :: "nat" :: Type)(lmbda("'u" :: piDefn("'u" :: "nat" :: Type)(FuncTyp(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h" :: "nat" :: Type)(lmbda("'i" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s" :: FuncTyp("nat" :: Type, Type))("'h" :: "nat" :: Type))("'i" :: Type))("punit" :: Type))))("'u" :: "nat" :: Type), ("'s" :: FuncTyp("nat" :: Type, Type))("'u" :: "nat" :: Type))))(({
    val rxyz = pprodInd.value(("'s" :: FuncTyp("nat" :: Type, Type))("'t" :: "nat" :: Type))(({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(lmbda("'h" :: "nat" :: Type)(lmbda("'i" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s" :: FuncTyp("nat" :: Type, Type))("'h" :: "nat" :: Type))("'i" :: Type))("punit" :: Type))))("'t" :: "nat" :: Type)).rec(("'s" :: FuncTyp("nat" :: Type, Type))("'t" :: "nat" :: Type))
    rxyz
  })(lmbda("'s" :: ("'s" :: FuncTyp("nat" :: Type, Type))("'t" :: "nat" :: Type))(lmbda("_" :: ({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h" :: "nat" :: Type)(lmbda("'i" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s" :: FuncTyp("nat" :: Type, Type))("'h" :: "nat" :: Type))("'i" :: Type))("punit" :: Type))))("'t" :: "nat" :: Type))("'s" :: ("'s" :: FuncTyp("nat" :: Type, Type))("'t" :: "nat" :: Type))))(({
    val rxyz = natInd.value.induc(lmbda("'v" :: "nat" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s" :: FuncTyp("nat" :: Type, Type))("'v" :: "nat" :: Type))(({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(lmbda("'h" :: "nat" :: Type)(lmbda("'i" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s" :: FuncTyp("nat" :: Type, Type))("'h" :: "nat" :: Type))("'i" :: Type))("punit" :: Type))))("'v" :: "nat" :: Type))))
    rxyz
  })(("pprod.mk" :: piDefn("'f" :: Type)(piDefn("'g" :: Type)(FuncTyp("'f" :: Type, FuncTyp("'g" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f" :: Type)("'g" :: Type))))))(("'s" :: FuncTyp("nat" :: Type, Type))("nat.zero" :: "nat" :: Type))("punit" :: Type)(("'u" :: piDefn("'u" :: "nat" :: Type)(FuncTyp(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h" :: "nat" :: Type)(lmbda("'i" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s" :: FuncTyp("nat" :: Type, Type))("'h" :: "nat" :: Type))("'i" :: Type))("punit" :: Type))))("'u" :: "nat" :: Type), ("'s" :: FuncTyp("nat" :: Type, Type))("'u" :: "nat" :: Type))))("nat.zero" :: "nat" :: Type)("punit.star" :: "punit" :: Type))("punit.star" :: "punit" :: Type))(lambda("'v" :: "nat" :: Type)(lmbda("'w" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s" :: FuncTyp("nat" :: Type, Type))("'v" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h" :: "nat" :: Type)(lmbda("'i" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s" :: FuncTyp("nat" :: Type, Type))("'h" :: "nat" :: Type))("'i" :: Type))("punit" :: Type))))("'v" :: "nat" :: Type)))(("pprod.mk" :: piDefn("'f" :: Type)(piDefn("'g" :: Type)(FuncTyp("'f" :: Type, FuncTyp("'g" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f" :: Type)("'g" :: Type))))))(("'s" :: FuncTyp("nat" :: Type, Type))(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'v" :: "nat" :: Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s" :: FuncTyp("nat" :: Type, Type))("'v" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h" :: "nat" :: Type)(lmbda("'i" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s" :: FuncTyp("nat" :: Type, Type))("'h" :: "nat" :: Type))("'i" :: Type))("punit" :: Type))))("'v" :: "nat" :: Type)))("punit" :: Type))(("'u" :: piDefn("'u" :: "nat" :: Type)(FuncTyp(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h" :: "nat" :: Type)(lmbda("'i" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s" :: FuncTyp("nat" :: Type, Type))("'h" :: "nat" :: Type))("'i" :: Type))("punit" :: Type))))("'u" :: "nat" :: Type), ("'s" :: FuncTyp("nat" :: Type, Type))("'u" :: "nat" :: Type))))(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'v" :: "nat" :: Type))(("pprod.mk" :: piDefn("'f" :: Type)(piDefn("'g" :: Type)(FuncTyp("'f" :: Type, FuncTyp("'g" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f" :: Type)("'g" :: Type))))))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s" :: FuncTyp("nat" :: Type, Type))("'v" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h" :: "nat" :: Type)(lmbda("'i" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s" :: FuncTyp("nat" :: Type, Type))("'h" :: "nat" :: Type))("'i" :: Type))("punit" :: Type))))("'v" :: "nat" :: Type)))("punit" :: Type)("'w" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s" :: FuncTyp("nat" :: Type, Type))("'v" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h" :: "nat" :: Type)(lmbda("'i" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s" :: FuncTyp("nat" :: Type, Type))("'h" :: "nat" :: Type))("'i" :: Type))("punit" :: Type))))("'v" :: "nat" :: Type)))("punit.star" :: "punit" :: Type)))(("pprod.mk" :: piDefn("'f" :: Type)(piDefn("'g" :: Type)(FuncTyp("'f" :: Type, FuncTyp("'g" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f" :: Type)("'g" :: Type))))))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s" :: FuncTyp("nat" :: Type, Type))("'v" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h" :: "nat" :: Type)(lmbda("'i" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s" :: FuncTyp("nat" :: Type, Type))("'h" :: "nat" :: Type))("'i" :: Type))("punit" :: Type))))("'v" :: "nat" :: Type)))("punit" :: Type)("'w" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s" :: FuncTyp("nat" :: Type, Type))("'v" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h" :: "nat" :: Type)(lmbda("'i" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s" :: FuncTyp("nat" :: Type, Type))("'h" :: "nat" :: Type))("'i" :: Type))("punit" :: Type))))("'v" :: "nat" :: Type)))("punit.star" :: "punit" :: Type)))))("'t" :: "nat" :: Type)))))
}