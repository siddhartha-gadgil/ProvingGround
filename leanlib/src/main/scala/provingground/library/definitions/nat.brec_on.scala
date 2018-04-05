package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object nat$brec_on {
  val value = lambda("'s_928955760" :: FuncTyp("nat" :: Type, Type))(lambda("'t_607759390" :: "nat" :: Type)(lmbda("'u_1284612690" :: piDefn("'u_1523983247" :: "nat" :: Type)(FuncTyp(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_159188406" :: "nat" :: Type)(lmbda("'i_854809894" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'h_159188406" :: "nat" :: Type))("'i_854809894" :: Type))("punit" :: Type))))("'u_1523983247" :: "nat" :: Type), ("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'u_1523983247" :: "nat" :: Type))))(({
    val rxyz = pprodInd.value(("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'t_607759390" :: "nat" :: Type))(({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(lmbda("'h_159188406" :: "nat" :: Type)(lmbda("'i_854809894" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'h_159188406" :: "nat" :: Type))("'i_854809894" :: Type))("punit" :: Type))))("'t_607759390" :: "nat" :: Type)).rec(("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'t_607759390" :: "nat" :: Type))
    rxyz
  })(lmbda("'s_274967088" :: ("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'t_607759390" :: "nat" :: Type))(lmbda("_" :: ({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_159188406" :: "nat" :: Type)(lmbda("'i_854809894" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'h_159188406" :: "nat" :: Type))("'i_854809894" :: Type))("punit" :: Type))))("'t_607759390" :: "nat" :: Type))("'s_274967088" :: ("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'t_607759390" :: "nat" :: Type))))(({
    val rxyz = natInd.value.induc(lmbda("'v_1087445862" :: "nat" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'v_1087445862" :: "nat" :: Type))(({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(lmbda("'h_159188406" :: "nat" :: Type)(lmbda("'i_854809894" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'h_159188406" :: "nat" :: Type))("'i_854809894" :: Type))("punit" :: Type))))("'v_1087445862" :: "nat" :: Type))))
    rxyz
  })(("pprod.mk" :: piDefn("'f" :: Type)(piDefn("'g" :: Type)(FuncTyp("'f" :: Type, FuncTyp("'g" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f" :: Type)("'g" :: Type))))))(("'s_928955760" :: FuncTyp("nat" :: Type, Type))("nat.zero" :: "nat" :: Type))("punit" :: Type)(("'u_1284612690" :: piDefn("'u_1523983247" :: "nat" :: Type)(FuncTyp(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_159188406" :: "nat" :: Type)(lmbda("'i_854809894" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'h_159188406" :: "nat" :: Type))("'i_854809894" :: Type))("punit" :: Type))))("'u_1523983247" :: "nat" :: Type), ("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'u_1523983247" :: "nat" :: Type))))("nat.zero" :: "nat" :: Type)("punit.star" :: "punit" :: Type))("punit.star" :: "punit" :: Type))(lambda("'v_256405719" :: "nat" :: Type)(lmbda("'w_221055569" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'v_256405719" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_159188406" :: "nat" :: Type)(lmbda("'i_854809894" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'h_159188406" :: "nat" :: Type))("'i_854809894" :: Type))("punit" :: Type))))("'v_256405719" :: "nat" :: Type)))(("pprod.mk" :: piDefn("'f" :: Type)(piDefn("'g" :: Type)(FuncTyp("'f" :: Type, FuncTyp("'g" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f" :: Type)("'g" :: Type))))))(("'s_928955760" :: FuncTyp("nat" :: Type, Type))(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'v_256405719" :: "nat" :: Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'v_256405719" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_159188406" :: "nat" :: Type)(lmbda("'i_854809894" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'h_159188406" :: "nat" :: Type))("'i_854809894" :: Type))("punit" :: Type))))("'v_256405719" :: "nat" :: Type)))("punit" :: Type))(("'u_1284612690" :: piDefn("'u_1523983247" :: "nat" :: Type)(FuncTyp(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_159188406" :: "nat" :: Type)(lmbda("'i_854809894" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'h_159188406" :: "nat" :: Type))("'i_854809894" :: Type))("punit" :: Type))))("'u_1523983247" :: "nat" :: Type), ("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'u_1523983247" :: "nat" :: Type))))(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'v_256405719" :: "nat" :: Type))(("pprod.mk" :: piDefn("'f" :: Type)(piDefn("'g" :: Type)(FuncTyp("'f" :: Type, FuncTyp("'g" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f" :: Type)("'g" :: Type))))))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'v_256405719" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_159188406" :: "nat" :: Type)(lmbda("'i_854809894" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'h_159188406" :: "nat" :: Type))("'i_854809894" :: Type))("punit" :: Type))))("'v_256405719" :: "nat" :: Type)))("punit" :: Type)("'w_221055569" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'v_256405719" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_159188406" :: "nat" :: Type)(lmbda("'i_854809894" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'h_159188406" :: "nat" :: Type))("'i_854809894" :: Type))("punit" :: Type))))("'v_256405719" :: "nat" :: Type)))("punit.star" :: "punit" :: Type)))(("pprod.mk" :: piDefn("'f" :: Type)(piDefn("'g" :: Type)(FuncTyp("'f" :: Type, FuncTyp("'g" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f" :: Type)("'g" :: Type))))))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'v_256405719" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_159188406" :: "nat" :: Type)(lmbda("'i_854809894" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'h_159188406" :: "nat" :: Type))("'i_854809894" :: Type))("punit" :: Type))))("'v_256405719" :: "nat" :: Type)))("punit" :: Type)("'w_221055569" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'v_256405719" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_159188406" :: "nat" :: Type)(lmbda("'i_854809894" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760" :: FuncTyp("nat" :: Type, Type))("'h_159188406" :: "nat" :: Type))("'i_854809894" :: Type))("punit" :: Type))))("'v_256405719" :: "nat" :: Type)))("punit.star" :: "punit" :: Type)))))("'t_607759390" :: "nat" :: Type)))))
}
