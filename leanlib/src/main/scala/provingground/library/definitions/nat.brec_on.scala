package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object nat$brec_on {
  lazy val value = lambda("'s_681745223" :: FuncTyp("nat" :: Type, Type))(lambda("'t_1766276349" :: "nat" :: Type)(lmbda("'u_455648273" :: piDefn("'u_1484477359" :: "nat" :: Type)(FuncTyp(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'h_403586353" :: "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'u_1484477359" :: "nat" :: Type), ("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'u_1484477359" :: "nat" :: Type))))(({
    val rxyz = pprodInd.value(("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'t_1766276349" :: "nat" :: Type))(({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'h_403586353" :: "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'t_1766276349" :: "nat" :: Type)).rec(("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'t_1766276349" :: "nat" :: Type))
    rxyz
  })(lmbda("'s_597289555" :: ("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'t_1766276349" :: "nat" :: Type))(lmbda("_" :: ({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'h_403586353" :: "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'t_1766276349" :: "nat" :: Type))("'s_597289555" :: ("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'t_1766276349" :: "nat" :: Type))))(({
    val rxyz = natInd.value.induc(lmbda("'v_1619197280" :: "nat" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'v_1619197280" :: "nat" :: Type))(({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'h_403586353" :: "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_1619197280" :: "nat" :: Type))))
    rxyz
  })(("pprod.mk" :: piDefn("'f_234597865" :: Type)(piDefn("'g_1111384708" :: Type)(FuncTyp("'f_234597865" :: Type, FuncTyp("'g_1111384708" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_234597865" :: Type)("'g_1111384708" :: Type))))))(("'s_681745223" :: FuncTyp("nat" :: Type, Type))("nat.zero" :: "nat" :: Type))("punit" :: Type)(("'u_455648273" :: piDefn("'u_1484477359" :: "nat" :: Type)(FuncTyp(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'h_403586353" :: "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'u_1484477359" :: "nat" :: Type), ("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'u_1484477359" :: "nat" :: Type))))("nat.zero" :: "nat" :: Type)("punit.star" :: "punit" :: Type))("punit.star" :: "punit" :: Type))(lambda("'v_406623033" :: "nat" :: Type)(lmbda("'w_1239964977" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'v_406623033" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'h_403586353" :: "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_406623033" :: "nat" :: Type)))(("pprod.mk" :: piDefn("'f_234597865" :: Type)(piDefn("'g_1111384708" :: Type)(FuncTyp("'f_234597865" :: Type, FuncTyp("'g_1111384708" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_234597865" :: Type)("'g_1111384708" :: Type))))))(("'s_681745223" :: FuncTyp("nat" :: Type, Type))(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'v_406623033" :: "nat" :: Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'v_406623033" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'h_403586353" :: "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_406623033" :: "nat" :: Type)))("punit" :: Type))(("'u_455648273" :: piDefn("'u_1484477359" :: "nat" :: Type)(FuncTyp(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'h_403586353" :: "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'u_1484477359" :: "nat" :: Type), ("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'u_1484477359" :: "nat" :: Type))))(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'v_406623033" :: "nat" :: Type))(("pprod.mk" :: piDefn("'f_234597865" :: Type)(piDefn("'g_1111384708" :: Type)(FuncTyp("'f_234597865" :: Type, FuncTyp("'g_1111384708" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_234597865" :: Type)("'g_1111384708" :: Type))))))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'v_406623033" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'h_403586353" :: "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_406623033" :: "nat" :: Type)))("punit" :: Type)("'w_1239964977" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'v_406623033" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'h_403586353" :: "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_406623033" :: "nat" :: Type)))("punit.star" :: "punit" :: Type)))(("pprod.mk" :: piDefn("'f_234597865" :: Type)(piDefn("'g_1111384708" :: Type)(FuncTyp("'f_234597865" :: Type, FuncTyp("'g_1111384708" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_234597865" :: Type)("'g_1111384708" :: Type))))))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'v_406623033" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'h_403586353" :: "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_406623033" :: "nat" :: Type)))("punit" :: Type)("'w_1239964977" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'v_406623033" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_681745223" :: FuncTyp("nat" :: Type, Type))("'h_403586353" :: "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_406623033" :: "nat" :: Type)))("punit.star" :: "punit" :: Type)))))("'t_1766276349" :: "nat" :: Type)))))
}
