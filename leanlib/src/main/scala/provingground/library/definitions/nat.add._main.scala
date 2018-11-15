package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object nat$add$_main {
  lazy val value = lmbda("'z_924862204" :: "nat" :: Type)(lmbda("'aa_63933964" :: "nat" :: Type)(({
    val rxyz = pprodInd.value(FuncTyp("nat" :: Type, "nat" :: Type))(({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'aa_63933964" :: "nat" :: Type)).rec(FuncTyp("nat" :: Type, "nat" :: Type))
    rxyz
  })(lmbda("'s_1610069062" :: FuncTyp("nat" :: Type, "nat" :: Type))(lmbda("_" :: ({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'aa_63933964" :: "nat" :: Type))("'s_1610069062" :: FuncTyp("nat" :: Type, "nat" :: Type))))(({
    val rxyz = natInd.value.induc(lmbda("'v_1619197280" :: "nat" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))(({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_1619197280" :: "nat" :: Type))))
    rxyz
  })(("pprod.mk" :: piDefn("'f_234597865" :: Type)(piDefn("'g_1111384708" :: Type)(FuncTyp("'f_234597865" :: Type, FuncTyp("'g_1111384708" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_234597865" :: Type)("'g_1111384708" :: Type))))))(FuncTyp("nat" :: Type, "nat" :: Type))("punit" :: Type)(lmbda("'ad_1031270242" :: "nat" :: Type)("'ad_1031270242" :: "nat" :: Type))("punit.star" :: "punit" :: Type))(lambda("'v_406623033" :: "nat" :: Type)(lmbda("'w_1245014100" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_406623033" :: "nat" :: Type)))(("pprod.mk" :: piDefn("'f_234597865" :: Type)(piDefn("'g_1111384708" :: Type)(FuncTyp("'f_234597865" :: Type, FuncTyp("'g_1111384708" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_234597865" :: Type)("'g_1111384708" :: Type))))))(FuncTyp("nat" :: Type, "nat" :: Type))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_406623033" :: "nat" :: Type)))("punit" :: Type))(lmbda("'ad_1031270242" :: "nat" :: Type)(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))(({
    val rxyz = pprodInd.value(FuncTyp("nat" :: Type, "nat" :: Type))(({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(lmbda("_" :: "nat" :: Type)(lmbda("'ak_1096040783" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'ak_1096040783" :: Type))("punit" :: Type))))("'v_406623033" :: "nat" :: Type)).rec(FuncTyp("nat" :: Type, "nat" :: Type))
    rxyz
  })(lmbda("'s_1815858308" :: FuncTyp("nat" :: Type, "nat" :: Type))(lmbda("_" :: ({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("_" :: "nat" :: Type)(lmbda("'ak_1096040783" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'ak_1096040783" :: Type))("punit" :: Type))))("'v_406623033" :: "nat" :: Type))("'s_1815858308" :: FuncTyp("nat" :: Type, "nat" :: Type))))("'w_1245014100" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_406623033" :: "nat" :: Type)))("'ad_1031270242" :: "nat" :: Type))))(("pprod.mk" :: piDefn("'f_234597865" :: Type)(piDefn("'g_1111384708" :: Type)(FuncTyp("'f_234597865" :: Type, FuncTyp("'g_1111384708" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_234597865" :: Type)("'g_1111384708" :: Type))))))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_406623033" :: "nat" :: Type)))("punit" :: Type)("'w_1245014100" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_406623033" :: "nat" :: Type)))("punit.star" :: "punit" :: Type)))))("'aa_63933964" :: "nat" :: Type))("'z_924862204" :: "nat" :: Type)))
}
