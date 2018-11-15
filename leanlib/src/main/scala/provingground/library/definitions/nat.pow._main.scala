package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object nat$pow$_main {
  lazy val value = lmbda("'ar_414967652" :: "nat" :: Type)(lmbda("'as_1776667304" :: "nat" :: Type)(({
    val rxyz = pprodInd.value("nat" :: Type)(({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("nat" :: Type)("'i_547011919" :: Type))("punit" :: Type))))("'as_1776667304" :: "nat" :: Type)).rec("nat" :: Type)
    rxyz
  })(lmbda("'s_830118413" :: "nat" :: Type)(lmbda("_" :: ({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("nat" :: Type)("'i_547011919" :: Type))("punit" :: Type))))("'as_1776667304" :: "nat" :: Type))("'s_830118413" :: "nat" :: Type)))(({
    val rxyz = natInd.value.induc(lmbda("'v_1619197280" :: "nat" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("nat" :: Type)(({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("nat" :: Type)("'i_547011919" :: Type))("punit" :: Type))))("'v_1619197280" :: "nat" :: Type))))
    rxyz
  })(("pprod.mk" :: piDefn("'f_234597865" :: Type)(piDefn("'g_1111384708" :: Type)(FuncTyp("'f_234597865" :: Type, FuncTyp("'g_1111384708" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_234597865" :: Type)("'g_1111384708" :: Type))))))("nat" :: Type)("punit" :: Type)(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("nat.zero" :: "nat" :: Type))("punit.star" :: "punit" :: Type))(lambda("'v_406623033" :: "nat" :: Type)(lmbda("'w_2018856934" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("nat" :: Type)(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("nat" :: Type)("'i_547011919" :: Type))("punit" :: Type))))("'v_406623033" :: "nat" :: Type)))(("pprod.mk" :: piDefn("'f_234597865" :: Type)(piDefn("'g_1111384708" :: Type)(FuncTyp("'f_234597865" :: Type, FuncTyp("'g_1111384708" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_234597865" :: Type)("'g_1111384708" :: Type))))))("nat" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("nat" :: Type)(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("nat" :: Type)("'i_547011919" :: Type))("punit" :: Type))))("'v_406623033" :: "nat" :: Type)))("punit" :: Type))(({
    val rxyz = pprodInd.value(FuncTyp("nat" :: Type, "nat" :: Type))(({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'ar_414967652" :: "nat" :: Type)).rec(FuncTyp("nat" :: Type, "nat" :: Type))
    rxyz
  })(lmbda("'s_1397176685" :: FuncTyp("nat" :: Type, "nat" :: Type))(lmbda("_" :: ({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'ar_414967652" :: "nat" :: Type))("'s_1397176685" :: FuncTyp("nat" :: Type, "nat" :: Type))))(({
    val rxyz = natInd.value.induc(lmbda("'v_1619197280" :: "nat" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))(({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_1619197280" :: "nat" :: Type))))
    rxyz
  })(("pprod.mk" :: piDefn("'f_234597865" :: Type)(piDefn("'g_1111384708" :: Type)(FuncTyp("'f_234597865" :: Type, FuncTyp("'g_1111384708" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_234597865" :: Type)("'g_1111384708" :: Type))))))(FuncTyp("nat" :: Type, "nat" :: Type))("punit" :: Type)(lmbda("'an_1274351627" :: "nat" :: Type)("nat.zero" :: "nat" :: Type))("punit.star" :: "punit" :: Type))(lambda("'v_172657575" :: "nat" :: Type)(lmbda("'w_79682874" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_172657575" :: "nat" :: Type)))(("pprod.mk" :: piDefn("'f_234597865" :: Type)(piDefn("'g_1111384708" :: Type)(FuncTyp("'f_234597865" :: Type, FuncTyp("'g_1111384708" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_234597865" :: Type)("'g_1111384708" :: Type))))))(FuncTyp("nat" :: Type, "nat" :: Type))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_172657575" :: "nat" :: Type)))("punit" :: Type))(lmbda("'an_1274351627" :: "nat" :: Type)(({
    val rxyz = pprodInd.value(FuncTyp("nat" :: Type, "nat" :: Type))(({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'an_1274351627" :: "nat" :: Type)).rec(FuncTyp("nat" :: Type, "nat" :: Type))
    rxyz
  })(lmbda("'s_1610069062" :: FuncTyp("nat" :: Type, "nat" :: Type))(lmbda("_" :: ({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'an_1274351627" :: "nat" :: Type))("'s_1610069062" :: FuncTyp("nat" :: Type, "nat" :: Type))))(({
    val rxyz = natInd.value.induc(lmbda("'v_1619197280" :: "nat" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))(({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_1619197280" :: "nat" :: Type))))
    rxyz
  })(("pprod.mk" :: piDefn("'f_234597865" :: Type)(piDefn("'g_1111384708" :: Type)(FuncTyp("'f_234597865" :: Type, FuncTyp("'g_1111384708" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_234597865" :: Type)("'g_1111384708" :: Type))))))(FuncTyp("nat" :: Type, "nat" :: Type))("punit" :: Type)(lmbda("'ad_1031270242" :: "nat" :: Type)("'ad_1031270242" :: "nat" :: Type))("punit.star" :: "punit" :: Type))(lambda("'v_683273221" :: "nat" :: Type)(lmbda("'w_820429884" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_683273221" :: "nat" :: Type)))(("pprod.mk" :: piDefn("'f_234597865" :: Type)(piDefn("'g_1111384708" :: Type)(FuncTyp("'f_234597865" :: Type, FuncTyp("'g_1111384708" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_234597865" :: Type)("'g_1111384708" :: Type))))))(FuncTyp("nat" :: Type, "nat" :: Type))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_683273221" :: "nat" :: Type)))("punit" :: Type))(lmbda("'ad_1031270242" :: "nat" :: Type)(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))(({
    val rxyz = pprodInd.value(FuncTyp("nat" :: Type, "nat" :: Type))(({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(lmbda("_" :: "nat" :: Type)(lmbda("'ak_1096040783" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'ak_1096040783" :: Type))("punit" :: Type))))("'v_683273221" :: "nat" :: Type)).rec(FuncTyp("nat" :: Type, "nat" :: Type))
    rxyz
  })(lmbda("'s_1815858308" :: FuncTyp("nat" :: Type, "nat" :: Type))(lmbda("_" :: ({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("_" :: "nat" :: Type)(lmbda("'ak_1096040783" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'ak_1096040783" :: Type))("punit" :: Type))))("'v_683273221" :: "nat" :: Type))("'s_1815858308" :: FuncTyp("nat" :: Type, "nat" :: Type))))("'w_820429884" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_683273221" :: "nat" :: Type)))("'ad_1031270242" :: "nat" :: Type))))(("pprod.mk" :: piDefn("'f_234597865" :: Type)(piDefn("'g_1111384708" :: Type)(FuncTyp("'f_234597865" :: Type, FuncTyp("'g_1111384708" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_234597865" :: Type)("'g_1111384708" :: Type))))))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_683273221" :: "nat" :: Type)))("punit" :: Type)("'w_820429884" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_683273221" :: "nat" :: Type)))("punit.star" :: "punit" :: Type)))))("'an_1274351627" :: "nat" :: Type))(({
    val rxyz = pprodInd.value(FuncTyp("nat" :: Type, "nat" :: Type))(({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(lmbda("_" :: "nat" :: Type)(lmbda("'au_384022397" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'au_384022397" :: Type))("punit" :: Type))))("'v_172657575" :: "nat" :: Type)).rec(FuncTyp("nat" :: Type, "nat" :: Type))
    rxyz
  })(lmbda("'s_1745250008" :: FuncTyp("nat" :: Type, "nat" :: Type))(lmbda("_" :: ({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("_" :: "nat" :: Type)(lmbda("'au_384022397" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'au_384022397" :: Type))("punit" :: Type))))("'v_172657575" :: "nat" :: Type))("'s_1745250008" :: FuncTyp("nat" :: Type, "nat" :: Type))))("'w_79682874" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_172657575" :: "nat" :: Type)))("'an_1274351627" :: "nat" :: Type))))(("pprod.mk" :: piDefn("'f_234597865" :: Type)(piDefn("'g_1111384708" :: Type)(FuncTyp("'f_234597865" :: Type, FuncTyp("'g_1111384708" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_234597865" :: Type)("'g_1111384708" :: Type))))))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_172657575" :: "nat" :: Type)))("punit" :: Type)("'w_79682874" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(FuncTyp("nat" :: Type, "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type))))("'v_172657575" :: "nat" :: Type)))("punit.star" :: "punit" :: Type)))))("'ar_414967652" :: "nat" :: Type))(({
    val rxyz = pprodInd.value("nat" :: Type)(({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(lmbda("_" :: "nat" :: Type)(lmbda("'ba_711735342" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("nat" :: Type)("'ba_711735342" :: Type))("punit" :: Type))))("'v_406623033" :: "nat" :: Type)).rec("nat" :: Type)
    rxyz
  })(lmbda("'s_101072823" :: "nat" :: Type)(lmbda("_" :: ({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("_" :: "nat" :: Type)(lmbda("'ba_711735342" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("nat" :: Type)("'ba_711735342" :: Type))("punit" :: Type))))("'v_406623033" :: "nat" :: Type))("'s_101072823" :: "nat" :: Type)))("'w_2018856934" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("nat" :: Type)(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("nat" :: Type)("'i_547011919" :: Type))("punit" :: Type))))("'v_406623033" :: "nat" :: Type)))))(("pprod.mk" :: piDefn("'f_234597865" :: Type)(piDefn("'g_1111384708" :: Type)(FuncTyp("'f_234597865" :: Type, FuncTyp("'g_1111384708" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_234597865" :: Type)("'g_1111384708" :: Type))))))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("nat" :: Type)(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("nat" :: Type)("'i_547011919" :: Type))("punit" :: Type))))("'v_406623033" :: "nat" :: Type)))("punit" :: Type)("'w_2018856934" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("nat" :: Type)(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("nat" :: Type)("'i_547011919" :: Type))("punit" :: Type))))("'v_406623033" :: "nat" :: Type)))("punit.star" :: "punit" :: Type)))))("'as_1776667304" :: "nat" :: Type))))
}
