package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object nat$brec_on {
  lazy val value = lambda("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))(lambda("'t_607759390_1043984436" :: "nat" :: Type)(lmbda("'u_1284612690_1754241356" :: piDefn("'u_1523983247_845669431" :: "nat" :: Type)(FuncTyp(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_159188406_1754517306" :: "nat" :: Type)(lmbda("'i_854809894_771231128" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'h_159188406_1754517306" :: "nat" :: Type))("'i_854809894_771231128" :: Type))("punit" :: Type))))("'u_1523983247_845669431" :: "nat" :: Type), ("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'u_1523983247_845669431" :: "nat" :: Type))))(({
    val rxyz = pprodInd.value(("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'t_607759390_1043984436" :: "nat" :: Type))(({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(lmbda("'h_159188406_412083631" :: "nat" :: Type)(lmbda("'i_854809894_1381443672" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'h_159188406_412083631" :: "nat" :: Type))("'i_854809894_1381443672" :: Type))("punit" :: Type))))("'t_607759390_1043984436" :: "nat" :: Type)).rec(("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'t_607759390_1043984436" :: "nat" :: Type))
    rxyz
  })(lmbda("'s_274967088_1770737690" :: ("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'t_607759390_1043984436" :: "nat" :: Type))(lmbda("_" :: ({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_159188406_67853935" :: "nat" :: Type)(lmbda("'i_854809894_1227291828" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'h_159188406_67853935" :: "nat" :: Type))("'i_854809894_1227291828" :: Type))("punit" :: Type))))("'t_607759390_1043984436" :: "nat" :: Type))("'s_274967088_1770737690" :: ("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'t_607759390_1043984436" :: "nat" :: Type))))(({
    val rxyz = natInd.value.induc(lmbda("'v_1087445862_622832074" :: "nat" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'v_1087445862_622832074" :: "nat" :: Type))(({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(lmbda("'h_159188406_1412085523" :: "nat" :: Type)(lmbda("'i_854809894_1399335171" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'h_159188406_1412085523" :: "nat" :: Type))("'i_854809894_1399335171" :: Type))("punit" :: Type))))("'v_1087445862_622832074" :: "nat" :: Type))))
    rxyz
  })(("pprod.mk" :: piDefn("'f_262906399" :: Type)(piDefn("'g_1652760707" :: Type)(FuncTyp("'f_262906399" :: Type, FuncTyp("'g_1652760707" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_262906399" :: Type)("'g_1652760707" :: Type))))))(("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("nat.zero" :: "nat" :: Type))("punit" :: Type)(("'u_1284612690_1754241356" :: piDefn("'u_1523983247_845669431" :: "nat" :: Type)(FuncTyp(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_159188406_1754517306" :: "nat" :: Type)(lmbda("'i_854809894_771231128" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'h_159188406_1754517306" :: "nat" :: Type))("'i_854809894_771231128" :: Type))("punit" :: Type))))("'u_1523983247_845669431" :: "nat" :: Type), ("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'u_1523983247_845669431" :: "nat" :: Type))))("nat.zero" :: "nat" :: Type)("punit.star" :: "punit" :: Type))("punit.star" :: "punit" :: Type))(lambda("'v_256405719_247687444" :: "nat" :: Type)(lmbda("'w_221055569_1032185676" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'v_256405719_247687444" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_159188406_1451467006" :: "nat" :: Type)(lmbda("'i_854809894_497084297" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'h_159188406_1451467006" :: "nat" :: Type))("'i_854809894_497084297" :: Type))("punit" :: Type))))("'v_256405719_247687444" :: "nat" :: Type)))(("pprod.mk" :: piDefn("'f_1577296185" :: Type)(piDefn("'g_630351122" :: Type)(FuncTyp("'f_1577296185" :: Type, FuncTyp("'g_630351122" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_1577296185" :: Type)("'g_630351122" :: Type))))))(("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'v_256405719_247687444" :: "nat" :: Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'v_256405719_247687444" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_159188406_444614576" :: "nat" :: Type)(lmbda("'i_854809894_1341430935" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'h_159188406_444614576" :: "nat" :: Type))("'i_854809894_1341430935" :: Type))("punit" :: Type))))("'v_256405719_247687444" :: "nat" :: Type)))("punit" :: Type))(("'u_1284612690_1754241356" :: piDefn("'u_1523983247_845669431" :: "nat" :: Type)(FuncTyp(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_159188406_1754517306" :: "nat" :: Type)(lmbda("'i_854809894_771231128" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'h_159188406_1754517306" :: "nat" :: Type))("'i_854809894_771231128" :: Type))("punit" :: Type))))("'u_1523983247_845669431" :: "nat" :: Type), ("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'u_1523983247_845669431" :: "nat" :: Type))))(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'v_256405719_247687444" :: "nat" :: Type))(("pprod.mk" :: piDefn("'f_385722590" :: Type)(piDefn("'g_457359589" :: Type)(FuncTyp("'f_385722590" :: Type, FuncTyp("'g_457359589" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_385722590" :: Type)("'g_457359589" :: Type))))))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'v_256405719_247687444" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_159188406_1354190091" :: "nat" :: Type)(lmbda("'i_854809894_1122200671" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'h_159188406_1354190091" :: "nat" :: Type))("'i_854809894_1122200671" :: Type))("punit" :: Type))))("'v_256405719_247687444" :: "nat" :: Type)))("punit" :: Type)("'w_221055569_1032185676" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'v_256405719_247687444" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_159188406_1451467006" :: "nat" :: Type)(lmbda("'i_854809894_497084297" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'h_159188406_1451467006" :: "nat" :: Type))("'i_854809894_497084297" :: Type))("punit" :: Type))))("'v_256405719_247687444" :: "nat" :: Type)))("punit.star" :: "punit" :: Type)))(("pprod.mk" :: piDefn("'f_1316515637" :: Type)(piDefn("'g_1490312049" :: Type)(FuncTyp("'f_1316515637" :: Type, FuncTyp("'g_1490312049" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_1316515637" :: Type)("'g_1490312049" :: Type))))))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'v_256405719_247687444" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_159188406_1193617332" :: "nat" :: Type)(lmbda("'i_854809894_1301821393" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'h_159188406_1193617332" :: "nat" :: Type))("'i_854809894_1301821393" :: Type))("punit" :: Type))))("'v_256405719_247687444" :: "nat" :: Type)))("punit" :: Type)("'w_221055569_1032185676" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'v_256405719_247687444" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_159188406_1451467006" :: "nat" :: Type)(lmbda("'i_854809894_497084297" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_928955760_783326521" :: FuncTyp("nat" :: Type, Type))("'h_159188406_1451467006" :: "nat" :: Type))("'i_854809894_497084297" :: Type))("punit" :: Type))))("'v_256405719_247687444" :: "nat" :: Type)))("punit.star" :: "punit" :: Type)))))("'t_607759390_1043984436" :: "nat" :: Type)))))
}
