package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object nat$brec_on {
  lazy val value = lambda("'s_1099773403" :: FuncTyp("nat" :: Type, Type))(lambda("'t_1370473497" :: "nat" :: Type)(lmbda("'u_2108981661" :: piDefn("'u_1469166675" :: "nat" :: Type)(FuncTyp(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_1573352955" :: "nat" :: Type)(lmbda("'i_830893068" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'h_1573352955" :: "nat" :: Type))("'i_830893068" :: Type))("punit" :: Type))))("'u_1469166675" :: "nat" :: Type), ("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'u_1469166675" :: "nat" :: Type))))(({
    val rxyz = pprodInd.value(("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'t_1370473497" :: "nat" :: Type))(({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(lmbda("'h_1573352955" :: "nat" :: Type)(lmbda("'i_830893068" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'h_1573352955" :: "nat" :: Type))("'i_830893068" :: Type))("punit" :: Type))))("'t_1370473497" :: "nat" :: Type)).rec(("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'t_1370473497" :: "nat" :: Type))
    rxyz
  })(lmbda("'s_66797858" :: ("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'t_1370473497" :: "nat" :: Type))(lmbda("_" :: ({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_1573352955" :: "nat" :: Type)(lmbda("'i_830893068" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'h_1573352955" :: "nat" :: Type))("'i_830893068" :: Type))("punit" :: Type))))("'t_1370473497" :: "nat" :: Type))("'s_66797858" :: ("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'t_1370473497" :: "nat" :: Type))))(({
    val rxyz = natInd.value.induc(lmbda("'v_877202618" :: "nat" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'v_877202618" :: "nat" :: Type))(({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(lmbda("'h_1573352955" :: "nat" :: Type)(lmbda("'i_830893068" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'h_1573352955" :: "nat" :: Type))("'i_830893068" :: Type))("punit" :: Type))))("'v_877202618" :: "nat" :: Type))))
    rxyz
  })(("pprod.mk" :: piDefn("'f_1394520732" :: Type)(piDefn("'g_1655163109" :: Type)(FuncTyp("'f_1394520732" :: Type, FuncTyp("'g_1655163109" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_1394520732" :: Type)("'g_1655163109" :: Type))))))(("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("nat.zero" :: "nat" :: Type))("punit" :: Type)(("'u_2108981661" :: piDefn("'u_1469166675" :: "nat" :: Type)(FuncTyp(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_1573352955" :: "nat" :: Type)(lmbda("'i_830893068" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'h_1573352955" :: "nat" :: Type))("'i_830893068" :: Type))("punit" :: Type))))("'u_1469166675" :: "nat" :: Type), ("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'u_1469166675" :: "nat" :: Type))))("nat.zero" :: "nat" :: Type)("punit.star" :: "punit" :: Type))("punit.star" :: "punit" :: Type))(lambda("'v_1605754716" :: "nat" :: Type)(lmbda("'w_1754830669" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'v_1605754716" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_1573352955" :: "nat" :: Type)(lmbda("'i_830893068" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'h_1573352955" :: "nat" :: Type))("'i_830893068" :: Type))("punit" :: Type))))("'v_1605754716" :: "nat" :: Type)))(("pprod.mk" :: piDefn("'f_1394520732" :: Type)(piDefn("'g_1655163109" :: Type)(FuncTyp("'f_1394520732" :: Type, FuncTyp("'g_1655163109" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_1394520732" :: Type)("'g_1655163109" :: Type))))))(("'s_1099773403" :: FuncTyp("nat" :: Type, Type))(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'v_1605754716" :: "nat" :: Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'v_1605754716" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_1573352955" :: "nat" :: Type)(lmbda("'i_830893068" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'h_1573352955" :: "nat" :: Type))("'i_830893068" :: Type))("punit" :: Type))))("'v_1605754716" :: "nat" :: Type)))("punit" :: Type))(("'u_2108981661" :: piDefn("'u_1469166675" :: "nat" :: Type)(FuncTyp(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_1573352955" :: "nat" :: Type)(lmbda("'i_830893068" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'h_1573352955" :: "nat" :: Type))("'i_830893068" :: Type))("punit" :: Type))))("'u_1469166675" :: "nat" :: Type), ("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'u_1469166675" :: "nat" :: Type))))(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'v_1605754716" :: "nat" :: Type))(("pprod.mk" :: piDefn("'f_1394520732" :: Type)(piDefn("'g_1655163109" :: Type)(FuncTyp("'f_1394520732" :: Type, FuncTyp("'g_1655163109" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_1394520732" :: Type)("'g_1655163109" :: Type))))))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'v_1605754716" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_1573352955" :: "nat" :: Type)(lmbda("'i_830893068" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'h_1573352955" :: "nat" :: Type))("'i_830893068" :: Type))("punit" :: Type))))("'v_1605754716" :: "nat" :: Type)))("punit" :: Type)("'w_1754830669" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'v_1605754716" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_1573352955" :: "nat" :: Type)(lmbda("'i_830893068" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'h_1573352955" :: "nat" :: Type))("'i_830893068" :: Type))("punit" :: Type))))("'v_1605754716" :: "nat" :: Type)))("punit.star" :: "punit" :: Type)))(("pprod.mk" :: piDefn("'f_1394520732" :: Type)(piDefn("'g_1655163109" :: Type)(FuncTyp("'f_1394520732" :: Type, FuncTyp("'g_1655163109" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_1394520732" :: Type)("'g_1655163109" :: Type))))))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'v_1605754716" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_1573352955" :: "nat" :: Type)(lmbda("'i_830893068" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'h_1573352955" :: "nat" :: Type))("'i_830893068" :: Type))("punit" :: Type))))("'v_1605754716" :: "nat" :: Type)))("punit" :: Type)("'w_1754830669" :: ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'v_1605754716" :: "nat" :: Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_1573352955" :: "nat" :: Type)(lmbda("'i_830893068" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'s_1099773403" :: FuncTyp("nat" :: Type, Type))("'h_1573352955" :: "nat" :: Type))("'i_830893068" :: Type))("punit" :: Type))))("'v_1605754716" :: "nat" :: Type)))("punit.star" :: "punit" :: Type)))))("'t_1370473497" :: "nat" :: Type)))))
}
