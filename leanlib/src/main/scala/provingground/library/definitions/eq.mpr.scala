package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object eq$mpr {
  lazy val value = lambda("'ab_1008167541" :: Type)(lambda("'ac_273818661" :: Type)(lmbda("_" :: ("eq" :: piDefn("'c_47853266" :: Type)(FuncTyp("'c_47853266" :: Type, FuncTyp("'c_47853266" :: Type, Prop))))(Type)("'ab_1008167541" :: Type)("'ac_273818661" :: Type))(lmbda("'ad_478626032" :: "'ac_273818661" :: Type)(({
    val rxyz = eqInd.value(Type)("'ac_273818661" :: Type).inducE(lambda("$br_1622762238" :: Type)(lmbda("_" :: ("eq" :: piDefn("'c_47853266" :: Type)(FuncTyp("'c_47853266" :: Type, FuncTyp("'c_47853266" :: Type, Prop))))(Type)("'ac_273818661" :: Type)("$br_1622762238" :: Type))("$br_1622762238" :: Type)))
    rxyz
  })("'ad_478626032" :: "'ac_273818661" :: Type)("'ab_1008167541" :: Type)("_" :: ("eq" :: piDefn("'c_47853266" :: Type)(FuncTyp("'c_47853266" :: Type, FuncTyp("'c_47853266" :: Type, Prop))))(Type)("'ac_273818661" :: Type)("'ab_1008167541" :: Type))))))
}
