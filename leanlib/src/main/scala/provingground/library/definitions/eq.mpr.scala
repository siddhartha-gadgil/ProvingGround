package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object eq$mpr {
  lazy val value = lambda("'ab_1058045042" :: Type)(lambda("'ac_1548192294" :: Type)(lmbda("_" :: ("eq" :: piDefn("'c_885647912" :: Type)(FuncTyp("'c_885647912" :: Type, FuncTyp("'c_885647912" :: Type, Prop))))(Type)("'ab_1058045042" :: Type)("'ac_1548192294" :: Type))(lmbda("'ad_461334327" :: "'ac_1548192294" :: Type)(({
    val rxyz = eqInd.value(Type)("'ac_1548192294" :: Type).inducE(lambda("$mje_19815537" :: Type)(lmbda("_" :: ("eq" :: piDefn("'c_885647912" :: Type)(FuncTyp("'c_885647912" :: Type, FuncTyp("'c_885647912" :: Type, Prop))))(Type)("'ac_1548192294" :: Type)("$mje_19815537" :: Type))("$mje_19815537" :: Type)))
    rxyz
  })("'ad_461334327" :: "'ac_1548192294" :: Type)("'ab_1058045042" :: Type)("_" :: ("eq" :: piDefn("'c_885647912" :: Type)(FuncTyp("'c_885647912" :: Type, FuncTyp("'c_885647912" :: Type, Prop))))(Type)("'ac_1548192294" :: Type)("'ab_1058045042" :: Type))))))
}
