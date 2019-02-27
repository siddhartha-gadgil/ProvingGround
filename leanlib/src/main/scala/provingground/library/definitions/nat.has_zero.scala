package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object nat$has_zero {
  lazy val value = ("has_zero.mk" :: piDefn("'c_1531913461" :: Type)(
    FuncTyp("'c_1531913461" :: Type,
            ("has_zero" :: FuncTyp(Type, Type))("'c_1531913461" :: Type))))(
    "nat" :: Type)("nat.zero" :: "nat" :: Type)
}
