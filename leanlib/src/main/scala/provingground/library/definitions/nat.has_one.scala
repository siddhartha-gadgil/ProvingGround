package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object nat$has_one {
  lazy val value = ("has_one.mk" :: piDefn("'c_264172685" :: Type)(
    FuncTyp("'c_264172685" :: Type,
            ("has_one" :: FuncTyp(Type, Type))("'c_264172685" :: Type))))(
    "nat" :: Type)(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))(
    "nat.zero" :: "nat" :: Type))
}
