package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object nat$le_refl {
  lazy val value = "nat.less_than_or_equal.refl" :: piDefn(
    "'c_308702732" :: "nat" :: Type)(
    ("nat.less_than_or_equal" :: FuncTyp("nat" :: Type,
                                         FuncTyp("nat" :: Type, Prop)))(
      "'c_308702732" :: "nat" :: Type)("'c_308702732" :: "nat" :: Type))
}
