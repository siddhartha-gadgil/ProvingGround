package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object pprod$fst {
  lazy val value = lambda("'p_1724748434" :: Type)(lambda("'q_224439820" :: Type)(({
    val rxyz = pprodInd.value("'p_1724748434" :: Type)("'q_224439820" :: Type).rec("'p_1724748434" :: Type)
    rxyz
  })(lmbda("'s_605670817" :: "'p_1724748434" :: Type)(lmbda("_" :: "'q_224439820" :: Type)("'s_605670817" :: "'p_1724748434" :: Type)))))
}
