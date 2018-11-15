package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object pprod$fst {
  lazy val value = lambda("'p_1107972026" :: Type)(lambda("'q_579249507" :: Type)(({
    val rxyz = pprodInd.value("'p_1107972026" :: Type)("'q_579249507" :: Type).rec("'p_1107972026" :: Type)
    rxyz
  })(lmbda("'s_888824813" :: "'p_1107972026" :: Type)(lmbda("_" :: "'q_579249507" :: Type)("'s_888824813" :: "'p_1107972026" :: Type)))))
}
