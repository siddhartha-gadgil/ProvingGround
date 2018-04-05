package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object pprod$fst {
  val value = lambda("'p_1335769311" :: Type)(lambda("'q_1400071888" :: Type)(({
    val rxyz = pprodInd.value("'p_1335769311" :: Type)("'q_1400071888" :: Type).rec("'p_1335769311" :: Type)
    rxyz
  })(lmbda("'s_59064292" :: "'p_1335769311" :: Type)(lmbda("_" :: "'q_1400071888" :: Type)("'s_59064292" :: "'p_1335769311" :: Type)))))
}
