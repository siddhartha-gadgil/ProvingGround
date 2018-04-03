
package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
object pprod$fst {
  val value = lambda("'p" :: Type)(lambda("'q" :: Type)(({
    val rxyz = pprodInd.value("'p" :: Type)("'q" :: Type).rec("'p" :: Type)
    rxyz
  })(lmbda("'s" :: "'p" :: Type)(lmbda("_" :: "'q" :: Type)("'s" :: "'p" :: Type)))))
}