package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object has_zero$zero {
  lazy val value = lambda("'i_1381096321" :: Type)(({
    val rxyz = has_zeroInd.value("'i_1381096321" :: Type).rec("'i_1381096321" :: Type)
    rxyz
  })(lmbda("'k_2035935672" :: "'i_1381096321" :: Type)("'k_2035935672" :: "'i_1381096321" :: Type)))
}
