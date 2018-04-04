package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._ // for safety
object id_rhs { val value = lambda("'b" :: Type)(lmbda("_" :: "'b" :: Type)("_" :: "'b" :: Type)) }
