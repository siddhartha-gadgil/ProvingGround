package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object iff_true_intro {
  lazy val value = lambda("'h_1903598270" :: Prop)(
    lmbda("_" :: "'h_1903598270" :: Prop)(
      "_" :: ("iff" :: FuncTyp(Prop, FuncTyp(Prop, Prop)))(
        "'h_1903598270" :: Prop)("true" :: Prop)))
}
