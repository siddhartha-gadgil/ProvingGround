package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object absurd {
  val value = lambda("'f_448452064" :: Prop)(lambda("'g_1603466541" :: Type)(lmbda("_" :: "'f_448452064" :: Prop)(lmbda("_" :: FuncTyp("'f_448452064" :: Prop, "false" :: Prop))(({
    val rxyz = falseInd.value.rec("'g_1603466541" :: Type)
    rxyz
  })("_" :: "false" :: Prop)))))
}
