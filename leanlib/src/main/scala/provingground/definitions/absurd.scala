package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._ // for safety
object absurd {
  val value = lambda("'f" :: Prop)(lambda("'g" :: Type)(lmbda("_" :: "'f" :: Prop)(lmbda("_" :: FuncTyp("'f" :: Prop, "false" :: Prop))(({
    val rxyz = falseInd.value.rec("'g" :: Type)
    rxyz
  })("_" :: "false" :: Prop)))))
}
