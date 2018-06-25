package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object absurd {
  lazy val value = lambda("'f_448452064_807574499" :: Prop)(
    lambda("'g_1603466541_263154585" :: Type)(
      lmbda("_" :: "'f_448452064_807574499" :: Prop)(lmbda(
        "_" :: FuncTyp("'f_448452064_807574499" :: Prop, "false" :: Prop))(({
        val rxyz = falseInd.value.rec("'g_1603466541_263154585" :: Type)
        rxyz
      })("_" :: "false" :: Prop)))))
}
