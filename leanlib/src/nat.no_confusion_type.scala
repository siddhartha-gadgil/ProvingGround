
package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
object nat$no_confusion_type {
  val value = lmbda("'k" :: Type)(lmbda("'l" :: "nat" :: Type)(lmbda("'m" :: "nat" :: Type)(({
    val rxyz = natInd.value.induc(lmbda("$vuyd" :: "nat" :: Type)(Type))
    rxyz
  })(({
    val rxyz = natInd.value.induc(lmbda("$vuyd" :: "nat" :: Type)(Type))
    rxyz
  })(FuncTyp("'k" :: Type, "'k" :: Type))(lambda("'k" :: "nat" :: Type)(lmbda("_" :: Type)("'k" :: Type)))("'m" :: "nat" :: Type))(lambda("'k" :: "nat" :: Type)(lmbda("_" :: Type)(({
    val rxyz = natInd.value.induc(lmbda("$vuyd" :: "nat" :: Type)(Type))
    rxyz
  })("'k" :: Type)(lambda("'k" :: "nat" :: Type)(lmbda("_" :: Type)(FuncTyp(FuncTyp(("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("nat" :: Type)("'k" :: "nat" :: Type)("'k" :: "nat" :: Type), "'k" :: Type), "'k" :: Type))))("'m" :: "nat" :: Type))))("'l" :: "nat" :: Type))))
}