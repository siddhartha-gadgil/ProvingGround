package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object nat$no_confusion_type {
  val value = lmbda("'k_549572857" :: Type)(lmbda("'l_779096294" :: "nat" :: Type)(lmbda("'m_899342415" :: "nat" :: Type)(({
    val rxyz = natInd.value.induc(lmbda("$vuyd_613901509" :: "nat" :: Type)(Type))
    rxyz
  })(({
    val rxyz = natInd.value.induc(lmbda("$vuyd_613901509" :: "nat" :: Type)(Type))
    rxyz
  })(FuncTyp("'k_549572857" :: Type, "'k_549572857" :: Type))(lambda("'k_621156251" :: "nat" :: Type)(lmbda("_" :: Type)("'k_549572857" :: Type)))("'m_899342415" :: "nat" :: Type))(lambda("'k_621156251" :: "nat" :: Type)(lmbda("_" :: Type)(({
    val rxyz = natInd.value.induc(lmbda("$vuyd_613901509" :: "nat" :: Type)(Type))
    rxyz
  })("'k_549572857" :: Type)(lambda("'k_1717304577" :: "nat" :: Type)(lmbda("_" :: Type)(FuncTyp(FuncTyp(("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("nat" :: Type)("'k_621156251" :: "nat" :: Type)("'k_1717304577" :: "nat" :: Type), "'k_549572857" :: Type), "'k_549572857" :: Type))))("'m_899342415" :: "nat" :: Type))))("'l_779096294" :: "nat" :: Type))))
}
