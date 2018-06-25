package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object nat$no_confusion_type {
  lazy val value = lmbda("'k_549572857_1909003488" :: Type)(lmbda("'l_779096294_1814577956" :: "nat" :: Type)(lmbda("'m_899342415_613316750" :: "nat" :: Type)(({
    val rxyz = natInd.value.induc(lmbda("_" :: "nat" :: Type)(Type))
    rxyz
  })(({
    val rxyz = natInd.value.induc(lmbda("_" :: "nat" :: Type)(Type))
    rxyz
  })(FuncTyp("'k_549572857_1909003488" :: Type, "'k_549572857_1909003488" :: Type))(lmbda("_" :: "nat" :: Type)(lmbda("_" :: Type)("'k_549572857_1909003488" :: Type)))("'m_899342415_613316750" :: "nat" :: Type))(lmbda("'k_621156251_1712470343" :: "nat" :: Type)(lmbda("_" :: Type)(({
    val rxyz = natInd.value.induc(lmbda("_" :: "nat" :: Type)(Type))
    rxyz
  })("'k_549572857_1909003488" :: Type)(lmbda("'k_1717304577_154566442" :: "nat" :: Type)(lmbda("_" :: Type)(FuncTyp(FuncTyp(("eq" :: piDefn("'c_2083738585" :: Type)(FuncTyp("'c_2083738585" :: Type, FuncTyp("'c_2083738585" :: Type, Prop))))("nat" :: Type)("'k_621156251_1712470343" :: "nat" :: Type)("'k_1717304577_154566442" :: "nat" :: Type), "'k_549572857_1909003488" :: Type), "'k_549572857_1909003488" :: Type))))("'m_899342415_613316750" :: "nat" :: Type))))("'l_779096294_1814577956" :: "nat" :: Type))))
}
