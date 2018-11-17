package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object nat$cases_on {
  lazy val value = lambda("'g_1359770574" :: FuncTyp("nat" :: Type, Type))(lambda("'h_162589899" :: "nat" :: Type)(lmbda("'i_1683820320" :: ("'g_1359770574" :: FuncTyp("nat" :: Type, Type))("nat.zero" :: "nat" :: Type))(lmbda("'j_218256450" :: piDefn("'j_1561032902" :: "nat" :: Type)(("'g_1359770574" :: FuncTyp("nat" :: Type, Type))(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'j_1561032902" :: "nat" :: Type))))(({
    val rxyz = natInd.value.induc(lmbda("$bhoov_266361025" :: "nat" :: Type)(("'g_1359770574" :: FuncTyp("nat" :: Type, Type))("$bhoov_266361025" :: "nat" :: Type)))
    rxyz
  })("'i_1683820320" :: ("'g_1359770574" :: FuncTyp("nat" :: Type, Type))("nat.zero" :: "nat" :: Type))(lambda("'k_519801512" :: "nat" :: Type)(lmbda("_" :: ("'g_1359770574" :: FuncTyp("nat" :: Type, Type))("'k_519801512" :: "nat" :: Type))(("'j_218256450" :: piDefn("'j_1561032902" :: "nat" :: Type)(("'g_1359770574" :: FuncTyp("nat" :: Type, Type))(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'j_1561032902" :: "nat" :: Type))))("'k_519801512" :: "nat" :: Type))))("'h_162589899" :: "nat" :: Type)))))
}
