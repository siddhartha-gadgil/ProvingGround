package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object nat$cases_on {
  lazy val value = lambda("'g_1492571035" :: FuncTyp("nat" :: Type, Type))(lambda("'h_967240790" :: "nat" :: Type)(lmbda("'i_208493792" :: ("'g_1492571035" :: FuncTyp("nat" :: Type, Type))("nat.zero" :: "nat" :: Type))(lmbda("'j_376604452" :: piDefn("'j_481711520" :: "nat" :: Type)(("'g_1492571035" :: FuncTyp("nat" :: Type, Type))(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'j_481711520" :: "nat" :: Type))))(({
    val rxyz = natInd.value.induc(lmbda("$bgkyt_230611191" :: "nat" :: Type)(("'g_1492571035" :: FuncTyp("nat" :: Type, Type))("$bgkyt_230611191" :: "nat" :: Type)))
    rxyz
  })("'i_208493792" :: ("'g_1492571035" :: FuncTyp("nat" :: Type, Type))("nat.zero" :: "nat" :: Type))(lambda("'k_1418369068" :: "nat" :: Type)(lmbda("_" :: ("'g_1492571035" :: FuncTyp("nat" :: Type, Type))("'k_1418369068" :: "nat" :: Type))(("'j_376604452" :: piDefn("'j_481711520" :: "nat" :: Type)(("'g_1492571035" :: FuncTyp("nat" :: Type, Type))(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'j_481711520" :: "nat" :: Type))))("'k_1418369068" :: "nat" :: Type))))("'h_967240790" :: "nat" :: Type)))))
}
