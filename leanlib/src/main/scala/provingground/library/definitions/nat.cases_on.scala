package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object nat$cases_on {
  val value = lambda("'g_1388308255_1084752373" :: FuncTyp("nat" :: Type, Type))(lambda("'h_265186006_452905067" :: "nat" :: Type)(lmbda("'i_1672558334_1653022641" :: ("'g_1388308255_1084752373" :: FuncTyp("nat" :: Type, Type))("nat.zero" :: "nat" :: Type))(lmbda("'j_1401456756_2063877820" :: piDefn("'j_444091710_373340266" :: "nat" :: Type)(("'g_1388308255_1084752373" :: FuncTyp("nat" :: Type, Type))(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'j_444091710_373340266" :: "nat" :: Type))))(({
    val rxyz = natInd.value.induc(lmbda("$vuyd_613901509_1380310658" :: "nat" :: Type)(("'g_1388308255_1084752373" :: FuncTyp("nat" :: Type, Type))("$vuyd_613901509_1380310658" :: "nat" :: Type)))
    rxyz
  })("'i_1672558334_1653022641" :: ("'g_1388308255_1084752373" :: FuncTyp("nat" :: Type, Type))("nat.zero" :: "nat" :: Type))(lambda("'k_621156251_1007861654" :: "nat" :: Type)(lmbda("_" :: ("'g_1388308255_1084752373" :: FuncTyp("nat" :: Type, Type))("'k_621156251_1007861654" :: "nat" :: Type))(("'j_1401456756_2063877820" :: piDefn("'j_444091710_373340266" :: "nat" :: Type)(("'g_1388308255_1084752373" :: FuncTyp("nat" :: Type, Type))(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'j_444091710_373340266" :: "nat" :: Type))))("'k_621156251_1007861654" :: "nat" :: Type))))("'h_265186006_452905067" :: "nat" :: Type)))))
}
