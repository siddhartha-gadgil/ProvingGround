package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object nat$cases_on {
  val value = lambda("'g_1388308255" :: FuncTyp("nat" :: Type, Type))(lambda("'h_265186006" :: "nat" :: Type)(lmbda("'i_1672558334" :: ("'g_1388308255" :: FuncTyp("nat" :: Type, Type))("nat.zero" :: "nat" :: Type))(lmbda("'j_1401456756" :: piDefn("'j_444091710" :: "nat" :: Type)(("'g_1388308255" :: FuncTyp("nat" :: Type, Type))(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'j_444091710" :: "nat" :: Type))))(({
    val rxyz = natInd.value.induc(lmbda("$vuyd_613901509" :: "nat" :: Type)(("'g_1388308255" :: FuncTyp("nat" :: Type, Type))("$vuyd_613901509" :: "nat" :: Type)))
    rxyz
  })("'i_1672558334" :: ("'g_1388308255" :: FuncTyp("nat" :: Type, Type))("nat.zero" :: "nat" :: Type))(lambda("'k_621156251" :: "nat" :: Type)(lmbda("_" :: ("'g_1388308255" :: FuncTyp("nat" :: Type, Type))("'k_621156251" :: "nat" :: Type))(("'j_1401456756" :: piDefn("'j_444091710" :: "nat" :: Type)(("'g_1388308255" :: FuncTyp("nat" :: Type, Type))(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'j_444091710" :: "nat" :: Type))))("'k_621156251" :: "nat" :: Type))))("'h_265186006" :: "nat" :: Type)))))
}
