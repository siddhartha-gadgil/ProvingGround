
package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
object nat$cases_on {
  val value = lambda("'g" :: FuncTyp("nat" :: Type, Type))(lambda("'h" :: "nat" :: Type)(lmbda("'i" :: ("'g" :: FuncTyp("nat" :: Type, Type))("nat.zero" :: "nat" :: Type))(lmbda("'j" :: piDefn("'j" :: "nat" :: Type)(("'g" :: FuncTyp("nat" :: Type, Type))(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'j" :: "nat" :: Type))))(({
    val rxyz = natInd.value.induc(lmbda("$vuyd" :: "nat" :: Type)(("'g" :: FuncTyp("nat" :: Type, Type))("$vuyd" :: "nat" :: Type)))
    rxyz
  })("'i" :: ("'g" :: FuncTyp("nat" :: Type, Type))("nat.zero" :: "nat" :: Type))(lambda("'k" :: "nat" :: Type)(lmbda("_" :: ("'g" :: FuncTyp("nat" :: Type, Type))("'k" :: "nat" :: Type))(("'j" :: piDefn("'j" :: "nat" :: Type)(("'g" :: FuncTyp("nat" :: Type, Type))(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'j" :: "nat" :: Type))))("'k" :: "nat" :: Type))))("'h" :: "nat" :: Type)))))
}