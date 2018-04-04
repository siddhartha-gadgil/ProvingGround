package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._ // for safety
object nat$below {
  val value = lmbda("'f" :: FuncTyp("nat" :: Type, Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h" :: "nat" :: Type)(lmbda("'i" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'f" :: FuncTyp("nat" :: Type, Type))("'h" :: "nat" :: Type))("'i" :: Type))("punit" :: Type)))))
}
