package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object nat$below {
  lazy val value = lmbda("'f_1357013420" :: FuncTyp("nat" :: Type, Type))(({
    val rxyz = natInd.value.rec(Type)
    rxyz
  })("punit" :: Type)(lmbda("'h_403586353" :: "nat" :: Type)(lmbda("'i_547011919" :: Type)(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(("'f_1357013420" :: FuncTyp("nat" :: Type, Type))("'h_403586353" :: "nat" :: Type))("'i_547011919" :: Type))("punit" :: Type)))))
}
