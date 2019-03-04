package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object nat$below {
  lazy val value = lmbda("'f_312518965" :: FuncTyp("nat" :: Type, Type))(
    ({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(
      lmbda("'h_1573352955" :: "nat" :: Type)(
        lmbda("'i_830893068" :: Type)(
          ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(
            ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(
              ("'f_312518965" :: FuncTyp("nat" :: Type, Type))(
                "'h_1573352955" :: "nat" :: Type))("'i_830893068" :: Type))(
            "punit" :: Type)))))
}
