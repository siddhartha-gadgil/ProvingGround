package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object nat$below {
  val value = lmbda("'f_2130154615_1119565184" :: FuncTyp("nat" :: Type, Type))(
    ({
      val rxyz = natInd.value.rec(Type)
      rxyz
    })("punit" :: Type)(
      lmbda("'h_159188406_1867843132" :: "nat" :: Type)(
        lmbda("'i_854809894_1371695489" :: Type)(
          ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(
            ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(
              ("'f_2130154615_1119565184" :: FuncTyp("nat" :: Type, Type))(
                "'h_159188406_1867843132" :: "nat" :: Type))(
              "'i_854809894_1371695489" :: Type))("punit" :: Type)))))
}
