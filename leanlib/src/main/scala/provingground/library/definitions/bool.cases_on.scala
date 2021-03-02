package provingground.library
import provingground._
import HoTT._
import induction._
object bool$cases_on {
  lazy val value = lambda("'e_1326394388" :: FuncTyp("bool" :: Type, Type))(lambda("'f_363947866" :: "bool" :: Type)(lmbda("'g_1623762820" :: ("'e_1326394388" :: FuncTyp("bool" :: Type, Type))("bool.ff" :: "bool" :: Type))(lmbda("'h_1083310302" :: ("'e_1326394388" :: FuncTyp("bool" :: Type, Type))("bool.tt" :: "bool" :: Type))(({
    val rxyz = boolInd.value.induc(lmbda("$ta_696879078" :: "bool" :: Type)(("'e_1326394388" :: FuncTyp("bool" :: Type, Type))("$ta_696879078" :: "bool" :: Type)))
    rxyz
  })("'g_1623762820" :: ("'e_1326394388" :: FuncTyp("bool" :: Type, Type))("bool.ff" :: "bool" :: Type))("'h_1083310302" :: ("'e_1326394388" :: FuncTyp("bool" :: Type, Type))("bool.tt" :: "bool" :: Type))("'f_363947866" :: "bool" :: Type)))))
}
