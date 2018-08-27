package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object bool$cases_on {
  lazy val value = lambda("'e_310649018" :: FuncTyp("bool" :: Type, Type))(lambda("'f_124292129" :: "bool" :: Type)(lmbda("'g_587414811" :: ("'e_310649018" :: FuncTyp("bool" :: Type, Type))("bool.ff" :: "bool" :: Type))(lmbda("'h_405609425" :: ("'e_310649018" :: FuncTyp("bool" :: Type, Type))("bool.tt" :: "bool" :: Type))(({
    val rxyz = boolInd.value.induc(lmbda("$a_252833942" :: "bool" :: Type)(("'e_310649018" :: FuncTyp("bool" :: Type, Type))("$a_252833942" :: "bool" :: Type)))
    rxyz
  })("'g_587414811" :: ("'e_310649018" :: FuncTyp("bool" :: Type, Type))("bool.ff" :: "bool" :: Type))("'h_405609425" :: ("'e_310649018" :: FuncTyp("bool" :: Type, Type))("bool.tt" :: "bool" :: Type))("'f_124292129" :: "bool" :: Type)))))
}
