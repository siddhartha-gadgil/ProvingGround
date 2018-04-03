
package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
object eq$subst { val value = lambda("'q" :: Type)(lambda("'r" :: FuncTyp("'q" :: Type, Prop))(lambda("'s" :: "'q" :: Type)(lambda("'t" :: "'q" :: Type)(lmbda("_" :: ("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("'q" :: Type)("'s" :: "'q" :: Type)("'t" :: "'q" :: Type))(lmbda("_" :: ("'r" :: FuncTyp("'q" :: Type, Prop))("'s" :: "'q" :: Type))("_" :: ("'r" :: FuncTyp("'q" :: Type, Prop))("'t" :: "'q" :: Type))))))) }