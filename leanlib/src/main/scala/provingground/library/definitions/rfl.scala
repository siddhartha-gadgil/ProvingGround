package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
object rfl { val value = lambda("'g" :: Type)(lambda("'h" :: "'g" :: Type)("_" :: ("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("'g" :: Type)("'h" :: "'g" :: Type)("'h" :: "'g" :: Type))) }
