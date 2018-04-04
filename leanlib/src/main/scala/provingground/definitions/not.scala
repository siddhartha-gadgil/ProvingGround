package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._ // for safety
object not { val value = lmbda("'b" :: Prop)(FuncTyp("'b" :: Prop, "false" :: Prop)) }
