
package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
object not { val value = lmbda("'b" :: Prop)(FuncTyp("'b" :: Prop, "false" :: Prop)) }