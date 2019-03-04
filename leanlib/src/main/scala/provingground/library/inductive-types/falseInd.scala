package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object falseInd {
  lazy val value = ConstructorSeqTL(
    ConstructorSeqDom.Empty.byTyp("false" :: Prop),
    "false" :: Prop)
}
