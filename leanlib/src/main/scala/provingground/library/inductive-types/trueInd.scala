package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object trueInd {
  lazy val value = ConstructorSeqTL(
    ConstructorSeqDom.Cons("_",
                           ConstructorShape.IdShape.byTyp("true" :: Prop),
                           ConstructorSeqDom.Empty.byTyp("true" :: Prop)),
    "true" :: Prop)
}
