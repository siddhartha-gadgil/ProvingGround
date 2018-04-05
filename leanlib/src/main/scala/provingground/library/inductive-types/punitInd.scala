package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object punitInd { val value = ConstructorSeqTL(ConstructorSeqDom.Cons("punit.star", ConstructorShape.IdShape.byTyp("punit" :: Type), ConstructorSeqDom.Empty.byTyp("punit" :: Type)), "punit" :: Type) }
