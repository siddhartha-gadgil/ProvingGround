package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object boolInd { lazy val value = ConstructorSeqTL(ConstructorSeqDom.Cons("bool.ff", ConstructorShape.IdShape.byTyp("bool" :: Type), ConstructorSeqDom.Cons("bool.tt", ConstructorShape.IdShape.byTyp("bool" :: Type), ConstructorSeqDom.Empty.byTyp("bool" :: Type))), "bool" :: Type) }
