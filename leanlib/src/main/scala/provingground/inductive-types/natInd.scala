package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._ // for safety
object natInd { val value = ConstructorSeqTL(ConstructorSeqDom.Cons(HoTT.Name("nat.zero"), ConstructorShape.IdShape.byTyp("nat" :: Type), ConstructorSeqDom.Cons(HoTT.Name("nat.succ"), ConstructorShape.FuncConsShape(IterFuncShape.IdIterShape.byTyp("nat" :: Type), ConstructorShape.IdShape.byTyp("nat" :: Type)), ConstructorSeqDom.Empty.byTyp("nat" :: Type))), "nat" :: Type) }
