package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object natInd {
  val value = ConstructorSeqTL(
    ConstructorSeqDom.Cons(
      "nat.zero",
      ConstructorShape.IdShape.byTyp("nat" :: Type),
      ConstructorSeqDom.Cons(
        "nat.succ",
        ConstructorShape.FuncConsShape(
          IterFuncShape.IdIterShape.byTyp("nat" :: Type),
          ConstructorShape.IdShape.byTyp("nat" :: Type)),
        ConstructorSeqDom.Empty.byTyp("nat" :: Type)
      )
    ),
    "nat" :: Type
  )
}
