package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object has_mulInd {
  val value = Subst.Lambda(
    "$buijsie" :: Type,
    ConstructorSeqTL(
      ConstructorSeqDom.Cons(
        ApplnSym(
          "has_mul.mk" :: piDefn("'d" :: Type)(
            FuncTyp(FuncTyp("'d" :: Type, FuncTyp("'d" :: Type, "'d" :: Type)),
                    ("has_mul" :: FuncTyp(Type, Type))("'d" :: Type))),
          "$buijsie" :: Type
        ),
        ConstructorShape.CnstFuncConsShape(
          FuncTyp("$buijsie" :: Type,
                  FuncTyp("$buijsie" :: Type, "$buijsie" :: Type)),
          ConstructorShape.IdShape.byTyp(
            ("has_mul" :: FuncTyp(Type, Type))("$buijsie" :: Type))
        ),
        ConstructorSeqDom.Empty.byTyp(
          ("has_mul" :: FuncTyp(Type, Type))("$buijsie" :: Type))
      ),
      ("has_mul" :: FuncTyp(Type, Type))("$buijsie" :: Type)
    )
  )
}
