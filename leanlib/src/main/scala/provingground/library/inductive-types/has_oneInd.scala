package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object has_oneInd {
  val value = Subst.Lambda(
    "$buifqqs" :: Type,
    ConstructorSeqTL(
      ConstructorSeqDom.Cons(
        ApplnSym("has_one.mk" :: piDefn("'c" :: Type)(
                   FuncTyp("'c" :: Type,
                           ("has_one" :: FuncTyp(Type, Type))("'c" :: Type))),
                 "$buifqqs" :: Type),
        ConstructorShape.CnstFuncConsShape(
          "$buifqqs" :: Type,
          ConstructorShape.IdShape.byTyp(
            ("has_one" :: FuncTyp(Type, Type))("$buifqqs" :: Type))),
        ConstructorSeqDom.Empty.byTyp(
          ("has_one" :: FuncTyp(Type, Type))("$buifqqs" :: Type))
      ),
      ("has_one" :: FuncTyp(Type, Type))("$buifqqs" :: Type)
    )
  )
}
