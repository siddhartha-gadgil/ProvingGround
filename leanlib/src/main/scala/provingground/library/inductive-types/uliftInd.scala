package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object uliftInd {
  lazy val value = Subst.Lambda(
    "$b" :: Type,
    ConstructorSeqTL(
      ConstructorSeqDom.Cons(
        ApplnSym(
          "ulift.up" :: piDefn("'c_586902701" :: Type)(
            FuncTyp("'c_586902701" :: Type,
                    ("ulift" :: FuncTyp(Type, Type))("'c_586902701" :: Type))),
          "$b" :: Type),
        ConstructorShape.CnstFuncConsShape(
          "$b" :: Type,
          ConstructorShape.IdShape.byTyp(
            ("ulift" :: FuncTyp(Type, Type))("$b" :: Type))),
        ConstructorSeqDom.Empty.byTyp(
          ("ulift" :: FuncTyp(Type, Type))("$b" :: Type))
      ),
      ("ulift" :: FuncTyp(Type, Type))("$b" :: Type)
    )
  )
}
