package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object has_leInd {
  lazy val value = Subst.Lambda(
    "$mpdiux" :: Type,
    ConstructorSeqTL(
      ConstructorSeqDom.Cons(
        ApplnSym(
          "has_le.mk" :: piDefn("'d_1129063827" :: Type)(
            FuncTyp(
              FuncTyp("'d_1129063827" :: Type,
                      FuncTyp("'d_1129063827" :: Type, Prop)),
              ("has_le" :: FuncTyp(Type, Type))("'d_1129063827" :: Type))),
          "$mpdiux" :: Type
        ),
        ConstructorShape.CnstFuncConsShape(
          FuncTyp("$mpdiux" :: Type, FuncTyp("$mpdiux" :: Type, Prop)),
          ConstructorShape.IdShape.byTyp(
            ("has_le" :: FuncTyp(Type, Type))("$mpdiux" :: Type))),
        ConstructorSeqDom.Empty.byTyp(
          ("has_le" :: FuncTyp(Type, Type))("$mpdiux" :: Type))
      ),
      ("has_le" :: FuncTyp(Type, Type))("$mpdiux" :: Type)
    )
  )
}
