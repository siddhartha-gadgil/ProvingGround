package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object has_addInd {
  lazy val value = Subst.Lambda(
    "$mpdirc" :: Type,
    ConstructorSeqTL(
      ConstructorSeqDom.Cons(
        ApplnSym(
          "has_add.mk" :: piDefn("'d_1188183567" :: Type)(
            FuncTyp(
              FuncTyp("'d_1188183567" :: Type,
                      FuncTyp("'d_1188183567" :: Type,
                              "'d_1188183567" :: Type)),
              ("has_add" :: FuncTyp(Type, Type))("'d_1188183567" :: Type))),
          "$mpdirc" :: Type
        ),
        ConstructorShape.CnstFuncConsShape(
          FuncTyp("$mpdirc" :: Type,
                  FuncTyp("$mpdirc" :: Type, "$mpdirc" :: Type)),
          ConstructorShape.IdShape.byTyp(
            ("has_add" :: FuncTyp(Type, Type))("$mpdirc" :: Type))),
        ConstructorSeqDom.Empty.byTyp(
          ("has_add" :: FuncTyp(Type, Type))("$mpdirc" :: Type))
      ),
      ("has_add" :: FuncTyp(Type, Type))("$mpdirc" :: Type)
    )
  )
}
