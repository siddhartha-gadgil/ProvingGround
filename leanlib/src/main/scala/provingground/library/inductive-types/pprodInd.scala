package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object pprodInd {
  lazy val value = Subst.Lambda(
    "$mpdiqi" :: Type,
    Subst.Lambda(
      "$mpdiqj" :: Type,
      ConstructorSeqTL(
        ConstructorSeqDom.Cons(
          ApplnSym(
            ("pprod.mk" :: piDefn("'f_1394520732" :: Type)(
              piDefn("'g_1655163109" :: Type)(
                FuncTyp("'f_1394520732" :: Type,
                        FuncTyp("'g_1655163109" :: Type,
                                ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(
                                  "'f_1394520732" :: Type)(
                                  "'g_1655163109" :: Type))))))(
              "$mpdiqi" :: Type),
            "$mpdiqj" :: Type
          ),
          ConstructorShape.CnstFuncConsShape(
            "$mpdiqi" :: Type,
            ConstructorShape.CnstFuncConsShape(
              "$mpdiqj" :: Type,
              ConstructorShape.IdShape.byTyp(
                ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(
                  "$mpdiqi" :: Type)("$mpdiqj" :: Type)))
          ),
          ConstructorSeqDom.Empty.byTyp(
            ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("$mpdiqi" :: Type)(
              "$mpdiqj" :: Type))
        ),
        ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("$mpdiqi" :: Type)(
          "$mpdiqj" :: Type)
      )
    )
  )
}
