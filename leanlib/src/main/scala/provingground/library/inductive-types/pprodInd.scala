package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object pprodInd {
  lazy val value = Subst.Lambda(
    "$buigwgj" :: Type,
    Subst.Lambda(
      "$buigwgk" :: Type,
      ConstructorSeqTL(
        ConstructorSeqDom.Cons(
          ApplnSym(
            ("pprod.mk" :: piDefn("'f_1037748589" :: Type)(
              piDefn("'g_733979854" :: Type)(
                FuncTyp("'f_1037748589" :: Type,
                        FuncTyp("'g_733979854" :: Type,
                                ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(
                                  "'f_1037748589" :: Type)(
                                  "'g_733979854" :: Type))))))(
              "$buigwgj" :: Type),
            "$buigwgk" :: Type
          ),
          ConstructorShape.CnstFuncConsShape(
            "$buigwgj" :: Type,
            ConstructorShape.CnstFuncConsShape(
              "$buigwgk" :: Type,
              ConstructorShape.IdShape.byTyp(
                ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))(
                  "$buigwgj" :: Type)("$buigwgk" :: Type)))
          ),
          ConstructorSeqDom.Empty.byTyp(
            ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("$buigwgj" :: Type)(
              "$buigwgk" :: Type))
        ),
        ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("$buigwgj" :: Type)(
          "$buigwgk" :: Type)
      )
    )
  )
}
