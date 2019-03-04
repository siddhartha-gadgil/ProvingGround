package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object has_zeroInd {
  lazy val value = Subst.Lambda(
    "$mpdipk" :: Type,
    ConstructorSeqTL(
      ConstructorSeqDom.Cons(
        ApplnSym("has_zero.mk" :: piDefn("'c_1531913461" :: Type)(
                   FuncTyp("'c_1531913461" :: Type,
                           ("has_zero" :: FuncTyp(Type, Type))(
                             "'c_1531913461" :: Type))),
                 "$mpdipk" :: Type),
        ConstructorShape.CnstFuncConsShape(
          "$mpdipk" :: Type,
          ConstructorShape.IdShape.byTyp(
            ("has_zero" :: FuncTyp(Type, Type))("$mpdipk" :: Type))),
        ConstructorSeqDom.Empty.byTyp(
          ("has_zero" :: FuncTyp(Type, Type))("$mpdipk" :: Type))
      ),
      ("has_zero" :: FuncTyp(Type, Type))("$mpdipk" :: Type)
    )
  )
}
