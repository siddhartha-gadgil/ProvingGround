package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object ExistsInd {
  lazy val value = Subst.Lambda("$fpq" :: Type, Subst.Lambda("$fpr" :: FuncTyp("$fpq" :: Type, Prop), ConstructorSeqTL(ConstructorSeqDom.Cons(ApplnSym(("Exists.intro" :: piDefn("'g_168077724" :: Type)(piDefn("'h_1260044138" :: FuncTyp("'g_168077724" :: Type, Prop))(piDefn("'i_721767496" :: "'g_168077724" :: Type)(FuncTyp(("'h_1260044138" :: FuncTyp("'g_168077724" :: Type, Prop))("'i_721767496" :: "'g_168077724" :: Type), ("Exists" :: piDefn("'c_735546885" :: Type)(FuncTyp(FuncTyp("'c_735546885" :: Type, Prop), Prop)))("'g_168077724" :: Type)("'h_1260044138" :: FuncTyp("'g_168077724" :: Type, Prop)))))))("$fpq" :: Type), "$fpr" :: FuncTyp("$fpq" :: Type, Prop)), {
    val x = "$fxr" :: "$fpq" :: Type
    x ~>: ConstructorShape.CnstFuncConsShape(("$fpr" :: FuncTyp("$fpq" :: Type, Prop))("$fxr" :: "$fpq" :: Type), ConstructorShape.IdShape.byTyp(("Exists" :: piDefn("'c_735546885" :: Type)(FuncTyp(FuncTyp("'c_735546885" :: Type, Prop), Prop)))("$fpq" :: Type)("$fpr" :: FuncTyp("$fpq" :: Type, Prop))))
  }, ConstructorSeqDom.Empty.byTyp(("Exists" :: piDefn("'c_735546885" :: Type)(FuncTyp(FuncTyp("'c_735546885" :: Type, Prop), Prop)))("$fpq" :: Type)("$fpr" :: FuncTyp("$fpq" :: Type, Prop)))), ("Exists" :: piDefn("'c_735546885" :: Type)(FuncTyp(FuncTyp("'c_735546885" :: Type, Prop), Prop)))("$fpq" :: Type)("$fpr" :: FuncTyp("$fpq" :: Type, Prop)))))
}
