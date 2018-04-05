package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object decidableInd { val value = Subst.Lambda("$cwtyw" :: Prop, ConstructorSeqTL(ConstructorSeqDom.Cons(ApplnSym("decidable.is_false" :: piDefn("'d" :: Prop)(FuncTyp(FuncTyp("'d" :: Prop, "false" :: Prop), ("decidable" :: FuncTyp(Prop, Type))("'d" :: Prop))), "$cwtyw" :: Prop), ConstructorShape.CnstFuncConsShape(FuncTyp("$cwtyw" :: Prop, "false" :: Prop), ConstructorShape.IdShape.byTyp(("decidable" :: FuncTyp(Prop, Type))("$cwtyw" :: Prop))), ConstructorSeqDom.Cons(ApplnSym("decidable.is_true" :: piDefn("'c" :: Prop)(FuncTyp("'c" :: Prop, ("decidable" :: FuncTyp(Prop, Type))("'c" :: Prop))), "$cwtyw" :: Prop), ConstructorShape.CnstFuncConsShape("$cwtyw" :: Prop, ConstructorShape.IdShape.byTyp(("decidable" :: FuncTyp(Prop, Type))("$cwtyw" :: Prop))), ConstructorSeqDom.Empty.byTyp(("decidable" :: FuncTyp(Prop, Type))("$cwtyw" :: Prop)))), ("decidable" :: FuncTyp(Prop, Type))("$cwtyw" :: Prop))) }
