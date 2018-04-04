package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
object decidableInd { val value = Subst.Lambda("$cwsie" :: Prop, ConstructorSeqTL(ConstructorSeqDom.Cons(HoTT.Name("(decidable.is_false) ($cwsie)"), ConstructorShape.CnstFuncConsShape(FuncTyp("$cwsie" :: Prop, "false" :: Prop), ConstructorShape.IdShape.byTyp(("decidable" :: FuncTyp(Prop, Type))("$cwsie" :: Prop))), ConstructorSeqDom.Cons(HoTT.Name("(decidable.is_true) ($cwsie)"), ConstructorShape.CnstFuncConsShape("$cwsie" :: Prop, ConstructorShape.IdShape.byTyp(("decidable" :: FuncTyp(Prop, Type))("$cwsie" :: Prop))), ConstructorSeqDom.Empty.byTyp(("decidable" :: FuncTyp(Prop, Type))("$cwsie" :: Prop)))), ("decidable" :: FuncTyp(Prop, Type))("$cwsie" :: Prop))) }
