
package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
object decidableInd { val value = Subst.Lambda("$cxbah" :: Prop, ConstructorSeqTL(ConstructorSeqDom.Cons(HoTT.Name("(decidable.is_false) ($cxbah)"), ConstructorShape.CnstFuncConsShape(FuncTyp("$cxbah" :: Prop, "false" :: Prop), ConstructorShape.IdShape.byTyp(("decidable" :: FuncTyp(Prop, Type))("$cxbah" :: Prop))), ConstructorSeqDom.Cons(HoTT.Name("(decidable.is_true) ($cxbah)"), ConstructorShape.CnstFuncConsShape("$cxbah" :: Prop, ConstructorShape.IdShape.byTyp(("decidable" :: FuncTyp(Prop, Type))("$cxbah" :: Prop))), ConstructorSeqDom.Empty.byTyp(("decidable" :: FuncTyp(Prop, Type))("$cxbah" :: Prop)))), ("decidable" :: FuncTyp(Prop, Type))("$cxbah" :: Prop))) }