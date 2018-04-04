package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
object pprodInd { val value = Subst.Lambda("$cwsij" :: Type, Subst.Lambda("$cwsik" :: Type, ConstructorSeqTL(ConstructorSeqDom.Cons(HoTT.Name("((pprod.mk) ($cwsij)) ($cwsik)"), ConstructorShape.CnstFuncConsShape("$cwsij" :: Type, ConstructorShape.CnstFuncConsShape("$cwsik" :: Type, ConstructorShape.IdShape.byTyp(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("$cwsij" :: Type)("$cwsik" :: Type)))), ConstructorSeqDom.Empty.byTyp(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("$cwsij" :: Type)("$cwsik" :: Type))), ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("$cwsij" :: Type)("$cwsik" :: Type)))) }
