
package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
object pprodInd { val value = Subst.Lambda("$cxbam" :: Type, Subst.Lambda("$cxban" :: Type, ConstructorSeqTL(ConstructorSeqDom.Cons(HoTT.Name("((pprod.mk) ($cxbam)) ($cxban)"), ConstructorShape.CnstFuncConsShape("$cxbam" :: Type, ConstructorShape.CnstFuncConsShape("$cxban" :: Type, ConstructorShape.IdShape.byTyp(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("$cxbam" :: Type)("$cxban" :: Type)))), ConstructorSeqDom.Empty.byTyp(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("$cxbam" :: Type)("$cxban" :: Type))), ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("$cxbam" :: Type)("$cxban" :: Type)))) }