package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object pprodInd { val value = Subst.Lambda("$cwtzu" :: Type, Subst.Lambda("$cwtzv" :: Type, ConstructorSeqTL(ConstructorSeqDom.Cons(ApplnSym(("pprod.mk" :: piDefn("'f" :: Type)(piDefn("'g" :: Type)(FuncTyp("'f" :: Type, FuncTyp("'g" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f" :: Type)("'g" :: Type))))))("$cwtzu" :: Type), "$cwtzv" :: Type), ConstructorShape.CnstFuncConsShape("$cwtzu" :: Type, ConstructorShape.CnstFuncConsShape("$cwtzv" :: Type, ConstructorShape.IdShape.byTyp(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("$cwtzu" :: Type)("$cwtzv" :: Type)))), ConstructorSeqDom.Empty.byTyp(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("$cwtzu" :: Type)("$cwtzv" :: Type))), ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("$cwtzu" :: Type)("$cwtzv" :: Type)))) }
