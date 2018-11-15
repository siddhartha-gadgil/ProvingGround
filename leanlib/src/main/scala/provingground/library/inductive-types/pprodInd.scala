package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object pprodInd { lazy val value = Subst.Lambda("$mpcetc" :: Type, Subst.Lambda("$mpcetd" :: Type, ConstructorSeqTL(ConstructorSeqDom.Cons(ApplnSym(("pprod.mk" :: piDefn("'f_234597865" :: Type)(piDefn("'g_1111384708" :: Type)(FuncTyp("'f_234597865" :: Type, FuncTyp("'g_1111384708" :: Type, ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("'f_234597865" :: Type)("'g_1111384708" :: Type))))))("$mpcetc" :: Type), "$mpcetd" :: Type), ConstructorShape.CnstFuncConsShape("$mpcetc" :: Type, ConstructorShape.CnstFuncConsShape("$mpcetd" :: Type, ConstructorShape.IdShape.byTyp(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("$mpcetc" :: Type)("$mpcetd" :: Type)))), ConstructorSeqDom.Empty.byTyp(("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("$mpcetc" :: Type)("$mpcetd" :: Type))), ("pprod" :: FuncTyp(Type, FuncTyp(Type, Type)))("$mpcetc" :: Type)("$mpcetd" :: Type)))) }
