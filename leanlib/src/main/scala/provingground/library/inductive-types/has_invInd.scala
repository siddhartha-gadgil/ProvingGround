package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object has_invInd { val value = Subst.Lambda("$buijshp" :: Type, ConstructorSeqTL(ConstructorSeqDom.Cons(ApplnSym("has_inv.mk" :: piDefn("'c" :: Type)(FuncTyp(FuncTyp("'c" :: Type, "'c" :: Type), ("has_inv" :: FuncTyp(Type, Type))("'c" :: Type))), "$buijshp" :: Type), ConstructorShape.CnstFuncConsShape(FuncTyp("$buijshp" :: Type, "$buijshp" :: Type), ConstructorShape.IdShape.byTyp(("has_inv" :: FuncTyp(Type, Type))("$buijshp" :: Type))), ConstructorSeqDom.Empty.byTyp(("has_inv" :: FuncTyp(Type, Type))("$buijshp" :: Type))), ("has_inv" :: FuncTyp(Type, Type))("$buijshp" :: Type))) }
