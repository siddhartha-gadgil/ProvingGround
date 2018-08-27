package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object orInd { lazy val value = Subst.Lambda("$fdo" :: Prop, Subst.Lambda("$fdp" :: Prop, ConstructorSeqTL(ConstructorSeqDom.Cons(ApplnSym(("or.inl" :: piDefn("'e" :: Prop)(piDefn("'f" :: Prop)(FuncTyp("'e" :: Prop, ("or" :: FuncTyp(Prop, FuncTyp(Prop, Prop)))("'e" :: Prop)("'f" :: Prop)))))("$fdo" :: Prop), "$fdp" :: Prop), ConstructorShape.CnstFuncConsShape("$fdo" :: Prop, ConstructorShape.IdShape.byTyp(("or" :: FuncTyp(Prop, FuncTyp(Prop, Prop)))("$fdo" :: Prop)("$fdp" :: Prop))), ConstructorSeqDom.Cons(ApplnSym(("or.inr" :: piDefn("'e" :: Prop)(piDefn("'f" :: Prop)(FuncTyp("'f" :: Prop, ("or" :: FuncTyp(Prop, FuncTyp(Prop, Prop)))("'e" :: Prop)("'f" :: Prop)))))("$fdo" :: Prop), "$fdp" :: Prop), ConstructorShape.CnstFuncConsShape("$fdp" :: Prop, ConstructorShape.IdShape.byTyp(("or" :: FuncTyp(Prop, FuncTyp(Prop, Prop)))("$fdo" :: Prop)("$fdp" :: Prop))), ConstructorSeqDom.Empty.byTyp(("or" :: FuncTyp(Prop, FuncTyp(Prop, Prop)))("$fdo" :: Prop)("$fdp" :: Prop)))), ("or" :: FuncTyp(Prop, FuncTyp(Prop, Prop)))("$fdo" :: Prop)("$fdp" :: Prop)))) }
