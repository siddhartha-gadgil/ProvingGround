package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object semigroupInd {
  val value = Subst.Lambda("$buigwhd" :: Type, ConstructorSeqTL(ConstructorSeqDom.Cons(ApplnSym("semigroup.mk" :: piDefn("'q" :: Type)(piDefn("'r" :: FuncTyp("'q" :: Type, FuncTyp("'q" :: Type, "'q" :: Type)))(FuncTyp(piDefn("'s" :: "'q" :: Type)(piDefn("'t" :: "'q" :: Type)(piDefn("'u" :: "'q" :: Type)(("eq" :: piDefn("'c_896019176" :: Type)(FuncTyp("'c_896019176" :: Type, FuncTyp("'c_896019176" :: Type, Prop))))("'q" :: Type)(("'r" :: FuncTyp("'q" :: Type, FuncTyp("'q" :: Type, "'q" :: Type)))(("'r" :: FuncTyp("'q" :: Type, FuncTyp("'q" :: Type, "'q" :: Type)))("'s" :: "'q" :: Type)("'t" :: "'q" :: Type))("'u" :: "'q" :: Type))(("'r" :: FuncTyp("'q" :: Type, FuncTyp("'q" :: Type, "'q" :: Type)))("'s" :: "'q" :: Type)(("'r" :: FuncTyp("'q" :: Type, FuncTyp("'q" :: Type, "'q" :: Type)))("'t" :: "'q" :: Type)("'u" :: "'q" :: Type)))))), ("semigroup" :: FuncTyp(Type, Type))("'q" :: Type)))), "$buigwhd" :: Type), {
    val x = "$buigxlo" :: FuncTyp("$buigwhd" :: Type, FuncTyp("$buigwhd" :: Type, "$buigwhd" :: Type))
    x ~>: ConstructorShape.CnstFuncConsShape(piDefn("'s" :: "$buigwhd" :: Type)(piDefn("'t" :: "$buigwhd" :: Type)(piDefn("'u" :: "$buigwhd" :: Type)(("eq" :: piDefn("'c_896019176" :: Type)(FuncTyp("'c_896019176" :: Type, FuncTyp("'c_896019176" :: Type, Prop))))("$buigwhd" :: Type)(("$buigxlo" :: FuncTyp("$buigwhd" :: Type, FuncTyp("$buigwhd" :: Type, "$buigwhd" :: Type)))(("$buigxlo" :: FuncTyp("$buigwhd" :: Type, FuncTyp("$buigwhd" :: Type, "$buigwhd" :: Type)))("'s" :: "$buigwhd" :: Type)("'t" :: "$buigwhd" :: Type))("'u" :: "$buigwhd" :: Type))(("$buigxlo" :: FuncTyp("$buigwhd" :: Type, FuncTyp("$buigwhd" :: Type, "$buigwhd" :: Type)))("'s" :: "$buigwhd" :: Type)(("$buigxlo" :: FuncTyp("$buigwhd" :: Type, FuncTyp("$buigwhd" :: Type, "$buigwhd" :: Type)))("'t" :: "$buigwhd" :: Type)("'u" :: "$buigwhd" :: Type)))))), ConstructorShape.IdShape.byTyp(("semigroup" :: FuncTyp(Type, Type))("$buigwhd" :: Type)))
  }, ConstructorSeqDom.Empty.byTyp(("semigroup" :: FuncTyp(Type, Type))("$buigwhd" :: Type))), ("semigroup" :: FuncTyp(Type, Type))("$buigwhd" :: Type)))
}
