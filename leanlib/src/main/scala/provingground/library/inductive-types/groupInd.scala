package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object groupInd {
  val value = Subst.Lambda("$buigxqj" :: Type, ConstructorSeqTL(ConstructorSeqDom.Cons(ApplnSym("group.mk" :: piDefn("'dh" :: Type)(piDefn("'di" :: FuncTyp("'dh" :: Type, FuncTyp("'dh" :: Type, "'dh" :: Type)))(piDefn("'dj" :: piDefn("'dj" :: "'dh" :: Type)(piDefn("'dk" :: "'dh" :: Type)(piDefn("'dl" :: "'dh" :: Type)(("eq" :: piDefn("'c_896019176" :: Type)(FuncTyp("'c_896019176" :: Type, FuncTyp("'c_896019176" :: Type, Prop))))("'dh" :: Type)(("'di" :: FuncTyp("'dh" :: Type, FuncTyp("'dh" :: Type, "'dh" :: Type)))(("'di" :: FuncTyp("'dh" :: Type, FuncTyp("'dh" :: Type, "'dh" :: Type)))("'dj" :: "'dh" :: Type)("'dk" :: "'dh" :: Type))("'dl" :: "'dh" :: Type))(("'di" :: FuncTyp("'dh" :: Type, FuncTyp("'dh" :: Type, "'dh" :: Type)))("'dj" :: "'dh" :: Type)(("'di" :: FuncTyp("'dh" :: Type, FuncTyp("'dh" :: Type, "'dh" :: Type)))("'dk" :: "'dh" :: Type)("'dl" :: "'dh" :: Type)))))))(piDefn("'dk" :: "'dh" :: Type)(piDefn("'dl" :: piDefn("'dl" :: "'dh" :: Type)(("eq" :: piDefn("'c_896019176" :: Type)(FuncTyp("'c_896019176" :: Type, FuncTyp("'c_896019176" :: Type, Prop))))("'dh" :: Type)(("'di" :: FuncTyp("'dh" :: Type, FuncTyp("'dh" :: Type, "'dh" :: Type)))("'dk" :: "'dh" :: Type)("'dl" :: "'dh" :: Type))("'dl" :: "'dh" :: Type)))(piDefn("'dm" :: piDefn("'dm" :: "'dh" :: Type)(("eq" :: piDefn("'c_896019176" :: Type)(FuncTyp("'c_896019176" :: Type, FuncTyp("'c_896019176" :: Type, Prop))))("'dh" :: Type)(("'di" :: FuncTyp("'dh" :: Type, FuncTyp("'dh" :: Type, "'dh" :: Type)))("'dm" :: "'dh" :: Type)("'dk" :: "'dh" :: Type))("'dm" :: "'dh" :: Type)))(piDefn("'dn" :: FuncTyp("'dh" :: Type, "'dh" :: Type))(FuncTyp(piDefn("'do" :: "'dh" :: Type)(("eq" :: piDefn("'c_896019176" :: Type)(FuncTyp("'c_896019176" :: Type, FuncTyp("'c_896019176" :: Type, Prop))))("'dh" :: Type)(("'di" :: FuncTyp("'dh" :: Type, FuncTyp("'dh" :: Type, "'dh" :: Type)))(("'dn" :: FuncTyp("'dh" :: Type, "'dh" :: Type))("'do" :: "'dh" :: Type))("'do" :: "'dh" :: Type))("'dk" :: "'dh" :: Type)), ("group" :: FuncTyp(Type, Type))("'dh" :: Type))))))))), "$buigxqj" :: Type), {
    val x = "$buijeob" :: FuncTyp("$buigxqj" :: Type, FuncTyp("$buigxqj" :: Type, "$buigxqj" :: Type))
    x ~>: {
      val x = "$buijest" :: piDefn("'dj" :: "$buigxqj" :: Type)(piDefn("'dk" :: "$buigxqj" :: Type)(piDefn("'dl" :: "$buigxqj" :: Type)(("eq" :: piDefn("'c_896019176" :: Type)(FuncTyp("'c_896019176" :: Type, FuncTyp("'c_896019176" :: Type, Prop))))("$buigxqj" :: Type)(("$buijeob" :: FuncTyp("$buigxqj" :: Type, FuncTyp("$buigxqj" :: Type, "$buigxqj" :: Type)))(("$buijeob" :: FuncTyp("$buigxqj" :: Type, FuncTyp("$buigxqj" :: Type, "$buigxqj" :: Type)))("'dj" :: "$buigxqj" :: Type)("'dk" :: "$buigxqj" :: Type))("'dl" :: "$buigxqj" :: Type))(("$buijeob" :: FuncTyp("$buigxqj" :: Type, FuncTyp("$buigxqj" :: Type, "$buigxqj" :: Type)))("'dj" :: "$buigxqj" :: Type)(("$buijeob" :: FuncTyp("$buigxqj" :: Type, FuncTyp("$buigxqj" :: Type, "$buigxqj" :: Type)))("'dk" :: "$buigxqj" :: Type)("'dl" :: "$buigxqj" :: Type))))))
      x ~>: {
        val x = "$buijezy" :: "$buigxqj" :: Type
        x ~>: {
          val x = "$buijhsv" :: piDefn("'dl" :: "$buigxqj" :: Type)(("eq" :: piDefn("'c_896019176" :: Type)(FuncTyp("'c_896019176" :: Type, FuncTyp("'c_896019176" :: Type, Prop))))("$buigxqj" :: Type)(("$buijeob" :: FuncTyp("$buigxqj" :: Type, FuncTyp("$buigxqj" :: Type, "$buigxqj" :: Type)))("$buijezy" :: "$buigxqj" :: Type)("'dl" :: "$buigxqj" :: Type))("'dl" :: "$buigxqj" :: Type))
          x ~>: {
            val x = "$buijlor" :: piDefn("'dm" :: "$buigxqj" :: Type)(("eq" :: piDefn("'c_896019176" :: Type)(FuncTyp("'c_896019176" :: Type, FuncTyp("'c_896019176" :: Type, Prop))))("$buigxqj" :: Type)(("$buijeob" :: FuncTyp("$buigxqj" :: Type, FuncTyp("$buigxqj" :: Type, "$buigxqj" :: Type)))("'dm" :: "$buigxqj" :: Type)("$buijezy" :: "$buigxqj" :: Type))("'dm" :: "$buigxqj" :: Type))
            x ~>: {
              val x = "$buijmln" :: FuncTyp("$buigxqj" :: Type, "$buigxqj" :: Type)
              x ~>: ConstructorShape.CnstFuncConsShape(piDefn("'do" :: "$buigxqj" :: Type)(("eq" :: piDefn("'c_896019176" :: Type)(FuncTyp("'c_896019176" :: Type, FuncTyp("'c_896019176" :: Type, Prop))))("$buigxqj" :: Type)(("$buijeob" :: FuncTyp("$buigxqj" :: Type, FuncTyp("$buigxqj" :: Type, "$buigxqj" :: Type)))(("$buijmln" :: FuncTyp("$buigxqj" :: Type, "$buigxqj" :: Type))("'do" :: "$buigxqj" :: Type))("'do" :: "$buigxqj" :: Type))("$buijezy" :: "$buigxqj" :: Type)), ConstructorShape.IdShape.byTyp(("group" :: FuncTyp(Type, Type))("$buigxqj" :: Type)))
            }
          }
        }
      }
    }
  }, ConstructorSeqDom.Empty.byTyp(("group" :: FuncTyp(Type, Type))("$buigxqj" :: Type))), ("group" :: FuncTyp(Type, Type))("$buigxqj" :: Type)))
}
