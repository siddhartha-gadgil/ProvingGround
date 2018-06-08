package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object decidableInd {
  val value = Subst.Lambda(
    "$buifqpu" :: Prop,
    ConstructorSeqTL(
      ConstructorSeqDom.Cons(
        ApplnSym(
          "decidable.is_false" :: piDefn("'d_36962281" :: Prop)(
            FuncTyp(
              FuncTyp("'d_36962281" :: Prop, "false" :: Prop),
              ("decidable" :: FuncTyp(Prop, Type))("'d_36962281" :: Prop))),
          "$buifqpu" :: Prop
        ),
        ConstructorShape.CnstFuncConsShape(
          FuncTyp("$buifqpu" :: Prop, "false" :: Prop),
          ConstructorShape.IdShape.byTyp(
            ("decidable" :: FuncTyp(Prop, Type))("$buifqpu" :: Prop))),
        ConstructorSeqDom.Cons(
          ApplnSym(
            "decidable.is_true" :: piDefn("'c_71821961" :: Prop)(
              FuncTyp(
                "'c_71821961" :: Prop,
                ("decidable" :: FuncTyp(Prop, Type))("'c_71821961" :: Prop))),
            "$buifqpu" :: Prop
          ),
          ConstructorShape.CnstFuncConsShape(
            "$buifqpu" :: Prop,
            ConstructorShape.IdShape.byTyp(
              ("decidable" :: FuncTyp(Prop, Type))("$buifqpu" :: Prop))),
          ConstructorSeqDom.Empty.byTyp(
            ("decidable" :: FuncTyp(Prop, Type))("$buifqpu" :: Prop))
        )
      ),
      ("decidable" :: FuncTyp(Prop, Type))("$buifqpu" :: Prop)
    )
  )
}
