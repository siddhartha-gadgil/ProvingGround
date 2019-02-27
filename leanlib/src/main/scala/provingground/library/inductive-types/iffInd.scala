package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object iffInd {
  lazy val value = Subst.Lambda(
    "$abel" :: Prop,
    Subst.Lambda(
      "$abem" :: Prop,
      ConstructorSeqTL(
        ConstructorSeqDom.Cons(
          ApplnSym(
            ("iff.intro" :: piDefn("'f_803143475" :: Prop)(
              piDefn("'g_561098509" :: Prop)(FuncTyp(
                FuncTyp("'f_803143475" :: Prop, "'g_561098509" :: Prop),
                FuncTyp(FuncTyp("'g_561098509" :: Prop, "'f_803143475" :: Prop),
                        ("iff" :: FuncTyp(Prop, FuncTyp(Prop, Prop)))(
                          "'f_803143475" :: Prop)("'g_561098509" :: Prop))
              ))))("$abel" :: Prop),
            "$abem" :: Prop
          ),
          ConstructorShape.CnstFuncConsShape(
            FuncTyp("$abel" :: Prop, "$abem" :: Prop),
            ConstructorShape.CnstFuncConsShape(
              FuncTyp("$abem" :: Prop, "$abel" :: Prop),
              ConstructorShape.IdShape.byTyp(
                ("iff" :: FuncTyp(Prop, FuncTyp(Prop, Prop)))("$abel" :: Prop)(
                  "$abem" :: Prop)))
          ),
          ConstructorSeqDom.Empty.byTyp(
            ("iff" :: FuncTyp(Prop, FuncTyp(Prop, Prop)))("$abel" :: Prop)(
              "$abem" :: Prop))
        ),
        ("iff" :: FuncTyp(Prop, FuncTyp(Prop, Prop)))("$abel" :: Prop)(
          "$abem" :: Prop)
      )
    )
  )
}
