package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object nat$less_than_or_equalInd {
  lazy val value = Subst.Lambda(
    "$mpdiru" :: "nat" :: Type,
    IndexedConstructorSeqDom.Cons(
      "_",
      IndexedConstructorShape.IndexedIdShape(
        TypFamilyPtn.FuncTypFamily(
          "nat" :: Type,
          TypFamilyPtn.IdTypFamily.byTyp(
            ("nat.less_than_or_equal" :: FuncTyp("nat" :: Type,
                                                 FuncTyp("nat" :: Type, Prop)))(
              "$mpdiru" :: "nat" :: Type)("$mpdiru" :: "nat" :: Type))
        ), {
          import shapeless._
          ("$mpdiru" :: "nat" :: Type) :: HNil
        }
      ),
      IndexedConstructorSeqDom.Cons(
        ApplnSym(
          "nat.less_than_or_equal.step" :: piDefn(
            "'e_633342745" :: "nat" :: Type)(
            piDefn("'f_132650318" :: "nat" :: Type)(FuncTyp(
              ("nat.less_than_or_equal" :: FuncTyp(
                "nat" :: Type,
                FuncTyp("nat" :: Type, Prop)))("'e_633342745" :: "nat" :: Type)(
                "'f_132650318" :: "nat" :: Type),
              ("nat.less_than_or_equal" :: FuncTyp(
                "nat" :: Type,
                FuncTyp("nat" :: Type, Prop)))("'e_633342745" :: "nat" :: Type)(
                ("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))(
                  "'f_132650318" :: "nat" :: Type))
            ))),
          "$mpdiru" :: "nat" :: Type
        ), {
          val x = "$mpditt" :: "nat" :: Type
          x ~>>: IndexedConstructorShape.IndexedIndexedFuncConsShape(
            IndexedIterFuncShape.IdIterShape(
              TypFamilyPtn.FuncTypFamily(
                "nat" :: Type,
                TypFamilyPtn.IdTypFamily.byTyp(
                  ("nat.less_than_or_equal" :: FuncTyp("nat" :: Type,
                                                       FuncTyp("nat" :: Type,
                                                               Prop)))(
                    "$mpdiru" :: "nat" :: Type)("$mpditt" :: "nat" :: Type))
              ), {
                import shapeless._
                ("$mpditt" :: "nat" :: Type) :: HNil
              }
            ),
            IndexedConstructorShape.IndexedIdShape(
              TypFamilyPtn.FuncTypFamily(
                "nat" :: Type,
                TypFamilyPtn.IdTypFamily.byTyp(
                  ("nat.less_than_or_equal" :: FuncTyp(
                    "nat" :: Type,
                    FuncTyp("nat" :: Type, Prop)))("$mpdiru" :: "nat" :: Type)(
                    ("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))(
                      "$mpditt" :: "nat" :: Type)))
              ), {
                import shapeless._
                ("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))(
                  "$mpditt" :: "nat" :: Type) :: HNil
              }
            ), {
              import shapeless._
              ("$mpditt" :: "nat" :: Type) :: HNil
            }
          )
        },
        IndexedConstructorSeqDom.Empty(
          ("nat.less_than_or_equal" :: FuncTyp(
            "nat" :: Type,
            FuncTyp("nat" :: Type, Prop)))("$mpdiru" :: "nat" :: Type),
          TypFamilyPtn.FuncTypFamily(
            "nat" :: Type,
            TypFamilyPtn.IdTypFamily.byTyp(
              ("nat.less_than_or_equal" :: FuncTyp("nat" :: Type,
                                                   FuncTyp("nat" :: Type,
                                                           Prop)))(
                "$mpdiru" :: "nat" :: Type)("$mpdiub" :: "nat" :: Type))
          )
        )
      )
    )
  )
}
