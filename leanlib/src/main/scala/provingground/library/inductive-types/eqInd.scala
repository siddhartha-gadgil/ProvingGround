package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object eqInd {
  lazy val value = Subst.Lambda("$abfl" :: Type, Subst.Lambda("$abfm" :: "$abfl" :: Type, IndexedConstructorSeqDom.Cons("_", IndexedConstructorShape.IndexedIdShape(TypFamilyPtn.FuncTypFamily("$abfl" :: Type, TypFamilyPtn.IdTypFamily.byTyp(("eq" :: piDefn("'c_885647912" :: Type)(FuncTyp("'c_885647912" :: Type, FuncTyp("'c_885647912" :: Type, Prop))))("$abfl" :: Type)("$abfm" :: "$abfl" :: Type)("$abfm" :: "$abfl" :: Type))), {
    import shapeless._
    ("$abfm" :: "$abfl" :: Type) :: HNil
  }), IndexedConstructorSeqDom.Empty(("eq" :: piDefn("'c_885647912" :: Type)(FuncTyp("'c_885647912" :: Type, FuncTyp("'c_885647912" :: Type, Prop))))("$abfl" :: Type)("$abfm" :: "$abfl" :: Type), TypFamilyPtn.FuncTypFamily("$abfl" :: Type, TypFamilyPtn.IdTypFamily.byTyp(("eq" :: piDefn("'c_885647912" :: Type)(FuncTyp("'c_885647912" :: Type, FuncTyp("'c_885647912" :: Type, Prop))))("$abfl" :: Type)("$abfm" :: "$abfl" :: Type)("$abgb" :: "$abfl" :: Type)))))))
}
