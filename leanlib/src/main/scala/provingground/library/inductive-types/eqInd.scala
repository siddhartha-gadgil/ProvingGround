package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object eqInd {
  lazy val value = Subst.Lambda("$nvz" :: Type, Subst.Lambda("$nwa" :: "$nvz" :: Type, IndexedConstructorSeqDom.Cons("_", IndexedConstructorShape.IndexedIdShape(TypFamilyPtn.FuncTypFamily("$nvz" :: Type, TypFamilyPtn.IdTypFamily.byTyp(("eq" :: piDefn("'c_47853266" :: Type)(FuncTyp("'c_47853266" :: Type, FuncTyp("'c_47853266" :: Type, Prop))))("$nvz" :: Type)("$nwa" :: "$nvz" :: Type)("$nwa" :: "$nvz" :: Type))), {
    import shapeless._
    ("$nwa" :: "$nvz" :: Type) :: HNil
  }), IndexedConstructorSeqDom.Empty(("eq" :: piDefn("'c_47853266" :: Type)(FuncTyp("'c_47853266" :: Type, FuncTyp("'c_47853266" :: Type, Prop))))("$nvz" :: Type)("$nwa" :: "$nvz" :: Type), TypFamilyPtn.FuncTypFamily("$nvz" :: Type, TypFamilyPtn.IdTypFamily.byTyp(("eq" :: piDefn("'c_47853266" :: Type)(FuncTyp("'c_47853266" :: Type, FuncTyp("'c_47853266" :: Type, Prop))))("$nvz" :: Type)("$nwa" :: "$nvz" :: Type)("$nwp" :: "$nvz" :: Type)))))))
}
