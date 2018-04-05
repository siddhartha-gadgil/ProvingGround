package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object eqInd {
  val value = Subst.Lambda("$cwuao" :: Type, Subst.Lambda("$cwuap" :: "$cwuao" :: Type, IndexedConstructorSeqDom.Cons("_", IndexedConstructorShape.IndexedIdShape(TypFamilyPtn.FuncTypFamily("$cwuao" :: Type, TypFamilyPtn.IdTypFamily.byTyp(("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("$cwuao" :: Type)("$cwuap" :: "$cwuao" :: Type)("$cwuap" :: "$cwuao" :: Type))), {
    import shapeless._
    ("$cwuap" :: "$cwuao" :: Type) :: HNil
  }), IndexedConstructorSeqDom.Empty(("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("$cwuao" :: Type)("$cwuap" :: "$cwuao" :: Type), TypFamilyPtn.FuncTypFamily("$cwuao" :: Type, TypFamilyPtn.IdTypFamily.byTyp(("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("$cwuao" :: Type)("$cwuap" :: "$cwuao" :: Type)("$cwube" :: "$cwuao" :: Type)))))))
}
