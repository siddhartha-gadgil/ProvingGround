package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._ // for safety
object eqInd {
  val value = Subst.Lambda("$cwsio" :: Type, Subst.Lambda("$cwsip" :: "$cwsio" :: Type, IndexedConstructorSeqDom.Cons(HoTT.Name("_"), IndexedConstructorShape.IndexedIdShape(TypFamilyPtn.FuncTypFamily("$cwsio" :: Type, TypFamilyPtn.IdTypFamily.byTyp(("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("$cwsio" :: Type)("$cwsip" :: "$cwsio" :: Type)("$cwsip" :: "$cwsio" :: Type))), {
    import shapeless._
    ("$cwsip" :: "$cwsio" :: Type) :: HNil
  }), IndexedConstructorSeqDom.Empty(("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("$cwsio" :: Type)("$cwsip" :: "$cwsio" :: Type), TypFamilyPtn.FuncTypFamily("$cwsio" :: Type, TypFamilyPtn.IdTypFamily.byTyp(("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("$cwsio" :: Type)("$cwsip" :: "$cwsio" :: Type)("$cwsiy" :: "$cwsio" :: Type)))))))
}
