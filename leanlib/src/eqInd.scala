
package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
object eqInd {
  val value = Subst.Lambda("$cxbar" :: Type, Subst.Lambda("$cxbas" :: "$cxbar" :: Type, IndexedConstructorSeqDom.Cons(HoTT.Name("_"), IndexedConstructorShape.IndexedIdShape(TypFamilyPtn.FuncTypFamily("$cxbar" :: Type, TypFamilyPtn.IdTypFamily.byTyp(("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("$cxbar" :: Type)("$cxbas" :: "$cxbar" :: Type)("$cxbas" :: "$cxbar" :: Type))), {
    import shapeless._
    ("$cxbas" :: "$cxbar" :: Type) :: HNil
  }), IndexedConstructorSeqDom.Empty(("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("$cxbar" :: Type)("$cxbas" :: "$cxbar" :: Type), TypFamilyPtn.FuncTypFamily("$cxbar" :: Type, TypFamilyPtn.IdTypFamily.byTyp(("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("$cxbar" :: Type)("$cxbas" :: "$cxbar" :: Type)("$cxbbb" :: "$cxbar" :: Type)))))))
}