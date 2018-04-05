package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object eqInd {
  val value = Subst.Lambda("$buijsiw" :: Type, Subst.Lambda("$buijsix" :: "$buijsiw" :: Type, IndexedConstructorSeqDom.Cons("_", IndexedConstructorShape.IndexedIdShape(TypFamilyPtn.FuncTypFamily("$buijsiw" :: Type, TypFamilyPtn.IdTypFamily.byTyp(("eq" :: piDefn("'c_555128100" :: Type)(FuncTyp("'c_555128100" :: Type, FuncTyp("'c_555128100" :: Type, Prop))))("$buijsiw" :: Type)("$buijsix" :: "$buijsiw" :: Type)("$buijsix" :: "$buijsiw" :: Type))), {
    import shapeless._
    ("$buijsix" :: "$buijsiw" :: Type) :: HNil
  }), IndexedConstructorSeqDom.Empty(("eq" :: piDefn("'c_555128100" :: Type)(FuncTyp("'c_555128100" :: Type, FuncTyp("'c_555128100" :: Type, Prop))))("$buijsiw" :: Type)("$buijsix" :: "$buijsiw" :: Type), TypFamilyPtn.FuncTypFamily("$buijsiw" :: Type, TypFamilyPtn.IdTypFamily.byTyp(("eq" :: piDefn("'c_555128100" :: Type)(FuncTyp("'c_555128100" :: Type, FuncTyp("'c_555128100" :: Type, Prop))))("$buijsiw" :: Type)("$buijsix" :: "$buijsiw" :: Type)("$buijsjm" :: "$buijsiw" :: Type)))))))
}
