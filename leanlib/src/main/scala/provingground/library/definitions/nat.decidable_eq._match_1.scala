package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
object nat$decidable_eq$_match_1 {
  val value = lambda("'v" :: "nat" :: Type)(lambda("'w" :: "nat" :: Type)(({
    val rxyz = decidableInd.value(("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("nat" :: Type)("'v" :: "nat" :: Type)("'w" :: "nat" :: Type)).induc(lmbda("$admxo" :: ("decidable" :: FuncTyp(Prop, Type))(("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("nat" :: Type)("'v" :: "nat" :: Type)("'w" :: "nat" :: Type)))(("decidable" :: FuncTyp(Prop, Type))(("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("nat" :: Type)(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'v" :: "nat" :: Type))(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'w" :: "nat" :: Type)))))
    rxyz
  })(lmbda("_" :: FuncTyp(("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("nat" :: Type)("'v" :: "nat" :: Type)("'w" :: "nat" :: Type), "false" :: Prop))("_" :: ("decidable" :: FuncTyp(Prop, Type))(("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("nat" :: Type)(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'v" :: "nat" :: Type))(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'w" :: "nat" :: Type)))))(lmbda("_" :: ("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("nat" :: Type)("'v" :: "nat" :: Type)("'w" :: "nat" :: Type))("_" :: ("decidable" :: FuncTyp(Prop, Type))(("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("nat" :: Type)(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'v" :: "nat" :: Type))(("nat.succ" :: FuncTyp("nat" :: Type, "nat" :: Type))("'w" :: "nat" :: Type)))))))
}
