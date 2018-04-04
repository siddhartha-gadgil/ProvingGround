package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._ // for safety
object decidable$cases_on {
  val value = lambda("'n" :: Prop)(lambda("'o" :: FuncTyp(("decidable" :: FuncTyp(Prop, Type))("'n" :: Prop), Type))(lambda("'p" :: ("decidable" :: FuncTyp(Prop, Type))("'n" :: Prop))(lmbda("'q" :: piDefn("'q" :: FuncTyp("'n" :: Prop, "false" :: Prop))(("'o" :: FuncTyp(("decidable" :: FuncTyp(Prop, Type))("'n" :: Prop), Type))(("decidable.is_false" :: piDefn("'d" :: Prop)(FuncTyp(FuncTyp("'d" :: Prop, "false" :: Prop), ("decidable" :: FuncTyp(Prop, Type))("'d" :: Prop))))("'n" :: Prop)("'q" :: FuncTyp("'n" :: Prop, "false" :: Prop)))))(lmbda("'r" :: piDefn("_" :: "'n" :: Prop)(("'o" :: FuncTyp(("decidable" :: FuncTyp(Prop, Type))("'n" :: Prop), Type))(("decidable.is_true" :: piDefn("'c" :: Prop)(FuncTyp("'c" :: Prop, ("decidable" :: FuncTyp(Prop, Type))("'c" :: Prop))))("'n" :: Prop)("_" :: "'n" :: Prop))))(({
    val rxyz = decidableInd.value("'n" :: Prop).induc(lmbda("$admxo" :: ("decidable" :: FuncTyp(Prop, Type))("'n" :: Prop))(("'o" :: FuncTyp(("decidable" :: FuncTyp(Prop, Type))("'n" :: Prop), Type))("$admxo" :: ("decidable" :: FuncTyp(Prop, Type))("'n" :: Prop))))
    rxyz
  })("'q" :: piDefn("'q" :: FuncTyp("'n" :: Prop, "false" :: Prop))(("'o" :: FuncTyp(("decidable" :: FuncTyp(Prop, Type))("'n" :: Prop), Type))(("decidable.is_false" :: piDefn("'d" :: Prop)(FuncTyp(FuncTyp("'d" :: Prop, "false" :: Prop), ("decidable" :: FuncTyp(Prop, Type))("'d" :: Prop))))("'n" :: Prop)("'q" :: FuncTyp("'n" :: Prop, "false" :: Prop)))))("'r" :: piDefn("_" :: "'n" :: Prop)(("'o" :: FuncTyp(("decidable" :: FuncTyp(Prop, Type))("'n" :: Prop), Type))(("decidable.is_true" :: piDefn("'c" :: Prop)(FuncTyp("'c" :: Prop, ("decidable" :: FuncTyp(Prop, Type))("'c" :: Prop))))("'n" :: Prop)("_" :: "'n" :: Prop))))("'p" :: ("decidable" :: FuncTyp(Prop, Type))("'n" :: Prop)))))))
}
