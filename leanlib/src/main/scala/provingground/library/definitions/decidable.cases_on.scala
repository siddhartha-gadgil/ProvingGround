package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._
object decidable$cases_on {
  val value = lambda("'n_712724742" :: Prop)(lambda("'o_2060137816" :: FuncTyp(("decidable" :: FuncTyp(Prop, Type))("'n_712724742" :: Prop), Type))(lambda("'p_864278074" :: ("decidable" :: FuncTyp(Prop, Type))("'n_712724742" :: Prop))(lmbda("'q_372673988" :: piDefn("'q_1399161972" :: FuncTyp("'n_712724742" :: Prop, "false" :: Prop))(("'o_2060137816" :: FuncTyp(("decidable" :: FuncTyp(Prop, Type))("'n_712724742" :: Prop), Type))(("decidable.is_false" :: piDefn("'d" :: Prop)(FuncTyp(FuncTyp("'d" :: Prop, "false" :: Prop), ("decidable" :: FuncTyp(Prop, Type))("'d" :: Prop))))("'n_712724742" :: Prop)("'q_1399161972" :: FuncTyp("'n_712724742" :: Prop, "false" :: Prop)))))(lmbda("'r_1054398262" :: piDefn("_" :: "'n_712724742" :: Prop)(("'o_2060137816" :: FuncTyp(("decidable" :: FuncTyp(Prop, Type))("'n_712724742" :: Prop), Type))(("decidable.is_true" :: piDefn("'c" :: Prop)(FuncTyp("'c" :: Prop, ("decidable" :: FuncTyp(Prop, Type))("'c" :: Prop))))("'n_712724742" :: Prop)("_" :: "'n_712724742" :: Prop))))(({
    val rxyz = decidableInd.value("'n_712724742" :: Prop).induc(lmbda("$admxo_2059613322" :: ("decidable" :: FuncTyp(Prop, Type))("'n_712724742" :: Prop))(("'o_2060137816" :: FuncTyp(("decidable" :: FuncTyp(Prop, Type))("'n_712724742" :: Prop), Type))("$admxo_2059613322" :: ("decidable" :: FuncTyp(Prop, Type))("'n_712724742" :: Prop))))
    rxyz
  })("'q_372673988" :: piDefn("'q_1399161972" :: FuncTyp("'n_712724742" :: Prop, "false" :: Prop))(("'o_2060137816" :: FuncTyp(("decidable" :: FuncTyp(Prop, Type))("'n_712724742" :: Prop), Type))(("decidable.is_false" :: piDefn("'d" :: Prop)(FuncTyp(FuncTyp("'d" :: Prop, "false" :: Prop), ("decidable" :: FuncTyp(Prop, Type))("'d" :: Prop))))("'n_712724742" :: Prop)("'q_1399161972" :: FuncTyp("'n_712724742" :: Prop, "false" :: Prop)))))("'r_1054398262" :: piDefn("_" :: "'n_712724742" :: Prop)(("'o_2060137816" :: FuncTyp(("decidable" :: FuncTyp(Prop, Type))("'n_712724742" :: Prop), Type))(("decidable.is_true" :: piDefn("'c" :: Prop)(FuncTyp("'c" :: Prop, ("decidable" :: FuncTyp(Prop, Type))("'c" :: Prop))))("'n_712724742" :: Prop)("_" :: "'n_712724742" :: Prop))))("'p_864278074" :: ("decidable" :: FuncTyp(Prop, Type))("'n_712724742" :: Prop)))))))
}
