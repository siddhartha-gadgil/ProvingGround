package provingground.library
import provingground._
import HoTT._
import induction._
import implicits._
import shapeless._
import Fold._ // for safety
object nat$no_confusion {
  val value = lambda("'q" :: Type)(lambda("'r" :: "nat" :: Type)(lambda("'s" :: "nat" :: Type)(({
    val rxyz = eqInd.value("nat" :: Type)("'r" :: "nat" :: Type).induc(lambda("$xkoe" :: "nat" :: Type)(lmbda("_" :: ("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("nat" :: Type)("'r" :: "nat" :: Type)("$xkoe" :: "nat" :: Type))(FuncTyp(("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("nat" :: Type)("'r" :: "nat" :: Type)("$xkoe" :: "nat" :: Type), ({
      val rxyz = natInd.value.induc(lmbda("$vuyd" :: "nat" :: Type)(Type))
      rxyz
    })(({
      val rxyz = natInd.value.induc(lmbda("$vuyd" :: "nat" :: Type)(Type))
      rxyz
    })(FuncTyp("'q" :: Type, "'q" :: Type))(lambda("'k" :: "nat" :: Type)(lmbda("_" :: Type)("'q" :: Type)))("$xkoe" :: "nat" :: Type))(lambda("'k" :: "nat" :: Type)(lmbda("_" :: Type)(({
      val rxyz = natInd.value.induc(lmbda("$vuyd" :: "nat" :: Type)(Type))
      rxyz
    })("'q" :: Type)(lambda("'k" :: "nat" :: Type)(lmbda("_" :: Type)(FuncTyp(FuncTyp(("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("nat" :: Type)("'k" :: "nat" :: Type)("'k" :: "nat" :: Type), "'q" :: Type), "'q" :: Type))))("$xkoe" :: "nat" :: Type))))("'r" :: "nat" :: Type)))))
    rxyz
  })(lmbda("_" :: ("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("nat" :: Type)("'r" :: "nat" :: Type)("'r" :: "nat" :: Type))(({
    val rxyz = natInd.value.induc(lmbda("$vuyd" :: "nat" :: Type)(({
      val rxyz = natInd.value.induc(lmbda("$vuyd" :: "nat" :: Type)(Type))
      rxyz
    })(({
      val rxyz = natInd.value.induc(lmbda("$vuyd" :: "nat" :: Type)(Type))
      rxyz
    })(FuncTyp("'q" :: Type, "'q" :: Type))(lambda("'k" :: "nat" :: Type)(lmbda("_" :: Type)("'q" :: Type)))("$vuyd" :: "nat" :: Type))(lambda("'k" :: "nat" :: Type)(lmbda("_" :: Type)(({
      val rxyz = natInd.value.induc(lmbda("$vuyd" :: "nat" :: Type)(Type))
      rxyz
    })("'q" :: Type)(lambda("'k" :: "nat" :: Type)(lmbda("_" :: Type)(FuncTyp(FuncTyp(("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("nat" :: Type)("'k" :: "nat" :: Type)("'k" :: "nat" :: Type), "'q" :: Type), "'q" :: Type))))("$vuyd" :: "nat" :: Type))))("$vuyd" :: "nat" :: Type)))
    rxyz
  })(lmbda("'t" :: "'q" :: Type)("'t" :: "'q" :: Type))(lambda("'k" :: "nat" :: Type)(lmbda("_" :: ({
    val rxyz = natInd.value.induc(lmbda("$vuyd" :: "nat" :: Type)(Type))
    rxyz
  })(({
    val rxyz = natInd.value.induc(lmbda("$vuyd" :: "nat" :: Type)(Type))
    rxyz
  })(FuncTyp("'q" :: Type, "'q" :: Type))(lambda("'k" :: "nat" :: Type)(lmbda("_" :: Type)("'q" :: Type)))("'k" :: "nat" :: Type))(lambda("'k" :: "nat" :: Type)(lmbda("_" :: Type)(({
    val rxyz = natInd.value.induc(lmbda("$vuyd" :: "nat" :: Type)(Type))
    rxyz
  })("'q" :: Type)(lambda("'k" :: "nat" :: Type)(lmbda("_" :: Type)(FuncTyp(FuncTyp(("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("nat" :: Type)("'k" :: "nat" :: Type)("'k" :: "nat" :: Type), "'q" :: Type), "'q" :: Type))))("'k" :: "nat" :: Type))))("'k" :: "nat" :: Type))(lmbda("'u" :: FuncTyp(("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("nat" :: Type)("'k" :: "nat" :: Type)("'k" :: "nat" :: Type), "'q" :: Type))("_" :: "'q" :: Type))))("'r" :: "nat" :: Type)))("'s" :: "nat" :: Type)("_" :: ("eq" :: piDefn("'c" :: Type)(FuncTyp("'c" :: Prop, FuncTyp("'c" :: Prop, Prop))))("nat" :: Type)("'r" :: "nat" :: Type)("'s" :: "nat" :: Type)))))
}
