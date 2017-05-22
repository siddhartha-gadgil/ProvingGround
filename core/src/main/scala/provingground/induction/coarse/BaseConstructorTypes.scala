package provingground.induction.coarse

import provingground._, induction._, HoTT._

//import ConstructorPattern._

//import ConstructorPattern._
import scala.language.implicitConversions

import IterFuncPattern._

//import RecFunction._

import induction.coarse.Implicits._

import FmlyPtn._

object BaseConstructorTypes {
  import ConstructorPattern._

  implicit def WAsPtn[H <: Term with Subs[H]](w: IdW[H]) = IdFmlyPtn[H, Term]

  case object SmallBool extends SmallTyp

  case object SmallNat extends SmallTyp

  val W = IdW[Term]()

  val ttC = W.constructor(SmallBool, "true")

  val ffC = W.constructor(SmallBool, "false")

  val tt: Term = ttC.cons

  val ff: Term = ffC.cons

  val BoolCons = List(ttC, ffC)

  val zeroC = W.constructor(SmallNat, "0")

  val succC = (W -->: W).constructor(SmallNat, "succ")

  val zero: Term = zeroC.cons

  val succ: Term = succC.cons

  val one = fold(succ)(zero)

  val NatCons = List(zeroC, succC)
}
