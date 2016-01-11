package provingground

import HoTT._

import ConstructorPattern._

import ConstructorPattern._

import RecFunction._

object BaseConstructorTypes{
  case object Bool extends SmallTyp

  case object Nat extends SmallTyp

  val ttC  = W.constructor(Bool, "true")

  val ffC = W.constructor(Bool, "false")

  val tt : Term = ttC.cons

  val ff : Term = ffC.cons

  val BoolCons = List(ttC, ffC)

  val zeroC = W.constructor(Nat, "0")

  val succC = (W -->: W).constructor(Nat, "succ")

  val zero : Term = zeroC.cons

  val succ : Term = succC.cons

  val one = fold(succ)(zero)

  val NatCons = List(zeroC, succC)
}
