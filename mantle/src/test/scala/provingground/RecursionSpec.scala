package provingground

import HoTT._
import org.scalatest.FlatSpec

import ConstructorPatterns._

import ConstructorPtn._

class RecursionSpec extends FlatSpec{
  case object Bool extends SmallTyp

  case object Nat extends SmallTyp

  val ttC  = W.constructor(Bool, "true")

  val ffC = W.constructor(Bool, "false")

  val tt : Term = ttC.cons

  val ff : Term = ffC.cons

  val BoolCons = List(ttC, ffC)

  "Boolean type" should "have constructors of type Bool" in {
    assert(tt.typ == Bool)
    assert(ff.typ == Bool)
  }

  it should "have constructor data for recursion to X with type X" in {
    assert(W.recDom(Bool, Bool) == Bool)
    assert(W.recDom(Bool, Nat) == Nat)
  }

}
