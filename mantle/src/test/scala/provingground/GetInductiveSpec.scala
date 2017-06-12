package provingground

import HoTT._
import org.scalatest.FlatSpec
import induction._, implicits._
// import library._

class GetInductiveSpec extends InductionSpecTL {
  "Getting inductive type definition" should "work for Natural numbers" in {
    // import Nats._
    assert(ConstructorSeqTL.getExst(Nat, NatInd.intros.terms).value == NatInd)
  }

  it should "work for Lists" in {
    // import Lists._
    assert(ConstructorSeqTL.getExst(ListA, ListAInd.intros.terms).value == ListAInd)
  }

  it should "work for Booleans" in {
    // import Bools._
    assert(ConstructorSeqTL.getExst(Bool, BoolInd.intros.terms).value == BoolInd)
  }

  it should "work for simple binary trees" in {
    assert(ConstructorSeqTL.getExst(T, TInd.intros.terms).value == TInd)
  }

  it should "work for complex binary trees" in {
    assert(ConstructorSeqTL.getExst(BT, BTInd.intros.terms).value == BTInd)
  }

}
