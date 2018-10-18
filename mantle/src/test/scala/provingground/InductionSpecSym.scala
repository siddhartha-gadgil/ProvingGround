package provingground

import HoTT._
import scalahott._
import org.scalatest.FlatSpec
import induction.implicits._
import provingground.library.{DoubleEvenSym, LocalConstImpliesConstSym, SuccNOrNEvenSym}
// import Fold._

import shapeless._
import NatRing._

class InductionSpecSym extends FlatSpec {

  val n = NatTyp.sym
  val m = NatTyp.sym

  val recNN = NatRing.rec(NatTyp)
  val recNNN = NatRing.rec(NatTyp ->: NatTyp)

  "Ackermann function recursively defined symbolically" should "give the correct values" in {
    val ackm = "ack(m)" :: NatTyp ->: NatTyp

    val ackmp1n = "ack(m+1)(n)" :: NatTyp

    val ack = recNNN(succ)(
      m :-> (ackm :-> recNN(ackm(Literal(1)))(n :-> (ackmp1n :-> (ackm(ackmp1n))))))

    assert(ack(Literal(2))(Literal(2)) == Literal(7))
    assert(ack(Literal(3))(Literal(1)) == Literal(13))
  }

  "Example Theorems Symbolically proved" should "have proofs with types the theorems" ignore {
    assert(DoubleEvenSym.pf.typ == DoubleEvenSym.thm)
    assert(SuccNOrNEvenSym.pf.typ == SuccNOrNEvenSym.thm)
    assert(LocalConstImpliesConstSym.pf.typ == LocalConstImpliesConstSym.thm)
  }

}
