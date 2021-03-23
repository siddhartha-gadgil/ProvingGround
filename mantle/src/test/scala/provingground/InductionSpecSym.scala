package provingground

import HoTT._
import scalahott._
import org.scalatest._, flatspec._
import induction.implicits._
import provingground.library.{
  DoubleEvenSym,
  LocalConstImpliesConstSym,
  SuccNOrNEvenSym
}
// import Fold._

import shapeless._
import NatRing._
import spire.math.SafeLong

class InductionSpecSym extends flatspec.AnyFlatSpec {

  val n = NatTyp.sym
  val m = NatTyp.sym

  val recNN: Func[ScalaTerm[SafeLong], Func[Func[NatRing.Nat, Func[ScalaTerm[
    SafeLong
  ], ScalaTerm[SafeLong]]], Func[NatRing.Nat, ScalaTerm[SafeLong]]]] =
    NatRing.rec(NatTyp)
  val recNNN: Func[Func[ScalaTerm[SafeLong], ScalaTerm[SafeLong]], Func[Func[
    NatRing.Nat,
    Func[Func[ScalaTerm[SafeLong], ScalaTerm[SafeLong]], Func[ScalaTerm[
      SafeLong
    ], ScalaTerm[SafeLong]]]
  ], Func[NatRing.Nat, Func[ScalaTerm[SafeLong], ScalaTerm[SafeLong]]]]] =
    NatRing.rec(NatTyp ->: NatTyp)

  "Ackermann function recursively defined symbolically" should "give the correct values" in {
    val ackm
        : Func[ScalaTerm[SafeLong], ScalaTerm[SafeLong]] = "ack(m)" :: NatTyp ->: NatTyp

    val ackmp1n: ScalaTerm[SafeLong] = "ack(m+1)(n)" :: NatTyp

    val ack: Func[NatRing.Nat, Func[ScalaTerm[SafeLong], ScalaTerm[SafeLong]]] =
      recNNN(succ)(
        m :-> (ackm :-> recNN(ackm(Literal(1)))(
          n :-> (ackmp1n :-> (ackm(ackmp1n)))
        ))
      )

    assert(ack(Literal(2))(Literal(2)) == Literal(7))
    assert(ack(Literal(3))(Literal(1)) == Literal(13))
  }

  "Example Theorems Symbolically proved" should "have proofs with types the theorems" in {
    assert(DoubleEvenSym.pf.typ == DoubleEvenSym.thm)
    assert(SuccNOrNEvenSym.pf.typ == SuccNOrNEvenSym.thm)
    assert(LocalConstImpliesConstSym.pf.typ == LocalConstImpliesConstSym.thm)
  }

}
