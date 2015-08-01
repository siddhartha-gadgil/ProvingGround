package provingground

import HoTT._
import org.scalatest.FlatSpec

import ConstructorPattern._

import ConstructorPattern._

import RecFunction._

import BaseTypes._

class RecursionSpec extends FlatSpec{

/*
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

  val succ : Func[Term, Term] = succC.cons

  val one = succ(zero)
*/

  "Boolean type" should "have constructors of type Bool" in {
    assert(tt.typ == Bool)
    assert(ff.typ == Bool)
  }

  it should "have constructor data for recursion to X with type X" in {
    assert(W.recDom(Bool, Bool) == Bool)
    assert(W.recDom(Bool, Nat) == Nat)
  }

  val recBool = recFunction(BoolCons, Bool)

  it should "have recursion function to X with type X -> X -> Bool -> X" in {
    assert(recBool.fullTyp(Bool) == Bool ->: Bool ->: Bool ->: Bool)

    assert(recBool.fullTyp(Nat) == Nat ->: Nat ->: Bool ->: Nat)

  }

  "Recursion defintion for a case" should "when applied to constructor give defining data, and other None" in {
    val fn = W.recDef(tt, ff, (Bool ->: Bool).symbObj("dummy-function"))

    assert(fn(tt) == Some(ff))

    assert(fn(ff) == None)
  }

  it should "modify a function according to the case" in {
    val dummy = (Bool ->: Bool).symbObj("dummy")

    val negTrue = W.recModify(tt)(ff)(dummy)(dummy)

    assert(negTrue(tt) == ff)

    val neg = W.recModify(ff)(tt)(negTrue)(negTrue)

    assert(neg(tt) == ff)

    assert(neg(ff) == tt)
  }

  val boolBoolFn =
    recBool.recursion(Bool)(recBool.fullTyp(Bool).symbObj("dummy-function")).asInstanceOf[Func[Term, Func[Term, Func[Term, Term]]]]

  "Dummy Recursion function from Bool to Bool" should "when applied to constructors give defining data" in {
    val neg = boolBoolFn(ff)(tt)

    assert(neg(tt) == ff)
  }

  val recBoolBool =
    recFn(BoolCons, Bool, Bool).asInstanceOf[Func[Term, Func[Term, Func[Term, Term]]]]

  "Recursion function from Bool to Bool" should "when applied to constructors give defining data" in {
      val neg = recBoolBool(ff)(tt)

      assert(neg(tt) == ff)
    }

  val recBoolNat =
      recFn(BoolCons, Bool, Nat).asInstanceOf[Func[Term, Func[Term, Func[Term, Term]]]]

  "Recursion function from Bool to Nat" should "when applied to constructors give defining data" in {
        val neg = recBoolNat(zero)(one)

        assert(neg(tt) == zero)

        assert(neg(ff) == one)
      }
}
