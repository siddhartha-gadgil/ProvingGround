package provingground

import HoTT._
import org.scalatest.FlatSpec

//import ConstructorPattern._

//import RecFunction.{ recFunction }

import BaseConstructorTypes._

import scala.util.Try

//import RecursiveDefinition._

import ConstructorSeq.recFn

class RecursionSpec extends FlatSpec {

  "Boolean type" should "have constructors of type Bool" in {
    assert(tt.typ == SmallBool)
    assert(ff.typ == SmallBool)
  }

  it should "have constructor data for recursion to X with type X" in {
    assert(W.recDataTyp(SmallBool, SmallBool) == SmallBool)
    assert(W.recDataTyp(SmallBool, SmallNat) == SmallNat)
  }

  /*  val recBool = recFunction(BoolCons, SmallBool)

  it should "have recursion function to X with type X -> X -> Bool -> X" in {
    assert(recBool.fullTyp(SmallBool) == SmallBool ->: SmallBool ->: SmallBool ->: SmallBool)

    assert(recBool.fullTyp(SmallNat) == SmallNat ->: SmallNat ->: SmallBool ->: SmallNat)

  }*/

  "Recursion defintion for a case" should "when applied to constructor give defining data, and other None" in {
    val fn =
      W.recDefCase(tt, ff, (SmallBool ->: SmallBool).symbObj("dummy-function"))

    assert(fn(tt) == Some(ff))

    assert(fn(ff) == None)
  }

  // it should "modify a function according to the case" in {
  //   val dummy = (SmallBool ->: SmallBool).symbObj("dummy")
  //
  //   val negTrue = W.recModify(tt)(ff)(dummy)(dummy)
  //
  //   assert(negTrue(tt) == ff)
  //
  //   val neg = W.recModify(ff)(tt)(negTrue)(negTrue)
  //
  //   assert(neg(tt) == ff)
  //
  //   assert(neg(ff) == tt)
  // }

  val recBoolBool = recFn(BoolCons, SmallBool, SmallBool)
    .asInstanceOf[Func[Term, Func[Term, Func[Term, Term]]]]

  val neg = recBoolBool(ff)(tt)

  "Recursion function from Bool to Bool" should "when applied to constructors give defining data" in {

    assert(neg(tt) == ff)
  }

  it should "give a formal object when applied to a variable" in {
    val negTry = Try(neg("x" :: SmallBool))
    assert(!(negTry.toOption.isEmpty))
  }

  val recBoolNat = recFn(BoolCons, SmallBool, SmallNat)
    .asInstanceOf[Func[Term, Func[Term, Func[Term, Term]]]]

  "Recursion function from Bool to Nat" should "when applied to constructors give defining data" in {
    val neg = recBoolNat(zero)(one)

    assert(neg(tt) == zero)

    assert(neg(ff) == one)
  }

  import Fold._
  val recNatNat = recFn(NatCons, SmallNat, SmallNat)

  "Recursion functions from Nat to Nat" should "recursively apply the definition" in {

    val x = "x" :: SmallNat

    val y = "y" :: SmallNat

    val next = lambda(x)(lambda(y)(succ(y)))

    val nextArg =
      lambda(x)(lambda(y)(succ(succ(x)))) // this is n+1 as we map succ(n) to function of n

    val plusOne = recNatNat(one)(next)

    val alsoPlusOne = recNatNat(one)(nextArg)

    assert(plusOne(zero) == one)

    assert(plusOne(one) == succ(one))

    assert(plusOne(succ(one)) == succ(succ(one)))

    assert(alsoPlusOne(zero) == one)

    assert(alsoPlusOne(one) == succ(one))

    assert(alsoPlusOne(succ(one)) == succ(succ(one)))
  }

  def recNat[C <: Term with Subs[C]](X: Typ[C]) = recFn(NatCons, SmallNat, X)

  def recBool[C <: Term with Subs[C]](X: Typ[C]) =
    recFn(BoolCons, SmallBool, X)

  "And defined recursively" should "have correct values" in {
    val a   = "a" :: SmallBool
    val and = recBool(SmallBool ->: SmallBool)(lambda(a)(a), lambda(a)(ff))
    assert(and(tt, tt) == tt)
  }

  "Sum defined recursively" should "have correct values" in {
    val n = "n" :: SmallNat
    val k = "k" :: SmallNat
    val f = "f" :: (SmallNat ->: SmallNat)
    val add = recNat(SmallNat ->: SmallNat)(
      lambda(n)(n),
      lambda(n)(
        lambda(f)(
          lambda(k)(succ(f(k)))
        )
      )
    )

    assert(add(one, one) == succ(one))
  }
}
