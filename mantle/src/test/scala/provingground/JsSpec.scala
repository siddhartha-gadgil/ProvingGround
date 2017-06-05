package provingground

import HoTT._
import org.scalatest.FlatSpec

import translation._, interface._

import TermJson._

class  JsSpec extends FlatSpec{
  def roundTripBase(t: Term) = termToJson(t).flatMap(jsonToTermBase)

  def checkBase(t: Term) = roundTripBase(t) == Some(t)

  "Basic (without induction) serialization" should "be correct for Universe" in {
    assert(checkBase(Type))
  }

  val A = "A" :: Type

  val B = "B" :: Type

  val f = "f" :: A ->: B

  val a = "a" :: A

  it should "be correct for function types and applications" in {
    assert(checkBase(A))

    assert(checkBase(A ->: B))

    assert(checkBase(f(a)))
  }

  it should "be correct for Products and CoProducts" in {
    assert(checkBase(ProdTyp(A, B)))

    assert(checkBase(PlusTyp(A, B)))
  }

  it should "be correct for  Equalities" in {
    assert(checkBase(a =:= a))
  }

  val mp = A :~> (B :~> (a :-> (f :-> f(a))))

  it should "be correct for Modus Ponens" in {
    assert(checkBase(mp))
  }

  val Cs = "C" :: A ->: Type

  it should "be correct for Pi-Types and Sigma-Types" in {
    assert(checkBase(a ~>: Cs(a)))
    assert(checkBase(sigma(a)(Cs(a))))
  }

  it should "be correct for Unit, Zero and Star" in {
    assert(checkBase(Unit))
    assert(checkBase(Zero))
    assert(checkBase(One))
  }
}
