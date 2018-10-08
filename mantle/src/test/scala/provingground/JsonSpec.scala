package provingground

import HoTT._
import org.scalatest.FlatSpec

import translation._, interface._

import TermJson._

class JsonSpec extends FlatSpec {
  def roundTripBase(t: Term): Option[Term] = termToJson(t).flatMap(jsonToTermBase)

  def checkBase(t: Term): Boolean = roundTripBase(t).contains(t)

  "Basic (without induction) serialization" should "be correct for Universe" in {
    assert(checkBase(Type))
  }

  val A: Typ[Term] with Subs[Typ[Term]] = "A" :: Type

  val B: Typ[Term] with Subs[Typ[Term]] = "B" :: Type

  val f: Func[Term, Term] with Subs[Func[Term, Term]] = "f" :: A ->: B

  val a: Term with Subs[Term] = "a" :: A

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

  val mp
    : FuncLike[Typ[Term] with Subs[Typ[Term]], FuncLike[Typ[Term] with Subs[Typ[Term]],
                        Func[Term with Subs[Term],
                             Func[Func[Term, Term] with Subs[Func[Term, Term]],
                                  Term]]]] = A :~> (B :~> (a :-> (f :-> f(a))))

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

  it should "be correct for Prop" in {
    assert(checkBase(Prop))
  }


  import library._, Nats._, Bools._, Vecs._

  def roundTrip(t: Term): Option[Term] =
    termToJson(t).flatMap(
      jsonToTerm(Map(Nat         -> NatInd, Bool -> BoolInd).lift,
                 Map((Vec: Term) -> VecAInd).lift)
    )

  def check(t: Term): Boolean = roundTrip(t).contains(t)

  import Fold._

  "Serialization with induction" should "work correctly" in {
    val double2 = roundTrip(double).get
    assert(double2(N(1)) == N(2))

    assert(roundTrip(double).contains(double))
    assert(check(DoubleEven.pf))

    assert(check(LocalConstImpliesConst.pf))

    assert(check(SuccNOrNEven.pf))

  }
}
