package provingground

import org.scalatest.FlatSpec
import provingground.HoTT._
import induction.TLImplicits._
import shapeless._

//These tests are not unit tests in the strict sense but used to help non regression and code coverage.
//It's only a palliative to proper and comprehensive testing.

//Examples come from https://stepik.org/course/ThCS-Introduction-to-programming-with-dependent-types-in-Scala-2294/
//ThCS. Introduction to programming with dependent types in Scala by Dmytro Mitin

class ProductTypeSpec extends FlatSpec {

  val A    = "A" :: Type
  val B    = "B" :: Type
  val a    = "a" :: A
  val b    = "b" :: B
  val pair = PairTerm(a, b)

  "Built-in ProdTyp with built in first and second functions" should "retrieve element of a ProdTyp" in {
    assert(pair.typ === ProdTyp(A, B))
    assert(pair.first === a)
    assert(pair.second === b)
  }

  "Built-in ProdTyp with user defined first and second functions" should "retrieve element of a ProdTyp" in {
    val recAandBA = ProdTyp(A, B).rec(A)
    val recAandBB = ProdTyp(A, B).rec(B)
    val first     = recAandBA(a :-> (b :-> a))
    val second    = recAandBB(a :-> (b :-> b))
    assert(first(pair) === a)
    assert(second(pair) === b)

  }

  "Built-in ProdTyp with user defined id function" should "return the same pair" in {
    val recAandBAandB = ProdTyp(A, B).rec(ProdTyp(A, B))
    val id            = recAandBAandB(a :-> (b :-> PairTerm(a, b)))
    assert(id(pair) === pair)
  }

  "User defined ProdTyp with user defined first and second functions" should "retrieve element of a ProdTyp" in {
    val AandB            = "(A, B)" :: Type
    val ProdInd          = ("mkPair" ::: A ->>: (B ->>: AandB)) =: AandB
    val makePair :: HNil = ProdInd.intros
    val pair             = makePair(a)(b)
    assert(pair.typ === AandB)
    val recAandBA = ProdInd.rec(A)
    val recAandBB = ProdInd.rec(B)
    val first     = recAandBA(a :-> (b :-> a))
    val second    = recAandBB(a :-> (b :-> b))
    assert(first(pair) === a)
    assert(second(pair) === b)
  }

  "User defined ProdTyp with user defined id function" should "return the same pair" in {
    val AandB            = "(A, B)" :: Type
    val ProdInd          = ("mkPair" ::: A ->>: (B ->>: AandB)) =: AandB
    val makePair :: HNil = ProdInd.intros
    val pair             = makePair(a)(b)
    assert(pair.typ === AandB)
    val recAandBAandB = ProdInd.rec(AandB)
    val id            = recAandBAandB(a :-> (b :-> makePair(a)(b)))
    assert(id(pair) === pair)
  }

}

class CoProductTypeSpec extends FlatSpec {

  val A = "A" :: Type
  val B = "B" :: Type
  val a = "a" :: A
  val b = "b" :: B

  "User defined idXOrDefault(x) function with built-in PlusTyp" should "be defined properly" in {
    val a1 = PlusTyp(A, B).incl1(a)
    assert(a1.typ === PlusTyp(A, B))
    val b1 = PlusTyp(A, B).incl2(b)
    assert(b1.typ === PlusTyp(A, B))
    val recAorBA     = PlusTyp(A, B).rec(A)
    val recAorBB     = PlusTyp(A, B).rec(B)
    val a0           = "a0" :: A
    val b0           = "b0" :: B
    val idAOrDefault = recAorBA(a :-> a)(b :-> a0)
    val idBOrDefault = recAorBB(a :-> b0)(b :-> b)
    assert(idAOrDefault(a1) === a)
    assert(idAOrDefault(b1) === a0)
    assert(idBOrDefault(a1) === b0)
    assert(idBOrDefault(b1) === b)
  }

  "User defined id function with built-in PlusTyp" should "be defined properly" in {
    val a1 = PlusTyp(A, B).incl1(a)
    assert(a1.typ === PlusTyp(A, B))
    val b1 = PlusTyp(A, B).incl2(b)
    assert(b1.typ === PlusTyp(A, B))
    val recAorBAorB = PlusTyp(A, B).rec(PlusTyp(A, B))
    val id =
      recAorBAorB(a :-> PlusTyp(A, B).incl1(a))(b :-> PlusTyp(A, B).incl2(b))
    assert(id(a1) === a1)
    assert(id(b1) === b1)
  }

  "User defined idOrDefault function with user defined PlusTyp" should "be defined properly" in {
    val AorB               = "A + B" :: Type
    val SumInd             = ("inl" ::: A ->>: AorB) |: ("inr" ::: B ->>: AorB) =: AorB
    val inl :: inr :: HNil = SumInd.intros
    val a1                 = inl(a)
    assert(a1.typ === AorB)
    val b1 = inr(b)
    assert(b1.typ === AorB)
    val recAorBA     = SumInd.rec(A)
    val recAorBB     = SumInd.rec(B)
    val a0           = "a0" :: A
    val b0           = "b0" :: B
    val idAOrDefault = recAorBA(a :-> a)(b :-> a0)
    val idBOrDefault = recAorBB(a :-> b0)(b :-> b)
    assert(idAOrDefault(a1) === a)
    assert(idAOrDefault(b1) === a0)
    assert(idBOrDefault(a1) === b0)
    assert(idBOrDefault(b1) === b)
  }

  "User defined id function with user defined PlusTyp" should "be defined properly" in {
    val AorB               = "A + B" :: Type
    val SumInd             = ("inl" ::: A ->>: AorB) |: ("inr" ::: B ->>: AorB) =: AorB
    val inl :: inr :: HNil = SumInd.intros
    val a1                 = inl(a)
    assert(a1.typ === AorB)
    val b1 = inr(b)
    assert(b1.typ === AorB)
    val recAorBAorB = SumInd.rec(AorB)
    val id          = recAorBAorB(a :-> inl(a))(b :-> inr(b))
    assert(id(a1) === a1)
    assert(id(b1) === b1)
  }

}

class FunctionTypeSpec extends FlatSpec {

  val A = "A" :: Type
  val a = "a" :: A
  val B = "B" :: Type
  val b = "b" :: B

  val f = "f" :: A ->: B

  import scalahott.ScalaRep._
  import NatInt.rep

  "Simple lambda function with built-in function type" should "be defined properly" in {
    val h = ((x: Int) => x * x).term
    assert(h.typ === NatInt ->: NatInt)
    assert(h(2.term) === 4.term)
  }

  "Simple user defined function with user defined function type" should "have proper tyoe" in {
    val AtoB          = "A → B" :: Type
    val AtoBInd       = ("λ" ::: (A ->: B) ->>: AtoB) =: AtoB
    val lmbda :: HNil = AtoBInd.intros
    // assert(lmbda(f).typ === f.typ)
    val recFunAB = AtoBInd.rec(A ->: B)
    val call     = recFunAB(f :-> (a :-> f(a)))
    assert(call.typ === AtoB ->: A ->: B)
    val g = "g" :: AtoB
    assert(call(g)(a).typ === B)
  }

  "Simple user defined function with user defined emulated type family" should "have proper type" in {
    val Fun           = "Fun" :: Type ->: Type ->: Type
    val FunInd        = ("λ" ::: A ~>>: (B ~>>: ((A ->: B) ->>: (Fun -> Fun(A)(B))))) =:: Fun
    val lmbda :: HNil = FunInd.intros
    assert(lmbda(A)(B)(f).typ === Fun(A)(B))
  }

  "Simple user defined function with user defined emulated type family" should "be defined properly" in {
    val Fun           = "Fun" :: Type ->: Type ->: Type
    val FunInd        = ("λ" ::: A ~>>: (B ~>>: ((A ->: B) ->>: (Fun -> Fun(A)(B))))) =:: Fun
    val lmbda :: HNil = FunInd.intros

    val g        = "g" :: Fun(A)(B)
    val indFunAB = FunInd.induc(A :~> (B :~> (g :-> (A ->: B))))
    val call     = indFunAB(A :~> (B :~> (f :-> (a :-> f(a)))))
    assert(call.typ === A ~>: (B ~>: (Fun(A)(B) ->: A ->: B)))

    assert(call(A)(B)(g)(a).typ === B)

    val square = lmbda(NatInt)(NatInt)(
      ((x: Int) => x * x).term.asInstanceOf[Func[Term, Term]])
    assert(square.typ === Fun(NatInt)(NatInt))
    assert(call(NatInt)(NatInt)(square)(2.term) === 4.term)
  }

}

class SigmaTypeSpec extends FlatSpec {
  val A    = "A" :: Type
  val B    = "B(_ : A)" :: A ->: Type
  val a    = "a" :: A
  val b    = "b" :: B(a)
  val pair = mkPair(a, b)

  "Build-in first and second method with built-in Sigma type" should "have proper type" in {
    assert(pair.typ === Sgma(a !: A, B(a)))
    assert(pair.first.typ === A)
    assert(pair.second.typ === B(a))
  }

  "User defined first and second method with built-in Sigma type" should "have proper type" in {
    val recSABA = Sgma(a !: A, B(a)).rec(A)
    val first   = recSABA(a :~> (b :-> a))
    val indSABB = Sgma(a !: A, B(a)).induc(a :~> (b :-> B(a)))
    val second  = indSABB(a :~> (b :-> b))
    assert(first(pair) === a)
    assert(second(pair).typ === B(a))
  }

  "User defined id function with built-in Sigma type" should "be defined properly" in {
    val recSABSAB = Sgma(a !: A, B(a)).rec(Sgma(a !: A, B(a)))
    val id =
      recSABSAB(a :~> (b :-> mkPair(a, b).asInstanceOf[AbsPair[Term, Term]]))
    assert(id(pair) === pair)
  }

  "User defined id with user defined Sigma type" should "be defined properly" in {
    val SigmaAB          = "Sigma(a : A, B(a))" :: Type
    val SigmaInd         = ("mkPair" ::: a ~>>: (B(a) ->>: SigmaAB)) =: SigmaAB
    val makePair :: HNil = SigmaInd.intros
    val pair             = makePair(a)(b)
    assert(pair.typ === SigmaAB)
    val recSABA = SigmaInd.rec(A)
    val first   = recSABA(a :~> (b :-> a))
    assert(first(pair) === a)
    val recSABSAB = SigmaInd.rec(SigmaAB)
    val id        = recSABSAB(a :~> (b :-> makePair(a)(b)))
    assert(id(pair) === pair)
  }

  "For arbitrary type user defined id with user defined Sigma type" should "be defined properly" in {
    val Sigma = "Σ" :: A ~>: ((A ->: Type) ->: Type)
    val SigmaInd = ("mkPair" ::: A ~>>: (B ~>>: (a ~>>: (B(a) ->>: (Sigma -> Sigma(
      A)(B)))))) =:: Sigma
    val makePair :: HNil = SigmaInd.intros
    val pair             = makePair(A)(B)(a)(b)
    assert(pair.typ === Sigma(A)(B))

    val p = "(a, b)" :: Sigma(A)(B)

    val indSABA = SigmaInd.induc(A :~> (B :~> (p :-> A)))
    val first   = indSABA(A :~> (B :~> (a :~> (b :-> a))))

    val indSABB = SigmaInd.induc(A :~> (B :~> (p :-> B(first(A)(B)(p)))))
    val second  = indSABB(A :~> (B :~> (a :~> (b :-> b))))

    assert(first(A)(B)(pair) === a)
    assert(second(A)(B)(pair) === b)

    val indSABSAB = SigmaInd.induc(A :~> (B :~> (p :-> Sigma(A)(B))))
    val id        = indSABSAB(A :~> (B :~> (a :~> (b :-> makePair(A)(B)(a)(b)))))

    assert(id(A)(B)(pair) === pair)
  }

}

class PiTypeSpec extends FlatSpec {
  val A = "A" :: Type
  val B = "B(_ : A)" :: A ->: Type
  val a = "a" :: A
  val f = "f" :: a ~>: B(a)

  val id = A :~> (a :-> a)

  "f(a)" should "have proper type" in {
    assert(f(a).typ === B(a))
  }

  "id function" should "have proper type" in {
    assert(id.typ === A ~>: (A ->: A))
  }

  "simple user defined function with user define PI type" should "be defined properly" in {
    val AtoB           = "Π(a:A) B(a)" :: Type
    val AtoBInd        = ("λ" ::: (a ~>: B(a)) ->>: AtoB) =: AtoB
    val lambda :: HNil = AtoBInd.intros
    assert(lambda(f).typ === AtoB)
    val g           = "g" :: AtoB
    val recDepFunAB = AtoBInd.rec(a ~>: B(a))
    val call        = recDepFunAB(f :-> (a :~> f(a)))
    assert(call.typ === AtoB ->: (a ~>: B(a)))
    assert(call(g)(a).typ === B(a))
    assert(call(lambda(f))(a) === f(a))
  }

  "For arbitrary type simple user defined function with user defined PI type" should "be defined properly" in {
    val Pi             = "Π" :: A ~>: ((A ->: Type) ->: Type)
    val PiInd          = ("λ" ::: A ~>>: (B ~>>: ((a ~>: B(a)) ->>: (Pi -> Pi(A)(B))))) =:: Pi
    val lambda :: HNil = PiInd.intros
    assert(lambda(A)(B)(f).typ === Pi(A)(B))

    val g           = "g" :: Pi(A)(B)
    val indDepFunAB = PiInd.induc(A :~> (B :~> (g :-> (a ~>: B(a)))))
    val call        = indDepFunAB(A :~> (B :~> (f :-> (a :~> f(a)))))
    assert(call.typ === A ~>: (B ~>: (Pi(A)(B) ->: (a ~>: B(a)))))
    assert(call(A)(B)(g)(a).typ === B(a))
    assert(call(A)(B)(lambda(A)(B)(f))(a) === f(a))
  }

}

class EmptyAndUnitTypeSpec extends FlatSpec {
  Zero
  val A = "A" :: Type
  val a = "a" :: A

  val recZA = Zero.rec(A)
  val z     = "z" :: Zero
  assert(recZA(z).typ === A)

  assert(Star.typ === Unit)
  val recUA = Unit.rec(A)
  assert(recUA(a)(Star) === a)
}

class BooleanTypeSpec extends FlatSpec {
  val Bool               = "Boolean" :: Type
  val b                  = "b" :: Bool
  val BoolInd            = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
  val tru :: fls :: HNil = BoolInd.intros

  "custom negation function" should "be defined properly" in {
    val recBB = BoolInd.rec(Bool)
    val not   = recBB(fls)(tru)
    assert(not(tru) === fls)
    assert(not(fls) === tru)
  }

  "custom conjunction function" should "be defined properly" in {
    val recBBB = BoolInd.rec(Bool ->: Bool)
    val and    = recBBB(b :-> b)(b :-> fls)
    assert(and(tru)(fls) === fls)
    assert(and(tru)(tru) === tru)
  }
}

class NaturalNumbersTypeSpec extends FlatSpec {
  val Nat                  = "Nat" :: Type
  val NatInd               = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
  val zero :: succ :: HNil = NatInd.intros
  val n                    = "n" :: Nat
  val m                    = "m" :: Nat
  val one                  = succ(zero)
  val two                  = succ(one)
  val three                = succ(two)
  val four                 = succ(three)
  val five                 = succ(four)
  val recNN                = NatInd.rec(Nat)

  "custom double function" should "be defined properly" in {
    val double = recNN(zero)(n :-> (m :-> succ(succ(m))))
    assert(double(one) === two)
    assert(double(two) === four)
  }

  "custom addition function" should "be defined properly" in {
    val recNNN = NatInd.rec(Nat ->: Nat)
    val addn   = "add(n)" :: Nat ->: Nat
    val add    = recNNN(m :-> m)(n :-> (addn :-> (m :-> succ(addn(m)))))
    assert(add(one)(two) === three)
    assert(add(two)(three) === five)
  }
}

class ListTypeSpec extends FlatSpec {
  "custom size list function" should "be defined properly" in {
    val Nat                  = "Nat" :: Type
    val NatInd               = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
    val zero :: succ :: HNil = NatInd.intros
    val one                  = succ(zero)
    val two                  = succ(one)
    val three                = succ(two)
    val A                    = "A" :: Type
    val ListA                = "List(A)" :: Type
    val ListAInd             = ("nil" ::: ListA) |: ("cons" ::: A ->>: ListA -->>: ListA) =: ListA
    val nil :: cons :: HNil  = ListAInd.intros
    val recLN                = ListAInd.rec(Nat)
    val a                    = "a" :: A
    val as                   = "as" :: ListA
    val n                    = "n" :: Nat
    val size                 = recLN(zero)(a :-> (as :-> (n :-> succ(n))))
    val a1                   = "a1" :: A
    val a2                   = "a2" :: A
    val list                 = cons(a)(cons(a1)(cons(a2)(nil)))
    assert(size(list) === three)
  }
}

class VectorOfFixLengthTypeSpec extends FlatSpec {
  "custom vector of fixed length" should "be defined properly" in {
    val Nat                  = "Nat" :: Type
    val NatInd               = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
    val zero :: succ :: HNil = NatInd.intros
    val one                  = succ(zero)
    val two                  = succ(one)
    val three                = succ(two)
    val n                    = "n" :: Nat
    val m                    = "m" :: Nat
    val A                    = "A" :: Type
    val Vec                  = "Vec" :: Nat ->: Type
    val VecInd = ("nil" ::: (Vec -> Vec(zero))) |: {
      "cons" ::: n ~>>: (A ->>: (Vec :> Vec(n)) -->>: (Vec -> Vec(succ(n))))
    } =:: Vec
    val vnil :: vcons :: HNil = VecInd.intros
    val recVN                 = VecInd.rec(Nat)

    val a     = "a" :: A
    val vn    = "v_n" :: Vec(n)
    val vsize = recVN(zero)(n :~> (a :-> (vn :-> (m :-> succ(m)))))
    val a1    = "a1" :: A
    val a2    = "a2" :: A
    val vect  = vcons(two)(a)(vcons(one)(a1)(vcons(zero)(a2)(vnil)))
    assert(vsize(three)(vect) == three)
  }
}

class IdentityTypeSpec extends FlatSpec {

  val A = "A" :: Type
  val a = "a" :: A

  "associativity of addition of natural numbers" should "be proved" in {
    val Nat                  = "Nat" :: Type
    val NatInd               = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
    val zero :: succ :: HNil = NatInd.intros
    val n                    = "n" :: Nat
    val m                    = "m" :: Nat
    val k                    = "k" :: Nat
    val recNNN               = NatInd.rec(Nat ->: Nat)
    val addn                 = "add(n)" :: Nat ->: Nat
    val add                  = recNNN(m :-> m)(n :-> (addn :-> (m :-> succ(addn(m)))))

    val one   = succ(zero)
    val two   = succ(one)
    val three = succ(two)

    val indN_assoc = NatInd.induc(
      n :-> (m ~>: (k ~>: (add(add(n)(m))(k) =:= add(n)(add(m)(k))))))
    val pf = "(n+m)+k=n+(m+k)" :: m ~>: (k ~>: (add(add(n)(m))(k) =:= add(n)(
      add(m)(k))))
    val assoc = indN_assoc(m :~> (k :~> add(m)(k).refl))(
      n :~> (pf :-> (m :~> (k :~>
        IdentityTyp
          .induced(succ)(add(add(n)(m))(k))(add(n)(add(m)(k)))(pf(m)(k))))))
    assert(
      assoc.typ === (n ~>: (m ~>: (k ~>: (add(add(n)(m))(k) =:= add(n)(
        add(m)(k)))))))
  }

  "refl(a)" should "have proper type" in {
    val IdA          = "Id(A)" :: A ->: A ->: Type
    val IdAInd       = ("refl" ::: a ~>>: (IdA -> IdA(a)(a))) =:: IdA
    val refl :: HNil = IdAInd.intros
    assert(refl(a).typ === IdA(a)(a))
  }

  "refl(A)(a)" should "have proper type" in {

    val Id           = "Id" :: A ~>: (A ->: A ->: Type)
    val IdInd        = ("refl" ::: A ~>>: (a ~>>: (Id -> Id(A)(a)(a)))) =:: Id
    val refl :: HNil = IdInd.intros
    assert(refl(A)(a).typ === Id(A)(a)(a))

  }
}

class EliminatorsSpec extends FlatSpec {
  val A  = "A" :: Type
  val a  = "a" :: A
  val a1 = "a1" :: A
  val a2 = "a2" :: A
  val B  = "B" :: Type
  val b  = "b" :: B
  val Ba = "B(_ : A)" :: A ->: Type
  val ba = "ba" :: Ba(a)
  val C  = "C" :: Type
  val c  = "c" :: C
  val c1 = "c1" :: C
  val c2 = "c2" :: C

  "Empty type" should "have proper type" in {
    assert(Zero.typ === Type)

    val recZC = Zero.rec(C)
    assert(recZC.typ === Zero ->: C)
    val z = "z" :: Zero
    assert(recZC(z).typ === C)

    val D = "D(_ : Zero)" :: Zero ->: Type
    //casting is not needed anymore
    //val indZD = Zero.induc(D.asInstanceOf[Func[AtomicTerm, Typ[Term]]]) !: z ~>: D(z)
    val indZD = Zero.induc(D)
    assert(indZD(z).typ === D(z))
  }

  "Unit type" should "be defined property" in {
    assert(Unit.typ === Type)
    assert(Star.typ === Unit)

    val recUC = Unit.rec(C)
    assert(recUC.typ === C ->: Unit ->: C)
    assert(recUC(c)(Star) === c)
    val D     = "D(_ : Unit)" :: Unit ->: Type
    val u     = "u" :: Unit
    val indUD = Unit.induc(D)
    assert(indUD.typ === D(Star) ->: u ~>: D(u))
    val d = "d" :: D(Star)
    assert(indUD(d)(Star) === d)
  }

  "User-defined Unit type" should "be defined property" in {
    val Unit         = "Unit" :: Type
    val UnitInd      = ("star" ::: Unit) =: Unit
    val star :: HNil = UnitInd.intros

    val recUC = UnitInd.rec(C)
    assert(recUC.typ === C ->: Unit ->: C)

    assert(recUC(c)(star) === c)

    val D     = "D(_ : Unit)" :: Unit ->: Type
    val u     = "u" :: Unit
    val indUD = UnitInd.induc(D)
    assert(indUD.typ === D(star) ->: u ~>: D(u))
    val d = "d" :: D(star)
    assert(indUD(d)(star) === d)
  }

  "Sum type" should "be defined property" in {
    PlusTyp(A, B) !: Type
    val s1 = PlusTyp(A, B).incl1(a)
    assert(s1.typ === PlusTyp(A, B))
    val s2 = PlusTyp(A, B).incl2(b)
    assert(s1.typ === PlusTyp(A, B))

    val recAorBC = PlusTyp(A, B).rec(C)
    assert(recAorBC.typ === (A ->: C) ->: (B ->: C) ->: PlusTyp(A, B) ->: C)
    val f = "f" :: A ->: C
    val g = "g" :: B ->: C
    assert(recAorBC(f)(g)(s1) === f(a))
    assert(recAorBC(f)(g)(s2) === g(b))

    val D        = "D(_ : A + B)" :: PlusTyp(A, B) ->: Type
    val s        = "s" :: PlusTyp(A, B)
    val indAorBD = PlusTyp(A, B).induc(D)
    assert(
      indAorBD.typ === (a ~>: D(PlusTyp(A, B).incl1(a))) ->: (b ~>: D(
        PlusTyp(A, B).incl2(b))) ->: s ~>: D(s))
    val f2 = "f2" :: a ~>: D(PlusTyp(A, B).incl1(a))
    val g2 = "g2" :: b ~>: D(PlusTyp(A, B).incl2(b))
    assert(indAorBD(f2)(g2)(s1) === f2(a))
    assert(indAorBD(f2)(g2)(s2) === g2(b))

  }
  "User-defined Sum type" should "be defined property" in {
    val AorB               = "A + B" :: Type
    val SumInd             = ("inl" ::: A ->>: AorB) |: ("inr" ::: B ->>: AorB) =: AorB
    val inl :: inr :: HNil = SumInd.intros
    val s1                 = inl(a)
    assert(s1.typ === AorB)
    val s2 = inr(b)
    assert(s2.typ === AorB)

    val recAorBC = SumInd.rec(C)
    assert(recAorBC.typ === (A ->: C) ->: (B ->: C) ->: AorB ->: C)
    val f = "f" :: A ->: C
    val g = "g" :: B ->: C
    assert(recAorBC(f)(g)(s1) === f(a))
    assert(recAorBC(f)(g)(s2) === g(b))

    val D        = "D(_ : A + B)" :: AorB ->: Type
    val s        = "s" :: AorB
    val indAorBD = SumInd.induc(D)
    assert(
      indAorBD.typ === (a ~>: D(inl(a))) ->: (b ~>: D(inr(b))) ->: s ~>: D(s))
    val f2 = "f2" :: a ~>: D(inl(a))
    val g2 = "g2" :: b ~>: D(inr(b))
    assert(indAorBD(f2)(g2)(s1) === f2(a))
    assert(indAorBD(f2)(g2)(s2) === g2(b))
  }

  "Product type" should "be defined property" in {
    assert(ProdTyp(A, B).typ === Type)
    val pair = PairTerm(a, b)
    assert(pair.typ === ProdTyp(A, B))

    val recAandBC = ProdTyp(A, B).rec(C)
    assert(recAandBC.typ === (A ->: B ->: C) ->: ProdTyp(A, B) ->: C)
    val f = "f" :: A ->: B ->: C
    assert(recAandBC(f)(pair) === f(a)(b))

    val D         = "D(_ : A x B)" :: A ->: B ->: Type
    val p         = "(a, b)" :: ProdTyp(A, B)
    val indAandBD = ProdTyp(A, B).induc(D)
    assert(
      indAandBD.typ === (a ~>: b ~>: D(a)(b)) ->: p ~>: D(p.first)(p.second))
    val f2 = "f2" :: a ~>: b ~>: D(a)(b)
    assert(indAandBD(f2)(pair) === f2(a)(b))
  }

  "User-defined Product type" should "be defined property" in {
    val AandB            = "A x B" :: Type
    val ProdInd          = ("mkPair" ::: A ->>: (B ->>: AandB)) =: AandB
    val makePair :: HNil = ProdInd.intros
    val pair             = makePair(a)(b)
    assert(pair.typ === AandB)

    val recAandBC = ProdInd.rec(C)
    assert(recAandBC.typ === (A ->: B ->: C) ->: AandB ->: C)
    val f = "f" :: A ->: B ->: C
    assert(recAandBC(f)(pair) === f(a)(b))

    val D         = "D(_ : A x B)" :: AandB ->: Type
    val p         = "(a, b)" :: AandB
    val indAandBD = ProdInd.induc(D)
    assert(indAandBD.typ === (a ~>: b ~>: D(makePair(a)(b))) ->: p ~>: D(p))
    val f2 = "f2" :: a ~>: b ~>: D(makePair(a)(b))
    assert(indAandBD(f2)(pair) === f2(a)(b))
  }

  "User-defined Function typ" should "be defined property" in {
    val AtoB          = "A → B" :: Type
    val FunInd        = ("λ" ::: (A ->: B) ->>: AtoB) =: AtoB
    val lmbda :: HNil = FunInd.intros
    val f             = "f" :: A ->: B
    val fun           = lmbda(f)
    assert(fun.typ === AtoB)

    val recFunC = FunInd.rec(C)
    assert(recFunC.typ === ((A ->: B) ->: C) ->: AtoB ->: C)
    val F = "F" :: (A ->: B) ->: C
    assert(recFunC(F)(fun) === F(f))

    val g       = "g" :: AtoB
    val D       = "D(_ : A → B)" :: AtoB ->: Type
    val indFunD = FunInd.induc(D)
    assert(indFunD.typ === (f ~>: D(lmbda(f))) ->: g ~>: D(g))
    val F2 = "F2" :: f ~>: D(lmbda(f))
    assert(indFunD(F2)(fun) === F2(f))
  }

  "Sigma-type" should "be defined property" in {
    assert(Sgma(a !: A, Ba(a)).typ === Type)
    val pair = mkPair(a, ba) !: Sgma(a !: A, Ba(a))

    val recSgmC = Sgma(a !: A, Ba(a)).rec(C)
    assert(recSgmC.typ === (a ~>: (Ba(a) ->: C)) ->: Sgma(a !: A, Ba(a)) ->: C)
    val f = "f" :: a ~>: (Ba(a) ->: C)
    assert(recSgmC(f)(pair) === f(a)(ba))

    val D       = "D(_ : Σ(a : A, B(a)))" :: a ~>: (Ba(a) ->: Type)
    val p       = "(a, ba)" :: Sgma(a !: A, Ba(a))
    val indSgmD = Sgma(a !: A, Ba(a)).induc(D)
    assert(indSgmD.typ ===  (a ~>: ba ~>: D(a)(ba) ) ->: p ~>: D(p.first)(p.second))
    val f2 = "f2" :: a ~>: ba ~>: D(a)(ba)
    assert(indSgmD(f2)(pair) === f2(a)(ba))
  }

  "User-defined Sigma-type" should "be defined property" in {
    val SigmaAB          = "Σ(a : A, B(a))" :: Type
    val SigmaInd         = ("mkPair" ::: a ~>>: (Ba(a) ->>: SigmaAB)) =: SigmaAB
    val makePair :: HNil = SigmaInd.intros
    val pair             = makePair(a)(ba)
    assert(pair.typ === SigmaAB)
    val recSgmC = SigmaInd.rec(C)
    assert(recSgmC.typ === (a ~>: (Ba(a) ->: C)) ->: SigmaAB ->: C)
    val f = "f" :: a ~>: (Ba(a) ->: C)
    assert(recSgmC(f)(pair) === f(a)(ba))

    val D       = "D(_ : Σ(a : A, B(a)))" :: SigmaAB ->: Type
    val p       = "(a, ba)" :: SigmaAB
    val indSgmD = SigmaInd.induc(D)
    assert(indSgmD.typ === (a ~>: ba ~>: D(makePair(a)(ba))) ->: p ~>: D(p))
    val f2 = "f2" :: a ~>: ba ~>: D(makePair(a)(ba))
    assert(indSgmD(f2)(pair) === f2(a)(ba))
  }

  "User-defined Pi-type" should "be defined property" in {
    val AtoB           = "Π(a : A, B(a))" :: Type
    val FunInd         = ("λ" ::: (a ~>: Ba(a)) ->>: AtoB) =: AtoB
    val lambda :: HNil = FunInd.intros
    val f              = "f" :: a ~>: Ba(a)
    val fun            = lambda(f)
    assert(fun.typ === AtoB)

    val recFunC = FunInd.rec(C)
    assert(recFunC.typ === ((a ~>: Ba(a)) ->: C) ->: AtoB ->: C)
    val F = "F" :: (a ~>: Ba(a)) ->: C
    recFunC(F)(fun) == F(f)

    val g       = "g" :: AtoB
    val D       = "D(_ : Π(a : A, B(a)))" :: AtoB ->: Type
    val indFunD = FunInd.induc(D)
    assert(indFunD.typ === (f ~>: D(lambda(f))) ->: g ~>: D(g))
    val F2 = "F2" :: f ~>: D(lambda(f))
    assert(indFunD(F2)(fun) === F2(f))
  }

  "Boolean" should "be defined property" in {
    val Bool               = "Boolean" :: Type
    val BoolInd            = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
    val tru :: fls :: HNil = BoolInd.intros

    val recBC = BoolInd.rec(C)
    assert(recBC.typ === C ->: C ->: Bool ->: C)
    assert(recBC(c1)(c2)(tru) === c1)
    assert(recBC(c1)(c2)(fls) === c2)

    val D     = "D(_ : Boolean)" :: Bool ->: Type
    val bool  = "b" :: Bool
    val indBC = BoolInd.induc(D)
    assert(indBC.typ === D(tru) ->: D(fls) ->: bool ~>: D(bool))
    val d1 = "d1" :: D(tru)
    val d2 = "d2" :: D(fls)
    assert(indBC(d1)(d2)(tru) === d1)
    assert(indBC(d1)(d2)(fls) === d2)
  }

  "Natural numbers" should "be defined property" in {
    val Nat                  = "Nat" :: Type
    val NatInd               = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
    val zero :: succ :: HNil = NatInd.intros
    val n                    = "n" :: Nat
    val m                    = "m" :: Nat

    val recNC = NatInd.rec(C)
    assert(recNC.typ === C ->: (Nat ->: C ->: C) ->: Nat ->: C)
    val f = "f" :: Nat ->: C ->: C
    assert(recNC(c)(f)(zero) === c)
    assert(recNC(c)(f)(succ(n)) === f(n)(recNC(c)(f)(n)))

    val D     = "D(_ : Nat)" :: Nat ->: Type
    val indND = NatInd.induc(D)
    assert(indND.typ === D(zero) ->: n ~>: (D(n) ->: D(succ(n))) ->: m ~>: D(m))
    val d  = "d" :: D(zero)
    val f2 = "f2" :: n ~>: (D(n) ->: D(succ(n)))
    assert(indND(d)(f2)(zero) === d)
    assert(indND(d)(f2)(succ(n)) === f2(n)(indND(d)(f2)(n)))
  }

  "List" should "be defined property" in {
    val ListA               = "List(A)" :: Type
    val ListAInd            = ("nil" ::: ListA) |: ("cons" ::: A ->>: ListA -->>: ListA) =: ListA
    val nil :: cons :: HNil = ListAInd.intros
    val as                  = "as" :: ListA
    val as1                 = "as1" :: ListA

    val recLC = ListAInd.rec(C)
    assert(recLC.typ === C ->: (A ->: ListA ->: C ->: C) ->: ListA ->: C)
    val f = "f" :: A ->: ListA ->: C ->: C
    assert(recLC(c)(f)(nil) === c)
    assert(recLC(c)(f)(cons(a)(as)) === f(a)(as)(recLC(c)(f)(as)))

    val D     = "D(_ : List(A))" :: ListA ->: Type
    val indLD = ListAInd.induc(D)
    assert(
      indLD.typ === D(nil) ->: a ~>: as ~>: (D(as) ->: D(cons(a)(as))) ->: as1 ~>: D(
        as1))
    val f2 = "f2" :: a ~>: as ~>: (D(as) ->: D(cons(a)(as)))
    val d  = "d" :: D(nil)
    assert(indLD(d)(f2)(nil) === d)
    assert(indLD(d)(f2)(cons(a)(as)) === f2(a)(as)(indLD(d)(f2)(as)))
  }

  "Vector" should "be defined property" in {
    val Nat                  = "Nat" :: Type
    val NatInd               = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
    val zero :: succ :: HNil = NatInd.intros
    val n                    = "n" :: Nat
    val m                    = "m" :: Nat
    val Vec                  = "Vec" :: Nat ->: Type
    val VecInd = ("nil" ::: (Vec -> Vec(zero))) |: {
      "cons" ::: n ~>>: (A ->>: (Vec :> Vec(n)) -->>: (Vec -> Vec(succ(n))))
    } =:: Vec
    val vnil :: vcons :: HNil = VecInd.intros
    val vn                    = "v_n" :: Vec(n)
    val vm                    = "v_m" :: Vec(m)

    val recVC = VecInd.rec(C)
    assert(recVC.typ === C ->: n ~>: (A ->: Vec(n) ->: C ->: C) ->: m ~>: (Vec(
      m) ->: C))
    val f = "f" :: n ~>: (A ->: Vec(n) ->: C ->: C)
    assert(recVC(c)(f)(zero)(vnil) === c)
    assert(
      recVC(c)(f)(succ(n))(vcons(n)(a)(vn)) === f(n)(a)(vn)(recVC(c)(f)(n)(vn)))

    val D     = "D(_ : Vec(_))" :: n ~>: (Vec(n) ->: Type)
    val indVD = VecInd.induc(D)
    assert(
      indVD.typ === D(zero)(vnil) ->: n ~>: a ~>: vn ~>: (D(n)(vn) ->: D(
        succ(n))(vcons(n)(a)(vn))) ->: m ~>: vm ~>: D(m)(vm))
    val f2 = "f2" :: n ~>: a ~>: vn ~>: (D(n)(vn) ->: D(succ(n))(
      vcons(n)(a)(vn)))
    val d = "d" :: D(zero)(vnil)
    assert(indVD(d)(f2)(zero)(vnil) === d)
    assert(
      indVD(d)(f2)(succ(n))(vcons(n)(a)(vn)) === f2(n)(a)(vn)(
        indVD(d)(f2)(n)(vn)))
  }

  "Identity type" should "be defined property" in {
    assert((a1 =:= a2).typ === Type)
    assert(a.refl.typ === (a =:= a))

    val recIdC = IdentityTyp.rec(A, C)
    assert(recIdC.typ === (A ->: C) ->: a1 ~>: a2 ~>: ((a1 =:= a2) ->: C))
    val f = "f" :: A ->: C
    recIdC(f)(a)(a)(a.refl) == f(a) // false

    val D  = "D(_ : a1 = a2)" :: a1 ~>: (a2 ~>: ((a1 =:= a2) ->: Type))
    val pf = "pf" :: (a1 =:= a2)
    //  val indIdD = IdentityTyp.induc(A, D) // doesn't compile
    //      !: (a ~>: D(a)(a)(a.refl)) ->: a1 ~>: a2 ~>: (pf ~>: D(a1)(a2)(pf))
    //  val f = "f" :: A ->: C
    //  indIdD(f)(a)(a)(a.refl) == f(a)
  }

  "User-defined Identity type" should "be defined property" in {
    val IdA          = "Id(A)" :: A ->: A ->: Type
    val IdAInd       = ("refl" ::: a ~>>: (IdA -> IdA(a)(a))) =:: IdA
    val refl :: HNil = IdAInd.intros
    assert(refl(a).typ === IdA(a)(a))

    val recIdC = IdAInd.rec(C)
    assert(recIdC.typ === (A ->: C) ->: a1 ~>: a2 ~>: (IdA(a1)(a2) ->: C))
    val f = "f" :: A ->: C
    assert(recIdC(f)(a)(a)(refl(a)) === f(a))

    val D      = "D(_ : a1 = a2)" :: a1 ~>: a2 ~>: (IdA(a1)(a2) ->: Type)
    val pf     = "a1 = a2" :: IdA(a1)(a2)
    val indIdD = IdAInd.induc(D)
    assert(
      indIdD.typ === a ~>: D(a)(a)(refl(a)) ->: a1 ~>: a2 ~>: pf ~>: D(a1)(a2)(
        pf))
    val f2 = "f2" :: a ~>: D(a)(a)(refl(a))
    assert(indIdD(f2)(a)(a)(refl(a)) === f2(a))
  }
}
