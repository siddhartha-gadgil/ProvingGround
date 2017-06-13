package provingground

import HoTT._
import org.scalatest.FlatSpec
import induction._, implicits._
// import library._

class GetInductiveSpec extends InductionSpecTL {
  "Getting inductive type definition" should "work for Natural numbers" in {
    assert(ConstructorSeqTL.getExst(Nat, NatInd.intros.terms).value == NatInd)
  }

  it should "work for Lists" in {
    assert(ConstructorSeqTL.getExst(ListA, ListAInd.intros.terms).value == ListAInd)
  }

  it should "work for Booleans" in {
    assert(ConstructorSeqTL.getExst(Bool, BoolInd.intros.terms).value == BoolInd)
  }

  it should "work for simple binary trees" in {
    assert(ConstructorSeqTL.getExst(T, TInd.intros.terms).value == TInd)
  }

  it should "work for complex binary trees" in {
    assert(ConstructorSeqTL.getExst(BT, BTInd.intros.terms).value == BTInd)
  }

  "Getting indexed inductive types" should "give correct definitions" in {
    val VecInd2 = TypFamilyExst.getIndexedConstructorSeq(Vec, VecInd.intros.terms).value

    val recVN2 = VecInd2.recE(Nat)

    import Fold._

    val size2 = recVN2(zero)(n :~> (a :-> (vn :-> (m :-> (succ(m))))))

    assert(size2(one)(v1) == one)

    val indVVV2 =
      VecInd2.inducE(n :~> (vn :-> (m ~>: (Vec(m) ->: Vec(add(n)(m))))))
    val concatVn = "concat(v_n)" :: (m ~>: (Vec(m) ->: Vec(add(n)(m))))
    val vconcat2 = indVVV2(m :~> (vm :-> vm))(
      n :~>
        (a :->
          (vn :->
            (concatVn :->
              (m :~> (vm :-> vcons(add(n)(m))(a)(concatVn(m)(vm))))))))
    assert(
      vconcat2(two)(v2)(two)(v2) == vcons(three)(a1)(
        vcons(two)(a)(vcons(one)(a1)(vcons(zero)(a)(vnil)))))
  }

}
