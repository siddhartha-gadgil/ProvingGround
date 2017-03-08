package provingground

import HoTT._
import org.scalatest.FlatSpec
import TLImplicits._
// import Fold._

import shapeless._

/* Cloned and modified from Tomoaki's tests
 */
class InductionSpecTL extends FlatSpec {

  // inductive types

  // Example: Boolean

  val Bool = "Boolean" :: Type
  val BoolInd = "true" ::: Bool |: "false" ::: Bool =: Bool

  val tt :: ff :: HNil = BoolInd.intros
  val recBoolBool = BoolInd.rec(Bool)

  "Boolean type" should "have constructors of type Bool" in {
    assert(tt.typ == Bool)
    assert(ff.typ == Bool)
  }

  "recBoolBool" should "be function with type Bool -> Bool -> Bool -> Bool" in {
    assert(recBoolBool.typ == Bool ->: Bool ->: Bool ->: Bool)
  }

  val recBBB = BoolInd.rec(Bool ->: Bool)

  "recBBB" should "be function with the properly defined type" in {
    assert(recBBB.typ == (Bool ->: Bool) ->: (Bool ->: Bool) ->:
        (Bool ->: Bool ->: Bool))
  }

  val not = recBoolBool(ff)(tt)

  "Recursion function" should "when applied to constructors give defining data" in {
    assert(not(ff) == tt && not(tt) == ff)
  }

  val b = "b" :: Bool
  val and = recBBB(lmbda(b)(b))(lmbda(b)(ff))

  "Recursion function recBBB from Bool to Bool" should "when applied to constructors give defining data" in {
    assert(and(tt)(tt) == tt && and(tt)(ff) == ff && and(ff)(tt) == ff &&
        and(ff)(ff) == ff)
  }

  // Example: natural numbers

  val Nat = "Nat" :: Type
  val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
  val zero :: succ :: HNil = NatInd.intros

  val one = succ(zero)
  val two = succ(one)
  val three = succ(two)
  val four = succ(three)
  val five = succ(four)
  val six = succ(five)
  val seven = succ(six)
  val eight = succ(seven)
  val nine = succ(eight)
  val ten = succ(nine)

  val n = "n" :: Nat
  val recNatBool = NatInd.rec(Bool)
  val isEven = recNatBool(tt)(n :-> (b :-> not(b)))

  "Recursion function isEven from Nat to Bool" should "be defined properly" in {
    assert(isEven(zero) == tt && isEven(one) == ff && isEven(two) == tt &&
        isEven(three) == ff)
  }

  val recNNN = NatInd.rec(Nat ->: Nat)

  "recNNN" should "have the propely defined type" in {
    assert(recNNN.typ == (Nat ->: Nat) ->:
        (Nat ->: (Nat ->: Nat) ->: (Nat ->: Nat)) ->: (Nat ->: (Nat ->: Nat)))
  }

  val m = "m" :: Nat
  val addn = "add(n)" :: Nat ->: Nat
  val add = recNNN(m :-> m)(n :-> (addn :-> (m :-> (succ(addn(m))))))

  "Recursion function add" should "be defined properly" in {
    assert(add(zero)(zero) == zero && add(zero)(one) == one &&
        add(one)(zero) == one)
    assert(add(two)(one) == three && add(two)(two) == four)
  }

  "Ackermann function recursively defined" should "give the correct values" in {
    val ackm = "ack(m)" :: Nat ->: Nat

    val ackmp1n = "ack(m+1)(n)" :: Nat

    val ack = recNNN(succ)(
        m :-> (ackm :-> recNN(ackm(one))(n :-> (ackmp1n :-> (ackm(ackmp1n))))))

    assert(ack(two)(two) == seven)
    assert(ack(three)(one) == add(seven)(six))
  }

  // Example: Lists

  val A = "A" :: Type
  val a = "a" :: A
  val a1 = "a1" :: A

  "Recursion function size from List(A) to Nat" should "be defined properly" in {

    val ListA = "List(A)" :: Type
    val ListAInd =
      ("nil" ::: ListA) |: ("cons" ::: A ->>: ListA -->>: ListA) =: ListA
    val nil :: cons :: HNil = ListAInd.intros

    val recLN = ListAInd.rec(Nat)

    val l = "l" :: ListA
    val size = recLN(zero)(a :-> (l :-> (n :-> (succ(n)))))

    assert(size(nil) == zero && size(cons(a)(cons(a)(nil))) == two)
  }

  // Example: binary trees

  val T = "Tree" :: Type
  val TInd = ("leaf" ::: T) |: ("node" ::: T -->>: T -->>: T) =: T
  val leaf :: node :: HNil = TInd.intros

  val t = node(node(leaf)(node(leaf)(leaf)))(node(leaf)(leaf))
  val recTN = TInd.rec(Nat)

  val t1 = "t1" :: T
  val t2 = "t2" :: T
  val vertices = recTN(one)(t1 :-> (m :-> (t2 :-> (n :-> (succ(add(n)(m)))))))

  "Recursion function vertices from Tree to Nat" should "be defined properly" in {
    assert(vertices(t) == nine)
  }

  // Example: binary trees in another way

  val BT = "BinTree" :: Type
  val BTInd = ("leafB" ::: BT) |: ("nodeB" ::: (Bool -|>: BT) -->>: BT) =: BT
  val leafB :: nodeB :: HNil = BTInd.intros

  val recBTN = BTInd.rec(Nat)

  val f = "f" :: Bool ->: BT
  val g = "g" :: Bool ->: Nat
  val leaves = recBTN(one)(f :-> (g :-> (add(g(ff))(g(tt)))))

  val bt = nodeB(b :-> leafB)
  val recBBT = BoolInd.rec(BT)
  val ttn = recBBT(leafB)(bt)
  val bt2 = nodeB(ttn)

  "Recursion function leaves from BinTree to Nat" should "be defined properly" in {
    assert(leaves(bt2) == three)
  }

  val recNN = NatInd.rec(Nat)
  val double = recNN(zero)(m :-> (n :-> (succ(succ(n)))))

  "Recursion function double" should "be defined properly" in {
    assert(double(zero) == zero && double(one) == two && double(two) == four)
  }

  val sumTo = recNN(zero)(m :-> (n :-> (add(succ(m))(n))))

  "Recursion function sumTo" should "be defined properly" in {
    assert(sumTo(zero) == zero && sumTo(one) == one && sumTo(four) == ten)
  }

  // Inductive definitions

  // Example: Vectors

  val V = "Vec" :: Nat ->: Type
  val nilv = "nil" :: V(zero)
  val consv = "cons" :: n ~>: (Nat ->: V(n) ->: V(succ(n)))

  val indNV = NatInd.induc(V)
  val v = "v_m" :: V(m)

  val countdown = indNV(nilv)(m :~> (v :-> consv(m)(succ(m))(v)))

  "Induction function countdown" should "be defined properly" in {
    assert(countdown(zero) == nilv)
    assert(countdown(three) == consv(two)(three)(
            consv(one)(two)(consv(zero)(one)(nilv))))
  }

  // Indexed Inductive types

  // TypeLeveL:
  val Vec = "Vec" :: Nat ->: Type

  val VecInd =
    ("nil" ::: (Vec -> Vec(zero))) |: {
      "cons" ::: n ~>>: (A ->>: (Vec :> Vec(n)) -->>: (Vec -> Vec(succ(n))))
    } =:: Vec

  val vnil :: vcons :: HNil = VecInd.intros

  val vn = "v_n" :: Vec(n)
  val vm = "v_m" :: Vec(m)
  val recVN = VecInd.rec(Nat)

  val size = recVN(zero)(n :~> (a :-> (vn :-> (m :-> (succ(m))))))
  val v1 = vcons(zero)(a)(vnil)
  val v2 = vcons(one)(a1)(v1)

  "Function with an indexed inductive type size" should "be defined properly" in {
    assert(size(zero)(vnil) == zero)
    assert(size(one)(v1) == one)
  }

  "Concatenating vectors using indexed induction" should "behave as expected" in {
    val indVVV =
      VecInd.induc(n :~> (vn :-> (m ~>: (Vec(m) ->: Vec(add(n)(m))))))
    val concatVn = "concat(v_n)" :: (m ~>: (Vec(m) ->: Vec(add(n)(m))))
    val vconcat = indVVV(m :~> (vm :-> vm))(n :~>
        (a :->
            (vn :->
                (concatVn :->
                    (m :~> (vm :-> vcons(add(n)(m))(a)(concatVn(m)(vm))))))))
    assert(vconcat(two)(v2)(two)(v2) == vcons(three)(a1)(
            vcons(two)(a)(vcons(one)(a1)(vcons(zero)(a)(vnil)))))
  }

  val VecN = "Vec(Nat)" :: Nat ->: Type
  val vnn = "v_n" :: VecN(n)
  val VecNInd =
    ("nil" ::: (VecN -> VecN(zero))) |: {
      "cons" ::: n ~>>:
      (Nat ->>: (VecN :> VecN(n)) -->>: (VecN -> VecN(succ(n))))
    } =:: VecN

  val recVNN = VecNInd.rec(Nat)
  val vnilN :: vconsN :: HNil = VecNInd.intros
  val k = "k" :: Nat
  val vsum = recVNN(zero)(n :~> (k :-> (vnn :-> (m :-> (add(m)(k))))))

  val w2 = vconsN(zero)(two)(vnilN)
  val v3 = vconsN(one)(one)(w2)

  "Function with an indexed inductive type vsum" should "be defined properly" in {
    assert(vsum(zero)(vnilN) == zero)
    assert(vsum(one)(w2) == two)
    assert(vsum(two)(v3) == three)
  }

  "Prepending to vectors using indexed induction" should "behave as expected" in {
    val fmly = n :~> (("v" :: VecN(n)) :-> VecN(succ(n)))
    val ind = VecNInd.induc(fmly)
    val v1 = vconsN(zero)(one)(vnilN)
    val iv = ind(v1)
    val tail = "tail" :: VecN(n)
    val result = "result" :: VecN(succ(n))
    val step =
      n :~> (m :~> (tail :~> (result :-> vconsN(succ(n))(two)(result))))
    assert(vsum(three)(iv(step)(two)(iv(step)(one)(v1))) == five)
  }
}
