package provingground

import HoTT._
import org.scalatest.FlatSpec
import Implicits._
import Fold._
import scala.language.existentials

class InductionSpec extends FlatSpec {

  // inductive types

  // Example: Boolean

  val Bool = "Boolean" :: Type
  val BoolInd = "true" ::: Bool |: "false" ::: Bool =: Bool

  val List(tt, ff) = BoolInd.intros
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
  val List(zero, succ) = NatInd.intros

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

  // Example: Lists

  val A = "A" :: Type
  val a = "a" :: A

  "Recursion function size from List(A) to Nat" should "be defined properly" in {

    val ListA = "List(A)" :: Type
    val ListAInd =
      ("nil" ::: ListA) |: ("cons" ::: A ->>: ListA -->>: ListA) =: ListA
    val List(nil, cons) = ListAInd.intros

    val recLN = ListAInd.rec(Nat)

    val l = "l" :: ListA
    val size = recLN(zero)(a :-> (l :-> (n :-> (succ(n)))))

    assert(size(nil) == zero && size(cons(a)(cons(a)(nil))) == two)
  }

  // Example: binary trees

  val T = "Tree" :: Type
  val TInd = ("leaf" ::: T) |: ("node" ::: T -->>: T -->>: T) =: T
  val List(leaf, node) = TInd.intros

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
  val List(leafB, nodeB) = BTInd.intros

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
  // val VecInd = ("nil" ::: (Vec -> Vec(zero))) |:   {"cons" ::: n ~>>: ( A ->>: (Vec :> Vec(n)) -->>:  (Vec -> Vec(succ(n))))} =:: Vec

  val IndN = new IndexedConstructorPatterns(Nat ->: Types)
  val Vec = "Vec" :: Nat ->: Type
  val VecPtn = new IndexedConstructorPatterns(Nat ->: Types)
  val VecFmly = VecPtn.Family(Vec)
  val VecInd = { "nil" ::: VecFmly.head(Vec(zero)) } |: {
    "cons" ::: n ~>>: (A ->>: Vec(n) -->>: VecFmly.head(Vec(succ(n))))
  } =: VecFmly
  val List(vnil, vcons) = VecInd.intros

  val vn = "v_n" :: Vec(n)
  val recVN = VecInd.rec(Nat)

  val size = recVN(zero)(n :~> (a :-> (vn :-> (m :-> (succ(m))))))
  val v1 = vcons(zero)(a)(vnil)

  "Function with an indexed inductive type size" should "be defined properly" in {
    assert(size(zero)(vnil) == zero)
    assert(size(one)(v1) == one)
  }

  val VecN = "Vec(Nat)" ::: Nat ->: Types
  val VecNFmly = VecPtn.Family(VecN)
  val vnn = "v_n" :: VecN(n)
  val VecNInd = { "nil" ::: VecNFmly.head(VecN(zero)) } |: {
    "cons" ::: n ~>>: (Nat ->>: VecN(n) -->>: VecNFmly.head(VecN(succ(n))))
  } =: VecNFmly
  val recVNN = VecNInd.rec(Nat)
  val List(vnilN, vconsN) = VecNInd.intros
  val k = "k" :: Nat
  val vsum = recVNN(zero)(n :~> (k :-> (vnn :-> (m :-> (add(m)(k))))))

  val v2 = vconsN(zero)(two)(vnilN)
  val v3 = vconsN(one)(one)(v2)

  "Function with an indexed inductive type vsum" should "be defined properly" in {
    assert(vsum(zero)(vnilN) == zero)
    assert(vsum(one)(v2) == two)
    assert(vsum(two)(v3) == three)
  }
}
