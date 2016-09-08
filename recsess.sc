import provingground._
import HoTT._
import TLImplicits._
val Bool = "Boolean" :: Type
val BoolInd = "true" ::: Bool |: "false" ::: Bool =: Bool
val List(tt, ff) = BoolInd.intros
tt
ff

BoolInd.rec(Bool)
val recBoolBool = BoolInd.rec(Bool)
recBoolBool.typ
import Fold._

val not = recBoolBool(ff)(tt)
not(ff)
not(tt)
assert(not(ff) == tt && not(tt) == ff)

val b= "b" :: Bool
val recBBB = BoolInd.rec(Bool ->: Bool)
recBBB.typ
val and = recBBB(lmbda(b)(b))(lmbda(b)(ff))
and(tt)(tt)
and(tt)(ff)
and(ff)(ff)
and(ff)(tt)
assert(and(tt)(tt)== tt && and(tt)(ff) == ff && and(ff)(tt) == ff && and(ff)(ff) == ff)

val Nat ="Nat" :: Type
val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
val List(zero, succ) = NatInd.intros

val recNatBool = NatInd.rec(Bool)
recNatBool.typ
val n = "n" :: Nat
val even = recNatBool(tt)(n :-> (b :-> not(b)))
val one = succ(zero)
val two = succ(one)
val three = succ(two)
val four = succ(three)
even(two)
even(three)

val recNNN = NatInd.rec(Nat ->: Nat)
recNNN.typ
val m = "m" :: Nat
val addn ="add(n)" :: Nat ->: Nat
val add = recNNN(m :-> m)(n :-> (addn :-> (m :-> (succ(addn(m))) ) ) )
add(two)(one)
assert(add(two)(one) == three)
add(two)(two) == four

val A ="A" :: Type
val ListA = "List(A)" :: Type
val ListAInd = ("nil" ::: ListA) |: ("cons" ::: A ->>: ListA -->>: ListA ) =: ListA
val List(nil, cons) = ListAInd.intros

val recLN = ListAInd.rec(Nat)
recLN.typ
val a = "a" :: A
val l = "l" :: ListA

val size = recLN(zero)(a :-> (l :-> (n :-> (succ(n)))))
size(nil)
size(cons(a)(cons(a)(nil)))

val T ="Tree" :: Type
val TInd = ("leaf" ::: T) |: ("node" ::: T -->>: T -->>: T) =: T
val List(leaf, node) = TInd.intros
import Fold._
val t = node(node(leaf)(node(leaf)(leaf)))(node(leaf)(leaf))

val recTN = TInd.rec(Nat)
recTN.typ

val t1 = "t1" :: T
val t2 = "t2" :: T

val vertices = recTN(one)(t1 :-> (m :->( t2 :-> (n :-> (succ(add(n)(m))  ) ) ) ) )

vertices(t)

val nine = succ(add(four)(four))
vertices(t) == nine
assert(vertices(t) == nine)

val BT ="BinTree" :: Type
val BTInd = ("leaf" ::: BT) |: ("node" ::: (Bool -|>: BT) -->>: BT )  =: BT
val List(bleaf, bnode) = BTInd.intros
val recBTN = BTInd.rec(Nat)
recBTN.typ
val f = "f" :: Bool ->: BT
val g = "g" :: Bool ->: Nat
val leaves = recBTN(one)(f :-> (g :-> (add(g(ff))(g(tt))) ))
leaves(bleaf)

val T1 = bnode(b :-> bleaf)
val recBBT = BoolInd.rec(BT)
recBBT.typ
val tn = recBBT(bleaf)(T1)
val T2 = bnode(tn)
leaves(T2)

import FansiShow._

val recNN = NatInd.rec(Nat)
val double = recNN(zero)(m :-> (n :-> (succ(succ(n)))))
double(two) == four
assert(double(two) == four)
double(succ(n))

val sumTo = recNN(zero)(m :-> (n :-> (add(succ(m))(n))))
sumTo(one)
sumTo(three).fansi
val ten = succ(nine)
sumTo(four) == ten
assert(sumTo(four) == ten)

val isEven = "isEven" :: Nat ->: Type
val zeroEven = "0even" :: isEven(zero)
val plusTwoEven = "_+2even" :: (n ~>: (isEven(n) ->: isEven(succ(succ(n)))))

val thmDoubleEven = n ~>: isEven(double(n))
val hyp = "isEven(double(n))" :: isEven(double(n))
val pfDoubleEven =
  NatInd.induc(n :-> isEven(double(n))){
    zeroEven}{
      n :~> (
        hyp :-> (
          plusTwoEven(double(n))(hyp)
          )
          )
    } !: thmDoubleEven
