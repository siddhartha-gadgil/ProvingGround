## Recursion for inductive types

We illustrate construction of inductive types, and defining functions on them recursively.

We begin with some imports. The import Implicits gives the operations to construct inductive types.
```tut
import provingground._
import HoTT._
import Implicits._
```

```tut
val Bool = "Boolean" :: Type
val boolInduc = "true" ::: Bool |: "false" ::: Bool =: Bool
val List(tt, ff) = boolInduc.intros
tt
ff
boolInduc.rec(Bool)
val recBoolBool = boolInduc.rec(Bool)
recBoolBool.typ
import Fold._
val not = recBoolBool(ff)(tt)
not(ff)
not(tt)
boolInduc
val b= "b" :: Bool
val recBBB = boolInduc.rec(Bool ->: Bool)
recBBB.typ
val and = recBBB(lmbda(b)(b))(lmbda(b)(ff))
and(tt)(tt)
and(tt)(ff)
and(ff)(ff)
and(ff)(tt)
val Nat ="Nat" :: Type
val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
val List(zero, succ) = NatInd.intros
val recNatBool = NatInd.rec(Bool)
recNatBool.typ
val n = "n" :: Nat
val isEven = recNatBool(tt)(lmbda(n)(lmbda(b)(not(b))))
val one = succ(zero)
val two = succ(one)
val three = succ(two)
val four = succ(three)
isEven(two)
isEven(three)
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
val n = "n" :: Nat
val size = recLN(zero)(a :-> (l :-> (n :-> (succ(n)))))
import Fold._
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



val BT ="BinTree" :: Type
val BTInd = ("leaf" ::: BT) |: ("node" ::: (Bool -|>: BT) -->>: BT )  =: BT
val List(leaf, node) = BTInd.intros
val recBTN = BTInd.rec(Nat)
recBTN.typ
val f = "f" :: Bool ->: BT
val g = "g" :: Bool ->: Nat
val leaves = recBTN(one)(f :-> (g :-> (add(g(ff))(g(tt))) ))
leaves(leaf)
val b = "b" :: Bool
val t = node(b :-> leaf)
val recBBT = boolInduc.rec(BT)
recBBT.typ
val ttn = recBBT(leaf)(t)
val t2 = node(ttn)
leaves(t2)



```
