## Recursion for inductive types

We illustrate construction of inductive types, and defining functions on them recursively.

We begin with some imports. The import Implicits gives the operations to construct inductive types.
```tut
import provingground._
import HoTT._
import Implicits._
```

We do not define inductive types, but instead define the _structure of an inductive type_ on a given, typically symbolic type.

The inductive structure is defined using a DSL to specify constructors. The Boolean type has constants true and false as constructors.
Constructors are obtained using the `:::` method on a _Constructor pattern_, which for constants is essentially the inductive type itself.
```tut
val Bool = "Boolean" :: Type
val BoolInd = "true" ::: Bool |: "false" ::: Bool =: Bool
```

From the inductive structure, we can obtain the introduction rules.
```tut
val List(tt, ff) = BoolInd.intros
tt
ff
```

The most important methods on an inductive structure are the `rec` method for making recursive definition on the inductive type,
and the corresponding method for dependent functions. The rec method takes as arguments the data giving the definition for the various constructors.
```tut
BoolInd.rec(Bool)
val recBoolBool = BoolInd.rec(Bool)
recBoolBool.typ
```

The compile time scala type of the recursion function is just `Term`. The `import Fold._` allows pattern matching and using the runtime type.
```tut
import Fold._
```

We can define functions recursively using terms obtained from the `rec` method.
In the case of Booleans, the arguments are just the value of the function at true and false. The result is a function `f: Bool ->: X` for a type `X`
```tut
val not = recBoolBool(ff)(tt)
not(ff)
not(tt)
assert(not(ff) == tt && not(tt) == ff)
```

We can similarly define the _and_ function by observing that _and(true)_ is the identity and _and(false)_ is the constant false function.
```tut
val b= "b" :: Bool
val recBBB = BoolInd.rec(Bool ->: Bool)
recBBB.typ
val and = recBBB(lmbda(b)(b))(lmbda(b)(ff))
and(tt)(tt)
and(tt)(ff)
and(ff)(ff)
and(ff)(tt)
assert(and(tt)(tt)== tt && and(tt)(ff) == ff && and(ff)(tt) == ff && and(ff)(ff) == ff)
```

The natural numbers `Nat` are an inductive type with two constructors, `zero` and `succ`, of types `Nat` and `Nat ->: Nat`, respectively.
The method on constructors corresponding to function types _with domain the inductive type being specified_ is `-->>:`.
```tut
val Nat ="Nat" :: Type
val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
val List(zero, succ) = NatInd.intros
```

To define recursively a function `f : Nat ->: X` for a type `X`, the data is

* `f(zero) : X`, i.e., data of type `X`
* `f(succ(n)) : X` as a function of `n : Nat` and `x: X`, i.e., data is of the form `Nat ->: X ->: X`

```tut
val recNatBool = NatInd.rec(Bool)
recNatBool.typ
val n = "n" :: Nat
val isEven = recNatBool(tt)(n :-> (b :-> not(b)))
val one = succ(zero)
val two = succ(one)
val three = succ(two)
val four = succ(three)
isEven(two)
isEven(three)
```

A more complicated example is addition of natural numbers.
```tut
val recNNN = NatInd.rec(Nat ->: Nat)
recNNN.typ
val m = "m" :: Nat
val addn ="add(n)" :: Nat ->: Nat
val add = recNNN(m :-> m)(n :-> (addn :-> (m :-> (succ(addn(m))) ) ) )
add(two)(one)
assert(add(two)(one) == three)
add(two)(two) == four
```

```tut
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
val recBBT = BoolInd.rec(BT)
recBBT.typ
val ttn = recBBT(leaf)(t)
val t2 = node(ttn)
leaves(t2)



```
