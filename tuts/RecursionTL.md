---
title: Inductive Types
layout: page
---

## Recursion for inductive types

We illustrate construction of inductive types, and defining functions on them recursively.

We begin with some imports. The import induction.TLImplicits gives the operations to construct inductive types.

```scala mdoc:to-string
import provingground._
import HoTT._
import induction._
import translation._
import TLImplicits._
import shapeless._
```

We do not define inductive types, but instead define the _structure of an inductive type_ on a given, typically symbolic type.

The inductive structure is defined using a DSL to specify constructors. The Boolean type has constants true and false as constructors.
Constructors are obtained using the `:::` method on a _Constructor pattern_, which for constants is essentially the inductive type itself.

```scala mdoc:to-string
val Bool = "Boolean" :: Type
val BoolInd = "true" ::: Bool |: "false" ::: Bool =: Bool
```

From the inductive structure, we can obtain the introduction rules.

```scala mdoc:to-string
val tt :: ff :: HNil = BoolInd.intros
tt
ff
```

The most important methods on an inductive structure are the `rec` method for making recursive definition on the inductive type,
and the corresponding method for dependent functions. The rec method takes as arguments the data giving the definition for the various constructors.

```scala mdoc:to-string
BoolInd.rec(Bool)
val recBoolBool = BoolInd.rec(Bool)
recBoolBool.typ
```



We can define functions recursively using terms obtained from the `rec` method.
In the case of Booleans, the arguments are just the value of the function at true and false. The result is a function `f: Bool ->: X` for a type `X`

```scala mdoc:to-string
val not = recBoolBool(ff)(tt)
not(ff)
not(tt)
assert(not(ff) == tt && not(tt) == ff)
```

We can similarly define the _and_ function by observing that _and(true)_ is the identity and _and(false)_ is the constant false function.

```scala mdoc:to-string
val b = "b" :: Bool
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
The method on constructors corresponding to function types we use if `-->>:`, which is used because the domain of the extension is also the type `Nat`. Note that extending the constructor by a constant type is very different (as we see with lists below), and a different method is used.

```scala mdoc:to-string
val Nat = "Nat" :: Type
val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
val zero :: succ :: HNil = NatInd.intros
```

To define recursively a function `f : Nat ->: X` for a type `X`, the data is

* `f(zero) : X`, i.e., data of type `X`
* `f(succ(n)) : X` as a function of `n : Nat` and `x: X`, i.e., data is of the form `Nat ->: X ->: X`

```scala mdoc:to-string
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
```

A more complicated example is addition of natural numbers.

```scala mdoc:to-string
val recNNN = NatInd.rec(Nat ->: Nat)
recNNN.typ
val m = "m" :: Nat
val addn = "add(n)" :: Nat ->: Nat
val add = recNNN(m :-> m)(n :-> (addn :-> (m :-> (succ(addn(m))) ) ) )
add(two)(one)
assert(add(two)(one) == three)
add(two)(two) == four
```

Lists of elements of a type `A` form an inductive type `ListA`, again with two constructors:

* `nil` of type `ListA`
* `cons` of type `A ->: ListA ->: ListA`

A recursively defined function `f` to a type `X` is specified by data:

* `f(nil) : X`
* `f(cons(a)(l))` as a function of `a`, `l` and 'f(l)', i.e., data has type `A ->: ListA ->: X ->: X`.

Note that `f(a)` does not make sense. Hence a different method, `->>:`, is used for such extensions.

```scala mdoc:to-string
val A = "A" :: Type
val ListA = "List(A)" :: Type
val ListAInd = ("nil" ::: ListA) |: ("cons" ::: A ->>: ListA -->>: ListA ) =: ListA
val nil :: cons :: HNil = ListAInd.intros
```

We can define the size of a list as a natural number recursively.

```scala mdoc:to-string
val recLN = ListAInd.rec(Nat)
recLN.typ
val a = "a" :: A
val l = "l" :: ListA

val size = recLN(zero)(a :-> (l :-> (n :-> (succ(n)))))
size(nil)
size(cons(a)(cons(a)(nil)))
```

Another interesting inductive type is a binary rooted tree. This is our first description.
We define the number of vertices recursively on this.

```scala mdoc:to-string
val T = "Tree" :: Type
val TInd = ("leaf" ::: T) |: ("node" ::: T -->>: T -->>: T) =: T
val leaf :: node :: HNil = TInd.intros
val ttt = node(node(leaf)(node(leaf)(leaf)))(node(leaf)(leaf))

val recTN = TInd.rec(Nat)
recTN.typ

val t1 = "t1" :: T
val t2 = "t2" :: T

val vertices = recTN(one)(t1 :-> (m :->( t2 :-> (n :-> (succ(add(n)(m))  ) ) ) ) )

vertices(ttt)

val nine = succ(add(four)(four))
vertices(ttt) == nine
assert(vertices(ttt) == nine)
```

We can implement binary trees in another way, which generalizes to binary rooted trees with varying degree.
Instead of a pair of trees, a node corresponds to functions from Booleans to binary rooted trees.

This involves more complex constructors, with an additional method `-|>:`.
The data for recursively defining `f` is also more complex.
We define the number of leaves in such a tree recursively.

```scala mdoc:to-string
val BT = "BinTree" :: Type
val BTInd = ("leaf" ::: BT) |: ("node" ::: (Bool -|>: BT) -->>: BT )  =: BT
val bleaf :: bnode :: HNil = BTInd.intros
val recBTN = BTInd.rec(Nat)
recBTN.typ
val f = "f" :: Bool ->: BT
val g = "g" :: Bool ->: Nat
val leaves = recBTN(one)(f :-> (g :-> (add(g(ff))(g(tt))) ))
leaves(bleaf)
// val b = "b" :: Bool
val t = bnode(b :-> bleaf)
val recBBT = BoolInd.rec(BT)
recBBT.typ
val ttn = recBBT(bleaf)(t)
val t2n = bnode(ttn)
leaves(t2n)
```

As some expresssions are very long, we import a method "FansiShow" that prints in a more concise way.
In the REPL, this gives coloured output using ANSI strings.

```scala mdoc:to-string
import FansiShow._
```

We define the double of a number recursively, mainly for use later. Observe the partial simplification.

```scala mdoc:to-string
val recNN = NatInd.rec(Nat)
val double = recNN(zero)(m :-> (n :-> (succ(succ(n)))))
double(two) == four
assert(double(two) == four)
double(succ(n))
```

All our recursive definitions so far of functions `f` have ignored `n` in defining `f(succ(n))`,
and are only in terms of `f(n)`. We see a more complex definition, the sum of numbers up to `n`.
Note that we are defining `sumTo(succ(m))` in terms of `m` and `n = sumTo(m)`, so this is `add(succ(m))(n)`

```scala mdoc:to-string
val sumTo = recNN(zero)(m :-> (n :-> (add(succ(m))(n))))
sumTo(one)
sumTo(three).fansi
val ten = succ(nine)
sumTo(four) == ten
assert(sumTo(four) == ten)
```



## Inductive definitions

In homotopy type theory, inductive definitions are the analogues of recursive definitions for dependent functions.
We see an example of such a definition.

The image is a family `V : Nat ->: Type` which we can think of as vectors of natural numbers indexed by length.
Just like actual vectors, we have `nil` and `cons` introduction rules, but here they are purely formal.

```scala mdoc:to-string
val V = "Vec" :: Nat ->: Type
val nilv = "nil" :: V(zero)
val consv = "cons" :: n ~>: (Nat ->: V(n) ->: V(succ(n)))
```

We have an induction function taking data for the cases and returning a dependent function.
This is defined by giving data for cases corresponding to the constructors.
Namely to define the dependent function `f`, we must specify

* `f(zero)` of type `V(zero)`
* `f(succ(m))` of type `V(succ(m))`, as a dependent function of `m` and of `f(m) : V(m)`.


We define inductively a countdown function, giving the vector counting down from `n`.

```scala mdoc:to-string
val indNV = NatInd.induc(V)

val v = "v_m" :: V(m)
val countdown = indNV(nilv)(m :~> (v :-> consv(m)(succ(m))(v)) )
countdown(zero)
countdown(one)
countdown(one).fansi
countdown(three).fansi
assert(countdown(three) ==
  consv(two)(three)(
    consv(one)(two)(
      consv(zero)(one)(nilv))))
countdown(zero) == nilv
countdown(nine).fansi
```

We now illustrate a simple instance of using _propositions as proofs_.
The type family `isEven : Nat ->: Type` gives a type representing whether a natural number is even.
This is an inductive type, but here we simply specify the type by  its introduction rules (constructors).
Such terms introduced by specifying types are logically _axioms_.

```scala mdoc:to-string
val isEven = "isEven" :: Nat ->: Type
val zeroEven = "0even" :: isEven(zero)
val plusTwoEven = "_+2even" :: (n ~>: (isEven(n) ->: isEven(succ(succ(n)))))
```

One can directly see that two and four are even.

```scala mdoc:to-string
val TwoEven = plusTwoEven(zero)(zeroEven)  !: isEven(two)
val FourEven = plusTwoEven(two)(TwoEven) !: isEven(four)
```

Here is a simple proof by induction. We prove the statement that the _double_ of every natural number is even.
The `induc` method gives a dependent function, which takes the base case and the induction step as arguments.
The _base case_ is inhabited by the constructor of type `isEven(zero)`.
The _induction step_ for `n` is a term of type `isEven(double(succ(n)))` as a function of `n` and
the _induction hypothesis_. Note that the induction hypothesis is a term of type `isEven(double(n))`.

```scala mdoc:to-string
val thmDoubleEven = n ~>: isEven(double(n))
val hyp = "isEven(double(n))" :: isEven(double(n))
val inducDoubleEven = NatInd.induc(n :-> isEven(double(n)))
val pfDoubleEven =
  inducDoubleEven(
    zeroEven){
      n :~> (
        hyp :-> (
          plusTwoEven(double(n))(hyp)
          )
          )
    }  !: thmDoubleEven
```

We next prove a more interesting statement, namely that for any natural number `n`, one of `n` and `n+1` is even.

```scala mdoc:to-string
val succEven = n :-> (isEven(n) || isEven(succ(n)))

val base = succEven(zero).incl1(zeroEven) !: succEven(zero)

val thmSuccEven = n ~>: (succEven(n))

val hyp1 = "n-is-Even" :: isEven(n)
val hyp2 = "(n+1)-is-Even" :: isEven(succ(n))

val step = (succEven(n).rec(succEven(succ(n)))){hyp1 :-> (succEven(succ(n)).incl2(plusTwoEven(n)(hyp1)))}{hyp2 :-> (succEven(succ(n)).incl1((hyp2)))}

val inducSuccEven = NatInd.induc(succEven)

val pf = inducSuccEven(base)(n :~> step) !: thmSuccEven
```

We now prove a result that has been a goal, namely that for a function on Natural numbers if `f(n)=f(n+1)` for all n,
`f` is constant.

```scala mdoc:to-string
val fn = "f" :: Nat ->: A
val ass = "assumption" :: n ~>: (fn(n) =:= fn(succ(n)))

val claim = n :-> (fn(zero) =:= fn(n))

val sbase = fn(zero).refl

val shyp = "hypothesis" :: (fn(zero) =:= fn(n))
val sstep = shyp :-> {IdentityTyp.trans(A)(fn(zero))(fn(n))(fn(succ(n)))(shyp)(ass(n)) }

val inducClaim = NatInd.induc(claim)

val spf = inducClaim(sbase)(n :~> sstep) !: (n ~>: (fn(zero) =:= fn(n)))

```


## Indexed Inductive types

A generalization of inductive types are _inductive type families_, i.e., inductive types depending on an index.
Unlike parametrized inductive types (such as lists), the constructors of an inductive type family involve in general several different indices.
Further, the recursion and induction function only allow construction of (dependent) functions on the whole family.

A typical example is vectors, defined as a family indexed by their length.

```scala mdoc:to-string
val Vec = "Vec" :: Nat ->: Type

val VecInd =
  ("nil" ::: (Vec -> Vec(zero))) |: {
    "cons" ::: n ~>>: (A ->>: (Vec :> Vec(n)) -->>: (Vec -> Vec(succ(n))))
  } =:: Vec

val vnil :: vcons :: HNil = VecInd.intros

vcons.typ.fansi
```

We can define function recursively on vectors of all indices. For instance, we can define the size.

```scala mdoc:to-string
val vn = "v_n" :: Vec(n)
val recVN = VecInd.rec(Nat)
val vsize = recVN(zero)(n :~>(a :-> (vn :->(m :->(succ(m))))))
vsize(zero)(vnil)
val v1 = vcons(zero)(a)(vnil)
vsize(one)(v1)
assert(vsize(one)(v1) == one)
```

For a more interesting example, we consider vectors with entries natural numbers, and define the sum of entries.

```scala mdoc:to-string
val VecN = "Vec(Nat)" :: Nat ->: Type
val vnn  = "v_n" :: VecN(n)
val VecNInd =
  ("nil" ::: (VecN -> VecN(zero))) |: {
    "cons" ::: n ~>>:
      (Nat ->>: (VecN :> VecN(n)) -->>: (VecN -> VecN(succ(n))))
  } =:: VecN

val recVNN                  = VecNInd.rec(Nat)
val vnilN :: vconsN :: HNil = VecNInd.intros

val k = "k" :: Nat
val vsum = recVNN(zero)(n :~>(k :-> (vnn :->(m :-> (add(m)(k)) ))))

vsum(zero)(vnilN)
val v2 = vconsN(zero)(two)(vnilN)
vsum(one)(v2)
assert(vsum(one)(v2) == two)

val v3 = vconsN(one)(one)(v2)
v3.fansi
vsum(two)(v3)
assert(vsum(two)(v3) == three)
```
