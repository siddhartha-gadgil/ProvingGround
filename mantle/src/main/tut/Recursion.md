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
The method on constructors corresponding to function types we use if `-->>:`, which is used because the domain of the extension is also the type `Nat`. Note that extending the constructor by a constant type is very different (as we see with lists below), and a different method is used.
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

Lists of elements of a type `A` form an inductive type `ListA`, again with two constructors:

* `nil` of type `ListA`
* `cons` of type `A ->: ListA ->: ListA`

A recursively defined function `f` to a type `X` is specified by data:

* `f(nil) : X`
* `f(cons(a)(l))` as a function of `a`, `l` and 'f(l)', i.e., data has type `A ->: ListA ->: X ->: X`.

Note that `f(a)` does not make sense. Hence a different method, `->>:`, is used for such extensions.

```tut
val A ="A" :: Type
val ListA = "List(A)" :: Type
val ListAInd = ("nil" ::: ListA) |: ("cons" ::: A ->>: ListA -->>: ListA ) =: ListA
val List(nil, cons) = ListAInd.intros
```

We can define the size of a list as a natural number recursively.
```tut
val recLN = ListAInd.rec(Nat)
recLN.typ
val a = "a" :: A
val l = "l" :: ListA
val n = "n" :: Nat
val size = recLN(zero)(a :-> (l :-> (n :-> (succ(n)))))
size(nil)
size(cons(a)(cons(a)(nil)))
```

Another interesting inductive type is a binary rooted tree. This is our first description.
We define the number of vertices recursively on this.
```tut
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
```

We can implement binary trees in another way, which generalizes to binary rooted trees with varying degree.
Instead of a pair of trees, a node corresponds to functions from Booleans to binary rooted trees.

This involves more complex constructors, with an additional method `-|>:`.
The data for recursively defining `f` is also more complex.
We define the number of leaves in such a tree recursively.
```tut
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

As some expresssions are very long, we import a method "FansiShow" that prints in a more concise way.
In the REPL, this gives coloured output using ANSI strings.
```tut
import FansiShow._
```

We define the double of a number recursively, mainly for use later. Observe the partial simplification.
```tut
val recNN = NatInd.rec(Nat)
val double = recNN(zero)(m :-> (n :-> (succ(succ(n)))))
double(two) == four
assert(double(two) == four)
double(succ(n))
```

All our recursive definitions so far of functions `f` have ignored `n` in defining `f(succ(n))`,
and are only in terms of `f(n)`. We see a more complex definition, the sum of numbers up to `n`.
Note that we are defining `sumTo(succ(m))` in terms of `m` and `n = sumTo(m)`, so this is `add(succ(m))(n)`
```tut
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

```tut
val V = "Vec" :: Nat ->: Type
val nil = "nil" :: V(zero)
val cons = "cons" :: n ~>: (Nat ->: V(n) ->: V(succ(n)))
```

We have an induction function taking data for the cases and returning a dependent function.
This is defined by giving data for cases corresponding to the constructors.
Namely to define the dependent function `f`, we must specify

* `f(zero)` of type `V(zero)`
* `f(succ(m))` of type `V(succ(m))`, as a dependent function of `m` and of `f(m) : V(m)`.


We define inductively a countdown function, giving the vector counting down from `n`.
```tut
val indNV = NatInd.induc(V)

val v = "v_m" :: V(m)
val countdown = indNV(nil)(m :~> (v :-> cons(m)(succ(m))(v)) )
countdown(zero)
countdown(one)
countdown(one).fansi
countdown(three).fansi
assert(countdown(three) == cons(two)(three)(cons(one)(two)(cons(zero)(one)(nil))))
countdown(zero) == nil
countdown(nine).fansi
```

We now illustrate a simple instance of using _propositions as proofs_.
The type family `isEven : Nat ->: Type` gives a type representing whether a natural number is even.
This is an inductive type, but here we simply specify the type by  its introduction rules (constructors).

```tut
val isEven = "isEven" :: Nat ->: Type
val zeroEven = "0even" :: isEven(zero)
val plusTwoEven = "_+2even" :: (n ~>: (isEven(n) ->: isEven(succ(succ(n)))))
```

One can directly see that two and four are even.
```tut
val TwoEven = plusTwoEven(zero)(zeroEven)  !: isEven(two)
val FourEven = plusTwoEven(two)(TwoEven) !: isEven(four)
```

Here is a simple proof by induction. We prove the statement that the _double_ of every natural number is even.
The `induc` method gives a dependent function, which takes the base case and the induction step as arguments.
The _base case_ is inhabited by the constructor of type `isEven(zero)`.
The _induction step_ for `n` is a term of type `isEven(double(succ(n)))` as a function of `n` and
the _induction hypothesis_. Note that the induction hypothesis is a term of type `isEven(double(n))`.
```tut
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
```
