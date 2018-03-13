---
title: Inductive Types
layout: page
---

## Recursion for inductive types

We illustrate construction of inductive types, and defining functions on them recursively.

We begin with some imports. The import induction.TLImplicits gives the operations to construct inductive types.

```scala
scala> import provingground._
import provingground._

scala> import HoTT._
import HoTT._

scala> import induction._
import induction._

scala> import translation._
import translation._

scala> import TLImplicits._
import TLImplicits._

scala> import shapeless._
import shapeless._
```

We do not define inductive types, but instead define the _structure of an inductive type_ on a given, typically symbolic type.

The inductive structure is defined using a DSL to specify constructors. The Boolean type has constants true and false as constructors.
Constructors are obtained using the `:::` method on a _Constructor pattern_, which for constants is essentially the inductive type itself.

```scala
scala> val Bool = "Boolean" :: Type
Bool: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = Boolean

scala> val BoolInd = "true" ::: Bool |: "false" ::: Bool =: Bool
BoolInd: provingground.induction.ConstructorSeqTL[shapeless.HNil :: shapeless.HNil :: shapeless.HNil,provingground.HoTT.Term,provingground.HoTT.Term :: provingground.HoTT.Term :: shapeless.HNil] = ConstructorSeqTL(Cons(true,IdShape(),Cons(false,IdShape(),Empty())),Boolean)
```

From the inductive structure, we can obtain the introduction rules.

```scala
scala> val tt :: ff :: HNil = BoolInd.intros
tt: provingground.HoTT.Term = true
ff: provingground.HoTT.Term = false

scala> tt
res0: provingground.HoTT.Term = true

scala> ff
res1: provingground.HoTT.Term = false
```

The most important methods on an inductive structure are the `rec` method for making recursive definition on the inductive type,
and the corresponding method for dependent functions. The rec method takes as arguments the data giving the definition for the various constructors.

```scala
scala> BoolInd.rec(Bool)
res2: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = (RecDataSym(true) :  Boolean) ↦ ((RecDataSym(false) :  Boolean) ↦ (rec(Boolean)(Boolean)(RecDataSym(true))(RecDataSym(false))))

scala> val recBoolBool = BoolInd.rec(Bool)
recBoolBool: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = (RecDataSym(true) :  Boolean) ↦ ((RecDataSym(false) :  Boolean) ↦ (rec(Boolean)(Boolean)(RecDataSym(true))(RecDataSym(false))))

scala> recBoolBool.typ
res3: provingground.HoTT.Typ[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]] = (Boolean) → ((Boolean) → ((Boolean) → (Boolean)))
```



We can define functions recursively using terms obtained from the `rec` method.
In the case of Booleans, the arguments are just the value of the function at true and false. The result is a function `f: Bool ->: X` for a type `X`

```scala
scala> val not = recBoolBool(ff)(tt)
not: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] = rec(Boolean)(Boolean)(false)(true)

scala> not(ff)
res4: provingground.HoTT.Term = true

scala> not(tt)
res5: provingground.HoTT.Term = false

scala> assert(not(ff) == tt && not(tt) == ff)
```

We can similarly define the _and_ function by observing that _and(true)_ is the identity and _and(false)_ is the constant false function.

```scala
scala> val b= "b" :: Bool
b: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = b

scala> val recBBB = BoolInd.rec(Bool ->: Bool)
recBBB: provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]] = (RecDataSym(true) :  (Boolean) → (Boolean)) ↦ ((RecDataSym(false) :  (Boolean) → (Boolean)) ↦ (rec(Boolean)((Boolean) → (Boolean))(RecDataSym(true))(RecDataSym(false))))

scala> recBBB.typ
res7: provingground.HoTT.Typ[provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]]] = ((Boolean) → (Boolean)) → (((Boolean) → (Boolean)) → ((Boolean) → ((Boolean) → (Boolean))))

scala> val and = recBBB(lmbda(b)(b))(lmbda(b)(ff))
and: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = rec(Boolean)((Boolean) → (Boolean))((b :  Boolean) ↦ (b))((_ :  Boolean) ↦ (false))

scala> and(tt)(tt)
res8: provingground.HoTT.Term = true

scala> and(tt)(ff)
res9: provingground.HoTT.Term = false

scala> and(ff)(ff)
res10: provingground.HoTT.Term = false

scala> and(ff)(tt)
res11: provingground.HoTT.Term = false

scala> assert(and(tt)(tt)== tt && and(tt)(ff) == ff && and(ff)(tt) == ff && and(ff)(ff) == ff)
```

The natural numbers `Nat` are an inductive type with two constructors, `zero` and `succ`, of types `Nat` and `Nat ->: Nat`, respectively.
The method on constructors corresponding to function types we use if `-->>:`, which is used because the domain of the extension is also the type `Nat`. Note that extending the constructor by a constant type is very different (as we see with lists below), and a different method is used.

```scala
scala> val Nat = "Nat" :: Type
Nat: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = Nat

scala> val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
NatInd: provingground.induction.ConstructorSeqTL[shapeless.HNil :: (provingground.induction.ConstructorShape.FuncConsShape.type :: shapeless.HNil) :: shapeless.HNil,provingground.HoTT.Term,provingground.HoTT.Term :: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] :: shapeless.HNil] = ConstructorSeqTL(Cons(0,IdShape(),Cons(succ,FuncConsShape(IdIterShape(),IdShape()),Empty())),Nat)

scala> val zero :: succ :: HNil = NatInd.intros
zero: provingground.HoTT.Term = 0
succ: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] = succ
```

To define recursively a function `f : Nat ->: X` for a type `X`, the data is

* `f(zero) : X`, i.e., data of type `X`
* `f(succ(n)) : X` as a function of `n : Nat` and `x: X`, i.e., data is of the form `Nat ->: X ->: X`

```scala
scala> val recNatBool = NatInd.rec(Bool)
recNatBool: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = (RecDataSym(0) :  Boolean) ↦ ((RecDataSym(succ) :  (Nat) → ((Boolean) → (Boolean))) ↦ (rec(Nat)(Boolean)(RecDataSym(0))(RecDataSym(succ))))

scala> recNatBool.typ
res13: provingground.HoTT.Typ[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]] = (Boolean) → (((Nat) → ((Boolean) → (Boolean))) → ((Nat) → (Boolean)))

scala> val n = "n" :: Nat
n: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = n

scala> val even = recNatBool(tt)(n :-> (b :-> not(b)))
even: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] = rec(Nat)(Boolean)(true)((_ :  Nat) ↦ ((b :  Boolean) ↦ ((rec(Boolean)(Boolean)(false)(true)) (b))))

scala> val one = succ(zero)
one: provingground.HoTT.Term = (succ) (0)

scala> val two = succ(one)
two: provingground.HoTT.Term = (succ) ((succ) (0))

scala> val three = succ(two)
three: provingground.HoTT.Term = (succ) ((succ) ((succ) (0)))

scala> val four = succ(three)
four: provingground.HoTT.Term = (succ) ((succ) ((succ) ((succ) (0))))

scala> even(two)
res14: provingground.HoTT.Term = true

scala> even(three)
res15: provingground.HoTT.Term = false
```

A more complicated example is addition of natural numbers.

```scala
scala> val recNNN = NatInd.rec(Nat ->: Nat)
recNNN: provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]] = (RecDataSym(0) :  (Nat) → (Nat)) ↦ ((RecDataSym(succ) :  (Nat) → (((Nat) → (Nat)) → ((Nat) → (Nat)))) ↦ (rec(Nat)((Nat) → (Nat))(RecDataSym(0))(RecDataSym(succ))))

scala> recNNN.typ
res16: provingground.HoTT.Typ[provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]]] = ((Nat) → (Nat)) → (((Nat) → (((Nat) → (Nat)) → ((Nat) → (Nat)))) → ((Nat) → ((Nat) → (Nat))))

scala> val m = "m" :: Nat
m: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = m

scala> val addn = "add(n)" :: Nat ->: Nat
addn: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = add(n)

scala> val add = recNNN(m :-> m)(n :-> (addn :-> (m :-> (succ(addn(m))) ) ) )
add: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = rec(Nat)((Nat) → (Nat))((m :  Nat) ↦ (m))((_ :  Nat) ↦ ((add(n) :  (Nat) → (Nat)) ↦ ((m :  Nat) ↦ ((succ) ((add(n)) (m))))))

scala> add(two)(one)
res17: provingground.HoTT.Term = (succ) ((succ) ((succ) (0)))

scala> assert(add(two)(one) == three)

scala> add(two)(two) == four
res19: Boolean = true
```

Lists of elements of a type `A` form an inductive type `ListA`, again with two constructors:

* `nil` of type `ListA`
* `cons` of type `A ->: ListA ->: ListA`

A recursively defined function `f` to a type `X` is specified by data:

* `f(nil) : X`
* `f(cons(a)(l))` as a function of `a`, `l` and 'f(l)', i.e., data has type `A ->: ListA ->: X ->: X`.

Note that `f(a)` does not make sense. Hence a different method, `->>:`, is used for such extensions.

```scala
scala> val A = "A" :: Type
A: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = A

scala> val ListA = "List(A)" :: Type
ListA: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = List(A)

scala> val ListAInd = ("nil" ::: ListA) |: ("cons" ::: A ->>: ListA -->>: ListA ) =: ListA
ListAInd: provingground.induction.ConstructorSeqTL[shapeless.HNil :: (provingground.induction.ConstructorShape.CnstFuncConsShape.type :: provingground.induction.ConstructorShape.FuncConsShape.type :: shapeless.HNil) :: shapeless.HNil,provingground.HoTT.Term,provingground.HoTT.Term :: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] :: shapeless.HNil] = ConstructorSeqTL(Cons(nil,IdShape(),Cons(cons,CnstFuncConsShape(A,FuncConsShape(IdIterShape(),IdShape())),Empty())),List(A))

scala> val nil :: cons :: HNil = ListAInd.intros
nil: provingground.HoTT.Term = nil
cons: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = cons
```

We can define the size of a list as a natural number recursively.

```scala
scala> val recLN = ListAInd.rec(Nat)
recLN: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = (RecDataSym(nil) :  Nat) ↦ ((RecDataSym(cons) :  (A) → ((List(A)) → ((Nat) → (Nat)))) ↦ (rec(List(A))(Nat)(RecDataSym(nil))(RecDataSym(cons))))

scala> recLN.typ
res20: provingground.HoTT.Typ[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]] = (Nat) → (((A) → ((List(A)) → ((Nat) → (Nat)))) → ((List(A)) → (Nat)))

scala> val a = "a" :: A
a: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = a

scala> val l = "l" :: ListA
l: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = l

scala> val n = "n" :: Nat
n: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = n

scala> val size = recLN(zero)(a :-> (l :-> (n :-> (succ(n)))))
size: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] = rec(List(A))(Nat)(0)((_ :  A) ↦ ((_ :  List(A)) ↦ ((n :  Nat) ↦ ((succ) (n)))))

scala> size(nil)
res21: provingground.HoTT.Term = 0

scala> size(cons(a)(cons(a)(nil)))
res22: provingground.HoTT.Term = (succ) ((succ) (0))
```

Another interesting inductive type is a binary rooted tree. This is our first description.
We define the number of vertices recursively on this.

```scala
scala> val T = "Tree" :: Type
T: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = Tree

scala> val TInd = ("leaf" ::: T) |: ("node" ::: T -->>: T -->>: T) =: T
TInd: provingground.induction.ConstructorSeqTL[shapeless.HNil :: (provingground.induction.ConstructorShape.FuncConsShape.type :: provingground.induction.ConstructorShape.FuncConsShape.type :: shapeless.HNil) :: shapeless.HNil,provingground.HoTT.Term,provingground.HoTT.Term :: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] :: shapeless.HNil] = ConstructorSeqTL(Cons(leaf,IdShape(),Cons(node,FuncConsShape(IdIterShape(),FuncConsShape(IdIterShape(),IdShape())),Empty())),Tree)

scala> val leaf :: node :: HNil = TInd.intros
leaf: provingground.HoTT.Term = leaf
node: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = node

scala> val t = node(node(leaf)(node(leaf)(leaf)))(node(leaf)(leaf))
t: provingground.HoTT.Term = ((node) (((node) (leaf)) (((node) (leaf)) (leaf)))) (((node) (leaf)) (leaf))

scala> val recTN = TInd.rec(Nat)
recTN: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = (RecDataSym(leaf) :  Nat) ↦ ((RecDataSym(node) :  (Tree) → ((Nat) → ((Tree) → ((Nat) → (Nat))))) ↦ (rec(Tree)(Nat)(RecDataSym(leaf))(RecDataSym(node))))

scala> recTN.typ
res23: provingground.HoTT.Typ[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]] = (Nat) → (((Tree) → ((Nat) → ((Tree) → ((Nat) → (Nat))))) → ((Tree) → (Nat)))

scala> val t1 = "t1" :: T
t1: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = t1

scala> val t2 = "t2" :: T
t2: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = t2

scala> val vertices = recTN(one)(t1 :-> (m :->( t2 :-> (n :-> (succ(add(n)(m))  ) ) ) ) )
vertices: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] = rec(Tree)(Nat)((succ) (0))((_ :  Tree) ↦ ((m :  Nat) ↦ ((_ :  Tree) ↦ ((n :  Nat) ↦ ((succ) (((rec(Nat)((Nat) → (Nat))((m :  Nat) ↦ (m))((_ :  Nat) ↦ ((add(n) :  (Nat) → (Nat)) ↦ ((m :  Nat) ↦ ((succ) ((add(n)) (m))))))) (n)) (m)))))))

scala> vertices(t)
res24: provingground.HoTT.Term = (succ) ((succ) ((succ) ((succ) ((succ) ((succ) ((succ) ((succ) ((succ) (0)))))))))

scala> val nine = succ(add(four)(four))
nine: provingground.HoTT.Term = (succ) ((succ) ((succ) ((succ) ((succ) ((succ) ((succ) ((succ) ((succ) (0)))))))))

scala> vertices(t) == nine
res25: Boolean = true

scala> assert(vertices(t) == nine)
```

We can implement binary trees in another way, which generalizes to binary rooted trees with varying degree.
Instead of a pair of trees, a node corresponds to functions from Booleans to binary rooted trees.

This involves more complex constructors, with an additional method `-|>:`.
The data for recursively defining `f` is also more complex.
We define the number of leaves in such a tree recursively.

```scala
scala> val BT = "BinTree" :: Type
BT: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = BinTree

scala> val BTInd = ("leaf" ::: BT) |: ("node" ::: (Bool -|>: BT) -->>: BT )  =: BT
BTInd: provingground.induction.ConstructorSeqTL[shapeless.HNil :: (provingground.induction.ConstructorShape.FuncConsShape.type :: shapeless.HNil) :: shapeless.HNil,provingground.HoTT.Term,provingground.HoTT.Term :: provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Term] :: shapeless.HNil] = ConstructorSeqTL(Cons(leaf,IdShape(),Cons(node,FuncConsShape(FuncShape(Boolean,IdIterShape()),IdShape()),Empty())),BinTree)

scala> val leaf :: node :: HNil = BTInd.intros
leaf: provingground.HoTT.Term = leaf
node: provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Term] = node

scala> val recBTN = BTInd.rec(Nat)
recBTN: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Term]],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = (RecDataSym(leaf) :  Nat) ↦ ((RecDataSym(node) :  ((Boolean) → (BinTree)) → (((Boolean) → (Nat)) → (Nat))) ↦ (rec(BinTree)(Nat)(RecDataSym(leaf))(RecDataSym(node))))

scala> recBTN.typ
res27: provingground.HoTT.Typ[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Term]],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]] = (Nat) → ((((Boolean) → (BinTree)) → (((Boolean) → (Nat)) → (Nat))) → ((BinTree) → (Nat)))

scala> val f = "f" :: Bool ->: BT
f: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = f

scala> val g = "g" :: Bool ->: Nat
g: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = g

scala> val leaves = recBTN(one)(f :-> (g :-> (add(g(ff))(g(tt))) ))
leaves: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] = rec(BinTree)(Nat)((succ) (0))((_ :  (Boolean) → (BinTree)) ↦ ((g :  (Boolean) → (Nat)) ↦ (((rec(Nat)((Nat) → (Nat))((m :  Nat) ↦ (m))((_ :  Nat) ↦ ((add(n) :  (Nat) → (Nat)) ↦ ((m :  Nat) ↦ ((succ) ((add(n)) (m))))))) ((g) (false))) ((g) (true)))))

scala> leaves(leaf)
res28: provingground.HoTT.Term = (succ) (0)

scala> val b = "b" :: Bool
b: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = b

scala> val t = node(b :-> leaf)
t: provingground.HoTT.Term = (node) ((_ :  Boolean) ↦ (leaf))

scala> val recBBT = BoolInd.rec(BT)
recBBT: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = (RecDataSym(true) :  BinTree) ↦ ((RecDataSym(false) :  BinTree) ↦ (rec(Boolean)(BinTree)(RecDataSym(true))(RecDataSym(false))))

scala> recBBT.typ
res29: provingground.HoTT.Typ[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]] = (BinTree) → ((BinTree) → ((Boolean) → (BinTree)))

scala> val ttn = recBBT(leaf)(t)
ttn: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] = rec(Boolean)(BinTree)(leaf)((node) ((_ :  Boolean) ↦ (leaf)))

scala> val t2 = node(ttn)
t2: provingground.HoTT.Term = (node) (rec(Boolean)(BinTree)(leaf)((node) ((_ :  Boolean) ↦ (leaf))))

scala> leaves(t2)
res30: provingground.HoTT.Term = (succ) ((succ) ((succ) (0)))
```

As some expresssions are very long, we import a method "FansiShow" that prints in a more concise way.
In the REPL, this gives coloured output using ANSI strings.

```scala
scala> import FansiShow._
import FansiShow._
```

We define the double of a number recursively, mainly for use later. Observe the partial simplification.

```scala
scala> val recNN = NatInd.rec(Nat)
recNN: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = (RecDataSym(0) :  Nat) ↦ ((RecDataSym(succ) :  (Nat) → ((Nat) → (Nat))) ↦ (rec(Nat)(Nat)(RecDataSym(0))(RecDataSym(succ))))

scala> val double = recNN(zero)(m :-> (n :-> (succ(succ(n)))))
double: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] = rec(Nat)(Nat)(0)((_ :  Nat) ↦ ((n :  Nat) ↦ ((succ) ((succ) (n)))))

scala> double(two) == four
res31: Boolean = true

scala> assert(double(two) == four)

scala> double(succ(n))
res33: provingground.HoTT.Term = (succ) ((succ) ((rec(Nat)(Nat)(0)((_ :  Nat) ↦ ((n :  Nat) ↦ ((succ) ((succ) (n)))))) (n)))
```

All our recursive definitions so far of functions `f` have ignored `n` in defining `f(succ(n))`,
and are only in terms of `f(n)`. We see a more complex definition, the sum of numbers up to `n`.
Note that we are defining `sumTo(succ(m))` in terms of `m` and `n = sumTo(m)`, so this is `add(succ(m))(n)`

```scala
scala> val sumTo = recNN(zero)(m :-> (n :-> (add(succ(m))(n))))
sumTo: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] = rec(Nat)(Nat)(0)((m :  Nat) ↦ ((n :  Nat) ↦ ((succ) (((rec(Nat)((Nat) → (Nat))((m :  Nat) ↦ (m))((_ :  Nat) ↦ ((add(n) :  (Nat) → (Nat)) ↦ ((m :  Nat) ↦ ((succ) ((add(n)) (m))))))) (m)) (n)))))

scala> sumTo(one)
res34: provingground.HoTT.Term = (succ) (0)

scala> sumTo(three).fansi
res35: String = succ(succ(succ(succ(succ(succ(0))))))

scala> val ten = succ(nine)
ten: provingground.HoTT.Term = (succ) ((succ) ((succ) ((succ) ((succ) ((succ) ((succ) ((succ) ((succ) ((succ) (0))))))))))

scala> sumTo(four) == ten
res36: Boolean = true

scala> assert(sumTo(four) == ten)
```



## Inductive definitions

In homotopy type theory, inductive definitions are the analogues of recursive definitions for dependent functions.
We see an example of such a definition.

The image is a family `V : Nat ->: Type` which we can think of as vectors of natural numbers indexed by length.
Just like actual vectors, we have `nil` and `cons` introduction rules, but here they are purely formal.

```scala
scala> val V = "Vec" :: Nat ->: Type
V: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]]] = Vec

scala> val nilv = "nil" :: V(zero)
nilv: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = nil

scala> val consv = "cons" :: n ~>: (Nat ->: V(n) ->: V(succ(n)))
consv: provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] with provingground.HoTT.Subs[provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]] = cons
```

We have an induction function taking data for the cases and returning a dependent function.
This is defined by giving data for cases corresponding to the constructors.
Namely to define the dependent function `f`, we must specify

* `f(zero)` of type `V(zero)`
* `f(succ(m))` of type `V(succ(m))`, as a dependent function of `m` and of `f(m) : V(m)`.


We define inductively a countdown function, giving the vector counting down from `n`.

```scala
scala> val indNV = NatInd.induc(V)
indNV: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.FuncLike[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.HoTT.FuncLike[provingground.HoTT.Term,provingground.HoTT.Term]]] = (InducDataSym(0) :  (Vec) (0)) ↦ ((InducDataSym(succ) :  $fzzf ~> ((Vec) ($fzzf)) → ((Vec) ((succ) ($fzzf)))) ↦ (ind(Nat)(Vec)(InducDataSym(0))(InducDataSym(succ))))

scala> val v = "v_m" :: V(m)
v: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = v_m

scala> val countdown = indNV(nilv)(m :~> (v :-> consv(m)(succ(m))(v)) )
countdown: provingground.HoTT.FuncLike[provingground.HoTT.Term,provingground.HoTT.Term] = ind(Nat)(Vec)(nil)((m :  Nat) ↦ ((v_m :  (Vec) (m)) ↦ ((((cons) (m)) ((succ) (m))) (v_m))))

scala> countdown(zero)
res38: provingground.HoTT.Term = nil

scala> countdown(one)
res39: provingground.HoTT.Term = (((cons) (0)) ((succ) (0))) (nil)

scala> countdown(one).fansi
res40: String = cons(0)(succ(0))(nil)

scala> countdown(three).fansi
res41: String = cons(succ(succ(0)))(succ(succ(succ(0))))(cons(succ(0))(succ(succ(0)))(cons(0)(succ(0))(nil)))

scala> assert(countdown(three) ==
     |   consv(two)(three)(
     |     consv(one)(two)(
     |       consv(zero)(one)(nilv))))

scala> countdown(zero) == nilv
res43: Boolean = true

scala> countdown(nine).fansi
res44: String = cons(succ(succ(succ(succ(succ(succ(succ(succ(0)))))))))(succ(succ(succ(succ(succ(succ(succ(succ(succ(0))))))))))(cons(succ(succ(succ(succ(succ(succ(succ(0))))))))(succ(succ(succ(succ(succ(succ(succ(succ(0)))))))))(cons(succ(succ(succ(succ(succ(succ(0)))))))(succ(succ(succ(succ(succ(succ(succ(0))))))))(cons(succ(succ(succ(succ(succ(0))))))(succ(succ(succ(succ(succ(succ(0)))))))(cons(succ(succ(succ(succ(0)))))(succ(succ(succ(succ(succ(0))))))(cons(succ(succ(succ(0))))(succ(succ(succ(succ(0)))))(cons(succ(succ(0)))(succ(succ(succ(0))))(cons(succ(0))(succ(succ(0)))(cons(0)(succ(0))(nil)))))))))
```

We now illustrate a simple instance of using _propositions as proofs_.
The type family `isEven : Nat ->: Type` gives a type representing whether a natural number is even.
This is an inductive type, but here we simply specify the type by  its introduction rules (constructors).
Such terms introduced by specifying types are logically _axioms_.

```scala
scala> val isEven = "isEven" :: Nat ->: Type
isEven: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]]] = isEven

scala> val zeroEven = "0even" :: isEven(zero)
zeroEven: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = 0even

scala> val plusTwoEven = "_+2even" :: (n ~>: (isEven(n) ->: isEven(succ(succ(n)))))
plusTwoEven: provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] with provingground.HoTT.Subs[provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = _+2even
```

One can directly see that two and four are even.

```scala
scala> val TwoEven = plusTwoEven(zero)(zeroEven)  !: isEven(two)
TwoEven: provingground.HoTT.Term = ((_+2even) (0)) (0even)

scala> val FourEven = plusTwoEven(two)(TwoEven) !: isEven(four)
FourEven: provingground.HoTT.Term = ((_+2even) ((succ) ((succ) (0)))) (((_+2even) (0)) (0even))
```

Here is a simple proof by induction. We prove the statement that the _double_ of every natural number is even.
The `induc` method gives a dependent function, which takes the base case and the induction step as arguments.
The _base case_ is inhabited by the constructor of type `isEven(zero)`.
The _induction step_ for `n` is a term of type `isEven(double(succ(n)))` as a function of `n` and
the _induction hypothesis_. Note that the induction hypothesis is a term of type `isEven(double(n))`.

```scala
scala> val thmDoubleEven = n ~>: isEven(double(n))
thmDoubleEven: provingground.HoTT.GenFuncTyp[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term] = n ~> (isEven) ((rec(Nat)(Nat)(0)((_ :  Nat) ↦ ((n :  Nat) ↦ ((succ) ((succ) (n)))))) (n))

scala> val hyp = "isEven(double(n))" :: isEven(double(n))
hyp: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = isEven(double(n))

scala> val inducDoubleEven = NatInd.induc(n :-> isEven(double(n)))
inducDoubleEven: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.FuncLike[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.HoTT.FuncLike[provingground.HoTT.Term,provingground.HoTT.Term]]] = (InducDataSym(0) :  (isEven) (0)) ↦ ((InducDataSym(succ) :  $xeah ~> ((isEven) ((rec(Nat)(Nat)(0)((_ :  Nat) ↦ ((n :  Nat) ↦ ((succ) ((succ) (n)))))) ($xeah))) → ((isEven) ((succ) ((succ) ((rec(Nat)(Nat)(0)((_ :  Nat) ↦ ((n :  Nat) ↦ ((succ) ((succ) (n)))))) ($xeah)))))) ↦ (ind(Nat)((n :  Nat) ↦ ((isEven) ((rec(Nat)(Nat)(0)((_ :  Nat) ↦ ((n :  Nat) ↦ ((succ) ((succ) (n)))))) (n))))(InducDataSym(0))(InducDataSym(succ))))

scala> val pfDoubleEven =
     |   inducDoubleEven(
     |     zeroEven){
     |       n :~> (
     |         hyp :-> (
     |           plusTwoEven(double(n))(hyp)
     |           )
     |           )
     |     }  !: thmDoubleEven
pfDoubleEven: provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term] = ind(Nat)((n :  Nat) ↦ ((isEven) ((rec(Nat)(Nat)(0)((_ :  Nat) ↦ ((n :  Nat) ↦ ((succ) ((succ) (n)))))) (n))))(0even)((n :  Nat) ↦ ((isEven(double(n)) :  (isEven) ((rec(Nat)(Nat)(0)((_ :  Nat) ↦ ((n :  Nat) ↦ ((succ) ((succ) (n)))))) (n))) ↦ (((_+2even) ((rec(Nat)(Nat)(0)((_ :  Nat) ↦ ((n :  Nat) ↦ ((succ) ((succ) (n)))))) (n))) (isEven(double(n))))))
```

We next prove a more interesting statement, namely that for any natural number `n`, one of `n` and `n+1` is even.

```scala
scala> val succEven = n :-> (isEven(n) || isEven(succ(n)))
succEven: provingground.HoTT.Func[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.PlusTyp[provingground.HoTT.Term,provingground.HoTT.Term]] = (n :  Nat) ↦ (PlusTyp((isEven) (n),(isEven) ((succ) (n))))

scala> val base = succEven(zero).incl1(zeroEven) !: succEven(zero)
base: provingground.HoTT.Term = FirstIncl(PlusTyp((isEven) (0),(isEven) ((succ) (0))),0even)

scala> val thmSuccEven = n ~>: (succEven(n))
thmSuccEven: provingground.HoTT.GenFuncTyp[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term] = n ~> PlusTyp((isEven) (n),(isEven) ((succ) (n)))

scala> val hyp1 = "n-is-Even" :: isEven(n)
hyp1: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = n-is-Even

scala> val hyp2 = "(n+1)-is-Even" :: isEven(succ(n))
hyp2: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = (n+1)-is-Even

scala> val step = (succEven(n).rec(succEven(succ(n)))){hyp1 :-> (succEven(succ(n)).incl2(plusTwoEven(n)(hyp1)))}{hyp2 :-> (succEven(succ(n)).incl1((hyp2)))}
step: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] = rec(PlusTyp((isEven) (n),(isEven) ((succ) (n))))(PlusTyp((isEven) ((succ) (n)),(isEven) ((succ) ((succ) (n)))))((n-is-Even :  (isEven) (n)) ↦ (ScndIncl(PlusTyp((isEven) ((succ) (n)),(isEven) ((succ) ((succ) (n)))),((_+2even) (n)) (n-is-Even))))(((n+1)-is-Even :  (isEven) ((succ) (n))) ↦ (FirstIncl(PlusTyp((isEven) ((succ) (n)),(isEven) ((succ) ((succ) (n)))),(n+1)-is-Even)))

scala> val inducSuccEven = NatInd.induc(succEven)
inducSuccEven: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.FuncLike[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.HoTT.FuncLike[provingground.HoTT.Term,provingground.HoTT.Term]]] = (InducDataSym(0) :  PlusTyp((isEven) (0),(isEven) ((succ) (0)))) ↦ ((InducDataSym(succ) :  $yelx ~> (PlusTyp((isEven) ($yelx),(isEven) ((succ) ($yelx)))) → (PlusTyp((isEven) ((succ) ($yelx)),(isEven) ((succ) ((succ) ($yelx)))))) ↦ (ind(Nat)((n :  Nat) ↦ (PlusTyp((isEven) (n),(isEven) ((succ) (n)))))(InducDataSym(0))(InducDataSym(succ))))

scala> val pf = inducSuccEven(base)(n :~> step) !: thmSuccEven
pf: provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term] = ind(Nat)((n :  Nat) ↦ (PlusTyp((isEven) (n),(isEven) ((succ) (n)))))(FirstIncl(PlusTyp((isEven) (0),(isEven) ((succ) (0))),0even))((n :  Nat) ↦ (rec(PlusTyp((isEven) (n),(isEven) ((succ) (n))))(PlusTyp((isEven) ((succ) (n)),(isEven) ((succ) ((succ) (n)))))((n-is-Even :  (isEven) (n)) ↦ (ScndIncl(PlusTyp((isEven) ((succ) (n)),(isEven) ((succ) ((succ) (n)))),((_+2even) (n)) (n-is-Even))))(((n+1)-is-Even :  (isEven) ((succ) (n))) ↦ (FirstIncl(PlusTyp((isEven) ((succ) (n)),(isEven) ((succ) ((succ) (n)))),(n+1)-is-Even)))))
```

We now prove a result that has been a goal, namely that for a function on Natural numbers if `f(n)=f(n+1)` for all n,
`f` is constant.

```scala
scala> val f = "f" :: Nat ->: A
f: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = f

scala> val ass = "assumption" :: n ~>: (f(n) =:= f(succ(n)))
ass: provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Equality[provingground.HoTT.Term]] with provingground.HoTT.Subs[provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Equality[provingground.HoTT.Term]]] = assumption

scala> val claim = n :-> (f(zero) =:= f(n))
claim: provingground.HoTT.Func[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.IdentityTyp[provingground.HoTT.Term]] = (n :  Nat) ↦ ((f) (0) = (f) (n))

scala> val base = f(zero).refl
base: provingground.HoTT.Refl[provingground.HoTT.Term] = Refl(A,(f) (0))

scala> val hyp = "hypothesis" :: (f(zero) =:= f(n))
hyp: provingground.HoTT.Equality[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Equality[provingground.HoTT.Term]] = hypothesis : ((f) (0) = (f) (n))

scala> val step = hyp :-> {IdentityTyp.trans(A)(f(zero))(f(n))(f(succ(n)))(hyp)(ass(n)) }
step: provingground.HoTT.Func[provingground.HoTT.Equality[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Equality[provingground.HoTT.Term]],provingground.HoTT.Equality[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term]]] = (hypothesis : ((f) (0) = (f) (n)) :  (f) (0) = (f) (n)) ↦ (((ind{($yrqy :  A) ↦ (($yrqz :  A) ↦ ($yrqy = $yrqz))((f) (0))((f) (n))}{($ypsu :  A) ↦ (($ypsv :  A) ↦ ((_ : ($ypsu = $ypsv) :  $ypsu = $ypsv) ↦ (($ypsv = (f) ((succ) (n))) → ($ypsu = (f) ((succ) (n))))))}(($ypsu :  A) ↦ (($yqbq : ($ypsu = (f) ((succ) (n))) :  $ypsu = (f) ((succ) (n))) ↦ ($yqbq : ($ypsu = (f) ((succ) (n))))))) (hypothesis : ((f) (0) = (f) (n)))) ((assumption) (n) : ((f) (n) = (f) ((succ) (n)))) : ((f) (0) = (f) ((s...

scala> val inducClaim = NatInd.induc(claim)
inducClaim: provingground.HoTT.Func[provingground.HoTT.Equality[provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.FuncLike[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Equality[provingground.HoTT.Term],provingground.HoTT.Equality[provingground.HoTT.Term]]],provingground.HoTT.FuncLike[provingground.HoTT.Term,provingground.HoTT.Equality[provingground.HoTT.Term]]]] = (InducDataSym(0) : ((f) (0) = (f) (0)) :  (f) (0) = (f) (0)) ↦ ((InducDataSym(succ) :  $yrtv ~> ((f) (0) = (f) ($yrtv)) → ((f) (0) = (f) ((succ) ($yrtv)))) ↦ (ind(Nat)((n :  Nat) ↦ ((f) (0) = (f) (n)))(InducDataSym(0) : ((f) (0) = (f) (0)))(InducDataSym(succ))))

scala> val pf = inducClaim(base)(n :~> step) !: (n ~>: (f(zero) =:= f(n)))
pf: provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Equality[provingground.HoTT.Term]] = ind(Nat)((n :  Nat) ↦ ((f) (0) = (f) (n)))(Refl(A,(f) (0)))((n :  Nat) ↦ ((hypothesis : ((f) (0) = (f) (n)) :  (f) (0) = (f) (n)) ↦ (((ind{($ywje :  A) ↦ (($ywjf :  A) ↦ ($ywje = $ywjf))((f) (0))((f) (n))}{($ypsu :  A) ↦ (($ypsv :  A) ↦ ((_ : ($ypsu = $ypsv) :  $ypsu = $ypsv) ↦ (($ypsv = (f) ((succ) (n))) → ($ypsu = (f) ((succ) (n))))))}(($ypsu :  A) ↦ (($yqbq : ($ypsu = (f) ((succ) (n))) :  $ypsu = (f) ((succ) (n))) ↦ ($yqbq : ($ypsu = (f) ((succ) (n))))))) (hypothesis : ((f) (0) = (f) (n)))) ((assumption) (n) : ((f) (n) = (f) ((succ) (n)))) : ((f) (0) = (f) ((succ) (n))))))
```


## Indexed Inductive types

A generalization of inductive types are _inductive type families_, i.e., inductive types depending on an index.
Unlike parametrized inductive types (such as lists), the constructors of an inductive type family involve in general several different indices.
Further, the recursion and induction function only allow construction of (dependent) functions on the whole family.

A typical example is vectors, defined as a family indexed by their length.

```scala
scala> val Vec = "Vec" :: Nat ->: Type
Vec: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]]] = Vec

scala> val VecInd =
     |   ("nil" ::: (Vec -> Vec(zero))) |: {
     |     "cons" ::: n ~>>: (A ->>: (Vec :> Vec(n)) -->>: (Vec -> Vec(succ(n))))
     |   } =:: Vec
VecInd: provingground.induction.IndexedConstructorSeqDom.Cons[(provingground.induction.IndexedConstructorShape.IndexedCnstDepFuncConsShape.type :: provingground.induction.IndexedConstructorShape.IndexedCnstFuncConsShape.type :: provingground.induction.IndexedConstructorShape.IndexedFuncConsShape.type :: shapeless.HNil) :: shapeless.HNil,shapeless.HNil,provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]]],provingground.HoTT.Term,provingground.HoTT.Term :: shapeless.HNil,provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],p...

scala> val vnil :: vcons :: HNil = VecInd.intros
vnil: provingground.HoTT.Term = nil
vcons: provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = cons

scala> vcons.typ.fansi
res45: String = ∏($yzre : Nat){ (A → (Vec($yzre) → Vec(succ($yzre)))) }
```

We can define function recursively on vectors of all indices. For instance, we can define the size.

```scala
scala> val vn = "v_n" :: Vec(n)
vn: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = v_n

scala> val recVN = VecInd.rec(Nat)
recVN: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]],provingground.HoTT.FuncLike[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]] = (RecDataSym(nil) :  Nat) ↦ ((RecDataSym(cons) :  $yzsc ~> (A) → (((Vec) ($yzsc)) → ((Nat) → (Nat)))) ↦ (($yzrl :  Nat) ↦ (rec{Vec($yzrl)}{Nat}(RecDataSym(nil))(RecDataSym(cons)))))

scala> val size = recVN(zero)(n :~>(a :-> (vn :->(m :->(succ(m))))))
size: provingground.HoTT.FuncLike[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = ($yzrl :  Nat) ↦ (rec{Vec($yzrl)}{Nat}(0)((n :  Nat) ↦ ((_ :  A) ↦ ((_ :  (Vec) (n)) ↦ ((m :  Nat) ↦ ((succ) (m)))))))

scala> size(zero)(vnil)
res46: provingground.HoTT.Term = 0

scala> val v1 = vcons(zero)(a)(vnil)
v1: provingground.HoTT.Term = (((cons) (0)) (a)) (nil)

scala> size(one)(v1)
res47: provingground.HoTT.Term = (succ) (0)

scala> assert(size(one)(v1) == one)
```

For a more interesting example, we consider vectors with entries natural numbers, and define the sum of entries.

```scala
scala> val VecN = "Vec(Nat)" :: Nat ->: Type
VecN: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]]] = Vec(Nat)

scala> val vnn  = "v_n" :: VecN(n)
vnn: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = v_n

scala> val VecNInd =
     |   ("nil" ::: (VecN -> VecN(zero))) |: {
     |     "cons" ::: n ~>>:
     |       (Nat ->>: (VecN :> VecN(n)) -->>: (VecN -> VecN(succ(n))))
     |   } =:: VecN
VecNInd: provingground.induction.IndexedConstructorSeqDom.Cons[(provingground.induction.IndexedConstructorShape.IndexedCnstDepFuncConsShape.type :: provingground.induction.IndexedConstructorShape.IndexedCnstFuncConsShape.type :: provingground.induction.IndexedConstructorShape.IndexedFuncConsShape.type :: shapeless.HNil) :: shapeless.HNil,shapeless.HNil,provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]]],provingground.HoTT.Term,provingground.HoTT.Term :: shapeless.HNil,provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],...

scala> val recVNN                  = VecNInd.rec(Nat)
recVNN: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]],provingground.HoTT.FuncLike[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]] = (RecDataSym(nil) :  Nat) ↦ ((RecDataSym(cons) :  $zcek ~> (Nat) → (((Vec(Nat)) ($zcek)) → ((Nat) → (Nat)))) ↦ (($zcdt :  Nat) ↦ (rec{Vec(Nat)($zcdt)}{Nat}(RecDataSym(nil))(RecDataSym(cons)))))

scala> val vnilN :: vconsN :: HNil = VecNInd.intros
vnilN: provingground.HoTT.Term = nil
vconsN: provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = cons

scala> val k = "k" :: Nat
k: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = k

scala> val vsum = recVNN(zero)(n :~>(k :-> (vnn :->(m :-> (add(m)(k)) ))))
vsum: provingground.HoTT.FuncLike[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = ($zcdt :  Nat) ↦ (rec{Vec(Nat)($zcdt)}{Nat}(0)((n :  Nat) ↦ ((k :  Nat) ↦ ((_ :  (Vec(Nat)) (n)) ↦ ((m :  Nat) ↦ (((rec(Nat)((Nat) → (Nat))((m :  Nat) ↦ (m))((_ :  Nat) ↦ ((add(n) :  (Nat) → (Nat)) ↦ ((m :  Nat) ↦ ((succ) ((add(n)) (m))))))) (m)) (k)))))))

scala> vsum(zero)(vnilN)
res49: provingground.HoTT.Term = 0

scala> val v2 = vconsN(zero)(two)(vnilN)
v2: provingground.HoTT.Term = (((cons) (0)) ((succ) ((succ) (0)))) (nil)

scala> vsum(one)(v2)
res50: provingground.HoTT.Term = (succ) ((succ) (0))

scala> assert(vsum(one)(v2) == two)

scala> val v3 = vconsN(one)(one)(v2)
v3: provingground.HoTT.Term = (((cons) ((succ) (0))) ((succ) (0))) ((((cons) (0)) ((succ) ((succ) (0)))) (nil))

scala> v3.fansi
res52: String = cons(succ(0))(succ(0))(cons(0)(succ(succ(0)))(nil))

scala> vsum(two)(v3)
res53: provingground.HoTT.Term = (succ) ((succ) ((succ) (0)))

scala> assert(vsum(two)(v3) == three)
```
