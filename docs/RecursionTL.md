---
title: Inductive Types
layout: page
---

## Recursion for inductive types

We illustrate construction of inductive types, and defining functions on them recursively.

We begin with some imports. The import Implicits gives the operations to construct inductive types.

```scala
scala> import provingground._
import provingground._

scala> import HoTT._
import HoTT._

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
Bool: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = Boolean : 𝒰 _0

scala> val BoolInd = "true" ::: Bool |: "false" ::: Bool =: Bool
BoolInd: provingground.ConstructorSeqTL[shapeless.HNil :: shapeless.HNil :: shapeless.HNil,provingground.HoTT.Term,provingground.HoTT.Term :: provingground.HoTT.Term :: shapeless.HNil] = ConstructorSeqTL(Cons(true,IdShape(),Cons(false,IdShape(),Empty())),Boolean : 𝒰 _0)
```

From the inductive structure, we can obtain the introduction rules.

```scala
scala> val tt :: ff :: HNil = BoolInd.intros
tt: provingground.HoTT.Term = true : (Boolean : 𝒰 _0)
ff: provingground.HoTT.Term = false : (Boolean : 𝒰 _0)

scala> tt
res0: provingground.HoTT.Term = true : (Boolean : 𝒰 _0)

scala> ff
res1: provingground.HoTT.Term = false : (Boolean : 𝒰 _0)
```

The most important methods on an inductive structure are the `rec` method for making recursive definition on the inductive type,
and the corresponding method for dependent functions. The rec method takes as arguments the data giving the definition for the various constructors.

```scala
scala> BoolInd.rec(Bool)
res2: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = (RecSym(true : (Boolean : 𝒰 _0)) : (Boolean : 𝒰 _0)) ↦ ((RecSym(false : (Boolean : 𝒰 _0)) : (Boolean : 𝒰 _0)) ↦ (rec(Boolean : 𝒰 _0)(Boolean : 𝒰 _0)(RecSym(true : (Boolean : 𝒰 _0)) : (Boolean : 𝒰 _0))(RecSym(false : (Boolean : 𝒰 _0)) : (Boolean : 𝒰 _0))))

scala> val recBoolBool = BoolInd.rec(Bool)
recBoolBool: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = (RecSym(true : (Boolean : 𝒰 _0)) : (Boolean : 𝒰 _0)) ↦ ((RecSym(false : (Boolean : 𝒰 _0)) : (Boolean : 𝒰 _0)) ↦ (rec(Boolean : 𝒰 _0)(Boolean : 𝒰 _0)(RecSym(true : (Boolean : 𝒰 _0)) : (Boolean : 𝒰 _0))(RecSym(false : (Boolean : 𝒰 _0)) : (Boolean : 𝒰 _0))))

scala> recBoolBool.typ
res3: provingground.HoTT.Typ[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]] = (Boolean : 𝒰 _0) → ((Boolean : 𝒰 _0) → ((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0)))
```



We can define functions recursively using terms obtained from the `rec` method.
In the case of Booleans, the arguments are just the value of the function at true and false. The result is a function `f: Bool ->: X` for a type `X`

```scala
scala> val not = recBoolBool(ff)(tt)
not: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] = rec(Boolean : 𝒰 _0)(Boolean : 𝒰 _0)(false : (Boolean : 𝒰 _0))(true : (Boolean : 𝒰 _0))

scala> not(ff)
res4: provingground.HoTT.Term = true : (Boolean : 𝒰 _0)

scala> not(tt)
res5: provingground.HoTT.Term = false : (Boolean : 𝒰 _0)

scala> assert(not(ff) == tt && not(tt) == ff)
```

We can similarly define the _and_ function by observing that _and(true)_ is the identity and _and(false)_ is the constant false function.

```scala
scala> val b= "b" :: Bool
b: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = b : (Boolean : 𝒰 _0)

scala> val recBBB = BoolInd.rec(Bool ->: Bool)
recBBB: provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]] = (RecSym(true : (Boolean : 𝒰 _0)) : ((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0))) ↦ ((RecSym(false : (Boolean : 𝒰 _0)) : ((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0))) ↦ (rec(Boolean : 𝒰 _0)((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0))(RecSym(true : (Boolean : 𝒰 _0)) : ((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0)))(RecSym(false : (Boolean : 𝒰 _0)) : ((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0)))))

scala> recBBB.typ
res7: provingground.HoTT.Typ[provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]]] = ((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0)) → (((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0)) → ((Boolean : 𝒰 _0) → ((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0))))

scala> val and = recBBB(lmbda(b)(b))(lmbda(b)(ff))
and: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = rec(Boolean : 𝒰 _0)((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0))((b : (Boolean : 𝒰 _0)) ↦ (b : (Boolean : 𝒰 _0)))((b : (Boolean : 𝒰 _0)) ↦ (false : (Boolean : 𝒰 _0)))

scala> and(tt)(tt)
res8: provingground.HoTT.Term = true : (Boolean : 𝒰 _0)

scala> and(tt)(ff)
res9: provingground.HoTT.Term = false : (Boolean : 𝒰 _0)

scala> and(ff)(ff)
res10: provingground.HoTT.Term = false : (Boolean : 𝒰 _0)

scala> and(ff)(tt)
res11: provingground.HoTT.Term = false : (Boolean : 𝒰 _0)

scala> assert(and(tt)(tt)== tt && and(tt)(ff) == ff && and(ff)(tt) == ff && and(ff)(ff) == ff)
```

The natural numbers `Nat` are an inductive type with two constructors, `zero` and `succ`, of types `Nat` and `Nat ->: Nat`, respectively.
The method on constructors corresponding to function types we use if `-->>:`, which is used because the domain of the extension is also the type `Nat`. Note that extending the constructor by a constant type is very different (as we see with lists below), and a different method is used.

```scala
scala> val Nat ="Nat" :: Type
Nat: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = Nat : 𝒰 _0

scala> val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
NatInd: provingground.ConstructorSeqTL[shapeless.HNil :: (provingground.ConstructorShape.FuncConsShape.type :: shapeless.HNil) :: shapeless.HNil,provingground.HoTT.Term,provingground.HoTT.Term :: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] :: shapeless.HNil] = ConstructorSeqTL(Cons(0,IdShape(),Cons(succ,FuncConsShape(IdIterShape(),IdShape()),Empty())),Nat : 𝒰 _0)

scala> val zero :: succ :: HNil = NatInd.intros
zero: provingground.HoTT.Term = 0 : (Nat : 𝒰 _0)
succ: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] = succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))
```

To define recursively a function `f : Nat ->: X` for a type `X`, the data is

* `f(zero) : X`, i.e., data of type `X`
* `f(succ(n)) : X` as a function of `n : Nat` and `x: X`, i.e., data is of the form `Nat ->: X ->: X`

```scala
scala> val recNatBool = NatInd.rec(Bool)
recNatBool: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = (RecSym(0 : (Nat : 𝒰 _0)) : (Boolean : 𝒰 _0)) ↦ ((RecSym(succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) : ((Nat : 𝒰 _0) → ((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0)))) ↦ (rec(Nat : 𝒰 _0)(Boolean : 𝒰 _0)(RecSym(0 : (Nat : 𝒰 _0)) : (Boolean : 𝒰 _0))(RecSym(succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) : ((Nat : 𝒰 _0) → ((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0))))))

scala> recNatBool.typ
res13: provingground.HoTT.Typ[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]] = (Boolean : 𝒰 _0) → (((Nat : 𝒰 _0) → ((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0))) → ((Nat : 𝒰 _0) → (Boolean : 𝒰 _0)))

scala> val n = "n" :: Nat
n: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = n : (Nat : 𝒰 _0)

scala> val even = recNatBool(tt)(n :-> (b :-> not(b)))
even: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] = rec(Nat : 𝒰 _0)(Boolean : 𝒰 _0)(true : (Boolean : 𝒰 _0))((n : (Nat : 𝒰 _0)) ↦ ((b : (Boolean : 𝒰 _0)) ↦ ((rec(Boolean : 𝒰 _0)(Boolean : 𝒰 _0)(false : (Boolean : 𝒰 _0))(true : (Boolean : 𝒰 _0))) (b : (Boolean : 𝒰 _0)) : (Boolean : 𝒰 _0))))

scala> val one = succ(zero)
one: provingground.HoTT.Term = (succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)

scala> val two = succ(one)
two: provingground.HoTT.Term = (succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)

scala> val three = succ(two)
three: provingground.HoTT.Term = (succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)

scala> val four = succ(three)
four: provingground.HoTT.Term = (succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)

scala> even(two)
res14: provingground.HoTT.Term = true : (Boolean : 𝒰 _0)

scala> even(three)
res15: provingground.HoTT.Term = false : (Boolean : 𝒰 _0)
```

A more complicated example is addition of natural numbers.

```scala
scala> val recNNN = NatInd.rec(Nat ->: Nat)
recNNN: provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]] = (RecSym(0 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ↦ ((RecSym(succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) : ((Nat : 𝒰 _0) → (((Nat : 𝒰 _0) → (Nat : 𝒰 _0)) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))))) ↦ (rec(Nat : 𝒰 _0)((Nat : 𝒰 _0) → (Nat : 𝒰 _0))(RecSym(0 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))(RecSym(s...

scala> recNNN.typ
res16: provingground.HoTT.Typ[provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]]] = ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)) → (((Nat : 𝒰 _0) → (((Nat : 𝒰 _0) → (Nat : 𝒰 _0)) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) → ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))))

scala> val m = "m" :: Nat
m: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = m : (Nat : 𝒰 _0)

scala> val addn ="add(n)" :: Nat ->: Nat
addn: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = add(n) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))

scala> val add = recNNN(m :-> m)(n :-> (addn :-> (m :-> (succ(addn(m))) ) ) )
add: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = rec(Nat : 𝒰 _0)((Nat : 𝒰 _0) → (Nat : 𝒰 _0))((m : (Nat : 𝒰 _0)) ↦ (m : (Nat : 𝒰 _0)))((n : (Nat : 𝒰 _0)) ↦ ((add(n) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ↦ ((m : (Nat : 𝒰 _0)) ↦ ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((add(n) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))))

scala> add(two)(one)
res17: provingground.HoTT.Term = (succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)

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
scala> val A ="A" :: Type
A: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = A : 𝒰 _0

scala> val ListA = "List(A)" :: Type
ListA: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = List(A) : 𝒰 _0

scala> val ListAInd = ("nil" ::: ListA) |: ("cons" ::: A ->>: ListA -->>: ListA ) =: ListA
ListAInd: provingground.ConstructorSeqTL[shapeless.HNil :: (provingground.ConstructorShape.CnstFuncConsShape.type :: provingground.ConstructorShape.FuncConsShape.type :: shapeless.HNil) :: shapeless.HNil,provingground.HoTT.Term,provingground.HoTT.Term :: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] :: shapeless.HNil] = ConstructorSeqTL(Cons(nil,IdShape(),Cons(cons,CnstFuncConsShape(A : 𝒰 _0,FuncConsShape(IdIterShape(),IdShape())),Empty())),List(A) : 𝒰 _0)

scala> val nil :: cons :: HNil = ListAInd.intros
nil: provingground.HoTT.Term = nil : (List(A) : 𝒰 _0)
cons: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = cons : ((A : 𝒰 _0) → ((List(A) : 𝒰 _0) → (List(A) : 𝒰 _0)))
```

We can define the size of a list as a natural number recursively.

```scala
scala> val recLN = ListAInd.rec(Nat)
recLN: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = (RecSym(nil : (List(A) : 𝒰 _0)) : (Nat : 𝒰 _0)) ↦ ((RecSym(cons : ((A : 𝒰 _0) → ((List(A) : 𝒰 _0) → (List(A) : 𝒰 _0)))) : ((A : 𝒰 _0) → ((List(A) : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))))) ↦ (rec(List(A) : 𝒰 _0)(Nat : 𝒰 _0)(RecSym(nil : (List(A) : 𝒰 _0)) : (Nat : 𝒰 _0))(RecSym(cons : ((A : 𝒰 _0) → ((List(A) : 𝒰 _0) → (List(A) : 𝒰 _0)))) : ((A : 𝒰 _0) → ((List(A) : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))))))

scala> recLN.typ
res20: provingground.HoTT.Typ[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]] = (Nat : 𝒰 _0) → (((A : 𝒰 _0) → ((List(A) : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) → ((List(A) : 𝒰 _0) → (Nat : 𝒰 _0)))

scala> val a = "a" :: A
a: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = a : (A : 𝒰 _0)

scala> val l = "l" :: ListA
l: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = l : (List(A) : 𝒰 _0)

scala> val n = "n" :: Nat
n: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = n : (Nat : 𝒰 _0)

scala> val size = recLN(zero)(a :-> (l :-> (n :-> (succ(n)))))
size: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] = rec(List(A) : 𝒰 _0)(Nat : 𝒰 _0)(0 : (Nat : 𝒰 _0))((a : (A : 𝒰 _0)) ↦ ((l : (List(A) : 𝒰 _0)) ↦ ((n : (Nat : 𝒰 _0)) ↦ ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))))

scala> size(nil)
res21: provingground.HoTT.Term = 0 : (Nat : 𝒰 _0)

scala> size(cons(a)(cons(a)(nil)))
res22: provingground.HoTT.Term = (succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)
```

Another interesting inductive type is a binary rooted tree. This is our first description.
We define the number of vertices recursively on this.

```scala
scala> val T ="Tree" :: Type
T: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = Tree : 𝒰 _0

scala> val TInd = ("leaf" ::: T) |: ("node" ::: T -->>: T -->>: T) =: T
TInd: provingground.ConstructorSeqTL[shapeless.HNil :: (provingground.ConstructorShape.FuncConsShape.type :: provingground.ConstructorShape.FuncConsShape.type :: shapeless.HNil) :: shapeless.HNil,provingground.HoTT.Term,provingground.HoTT.Term :: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] :: shapeless.HNil] = ConstructorSeqTL(Cons(leaf,IdShape(),Cons(node,FuncConsShape(IdIterShape(),FuncConsShape(IdIterShape(),IdShape())),Empty())),Tree : 𝒰 _0)

scala> val leaf :: node :: HNil = TInd.intros
leaf: provingground.HoTT.Term = leaf : (Tree : 𝒰 _0)
node: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = node : ((Tree : 𝒰 _0) → ((Tree : 𝒰 _0) → (Tree : 𝒰 _0)))

scala> val t = node(node(leaf)(node(leaf)(leaf)))(node(leaf)(leaf))
t: provingground.HoTT.Term = ((node : ((Tree : 𝒰 _0) → ((Tree : 𝒰 _0) → (Tree : 𝒰 _0)))) (((node : ((Tree : 𝒰 _0) → ((Tree : 𝒰 _0) → (Tree : 𝒰 _0)))) (leaf : (Tree : 𝒰 _0)) : ((Tree : 𝒰 _0) → (Tree : 𝒰 _0))) (((node : ((Tree : 𝒰 _0) → ((Tree : 𝒰 _0) → (Tree : 𝒰 _0)))) (leaf : (Tree : 𝒰 _0)) : ((Tree : 𝒰 _0) → (Tree : 𝒰 _0))) (leaf : (Tree : 𝒰 _0)) : (Tree : 𝒰 _0)) : (Tree : 𝒰 _0)) : ((Tree : 𝒰 _0) → (Tree : 𝒰 _0))) (((node : ((Tree : 𝒰 _0) → ((Tree : 𝒰 _0) → (Tree : 𝒰 _0)))) (leaf : (Tree : 𝒰 _0)) : ((Tree : 𝒰 _0) → (Tree : 𝒰 _0))) (leaf : (Tree : 𝒰 _0)) : (Tree : 𝒰 _0)) : (Tree : 𝒰 _0)

scala> val recTN = TInd.rec(Nat)
recTN: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = (RecSym(leaf : (Tree : 𝒰 _0)) : (Nat : 𝒰 _0)) ↦ ((RecSym(node : ((Tree : 𝒰 _0) → ((Tree : 𝒰 _0) → (Tree : 𝒰 _0)))) : ((Tree : 𝒰 _0) → ((Nat : 𝒰 _0) → ((Tree : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))))) ↦ (rec(Tree : 𝒰 _0)(Nat : 𝒰 _0)(RecSym(leaf : (Tree : 𝒰 _0)) : (Nat : 𝒰 _0))(RecSym(node : ((Tree : 𝒰 _0) → ((Tree : 𝒰 _0) → (Tree : 𝒰 _0)))) : ((Tree : 𝒰 _0) → ((Nat : 𝒰 _0) → ((Tree : 𝒰 _0) → ((Nat ...

scala> recTN.typ
res23: provingground.HoTT.Typ[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]] = (Nat : 𝒰 _0) → (((Tree : 𝒰 _0) → ((Nat : 𝒰 _0) → ((Tree : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))))) → ((Tree : 𝒰 _0) → (Nat : 𝒰 _0)))

scala> val t1 = "t1" :: T
t1: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = t1 : (Tree : 𝒰 _0)

scala> val t2 = "t2" :: T
t2: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = t2 : (Tree : 𝒰 _0)

scala> val vertices = recTN(one)(t1 :-> (m :->( t2 :-> (n :-> (succ(add(n)(m))  ) ) ) ) )
vertices: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] = rec(Tree : 𝒰 _0)(Nat : 𝒰 _0)((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0))((t1 : (Tree : 𝒰 _0)) ↦ ((m : (Nat : 𝒰 _0)) ↦ ((t2 : (Tree : 𝒰 _0)) ↦ ((n : (Nat : 𝒰 _0)) ↦ ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((rec(Nat : 𝒰 _0)((Nat : 𝒰 _0) → (Nat : 𝒰 _0))((m : (Nat : 𝒰 _0)) ↦ (m : (Nat : 𝒰 _0)))((n : (Nat : 𝒰 _0)) ↦ ((add(n) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ↦ ((m : (Nat : 𝒰 _0)) ↦ ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((add(n) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))))) (n : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0))))))

scala> vertices(t)
res24: provingground.HoTT.Term = (succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)

scala> val nine = succ(add(four)(four))
nine: provingground.HoTT.Term = (succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)

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
scala> val BT ="BinTree" :: Type
BT: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = BinTree : 𝒰 _0

scala> val BTInd = ("leaf" ::: BT) |: ("node" ::: (Bool -|>: BT) -->>: BT )  =: BT
BTInd: provingground.ConstructorSeqTL[shapeless.HNil :: (provingground.ConstructorShape.FuncConsShape.type :: shapeless.HNil) :: shapeless.HNil,provingground.HoTT.Term,provingground.HoTT.Term :: provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Term] :: shapeless.HNil] = ConstructorSeqTL(Cons(leaf,IdShape(),Cons(node,FuncConsShape(FuncShape(Boolean : 𝒰 _0,IdIterShape()),IdShape()),Empty())),BinTree : 𝒰 _0)

scala> val leaf :: node :: HNil = BTInd.intros
leaf: provingground.HoTT.Term = leaf : (BinTree : 𝒰 _0)
node: provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Term] = node : (((Boolean : 𝒰 _0) → (BinTree : 𝒰 _0)) → (BinTree : 𝒰 _0))

scala> val recBTN = BTInd.rec(Nat)
recBTN: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Term]],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = (RecSym(leaf : (BinTree : 𝒰 _0)) : (Nat : 𝒰 _0)) ↦ ((RecSym(node : (((Boolean : 𝒰 _0) → (BinTree : 𝒰 _0)) → (BinTree : 𝒰 _0))) : (((Boolean : 𝒰 _0) → (BinTree : 𝒰 _0)) → (((Boolean : 𝒰 _0) → (Nat : 𝒰 _0)) → (Nat : 𝒰 _0)))) ↦ (rec(BinTree : 𝒰 _0)(Nat : 𝒰 _0)(RecSym(leaf : (BinTree : 𝒰 _0)) : (Nat : 𝒰 _0))(RecSym(node : (((Boolean : 𝒰 _0) → (BinTree : 𝒰 _0)) → (BinTree : 𝒰 _0))) : (((Boolean : 𝒰 _0) →...

scala> recBTN.typ
res27: provingground.HoTT.Typ[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term],provingground.HoTT.Term]],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]] = (Nat : 𝒰 _0) → ((((Boolean : 𝒰 _0) → (BinTree : 𝒰 _0)) → (((Boolean : 𝒰 _0) → (Nat : 𝒰 _0)) → (Nat : 𝒰 _0))) → ((BinTree : 𝒰 _0) → (Nat : 𝒰 _0)))

scala> val f = "f" :: Bool ->: BT
f: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = f : ((Boolean : 𝒰 _0) → (BinTree : 𝒰 _0))

scala> val g = "g" :: Bool ->: Nat
g: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = g : ((Boolean : 𝒰 _0) → (Nat : 𝒰 _0))

scala> val leaves = recBTN(one)(f :-> (g :-> (add(g(ff))(g(tt))) ))
leaves: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] = rec(BinTree : 𝒰 _0)(Nat : 𝒰 _0)((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0))((f : ((Boolean : 𝒰 _0) → (BinTree : 𝒰 _0))) ↦ ((g : ((Boolean : 𝒰 _0) → (Nat : 𝒰 _0))) ↦ (((rec(Nat : 𝒰 _0)((Nat : 𝒰 _0) → (Nat : 𝒰 _0))((m : (Nat : 𝒰 _0)) ↦ (m : (Nat : 𝒰 _0)))((n : (Nat : 𝒰 _0)) ↦ ((add(n) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ↦ ((m : (Nat : 𝒰 _0)) ↦ ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((add(n) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))))) ((g : ((Boolean : 𝒰 _0) → (Nat : 𝒰 _0))) (false : (Boolean : 𝒰 _0)) : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((g : ((Boolean : 𝒰 _0) → (Nat : 𝒰 _0))) (true : (Boole...

scala> leaves(leaf)
res28: provingground.HoTT.Term = (succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)

scala> val b = "b" :: Bool
b: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = b : (Boolean : 𝒰 _0)

scala> val t = node(b :-> leaf)
t: provingground.HoTT.Term = (node : (((Boolean : 𝒰 _0) → (BinTree : 𝒰 _0)) → (BinTree : 𝒰 _0))) ((b : (Boolean : 𝒰 _0)) ↦ (leaf : (BinTree : 𝒰 _0))) : (BinTree : 𝒰 _0)

scala> val recBBT = BoolInd.rec(BT)
recBBT: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = (RecSym(true : (Boolean : 𝒰 _0)) : (BinTree : 𝒰 _0)) ↦ ((RecSym(false : (Boolean : 𝒰 _0)) : (BinTree : 𝒰 _0)) ↦ (rec(Boolean : 𝒰 _0)(BinTree : 𝒰 _0)(RecSym(true : (Boolean : 𝒰 _0)) : (BinTree : 𝒰 _0))(RecSym(false : (Boolean : 𝒰 _0)) : (BinTree : 𝒰 _0))))

scala> recBBT.typ
res29: provingground.HoTT.Typ[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]] = (BinTree : 𝒰 _0) → ((BinTree : 𝒰 _0) → ((Boolean : 𝒰 _0) → (BinTree : 𝒰 _0)))

scala> val ttn = recBBT(leaf)(t)
ttn: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] = rec(Boolean : 𝒰 _0)(BinTree : 𝒰 _0)(leaf : (BinTree : 𝒰 _0))((node : (((Boolean : 𝒰 _0) → (BinTree : 𝒰 _0)) → (BinTree : 𝒰 _0))) ((b : (Boolean : 𝒰 _0)) ↦ (leaf : (BinTree : 𝒰 _0))) : (BinTree : 𝒰 _0))

scala> val t2 = node(ttn)
t2: provingground.HoTT.Term = (node : (((Boolean : 𝒰 _0) → (BinTree : 𝒰 _0)) → (BinTree : 𝒰 _0))) (rec(Boolean : 𝒰 _0)(BinTree : 𝒰 _0)(leaf : (BinTree : 𝒰 _0))((node : (((Boolean : 𝒰 _0) → (BinTree : 𝒰 _0)) → (BinTree : 𝒰 _0))) ((b : (Boolean : 𝒰 _0)) ↦ (leaf : (BinTree : 𝒰 _0))) : (BinTree : 𝒰 _0))) : (BinTree : 𝒰 _0)

scala> leaves(t2)
res30: provingground.HoTT.Term = (succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)
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
recNN: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = (RecSym(0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) ↦ ((RecSym(succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0)))) ↦ (rec(Nat : 𝒰 _0)(Nat : 𝒰 _0)(RecSym(0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0))(RecSym(succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) : ((Nat : 𝒰 _0) → ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))))))

scala> val double = recNN(zero)(m :-> (n :-> (succ(succ(n)))))
double: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] = rec(Nat : 𝒰 _0)(Nat : 𝒰 _0)(0 : (Nat : 𝒰 _0))((m : (Nat : 𝒰 _0)) ↦ ((n : (Nat : 𝒰 _0)) ↦ ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0))))

scala> double(two) == four
res31: Boolean = true

scala> assert(double(two) == four)

scala> double(succ(n))
res33: provingground.HoTT.Term = (succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((rec(Nat : 𝒰 _0)(Nat : 𝒰 _0)(0 : (Nat : 𝒰 _0))((m : (Nat : 𝒰 _0)) ↦ ((n : (Nat : 𝒰 _0)) ↦ ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0))))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)
```

All our recursive definitions so far of functions `f` have ignored `n` in defining `f(succ(n))`,
and are only in terms of `f(n)`. We see a more complex definition, the sum of numbers up to `n`.
Note that we are defining `sumTo(succ(m))` in terms of `m` and `n = sumTo(m)`, so this is `add(succ(m))(n)`

```scala
scala> val sumTo = recNN(zero)(m :-> (n :-> (add(succ(m))(n))))
sumTo: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] = rec(Nat : 𝒰 _0)(Nat : 𝒰 _0)(0 : (Nat : 𝒰 _0))((m : (Nat : 𝒰 _0)) ↦ ((n : (Nat : 𝒰 _0)) ↦ ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (((rec(Nat : 𝒰 _0)((Nat : 𝒰 _0) → (Nat : 𝒰 _0))((m : (Nat : 𝒰 _0)) ↦ (m : (Nat : 𝒰 _0)))((n : (Nat : 𝒰 _0)) ↦ ((add(n) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ↦ ((m : (Nat : 𝒰 _0)) ↦ ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((add(n) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)))))) (m : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0))))

scala> sumTo(one)
res34: provingground.HoTT.Term = (succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)

scala> sumTo(three).fansi
res35: String = succ(succ(succ(succ(succ(succ(0))))))

scala> val ten = succ(nine)
ten: provingground.HoTT.Term = (succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)

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
V: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]]] = Vec : ((Nat : 𝒰 _0) → (𝒰 _0))

scala> val nilv = "nil" :: V(zero)
nilv: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = nil : ((Vec : ((Nat : 𝒰 _0) → (𝒰 _0))) (0 : (Nat : 𝒰 _0)) : 𝒰 _0)

scala> val consv = "cons" :: n ~>: (Nat ->: V(n) ->: V(succ(n)))
consv: provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] with provingground.HoTT.Subs[provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]]] = cons : (n : (Nat : 𝒰 _0) ~> (Nat : 𝒰 _0) → (((Vec : ((Nat : 𝒰 _0) → (𝒰 _0))) (n : (Nat : 𝒰 _0)) : 𝒰 _0) → ((Vec : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0)))
```

We have an induction function taking data for the cases and returning a dependent function.
This is defined by giving data for cases corresponding to the constructors.
Namely to define the dependent function `f`, we must specify

* `f(zero)` of type `V(zero)`
* `f(succ(m))` of type `V(succ(m))`, as a dependent function of `m` and of `f(m) : V(m)`.


We define inductively a countdown function, giving the vector counting down from `n`.

```scala
scala> val indNV = NatInd.induc(V)
indNV: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.FuncLike[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.HoTT.FuncLike[provingground.HoTT.Term,provingground.HoTT.Term]]] = (InducSym(0 : (Nat : 𝒰 _0)) : ((Vec : ((Nat : 𝒰 _0) → (𝒰 _0))) (0 : (Nat : 𝒰 _0)) : 𝒰 _0)) ↦ ((InducSym(succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) : ($oujo : (Nat : 𝒰 _0) ~> ((Vec : ((Nat : 𝒰 _0) → (𝒰 _0))) ($oujo : (Nat : 𝒰 _0)) : 𝒰 _0) → ((Vec : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ($oujo : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0))) ↦ (ind(Nat : 𝒰 _0)(Vec : ((Nat : 𝒰 _0) → (𝒰 _0)))(InducSym(0 : (Nat : 𝒰 _0)) : ((Vec : ((Nat : 𝒰 _0) → (𝒰 _0))) (0 : (Nat : 𝒰 _0...

scala> val v = "v_m" :: V(m)
v: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = v_m : ((Vec : ((Nat : 𝒰 _0) → (𝒰 _0))) (m : (Nat : 𝒰 _0)) : 𝒰 _0)

scala> val countdown = indNV(nilv)(m :~> (v :-> consv(m)(succ(m))(v)) )
countdown: provingground.HoTT.FuncLike[provingground.HoTT.Term,provingground.HoTT.Term] = ind(Nat : 𝒰 _0)(Vec : ((Nat : 𝒰 _0) → (𝒰 _0)))(nil : ((Vec : ((Nat : 𝒰 _0) → (𝒰 _0))) (0 : (Nat : 𝒰 _0)) : 𝒰 _0))((m : (Nat : 𝒰 _0)) ↦ ((v_m : ((Vec : ((Nat : 𝒰 _0) → (𝒰 _0))) (m : (Nat : 𝒰 _0)) : 𝒰 _0)) ↦ ((((cons : (n : (Nat : 𝒰 _0) ~> (Nat : 𝒰 _0) → (((Vec : ((Nat : 𝒰 _0) → (𝒰 _0))) (n : (Nat : 𝒰 _0)) : 𝒰 _0) → ((Vec : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0)))) (m : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (((Vec : ((Nat : 𝒰 _0) → (𝒰 _0))) (m : (Nat : 𝒰 _0)) : 𝒰 _0) → ((Vec : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (m : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0)))) ((succ : ((Nat ...

scala> countdown(zero)
res38: provingground.HoTT.Term = nil : ((Vec : ((Nat : 𝒰 _0) → (𝒰 _0))) (0 : (Nat : 𝒰 _0)) : 𝒰 _0)

scala> countdown(one)
res39: provingground.HoTT.Term = (((cons : (n : (Nat : 𝒰 _0) ~> (Nat : 𝒰 _0) → (((Vec : ((Nat : 𝒰 _0) → (𝒰 _0))) (n : (Nat : 𝒰 _0)) : 𝒰 _0) → ((Vec : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0)))) (0 : (Nat : 𝒰 _0)) : ((Nat : 𝒰 _0) → (((Vec : ((Nat : 𝒰 _0) → (𝒰 _0))) (0 : (Nat : 𝒰 _0)) : 𝒰 _0) → ((Vec : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0)))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (((Vec : ((Nat : 𝒰 _0) → (𝒰 _0))) (0 : (Nat : 𝒰 _0)) : 𝒰 _0) → ((Vec : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0))) (nil : ((Vec : ((N...

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
isEven: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]]] = isEven : ((Nat : 𝒰 _0) → (𝒰 _0))

scala> val zeroEven = "0even" :: isEven(zero)
zeroEven: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = 0even : ((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) (0 : (Nat : 𝒰 _0)) : 𝒰 _0)

scala> val plusTwoEven = "_+2even" :: (n ~>: (isEven(n) ->: isEven(succ(succ(n)))))
plusTwoEven: provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] with provingground.HoTT.Subs[provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = _+2even : (n : (Nat : 𝒰 _0) ~> ((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) (n : (Nat : 𝒰 _0)) : 𝒰 _0) → ((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0))
```

One can directly see that two and four are even.

```scala
scala> val TwoEven = plusTwoEven(zero)(zeroEven)  !: isEven(two)
TwoEven: provingground.HoTT.Term = ((_+2even : (n : (Nat : 𝒰 _0) ~> ((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) (n : (Nat : 𝒰 _0)) : 𝒰 _0) → ((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) (0 : (Nat : 𝒰 _0)) : 𝒰 _0) → ((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0))) (0even : ((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) (0 : (Nat : 𝒰 _0)) : 𝒰 _0)) : ((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : ?...

scala> val FourEven = plusTwoEven(two)(TwoEven) !: isEven(four)
FourEven: provingground.HoTT.Term = ((_+2even : (n : (Nat : 𝒰 _0) ~> ((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) (n : (Nat : 𝒰 _0)) : 𝒰 _0) → ((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0) → ((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _...
```

Here is a simple proof by induction. We prove the statement that the _double_ of every natural number is even.
The `induc` method gives a dependent function, which takes the base case and the induction step as arguments.
The _base case_ is inhabited by the constructor of type `isEven(zero)`.
The _induction step_ for `n` is a term of type `isEven(double(succ(n)))` as a function of `n` and
the _induction hypothesis_. Note that the induction hypothesis is a term of type `isEven(double(n))`.

```scala
scala> val thmDoubleEven = n ~>: isEven(double(n))
thmDoubleEven: provingground.HoTT.GenFuncTyp[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term] = n : (Nat : 𝒰 _0) ~> (isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((rec(Nat : 𝒰 _0)(Nat : 𝒰 _0)(0 : (Nat : 𝒰 _0))((m : (Nat : 𝒰 _0)) ↦ ((n : (Nat : 𝒰 _0)) ↦ ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0))))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0

scala> val hyp = "isEven(double(n))" :: isEven(double(n))
hyp: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = isEven(double(n)) : ((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((rec(Nat : 𝒰 _0)(Nat : 𝒰 _0)(0 : (Nat : 𝒰 _0))((m : (Nat : 𝒰 _0)) ↦ ((n : (Nat : 𝒰 _0)) ↦ ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0))))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0)

scala> val inducDoubleEven = NatInd.induc(n :-> isEven(double(n)))
inducDoubleEven: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.FuncLike[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.HoTT.FuncLike[provingground.HoTT.Term,provingground.HoTT.Term]]] = (InducSym(0 : (Nat : 𝒰 _0)) : ((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) (0 : (Nat : 𝒰 _0)) : 𝒰 _0)) ↦ ((InducSym(succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) : ($plyy : (Nat : 𝒰 _0) ~> ((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((rec(Nat : 𝒰 _0)(Nat : 𝒰 _0)(0 : (Nat : 𝒰 _0))((m : (Nat : 𝒰 _0)) ↦ ((n : (Nat : 𝒰 _0)) ↦ ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0))))) ($plyy : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 ...

scala> val pfDoubleEven =
     |   inducDoubleEven(
     |     zeroEven){
     |       n :~> (
     |         hyp :-> (
     |           plusTwoEven(double(n))(hyp)
     |           )
     |           )
     |     }  !: thmDoubleEven
pfDoubleEven: provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term] = ind(Nat : 𝒰 _0)((n : (Nat : 𝒰 _0)) ↦ ((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((rec(Nat : 𝒰 _0)(Nat : 𝒰 _0)(0 : (Nat : 𝒰 _0))((m : (Nat : 𝒰 _0)) ↦ ((n : (Nat : 𝒰 _0)) ↦ ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0))))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0))(0even : ((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) (0 : (Nat : 𝒰 _0)) : 𝒰 _0))((n : (Nat : 𝒰 _0)) ↦ ((isEven(double(n)) : ((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((rec(Nat : 𝒰 _0)(Nat : 𝒰 _0)(0 : (Nat : 𝒰 _0))((m : (Nat : 𝒰 _0)) ↦ ((n : (Nat : 𝒰 _0)) ↦ ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰...
```

We next prove a more interesting statement, namely that for any natural number `n`, one of `n` and `n+1` is even.

```scala
scala> val succEven = n :-> (isEven(n) || isEven(succ(n)))
succEven: provingground.HoTT.Func[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.PlusTyp[provingground.HoTT.Term,provingground.HoTT.Term]] = (n : (Nat : 𝒰 _0)) ↦ (PlusTyp((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) (n : (Nat : 𝒰 _0)) : 𝒰 _0,(isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0))

scala> val base = succEven(zero).incl1(zeroEven) !: succEven(zero)
base: provingground.HoTT.Term = FirstIncl(PlusTyp((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) (0 : (Nat : 𝒰 _0)) : 𝒰 _0,(isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0),0even : ((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) (0 : (Nat : 𝒰 _0)) : 𝒰 _0))

scala> val thmSuccEven = n ~>: (succEven(n))
thmSuccEven: provingground.HoTT.GenFuncTyp[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term] = n : (Nat : 𝒰 _0) ~> PlusTyp((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) (n : (Nat : 𝒰 _0)) : 𝒰 _0,(isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0)

scala> val hyp1 = "n-is-Even" :: isEven(n)
hyp1: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = n-is-Even : ((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) (n : (Nat : 𝒰 _0)) : 𝒰 _0)

scala> val hyp2 = "(n+1)-is-Even" :: isEven(succ(n))
hyp2: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = (n+1)-is-Even : ((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0)

scala> val step = (succEven(n).rec(succEven(succ(n)))){hyp1 :-> (succEven(succ(n)).incl2(plusTwoEven(n)(hyp1)))}{hyp2 :-> (succEven(succ(n)).incl1((hyp2)))}
step: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] = rec(PlusTyp((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) (n : (Nat : 𝒰 _0)) : 𝒰 _0,(isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0))(PlusTyp((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0,(isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0))((n-is-Even : ((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) (n : (Nat : 𝒰 _0)) : 𝒰 _0)) ↦ (ScndIncl(PlusTyp((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _...

scala> val inducSuccEven = NatInd.induc(succEven)
inducSuccEven: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.FuncLike[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.HoTT.FuncLike[provingground.HoTT.Term,provingground.HoTT.Term]]] = (InducSym(0 : (Nat : 𝒰 _0)) : (PlusTyp((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) (0 : (Nat : 𝒰 _0)) : 𝒰 _0,(isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0))) ↦ ((InducSym(succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) : ($pran : (Nat : 𝒰 _0) ~> (PlusTyp((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ($pran : (Nat : 𝒰 _0)) : 𝒰 _0,(isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) ($pran : (Nat : 𝒰 _0)) : (Nat : ...

scala> val pf = inducSuccEven(base)(n :~> step) !: thmSuccEven
pf: provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term] = ind(Nat : 𝒰 _0)((n : (Nat : 𝒰 _0)) ↦ (PlusTyp((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) (n : (Nat : 𝒰 _0)) : 𝒰 _0,(isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0)))(FirstIncl(PlusTyp((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) (0 : (Nat : 𝒰 _0)) : 𝒰 _0,(isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : 𝒰 _0),0even : ((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) (0 : (Nat : 𝒰 _0)) : 𝒰 _0)))((n : (Nat : 𝒰 _0)) ↦ (rec(PlusTyp((isEven : ((Nat : 𝒰 _0) → (𝒰 _0))) (n : (Nat : 𝒰 _0)) : 𝒰 _0,(isEven : ((Nat : 𝒰 _0) → (𝒰 _0))...
```

We now prove a result that has been a goal, namely that for a function on Natural numbers if `f(n)=f(n+1)` for all n,
`f` is constant.

```scala
scala> val f = "f" :: Nat ->: A
f: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))

scala> val ass = "assumption" :: n ~>: (f(n) =:= f(succ(n)))
ass: provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term]] = assumption : (n : (Nat : 𝒰 _0) ~> (f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (A : 𝒰 _0) = (f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (A : 𝒰 _0))

scala> val claim = n :-> (f(zero) =:= f(n))
claim: provingground.HoTT.Func[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.IdentityTyp[provingground.HoTT.Term]] = (n : (Nat : 𝒰 _0)) ↦ ((f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (A : 𝒰 _0) = (f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (A : 𝒰 _0))

scala> val base = f(zero).refl
base: provingground.HoTT.Refl[provingground.HoTT.Term] = Refl(A : 𝒰 _0,(f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (A : 𝒰 _0))

scala> val hyp = "hypothesis" :: (f(zero) =:= f(n))
hyp: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = hypothesis : ((f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (A : 𝒰 _0) = (f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (A : 𝒰 _0))

scala> val step = hyp :-> {IdentityTyp.trans(A)(f(zero))(f(n))(f(succ(n)))(hyp)(ass(n)) }
step: provingground.HoTT.Func[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term] = (hypothesis : ((f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (A : 𝒰 _0) = (f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (A : 𝒰 _0))) ↦ (((ind((f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (A : 𝒰 _0) = (f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (A : 𝒰 _0))(provingground.HoTT$IdentityTyp$InducFn$$Lambda$27671/825173499@590c0d28)(($ptnf : (A : 𝒰 _0)) ↦ (($pude : ($ptnf : (A : 𝒰 _0) = (f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) ((succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (Nat : 𝒰 _0)) : (A : 𝒰 _0))) ↦ ($pude : ($ptnf : (A : 𝒰 _0) = (f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) ((succ : ((Nat : 𝒰 ...

scala> val inducClaim = NatInd.induc(claim)
inducClaim: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.FuncLike[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.HoTT.FuncLike[provingground.HoTT.Term,provingground.HoTT.Term]]] = (InducSym(0 : (Nat : 𝒰 _0)) : ((f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (A : 𝒰 _0) = (f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (A : 𝒰 _0))) ↦ ((InducSym(succ : ((Nat : 𝒰 _0) → (Nat : 𝒰 _0))) : ($pvkf : (Nat : 𝒰 _0) ~> ((f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (A : 𝒰 _0) = (f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) ($pvkf : (Nat : 𝒰 _0)) : (A : 𝒰 _0)) → ((f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (A : 𝒰 _0) = (f : ((Nat : 𝒰 _0) → (A :...

scala> val pf = inducClaim(base)(n :~> step) !: (n ~>: (f(zero) =:= f(n)))
pf: provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term] = ind(Nat : 𝒰 _0)((n : (Nat : 𝒰 _0)) ↦ ((f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (A : 𝒰 _0) = (f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (A : 𝒰 _0)))(Refl(A : 𝒰 _0,(f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (A : 𝒰 _0)))((n : (Nat : 𝒰 _0)) ↦ ((hypothesis : ((f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (A : 𝒰 _0) = (f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (A : 𝒰 _0))) ↦ (((ind((f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) (0 : (Nat : 𝒰 _0)) : (A : 𝒰 _0) = (f : ((Nat : 𝒰 _0) → (A : 𝒰 _0))) (n : (Nat : 𝒰 _0)) : (A : 𝒰 _0))(provingground.HoTT$IdentityTyp$InducFn$$Lambda$27671/82...
```


## Indexed Inductive types

A generalization of inductive types are _inductive type families_, i.e., inductive types depending on an index.
Unlike parametrized inductive types (such as lists), the constructors of an inductive type family involve in general several different indices.
Further, the recursion and induction function only allow construction of (dependent) functions on the whole family.

A typical example is vectors, defined as a family indexed by their length.

```scala
val IndN = new IndexedConstructorPatterns(Nat ->: Types)
val Vec = "Vec" :: Nat ->: Type
val VecPtn = new IndexedConstructorPatterns(Nat ->: Types)
val VecFmly = VecPtn.Family(Vec)
val VecInd = {"nil" ::: VecFmly.head(Vec(zero))} |:  {"cons" ::: n ~>>: (A ->>: Vec(n) -->>: VecFmly.head(Vec(succ(n))))} =: VecFmly
val vnil :: vcons :: HNil = VecInd.intros
vcons.typ.fansi
```

We can define function recursively on vectors of all indices. For instance, we can define the size.

```scala
val vn = "v_n" :: Vec(n)
val recVN = VecInd.rec(Nat)
val size = recVN(zero)(n :~>(a :-> (vn :->(m :->(succ(m))))))
size(zero)(vnil)
val v1 = vcons(zero)(a)(vnil)
size(one)(v1)
assert(size(one)(v1) == one)
```

For a more interesting example, we consider vectors with entries natural numbers, and define the sum of entries.

```scala
val VecN = "Vec(Nat)" ::: Nat ->: Types
val VecNFmly = VecPtn.Family(VecN)

val vnn = "v_n" :: VecN(n)
val VecNInd = {"nil" ::: VecNFmly.head(VecN(zero))} |:  {"cons" ::: n ~>>: (Nat ->>: VecN(n) -->>: VecNFmly.head(VecN(succ(n))))} =: VecNFmly

val recVNN = VecNInd.rec(Nat)
val vnilN :: vconsN :: Hnil = VecNInd.intros

val k ="k" :: Nat
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
