---
title: HoTT
layout: page
---

## Provingground - Basic Homotopy Type theory implementation

These notes concern the object _HoTT_, which has the core implementation of homotopy type theory.

The major components of homotopy type theory implemented in the object HoTT are

* Terms, types and Universes.
* Function and dependent function types.
* λs.
* Pairs and Dependent pairs.
* Disjoint union types.
* Types 0 and 1 and an object in the latter.
* Identity types

Inductive types, induction and recursion are in different objects as they are rather subtle. The other major way (also not in the _HoTT_ object) of constructing non-composite types is to wrap scala types, possibly including symbolic algebra.


The _core_ project contains code that is agnostic to how it is run. In particular this also compiles to scala-js.




### Universes, Symbolic types

We have a family of universes, but mostly use the first one denoted by Type. Given a type, we can construct symbolic objects of that type. We construct such a type _A_.


```scala
scala> import provingground._
import provingground._

scala> import HoTT._
import HoTT._

scala> val A ="A" :: Type
A: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = A : 𝒰 _0

scala> A == Type.::("A")
res0: Boolean = true
```

We consider a symbolic object of the type _A_


```scala
scala> val a ="a" :: A
a: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = a : (A : 𝒰 _0)
```


## Function types, lambdas, Identity

Given types A and B, we have the function type A → B. An element of this is a function from A to B.

We can construct functions using λ's. Here, for the type _A_, we construct the identity on _A_ using a lambda. We can then view this as a dependent function of _A_, giving the identity function.

In this definition, two λ's are used, with the method _lmbda_ telling the TypecompilerType that the result is a (non-dependent) function.


```scala
scala> val id = lambda(A)(lmbda(a)(a))
id: provingground.HoTT.FuncLike[provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]],provingground.HoTT.Func[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term]]] = (A : 𝒰 _0) ↦ ((a : (A : 𝒰 _0)) ↦ (a : (A : 𝒰 _0)))
```


The type of the identity function is a mixture of Pi-types and function types. Which of these to use is determined by checking dependence of the type of the value on the varaible in a λ-definition.


```scala
scala> id.typ
res1: provingground.HoTT.Typ[provingground.HoTT.FuncLike[provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]],provingground.HoTT.Func[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term]]]] = A : 𝒰 _0 ~> (A : 𝒰 _0) → (A : 𝒰 _0)

scala> lmbda(a)(a).typ
res2: provingground.HoTT.Typ[provingground.HoTT.Func[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term]]] = (A : 𝒰 _0) → (A : 𝒰 _0)

scala> lmbda(a)(a).typ.dependsOn(A)
res3: Boolean = true
```


The lambdas have the same effect at runtime. It is checked if the type of the value depends on the variable.
The result is either _LambdaFixed_ or _Lambda_ accordingly.


```scala
scala> val indep = lmbda(a)(a)
indep: provingground.HoTT.Func[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term]] = (a : (A : 𝒰 _0)) ↦ (a : (A : 𝒰 _0))

scala> val dep = lambda(a)(a)
dep: provingground.HoTT.FuncLike[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term]] = (a : (A : 𝒰 _0)) ↦ (a : (A : 𝒰 _0))

scala> indep == dep
res4: Boolean = true
```

Note that we have alternative notation for lambdas, the maps to methods `:->` and `:~>`.
For instance, we can define the identity using these.

```scala
scala> assert(id == A :~> (a :-> a))
```

### Hygiene for λs

A new variable object, which has the same toString, is created in making lambdas. This is to avoid name clashes.


```scala
scala> val l = dep.asInstanceOf[LambdaFixed[Term, Term]]
l: provingground.HoTT.LambdaFixed[provingground.HoTT.Term,provingground.HoTT.Term] = (a : (A : 𝒰 _0)) ↦ (a : (A : 𝒰 _0))

scala> l.variable
res6: provingground.HoTT.Term = a : (A : 𝒰 _0)

scala> l.variable == a
res7: Boolean = false
```

## Modus Ponens

We construct Modus Ponens, as an object in Homotopy Type theory. Note that A ->: B is the function type A → B.


```scala
scala> val B = "B" :: Type
B: provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]] = B : 𝒰 _0

scala> val f = "f" :: (A ->: B)
f: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = f : ((A : 𝒰 _0) → (B : 𝒰 _0))

scala> val mp = lambda(A)(lambda(B)(lmbda(a)(lmbda(f)(f(a)))))
mp: provingground.HoTT.FuncLike[provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]],provingground.HoTT.FuncLike[provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]],provingground.HoTT.Func[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.HoTT.Term]]]] = (A : 𝒰 _0) ↦ ((B : 𝒰 _0) ↦ ((a : (A : 𝒰 _0)) ↦ ((f : ((A : 𝒰 _0) → (B : 𝒰 _0))) ↦ ((f : ((A : 𝒰 _0) → (B : 𝒰 _0))) (a : (A : 𝒰 _0)) : (B : 𝒰 _0)...
```

The type of Modus Ponens is again a mixture of Pi-types and function types.


```scala
scala> mp.typ
res8: provingground.HoTT.Typ[provingground.HoTT.FuncLike[provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]],provingground.HoTT.FuncLike[provingground.HoTT.Typ[provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Typ[provingground.HoTT.Term]],provingground.HoTT.Func[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.HoTT.Term]]]]] = A : 𝒰 _0 ~> B : 𝒰 _0 ~> (A : 𝒰 _0) → (((A : 𝒰 _0) → (B : 𝒰 _0)) → (B : 𝒰 _0))
```


We can apply modus ponens with the roles of _A_ and _B_ reversed. This still works because variable clashes are avoided.


```scala
scala> val mpBA = mp(B)(A)
mpBA: provingground.HoTT.Func[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.Func[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]],provingground.HoTT.Term]] = (a : (B : 𝒰 _0)) ↦ ((f : ((B : 𝒰 _0) → (A : 𝒰 _0))) ↦ ((f : ((B : 𝒰 _0) → (A : 𝒰 _0))) (a : (B : 𝒰 _0)) : (A : 𝒰 _0)))

scala> mpBA.typ == B ->: (B ->: A) ->: A
res9: Boolean = true
```


### Equality of λs

Lambdas do not depend on the name of the variable.


```scala
scala> val aa = "aa" :: A
aa: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = aa : (A : 𝒰 _0)

scala> lmbda(aa)(aa) == lmbda(a)(a)
res10: Boolean = true

scala> (lmbda(aa)(aa))(a) == a
res11: Boolean = true
```


## Dependent types

Given a type family, we can construct the corresponding Pi-types and Sigma-types. We start with a formal type family, which is just a symbolic object of the appropriate type.


```scala
scala> val Bs = "B(_ : A)" :: (A ->: Type)
Bs: provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Typ[provingground.HoTT.Term]]] = B(_ : A) : ((A : 𝒰 _0) → (𝒰 _0))
```


### Pi-Types

In addition to the case class constructor, there is an agda/shapeless-like  convenience method for constructing Pi-types. Namely, given a type expression that depends on a varaible _a : A_, we can construct the Pi-type correspoding to the obtained λ-expression.

Note that the !: method just claims and checks a type, and is useful (e.g. here) for documentation.


```scala
scala> val fmly = (a !: A) ~>: (Bs(a) ->: A)
fmly: provingground.HoTT.GenFuncTyp[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]] = a : (A : 𝒰 _0) ~> ((B(_ : A) : ((A : 𝒰 _0) → (𝒰 _0))) (a : (A : 𝒰 _0)) : 𝒰 _0) → (A : 𝒰 _0)
```


### Sigma-types

There is also a convenience method for defining Sigma types using λs.


```scala
scala> Sgma(a !: A, Bs(a))
res12: provingground.HoTT.SigmaTyp[provingground.HoTT.Term,provingground.HoTT.Term] = ∑((a : (A : 𝒰 _0)) ↦ ((B(_ : A) : ((A : 𝒰 _0) → (𝒰 _0))) (a : (A : 𝒰 _0)) : 𝒰 _0))
```



```scala
scala> Sgma(a !: A, Bs(a) ->: Bs(a) ->: A)
res13: provingground.HoTT.SigmaTyp[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Func[provingground.HoTT.Term,provingground.HoTT.Term]]] = ∑((a : (A : 𝒰 _0)) ↦ (((B(_ : A) : ((A : 𝒰 _0) → (𝒰 _0))) (a : (A : 𝒰 _0)) : 𝒰 _0) → (((B(_ : A) : ((A : 𝒰 _0) → (𝒰 _0))) (a : (A : 𝒰 _0)) : 𝒰 _0) → (A : 𝒰 _0))))
```


## Pair types

Like functions and dependent functions, pairs and dependent pairs can be handled together. The _mkPair_ function assignes the right type after checking dependence, choosing between pair types, pairs and dependent pairs.


```scala
scala> val ba = "b(a)" :: Bs(a)
ba: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = b(a) : ((B(_ : A) : ((A : 𝒰 _0) → (𝒰 _0))) (a : (A : 𝒰 _0)) : 𝒰 _0)

scala> val b = "b" :: B
b: provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term] = b : (B : 𝒰 _0)

scala> mkPair(A, B)
res14: provingground.HoTT.AbsPair[provingground.HoTT.Term,provingground.HoTT.Term] = ((A : 𝒰 _0) , (B : 𝒰 _0))

scala> mkPair(a, b)
res15: provingground.HoTT.AbsPair[provingground.HoTT.Term,provingground.HoTT.Term] = ((a : (A : 𝒰 _0)) , (b : (B : 𝒰 _0)))

scala> mkPair(a, b).typ
res16: provingground.HoTT.Typ[U] forSome { type U >: _1.type <: provingground.HoTT.Term with provingground.HoTT.Subs[U]; val _1: provingground.HoTT.AbsPair[provingground.HoTT.Term,provingground.HoTT.Term] } = ((A : 𝒰 _0) , (B : 𝒰 _0))

scala> mkPair(a, ba).typ
res17: provingground.HoTT.Typ[U] forSome { type U >: _1.type <: provingground.HoTT.Term with provingground.HoTT.Subs[U]; val _1: provingground.HoTT.AbsPair[provingground.HoTT.Term,provingground.HoTT.Term] } = ∑((a : (A : 𝒰 _0)) ↦ ((B(_ : A) : ((A : 𝒰 _0) → (𝒰 _0))) (a : (A : 𝒰 _0)) : 𝒰 _0))
```


```scala
scala> mkPair(A, B).asInstanceOf[ProdTyp[Term, Term]]
res18: provingground.HoTT.ProdTyp[provingground.HoTT.Term,provingground.HoTT.Term] = ((A : 𝒰 _0) , (B : 𝒰 _0))
```


## Plus types

We can also construct the plus type _A plus B_, which comes with two inclusion functions.


```scala
scala> val AplusB = PlusTyp(A, B)
AplusB: provingground.HoTT.PlusTyp[provingground.HoTT.Term,provingground.HoTT.Term] = PlusTyp(A : 𝒰 _0,B : 𝒰 _0)
```


```scala
scala> AplusB.incl1(a)
res19: provingground.HoTT.PlusTyp.FirstIncl[provingground.HoTT.Term,provingground.HoTT.Term] = FirstIncl(PlusTyp(A : 𝒰 _0,B : 𝒰 _0),a : (A : 𝒰 _0))
```


```scala
scala> AplusB.incl2
res20: provingground.HoTT.Func[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term],provingground.HoTT.PlusTyp.ScndIncl[provingground.HoTT.Term,provingground.HoTT.Term]] = ($pyqy : (B : 𝒰 _0)) ↦ (ScndIncl(PlusTyp(A : 𝒰 _0,B : 𝒰 _0),$pyqy : (B : 𝒰 _0)))
```

In the above, a λ was used, with a variable automatically generated. These have names starting with $ to avoid collision with user defined ones.

## Identity type

We have an identity type associated to a type _A_, with reflexivity giving terms of this type.


```scala
scala> val eqAa = IdentityTyp(A, a, a)
eqAa: provingground.HoTT.IdentityTyp[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term]] = a : (A : 𝒰 _0) = a : (A : 𝒰 _0)

scala> val ref = Refl(A, a)
ref: provingground.HoTT.Refl[provingground.HoTT.Term with provingground.HoTT.Subs[provingground.HoTT.Term]] = Refl(A : 𝒰 _0,a : (A : 𝒰 _0))
```


```scala
scala> ref.typ == eqAa
res21: Boolean = true
```


## The Unit and the  Nought

Finally, we have the types corresponding to _True_ and _False_


```scala
scala> Unit
res22: provingground.HoTT.Unit.type = Unit

scala> Zero
res23: provingground.HoTT.Zero.type = Zero

scala> Star !: Unit
res24: provingground.HoTT.Term = Star
```
