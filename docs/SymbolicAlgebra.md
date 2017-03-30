---
title: Symbolic algebra
layout: page
---

## Symbolic Algebra for natural numbers

To efficiently manipulate expressions in natural numbers, or more generally rings (and fields), proving-ground has special HoTT types wrapping scala types that are Rings, Rigs, Fields etc in the spire library.

As a consequence:
* Symbolic expressions that are equal become definitionally equal, i.e., equal as scala objects.
* We define recursion which expands for (sums with) literals
* Expressions involving literals and variables are simplified as much as possible.

The ring of natural numbers is an object NatRing. This has
* a HoTT type NatTyp,
* a scala type Nat
* a scala representation
* a (spire) ring structure on the underlying terms.

```scala
scala> import provingground._
import provingground._

scala> import NatRing._
import NatRing._
```

```scala
scala> val n = "n" :: NatTyp
n: provingground.RepTerm[spire.math.SafeLong] with provingground.HoTT.Subs[provingground.RepTerm[spire.math.SafeLong]] = n : (Nat.Typ)

scala> val m = "m" :: NatTyp
m: provingground.RepTerm[spire.math.SafeLong] with provingground.HoTT.Subs[provingground.RepTerm[spire.math.SafeLong]] = m : (Nat.Typ)

scala> val k = "k" :: NatTyp
k: provingground.RepTerm[spire.math.SafeLong] with provingground.HoTT.Subs[provingground.RepTerm[spire.math.SafeLong]] = k : (Nat.Typ)
```

Spire implicits let us use the addition and multiplication operations.

```scala
scala> import spire.math._
import spire.math._

scala> import spire.algebra._
import spire.algebra._

scala> import spire.implicits._
import spire.implicits._
```

### Addition and multiplication
A sum gives a SigmaTerm, which only stores a set of terms being added.

```scala
scala> n + m
res0: provingground.NatRing.LocalTerm = SigmaTerm(Set(n : (Nat.Typ), m : (Nat.Typ)))

scala> (n + m) + n
res1: provingground.NatRing.LocalTerm = SigmaTerm(Set(m : (Nat.Typ), ((prod) (ScalaSymbol(2) : (Nat.Typ)) : ((Nat.Typ) → (Nat.Typ))) (n : (Nat.Typ)) : (Nat.Typ)))
```

Addition is commutative and associative, even when it involves repeated terms.
```scala
scala> n + m == m + n
res2: Boolean = true

scala> (n + m) + k == n + (m + k)
res3: Boolean = true

scala> assert(n + m == m + n)

scala> assert((n + m) + k == n + (m + k))

scala> (n + n) + m == (n + m) + n
res6: Boolean = true

scala> assert{(n + n) + m == (n + m) + n}
```

Similarly, multiplication is commutative and associative, and distributes over addition. Multiplication gives Pi-terms with parameter a map to exponents.

```scala
scala> n * m == m * n
res8: Boolean = true

scala> assert{n * m == m * n}

scala> n * (m * k)
res10: provingground.NatRing.LocalTerm = PiTerm(Map(m : (Nat.Typ) -> 1, k : (Nat.Typ) -> 1, n : (Nat.Typ) -> 1))

scala> n * (m + k)
res11: provingground.NatRing.LocalTerm = SigmaTerm(Set(PiTerm(Map(n : (Nat.Typ) -> 1, m : (Nat.Typ) -> 1)), PiTerm(Map(n : (Nat.Typ) -> 1, k : (Nat.Typ) -> 1))))

scala> assert(n* (m + k) == n * m + n * k)
```

When literals are involved, the expresssions are simplified

```scala
scala> 1 + (n + 2)
res13: provingground.NatRing.LocalTerm = ((sum) (ScalaSymbol(3) : (Nat.Typ)) : ((Nat.Typ) → (Nat.Typ))) (n : (Nat.Typ)) : (Nat.Typ)
```

### Symbolic definitions

We can use the expressions from these functions in lambdas. For this we need correct substitution.

```scala
scala> import HoTT._
import HoTT._

scala> val fn = lmbda(n)(n * n)
fn: provingground.HoTT.Func[provingground.RepTerm[spire.math.SafeLong] with provingground.HoTT.Subs[provingground.RepTerm[spire.math.SafeLong]],provingground.NatRing.LocalTerm] = (n : (Nat.Typ)) ↦ (PiTerm(Map(n : (Nat.Typ) -> 2)))

scala> fn(3)
res14: provingground.NatRing.LocalTerm = ScalaSymbol(9) : (Nat.Typ)

scala> assert(fn(3) == (9: Nat))

scala> fn(k)
res16: provingground.NatRing.LocalTerm = PiTerm(Map(k : (Nat.Typ) -> 2))
```

We have used an implicit conversion above to view `9` as a member of the type `Nat`

### Recursive definitions

We can define a function f recursively on natural numbers, given the value `f(0)` and given `f(n+1)` as a (curryed) function of `n+1` and `f(n)`. This expands for literals.

```scala
scala> val m = lmbda(n)(prod(n + 1))
m: provingground.HoTT.Func[provingground.RepTerm[spire.math.SafeLong] with provingground.HoTT.Subs[provingground.RepTerm[spire.math.SafeLong]],provingground.HoTT.Func[provingground.NatRing.LocalTerm,provingground.NatRing.LocalTerm]] = (n : (Nat.Typ)) ↦ ((provingground.HoTT$Typ$newname$2$@1684fb4b : (Nat.Typ)) ↦ (SigmaTerm(Set(provingground.HoTT$Typ$newname$2$@1684fb4b : (Nat.Typ), PiTerm(Map(n : (Nat.Typ) -> 1, provingground.HoTT$Typ$newname$2$@1684fb4b : (Nat.Typ) -> 1))))))

scala> val factorial = Rec(1: Nat, m)
factorial: provingground.NatRing.Rec[provingground.NatRing.LocalTerm] = <function1>

scala> factorial(3)
res17: provingground.NatRing.LocalTerm = ScalaSymbol(6) : (Nat.Typ)

scala> factorial(5)
res18: provingground.NatRing.LocalTerm = ScalaSymbol(120) : (Nat.Typ)

scala> assert(factorial(5) == (120 : Nat))
```

### Simplifying recursive functions

If we apply a recursive function to a sum n+k with k a literal (say k = 2), then the result simplifies as much as possible by expanding tail recursively in the literal.

```scala
scala> factorial(k + 2) == factorial(k) * (k + 2) * (k + 1)
res20: Boolean = true

scala> assert{factorial(k + 2) == factorial(k) * (k + 2) * (k + 1)}
```
