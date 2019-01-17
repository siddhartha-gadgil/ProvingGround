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

```scala mdoc:to-string
import provingground._
import scalahott._
import NatRing._
```

```scala mdoc:to-string
val n = "n" :: NatTyp
val m = "m" :: NatTyp
val k = "k" :: NatTyp
```

Spire implicits let us use the addition and multiplication operations.

```scala mdoc:to-string
import spire.math._
import spire.algebra._
import spire.implicits._
```

### Addition and multiplication
A sum gives a SigmaTerm, which only stores a set of terms being added.

```scala mdoc:to-string
n + m
(n + m) + n
```

Addition is commutative and associative, even when it involves repeated terms.
```scala mdoc:to-string
n + m == m + n
(n + m) + k == n + (m + k)
```

```scala mdoc:to-string
assert(n + m == m + n)
assert((n + m) + k == n + (m + k))
```

```scala mdoc:to-string
(n + n) + m == (n + m) + n
assert{(n + n) + m == (n + m) + n}
```

Similarly, multiplication is commutative and associative, and distributes over addition. Multiplication gives Pi-terms with parameter a map to exponents.

```scala mdoc:to-string
n * m == m * n
assert{n * m == m * n}
```

```scala mdoc:to-string
n * (m * k)

n * (m + k)

assert(n* (m + k) == n * m + n * k)
```

When literals are involved, the expresssions are simplified

```scala mdoc:to-string
1 + (n + 2)
```

### Symbolic definitions

We can use the expressions from these functions in lambdas. For this we need correct substitution.

```scala mdoc:to-string
import HoTT._
val fn = lmbda(n)(n * n)

fn(3)

assert(fn(3) == (9: Nat))

fn(k)
```

We have used an implicit conversion above to view `9` as a member of the type `Nat`

### Recursive definitions

We can define a function f recursively on natural numbers, given the value `f(0)` and given `f(n+1)` as a (curryed) function of `n+1` and `f(n)`. This expands for literals.

```scala mdoc:to-string
val mf = lmbda(n)(prod(n + 1))
val factorial = Rec(1: Nat, mf)
```

```scala mdoc:to-string
factorial(3)
factorial(5)
assert(factorial(5) == (120 : Nat))
```

### Simplifying recursive functions

If we apply a recursive function to a sum n+k with k a literal (say k = 2), then the result simplifies as much as possible by expanding tail recursively in the literal.

```scala mdoc:to-string
factorial(k + 2) == factorial(k) * (k + 2) * (k + 1)

assert{factorial(k + 2) == factorial(k) * (k + 2) * (k + 1)}
```
