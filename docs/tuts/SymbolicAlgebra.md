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
scala>    

scala> import provingground._ 
import provingground._

scala> import functionfinder._ 
import functionfinder._

scala> import NatRing._ 
import NatRing._
```




```scala
scala> val n = "n" :: NatTyp 
n: RepTerm[spire.math.SafeLong] with HoTT.Subs[RepTerm[spire.math.SafeLong]] = n

scala> val m = "m" :: NatTyp 
m: RepTerm[spire.math.SafeLong] with HoTT.Subs[RepTerm[spire.math.SafeLong]] = m

scala> val k = "k" :: NatTyp 
k: RepTerm[spire.math.SafeLong] with HoTT.Subs[RepTerm[spire.math.SafeLong]] = k
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
res15: LocalTerm = (n + m)

scala> (n + m) + n 
res16: LocalTerm = (m + ((prod) (2)) (n))
```



Addition is commutative and associative, even when it involves repeated terms.

```scala
scala> n + m == m + n 
res17: Boolean = true

scala> (n + m) + k == n + (m + k) 
res18: Boolean = true

scala> assert(n + m == m + n) 


scala> assert((n + m) + k == n + (m + k)) 


scala>  

scala> (n + n) + m == (n + m) + n 
res21: Boolean = true

scala> assert{(n + n) + m == (n + m) + n}
```



Similarly, multiplication is commutative and associative, and distributes over addition. Multiplication gives Pi-terms with parameter a map to exponents.


```scala
scala> n * m == m * n 
res23: Boolean = true

scala> assert{n * m == m * n} 


scala>  

scala> n * (m * k) 
res25: LocalTerm = (k * n * m)

scala>  

scala> n * (m + k) 
res26: LocalTerm = ((n * m) + (k * n))

scala>  

scala> assert(n* (m + k) == n * m + n * k)
```



When literals are involved, the expresssions are simplified


```scala
scala> 1 + (n + 2) 
res28: LocalTerm = ((sum) (3)) (n)
```



### Symbolic definitions

We can use the expressions from these functions in lambdas. For this we need correct substitution.


```scala
scala> import HoTT._ 
import HoTT._

scala> val fn = lmbda(n)(n * n) 
fn: Func[RepTerm[SafeLong] with Subs[RepTerm[SafeLong]], LocalTerm] = (n :  Nat.Typ) ↦ ((n^{2}))

scala>  

scala> fn(3) 
res31: LocalTerm = 9

scala>  

scala> assert(fn(3) == (9: Nat)) 


scala>  

scala> fn(k) 
res33: LocalTerm = (k^{2})
```



We have used an implicit conversion above to view `9` as a member of the type `Nat`

### Recursive definitions

We can define a function f recursively on natural numbers, given the value `f(0)` and given `f(n+1)` as a (curryed) function of `n+1` and `f(n)`. This expands for literals.


```scala
scala> val m = lmbda(n)(prod(n + 1)) 
m: Func[RepTerm[SafeLong] with Subs[RepTerm[SafeLong]], Func[LocalTerm, LocalTerm]] = (n :  Nat.Typ) ↦ ((provingground.HoTT$Typ$newname$2$@e868c41 :  Nat.Typ) ↦ ((provingground.HoTT$Typ$newname$2$@e868c41 + (provingground.HoTT$Typ$newname$2$@e868c41 * n))))

scala> val factorial = Rec(1: Nat, m) 
factorial: Rec[LocalTerm] = <function1>

scala>  

scala> factorial(3) 
res36: LocalTerm = 6

scala> factorial(5) 
res37: LocalTerm = 120

scala> assert(factorial(5) == (120 : Nat))
```



### Simplifying recursive functions

If we apply a recursive function to a sum n+k with k a literal (say k = 2), then the result simplifies as much as possible by expanding tail recursively in the literal.


```scala
scala> factorial(k + 2) == factorial(k) * (k + 2) * (k + 1) 
res39: Boolean = true

scala>  

scala> assert{factorial(k + 2) == factorial(k) * (k + 2) * (k + 1)}
```



