---
title: ScalaRep
layout: page
---
## Scala Representations

Scala objects are integrated with HoTT by using wrappers, combinators and implicit based convenience methods. In this note we look at the basic representations. The main power of this is to provide automatically (through implicits) types and scala bindings for functions from the basic ones.

A more advanced form of Scala representations also makes symbolic algebra simplifications. The basic form should be used, for example, for group presentations, where simplifications are not expected.





```scala mdoc
import provingground._
import HoTT._
import scalahott._
import ScalaRep._
```

We consider the type of Natural numbers formed from Integers. This is defined in ScalaRep as:

```scala
case object NatInt extends ScalaTyp[Int]
```

**Warning:** This is an unsafe type, as Integers can overflow, and there is no checking for positivity.
There is an alternative implementation using spire which is safe. We see this in the symbolic algebra notes.


```scala mdoc
NatInt
```

### Conversion using the term method

The term method converts a scala object, with scala type T say, into a Term, provided there is an implicit representation with scala type T.


```scala mdoc
import NatInt.rep
1.term
```

### Functions to FuncTerms

Given the representation of Int, there are combinators that give representations of, for instance Int => Int => Int. Note also that the type of the resulting term is a type parameter of the scala representations, so we get a refined compile time type


```scala mdoc
val sum = ((n: Int) => (m: Int) => n + m).term
```


```scala mdoc
sum(1.term)(2.term)
```


```scala mdoc
val n = "n" :: NatInt
sum(n)(2.term)
```


```scala mdoc
val s = lmbda(n)(sum(n)(2.term))
```


```scala mdoc
s(3.term)
```

We will also define the product


```scala mdoc
val prod = ((n : Int) => (m: Int) => n * m).term
```


```scala mdoc
prod(2.term)(4.term)
```
