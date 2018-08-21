---
title: ScalaRep
layout: page
---
## Scala Representations

Scala objects are integrated with HoTT by using wrappers, combinators and implicit based convenience methods. In this note we look at the basic representations. The main power of this is to provide automatically (through implicits) types and scala bindings for functions from the basic ones.

A more advanced form of Scala representations also makes symbolic algebra simplifications. The basic form should be used, for example, for group presentations, where simplifications are not expected.






```scala
scala>    

scala> import provingground._ 
import provingground._

scala> import HoTT._ 
import HoTT._

scala> import scalahott._ 
import scalahott._

scala> import ScalaRep._ 
import ScalaRep._
```



We consider the type of Natural numbers formed from Integers. This is defined in ScalaRep as:


```scala
scala> NatInt 
res10: NatInt.type = NatInt
```



### Conversion using the term method

The term method converts a scala object, with scala type T say, into a Term, provided there is an implicit representation with scala type T.



```scala
scala> import NatInt.rep 
import NatInt.rep

scala> 1.term 
res12: RepTerm[Int] = 1
```



### Functions to FuncTerms

Given the representation of Int, there are combinators that give representations of, for instance Int => Int => Int. Note also that the type of the resulting term is a type parameter of the scala representations, so we get a refined compile time type



```scala
scala> val sum = ((n: Int) => (m: Int) => n + m).term 
sum: Func[RepTerm[Int], Func[RepTerm[Int], RepTerm[Int]]] = <function1>
```





```scala
scala> sum(1.term)(2.term) 
res14: RepTerm[Int] = 3
```





```scala
scala> val n = "n" :: NatInt 
n: RepTerm[Int] with Subs[RepTerm[Int]] = n

scala> sum(n)(2.term) 
res16: RepTerm[Int] = ((<function1>) (n)) (2)
```





```scala
scala> val s = lmbda(n)(sum(n)(2.term)) 
s: Func[RepTerm[Int] with Subs[RepTerm[Int]], RepTerm[Int]] = (n :  NatInt) ↦ (((<function1>) (n)) (2))
```





```scala
scala> s(3.term) 
res18: RepTerm[Int] = 5
```



We will also define the product



```scala
scala> val prod = ((n : Int) => (m: Int) => n * m).term 
prod: Func[RepTerm[Int], Func[RepTerm[Int], RepTerm[Int]]] = <function1>
```





```scala
scala> prod(2.term)(4.term) 
res20: RepTerm[Int] = 8
```



