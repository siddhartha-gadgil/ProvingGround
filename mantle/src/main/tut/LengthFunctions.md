---
title: Internal repetition for Length functions
layout: page
---

We _formalize_ the combinatorial group theory part of the internal repetition lemma.

```tut
import provingground._, HoTT._

import functionfinder._, andrewscurtis.FreeGroups._
import spire.implicits._
import LinNormBound.{x => _, y =>_, _}
import NatRing.{ x=>_,  Literal => nat, _}, QField.{w => _, x =>_, y=>_, z=>_, Literal => rat, _}, FreeGroup.{Literal => elem, _}
import Theorems.{PowerDistributive, ConjPower}

val w = "w" :: FreeGroup
val y = "y" :: FreeGroup
val z = "z" :: FreeGroup
val s = "s" :: FreeGroup
val t = "t" :: FreeGroup

val wy = w |+| y
val zwbar = z |+| w.inverse
val wyn = FreeGroup.power(wy)(n)
val zwbarn = FreeGroup.power(zwbar)(n)

val c = n :-> (wyn |+| s.inverse |+| t |+| zwbarn) // this is the function we have to bound.



/*
* `d(n)` looks like c (n+1) but is not so by definition as by definition g^{n+1} = g^n g
* We use g^ng^m = g^{n+m} to prove this. the proof is `dIsc`
*/

val d = n :-> (wy |+| wyn |+| s.inverse |+| t |+| zwbarn  |+| zwbar)
val dIsc = FreeGroup.rm(s.inverse |+| t |+| zwbarn |+| zwbar) *: (PowerDistributive.pf(wy)(nat(1))(n))
assert(dIsc.typ == (d(n) =:= c(succ(n))))

/*
* b(n) is the conjugacy-reduced form of d(n), which is (propositionally) equal to c(n+1)
* we show in `bIsc` that `l(c(n+1)) = l(b(n))`
*/

val b = n :-> (y |+| wyn |+| s.inverse |+| t |+| zwbarn  |+| z)
val bd = conjInv(w)(y |+| c(n) |+| z)
val bIsc = bd && (l *: dIsc)
assert(bIsc.typ == (l(b(n)) =:= l(c(succ(n))) ))

val r = incl(QField)
// f is the bound in the `main internal repetition` lemma

val f = n :-> (l(s.inverse) + l(t) + ((l(y) + l(z)) * r(n) ) )

val lemma = n :-> (leq (l(c(n)) )(f(n) ) )

val baseCase = triang(inv(s))(t) !: (lemma(0)) // base case for induction

/*
* We have to work for the induction step.
* We first assume the induction hypothesis
*/

val hyp = "hyp" :: lemma(n)
// We bound `l(b(n))` in terms of `l(c(n))`

val lbnBoundedlcnlylz = triang(y)(c(n)) + triang(y |+| c(n))(z)
assert(lbnBoundedlcnlylz.typ == (leq(l(b(n)))(l(c(n)) + l(y) + l(z))))

// Using the hypothesis, we get a bound on l(b(n))
val lbnBounded = lbnBoundedlcnlylz + hyp
assert(lbnBounded.typ == leq(l(b(n)) )(f(succ(n))) )

// Now we put together l(c(n+1)) = l(b(n)) and the bound on l(b(n)) to get the bound
val bnd = "bound" :: QField.LocalTyp
val cbnd = bIsc.lift(bnd :-> (leq(bnd)(f(succ(n)) ) ))
val step = cbnd(lbnBounded)
assert(step.typ == lemma(succ(n)))

// The lemma is proved by induction
val lemmaProof = Induc(lemma, baseCase, n :~> (hyp :-> step))
assert(lemmaProof.typ == (n ~>: (lemma(n))) )

// Some work remains, essentially to use symbolic algebra like x^{2n} = x^nx^n
// and to put together equations and inequalities
val x = "x" :: FreeGroup
val g = "g" :: FreeGroup
val pown = g :-> FreeGroup.power(g)(n)

// The hypothesis of `x` being conjugate to `wy` and `zw^{-1}`
val c1 = "x ~ wy" :: (x =:= (s |+| w |+| y |+| s.inverse))
val c2 = "x ~ zw^{-1}" :: (x =:= (t |+| z |+| w.inverse |+| t.inverse))

// We raise the hypothesis to the power of n, and use the identity for powers of conjugates
val xnConjwyn = (pown *: c1) && ConjPower.pf(s)(wy)(n)
val xnConjzwbarn= (pown *: c2) && ConjPower.pf(t)(zwbar)(n)
assert(xnConjwyn.typ == (pown(x) =:= (s |+| pown(wy)  |+| s.inverse  ) ) )
assert(xnConjzwbarn.typ == (pown(x) =:= (t |+| pown(zwbar)  |+| t.inverse  ) ) )

// We put together the two conjugacy relations to get teh formula for x^nx^n
val t1 = s |+| pown(wy)  |+| s.inverse
val t2 = t |+| pown(zwbar)  |+| t.inverse
val xnxnExpr = (FreeGroup.rm(pown(x)) *: xnConjwyn) && (FreeGroup.lm(t1) *: xnConjzwbarn)
assert(xnxnExpr.typ == ((pown(x) |+| pown(x)) =:= (t1 |+| t2 )   ))

// Using x^nx^n = x^{2n}, we get the formula for x^{2n}
val x2nExpr =PowerDistributive.pf(x)(n)(n).sym && xnxnExpr
assert(x2nExpr.typ == (FreeGroup.power(x)(NatRing.prod(n)(nat(2))) =:= (s |+| c(n) |+| t.inverse)))

// We now bound the length of the rhs of the  above identity for x^{2n}
val thmBound = f(n) + l(s) + l(t.inverse)
val exprBound = lemmaProof(n) + triang(s)(c(n)) + triang(s |+| c(n))(t.inverse)
assert(exprBound.typ == leq(l(s |+| c(n) |+| t.inverse ))(thmBound))

// We easily deduce the bound on l(x^{2n})
val thmProof = x2nExpr.sym.lift (g :-> leq(l(g))(thmBound))(exprBound)
val x2n = FreeGroup.power(x)(NatRing.prod(n)(nat(2)))
assert(thmProof.typ == leq(l(x2n))(thmBound ))

```
