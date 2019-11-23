---
title: Internal repetition for length functions
layout: page
---

We formalize (in code) the combinatorial group theory part of the _spltting lemma_ of the paper [Homogenous length functions on Groups](https://arxiv.org/abs/1801.03908). This is based on an implementation (in _scala_) of a large part of Homotopy type theory equipped with symbolic algebra in this project.

## The statement

Assume we are given the following:

* A group $G$.
* A (length) function $l: G \\to \\mathbb{Q}$ such that
  * For $g, h \\in G$, $l(gh) \\leq l(g) + l(h)$,
  * For $g, h\\in G$, $l(ghg^{-1}) = l(g$).
* Elements $x, y, z, w \\in G$ such that
  * $x \\sim yw$ (here $\\sim$ means conjugate in $G$),
  * $x \\sim zw^{-1}$.

Then we have the following:

__Theorem:__ There exists a constant $A\\in\\mathbb{Q}$ so that for $n \\in \\mathbb{N}$,

$$l(x^{2n}) \leq n(l(y) + l(z)) +A.$$

In the above paper, homogeneity is used to deduce a bound for $l(x)$, and on taking the limit we get $l(x) \\leq \\frac{l(y) + l(z)}{2}$.

## Preliminaries

We start with some imports. This is a bit ugly due to avoiding variable name collisions.

```scala mdoc:to-string
import provingground._, HoTT._

import scalahott._, andrewscurtis.FreeGroups._
import spire.implicits._
import NatRing.{ x=>_,  Literal => nat, leq => natLeq, _}, QField.{w => _, x =>_, y=>_, z=>_, Literal => rat, _}, FreeGroup.{Literal => elem, _}
import Theorems.{PowerDistributive, ConjPower}
```

## The Setup

We introduce terms for the length function $l$, as well as _witnesses_ for the assumptions on $l$.

```scala mdoc:to-string
val l = "l" :: FreeGroup ->: QTyp

val g = "g" :: FreeGroup
val h = "h" :: FreeGroup
val n = "n" :: NatTyp
```

```scala mdoc:to-string
val triang =
  "triangle-inequality" :: (
    g ~>: (h ~>: (
      (leq(l(g |+| h))(l(g) + l(h)))
    ))
  )
```

```scala mdoc:to-string
val conjInv =
  "conjugacy-invariance" :: (
    g ~>: (
      h ~>: (
        (l(h) =:= (l(g |+| h |+| g.inverse)))
      )
    )
  )

```

Next, we introduce variables for $x, y, z, s, t \in G$, where $s, t \in G$ are the elements so that the conjugacies $x \\sim wy$ and $x \\sim zw^{-1}$ are given by the equations  $x = swys^{-1}$ and $x = tzw^{-1}t^{-1}$.  

```scala mdoc:to-string
val w = "w" :: FreeGroup
val y = "y" :: FreeGroup
val z = "z" :: FreeGroup
val s = "s" :: FreeGroup
val t = "t" :: FreeGroup
```

So far these are all independent. We shall introduce terms as witnesses for the above equations later, as we first prove a lemma not involving $x$.

## Statement of lemma

The main internal repetition trick is captured the bound

$$l((wy)^ns^{-1}t(zw^{-1})^n)\leq n(l(y) + l(z)) + l(s^{-1}) + l(t).$$

We define $c(n) = (wy)^ns^{-1}t(zw^{-1})^n$ and $f(n) = n(l(y) + l(z)) + l(s^{-1}) + l(t)$, so the inequality is $l(c(n))\leq(f(n))$. We encode this  below as the _main lemma_.

```scala mdoc:to-string
val wy = w |+| y
val zwbar = z |+| w.inverse
val wyn = FreeGroup.power(wy)(n)
val zwbarn = FreeGroup.power(zwbar)(n)
```

```scala mdoc:to-string
val c = n :-> (wyn |+| s.inverse |+| t |+| zwbarn) // this is the function we have to bound.

val r = incl(QField)

val f = n :-> (l(s.inverse) + l(t) + ((l(y) + l(z)) * r(n) ) )
```

```scala mdoc:to-string
val lemma = n :-> (leq (l(c(n)) )(f(n) ) )
```

## Proving the lemma

Let $d(n) = (wy)(wy)^ns^{-1}t(zw^{-1})^n(zw)$. Note that by definition $g^{n+1} = g^n g$, so we need to prove $d(n) = c(n+1)$.
We use $g^ng^m = g^{n+m}$ (proved in our code) to prove this. The proof is given by the term `dIsc`.

```scala mdoc:to-string
val d = n :-> (wy |+| wyn |+| s.inverse |+| t |+| zwbarn  |+| zwbar)
val dIsc = FreeGroup.rm(s.inverse |+| t |+| zwbarn |+| zwbar) *: (PowerDistributive.pf(wy)(nat(1))(n))
```

```scala mdoc:to-string
assert(dIsc.typ == (d(n) =:= c(succ(n))))
```

Next, let $b(n) = y(wy)^ns^{-1}t(zw^{-1})^nz$ be the conjugacy-reduced form of $d(n)$. Using $d(n) = c(n+1)$ and conjugacy invariance, we show that $l(c(n+1)) = l(b(n))$. The term `bIsc` is the proof of `l(c(n+1)) = l(b(n))`.

```scala mdoc:to-string
val b = n :-> (y |+| wyn |+| s.inverse |+| t |+| zwbarn  |+| z)
val lbIsld = conjInv(w)(y |+| c(n) |+| z)
val bIsc = lbIsld && (l *: dIsc)
```

```scala mdoc:to-string
assert(bIsc.typ == (l(b(n)) =:= l(c(succ(n))) ))
```

With these preliminaries, we can prove the lemma by induction. First note the base case.

```scala mdoc:to-string
val baseCase = triang(inv(s))(t) !: (lemma(0))
```

The induction step takes more work.
We first assume the induction hypothesis.

```scala mdoc:to-string
val hyp = "hyp" :: lemma(n)
```

Next, we bound $l(b(n))$ (which we know is $l(c(n+ 1)))$ in terms of $l(c(n))$.

```scala mdoc:to-string
val lbnBoundedlcnlylz = triang(y)(c(n)) + triang(y |+| c(n))(z)
```

```scala mdoc:to-string
assert(lbnBoundedlcnlylz.typ == (leq(l(b(n)))(l(c(n)) + l(y) + l(z))))
```

Now, using the induction hypothesis, we get a bound on $l(b(n))$.

```scala mdoc:to-string
val lbnBounded = lbnBoundedlcnlylz + hyp
```

```scala mdoc:to-string
assert(lbnBounded.typ == leq(l(b(n)) )(f(succ(n))) )
```

Next, we put together $l(c(n+1)) = l(b(n))$ and the bound on $l(b(n))$ to get the required bound on $l(c(n+1))$.

```scala mdoc:to-string
val bnd = "bound" :: QField.LocalTyp
val cbnd = bIsc.lift(bnd :-> (leq(bnd)(f(succ(n)) ) ))
val step = cbnd(lbnBounded)
```

```scala mdoc:to-string
assert(step.typ == lemma(succ(n)))
```

Finally, the lemma is proved by induction.

```scala mdoc:to-string
val lemmaProof = Induc(lemma, baseCase, n :~> (hyp :-> step))
```

```scala mdoc:to-string
assert(lemmaProof.typ == (n ~>: (lemma(n))) )
```

## Rest of the proof of the theorem

Some work remains, essentially to use symbolic algebra for equations such as $x^{2n} = x^nx^n$
and to put together equations and inequalities. We introduce terms witnessing the hypotheses $x=swys^{-1}$ and $x=tzw^{-1}t^{-1}$

```scala mdoc:to-string
val x = "x" :: FreeGroup
// val g = "g" :: FreeGroup
val pown = g :-> FreeGroup.power(g)(n)
```

```scala mdoc:to-string
val c1 = "x ~ wy" :: (x =:= (s |+| w |+| y |+| s.inverse))
val c2 = "x ~ zw^{-1}" :: (x =:= (t |+| z |+| w.inverse |+| t.inverse))
```

We deduce using a theorem (in our code) about powers of conjugates that $x^n = s(wy)^ns^{-1} = t(zw^{-1})^nt^{-1}$.

```scala mdoc:to-string
val xnConjwyn = (pown *: c1) && ConjPower.pf(s)(wy)(n)
val xnConjzwbarn= (pown *: c2) && ConjPower.pf(t)(zwbar)(n)
```

```scala mdoc:to-string
assert(xnConjwyn.typ == (pown(x) =:= (s |+| pown(wy)  |+| s.inverse  ) ) )
assert(xnConjzwbarn.typ == (pown(x) =:= (t |+| pown(zwbar)  |+| t.inverse  ) ) )
```

We use the above equations to show that $x^nx^n = s(wy)^ns^{-1}t(zw^{-1})^nt^{-1}$.

```scala mdoc:to-string
val t1 = s |+| pown(wy)  |+| s.inverse
val t2 = t |+| pown(zwbar)  |+| t.inverse
```

```scala mdoc:to-string
val xnxnExpr = (FreeGroup.rm(pown(x)) *: xnConjwyn) && (FreeGroup.lm(t1) *: xnConjzwbarn)
```

```scala mdoc:to-string
assert(xnxnExpr.typ == ((pown(x) |+| pown(x)) =:= (t1 |+| t2 )   ))
```

Using $x^nx^n = x^{2n}$, we get the formula $x^{2n} = s(wy)^ns^{-1}t(zw^{-1})^nt^{-1}$.

```scala mdoc:to-string
val x2nExpr =PowerDistributive.pf(x)(n)(n).sym && xnxnExpr
```

```scala mdoc:to-string
assert(x2nExpr.typ == (FreeGroup.power(x)(NatRing.prod(n)(nat(2))) =:= (s |+| c(n) |+| t.inverse)))
```

We now bound the length of the right hand side $s(wy)^ns^{-1}t(zw^{-1})^nt^{-1}$.

```scala mdoc:to-string
val thmBound = f(n) + l(s) + l(t.inverse)
```

```scala mdoc:to-string
val exprBound = lemmaProof(n) + triang(s)(c(n)) + triang(s |+| c(n))(t.inverse)
```

```scala mdoc:to-string
assert(exprBound.typ == leq(l(s |+| c(n) |+| t.inverse ))(thmBound))
```

We easily deduce the bound on $l(x^{2n})$ to complete the proof.

```scala mdoc:to-string
val thmProof = x2nExpr.sym.lift (g :-> leq(l(g))(thmBound))(exprBound)
```

```scala mdoc:to-string
val x2n = FreeGroup.power(x)(NatRing.prod(n)(nat(2)))
```

```scala mdoc:to-string
assert(thmProof.typ == leq(l(x2n))(thmBound ))

```
