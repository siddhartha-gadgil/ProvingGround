---
title: Internal repetition for length functions
layout: page
---

We formalize (in code) the combinatorial group theory part of the _spltting lemma_ of the paper [Homogenous length functions on Groups](https://arxiv.org/abs/1801.03908). This is based on an implementation (in _scala_) of a large part of Homotopy type theory equipped with symbolic algebra in this project.

## The statement

Assume we are given the following:

* A group $G$.
* A (length) function $l: G \\to \\mathbb{Q}$ such that
  - For $g, h \\in G$, $l(gh) \\leq l(g) + l(h)$,
  - For $g, h\\in G$, $l(ghg^{-1}) = l(g$).
* Elements $x, y, z, w \\in G$ such that
  - $x \\sim yw$ (here $\\sim$ means conjugate in $G$),
  - $x \\sim zw^{-1}$.

Then we have the following:

__Theorem:__ There exists a constant $A\\in\\mathbb{Q}$ so that for $n \\in \\mathbb{N}$,

$$l(x^{2n}) \leq n(l(y) + l(z)) +A.$$

In the above paper, homogeneity is used to deduce a bound for $l(x)$, and on taking the limit we get $l(x) \\leq \\frac{l(y) + l(z)}{2}$.


## Preliminaries

We start with some imports. This is a bit ugly due to avoiding variable name collisions.

```scala
scala> import provingground._, HoTT._
import provingground._
import HoTT._

scala> import functionfinder._, andrewscurtis.FreeGroups._
import functionfinder._
import andrewscurtis.FreeGroups._

scala> import spire.implicits._
import spire.implicits._

scala> import NatRing.{ x=>_,  Literal => nat, _}, QField.{w => _, x =>_, y=>_, z=>_, Literal => rat, _}, FreeGroup.{Literal => elem, _}
import NatRing.{x=>_, Literal=>nat, _}
import QField.{w=>_, x=>_, y=>_, z=>_, Literal=>rat, _}
import FreeGroup.{Literal=>elem, _}

scala> import Theorems.{PowerDistributive, ConjPower}
import Theorems.{PowerDistributive, ConjPower}
```

## The Setup

We introduce terms for the length function $l$, as well as _witnesses_ for the assumptions on $l$.

```scala
scala> val l = "l" :: FreeGroup ->: QTyp
l: provingground.HoTT.Func[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word],provingground.functionfinder.RepTerm[spire.math.Rational]] with provingground.HoTT.Subs[provingground.HoTT.Func[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word],provingground.functionfinder.RepTerm[spire.math.Rational]]] = l

scala> val g = "g" :: FreeGroup
g: provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word]] = g

scala> val h = "h" :: FreeGroup
h: provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word]] = h

scala> val n = "n" :: NatTyp
n: provingground.functionfinder.RepTerm[spire.math.SafeLong] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[spire.math.SafeLong]] = n

scala> val triang =
     |   "triangle-inequality" :: (
     |     g ~>: (h ~>: (
     |       (leq(l(g |+| h))(l(g) + l(h)))
     |     ))
     |   )
triang: provingground.HoTT.FuncLike[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word]],provingground.HoTT.FuncLike[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word]],provingground.functionfinder.QField.PosWit]] with provingground.HoTT.Subs[provingground.HoTT.FuncLike[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word]],provingground.HoTT.FuncLike...

scala> val conjInv =
     |   "conjugacy-invariance" :: (
     |     g ~>: (
     |       h ~>: (
     |         (l(h) =:= (l(g |+| h |+| g.inverse)))
     |       )
     |     )
     |   )
conjInv: provingground.HoTT.FuncLike[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word]],provingground.HoTT.FuncLike[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word]],provingground.HoTT.Equality[provingground.functionfinder.RepTerm[spire.math.Rational]]]] with provingground.HoTT.Subs[provingground.HoTT.FuncLike[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[provingground.andrewscurtis....
```

Next, we introduce variables for $x, y, z, s, t \in G$, where $s, t \in G$ are the elements so that the conjugacies $x \\sim wy$ and $x \\sim zw^{-1}$ are given by the equations  $x = swys^{-1}$ and $x = tzw^{-1}t^{-1}$.  

```scala
scala> val w = "w" :: FreeGroup
w: provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word]] = w

scala> val y = "y" :: FreeGroup
y: provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word]] = y

scala> val z = "z" :: FreeGroup
z: provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word]] = z

scala> val s = "s" :: FreeGroup
s: provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word]] = s

scala> val t = "t" :: FreeGroup
t: provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word]] = t
```

So far these are all independent. We shall introduce terms as witnesses for the above equations later, as we first prove a lemma not involving $x$.

## Statement of lemma

The main internal repetition trick is captured the bound $l((wy)^ns^{-1}t(zw^{-1})^n)\\leq n(l(y) + l(z)) + l(s^{-1}) + l(t)$. We define $c(n) = (wy)^ns^{-1}t(zw^{-1})^n$ and $f(n) = n(l(y) + l(z)) + l(s^{-1}) + l(t)$, so the lemms is $l(c(n))\leq(f(n))$. We encode this  below.


```scala
scala> val wy = w |+| y
wy: provingground.functionfinder.FreeGroup.LocalTerm = ((mul) (w)) (y)

scala> val zwbar = z |+| w.inverse
zwbar: provingground.functionfinder.FreeGroup.LocalTerm = ((mul) (z)) ((inv) (w))

scala> val wyn = FreeGroup.power(wy)(n)
wyn: provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] = (<function1>) (n)

scala> val zwbarn = FreeGroup.power(zwbar)(n)
zwbarn: provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] = (<function1>) (n)

scala> val c = n :-> (wyn |+| s.inverse |+| t |+| zwbarn) // this is the function we have to bound.
c: provingground.HoTT.Func[provingground.functionfinder.RepTerm[spire.math.SafeLong] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[spire.math.SafeLong]],provingground.functionfinder.FreeGroup.LocalTerm] = (n :  Nat.Typ) ↦ (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n)))))

scala> val r = incl(QField)
r: provingground.functionfinder.NatRing.Rec[provingground.functionfinder.QField.LocalTerm] = <function1>

scala> val f = n :-> (l(s.inverse) + l(t) + ((l(y) + l(z)) * r(n) ) )
f: provingground.HoTT.Func[provingground.functionfinder.RepTerm[spire.math.SafeLong] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[spire.math.SafeLong]],provingground.functionfinder.QField.LocalTerm] = (n :  Nat.Typ) ↦ (((l) (t) + (l) ((inv) (s)) + ((l) (z) * (<function1>) (n)) + ((<function1>) (n) * (l) (y))))

scala> val lemma = n :-> (leq (l(c(n)) )(f(n) ) )
lemma: provingground.HoTT.Func[provingground.functionfinder.RepTerm[spire.math.SafeLong] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[spire.math.SafeLong]],provingground.functionfinder.QField.Pos] = (n :  Nat.Typ) ↦ (Pos((((<function1>) (n) * (l) (y)) + (l) (t) + (l) ((inv) (s)) + ((prod) (-1)) ((l) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n)))))) + ((l) (z) * (<function1>) (n)))))
```

## Proving the lemma

Let $d(n) = (wy)(wy)^ns^{-1}t(zw^{-1})^n(zw)$. Note that by definition $g^{n+1} = g^n g$, so we need to prove $d(n) = c(n+1)$.
We use $g^ng^m = g^{n+m}$ (proved in our code) to prove this. The proof is given by the term `dIsc`.

```scala
scala> val d = n :-> (wy |+| wyn |+| s.inverse |+| t |+| zwbarn  |+| zwbar)
d: provingground.HoTT.Func[provingground.functionfinder.RepTerm[spire.math.SafeLong] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[spire.math.SafeLong]],provingground.functionfinder.FreeGroup.LocalTerm] = (n :  Nat.Typ) ↦ (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))

scala> val dIsc = FreeGroup.rm(s.inverse |+| t |+| zwbarn |+| zwbar) *: (PowerDistributive.pf(wy)(nat(1))(n))
dIsc: provingground.HoTT.Equality[provingground.functionfinder.FreeGroup.LocalTerm] = (ind{($cosg :  FreeGroup) ↦ (($cosh :  FreeGroup) ↦ ($cosg = $cosh))(((mul) (w)) (((mul) (y)) ((<function1>) (n))))(((mul) ((<function1>) (n))) (((mul) (w)) (y)))}{($aaxm :  FreeGroup) ↦ (($aaxn :  FreeGroup) ↦ ((_ : ($aaxm = $aaxn) :  $aaxm = $aaxn) ↦ (((mul) ($aaxm)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))) = ((mul) ($aaxn)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))}(($aaxm :  FreeGroup) ↦ (Refl(FreeGroup,((mul) ($aaxm)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) ((<function1>) (n) : (((mul) (w)) (((mul) (y)) ((<function...

scala> assert(dIsc.typ == (d(n) =:= c(succ(n))))
```

Next, let $b(n) = y(wy)^ns^{-1}t(zw^{-1})^nz$ be the conjugacy-reduced form of $d(n)$. Using $d(n) = c(n+1)$ and conjugacy invariance, we show that $l(c(n+1)) = l(b(n))$. The term `bIsc` is the proof of `l(c(n+1)) = l(b(n))`.

```scala
scala> val b = n :-> (y |+| wyn |+| s.inverse |+| t |+| zwbarn  |+| z)
b: provingground.HoTT.Func[provingground.functionfinder.RepTerm[spire.math.SafeLong] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[spire.math.SafeLong]],provingground.functionfinder.FreeGroup.LocalTerm] = (n :  Nat.Typ) ↦ (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z))))))

scala> val lbIsld = conjInv(w)(y |+| c(n) |+| z)
lbIsld: provingground.HoTT.Equality[provingground.functionfinder.RepTerm[spire.math.Rational]] = ((conjugacy-invariance) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))) : ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))) = (l) (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))

scala> val bIsc = lbIsld && (l *: dIsc)
bIsc: provingground.HoTT.Equality[provingground.functionfinder.RepTerm[spire.math.Rational] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[spire.math.Rational]]] = ((ind{($grjk :  Q.Typ) ↦ (($grjl :  Q.Typ) ↦ ($grjk = $grjl))((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))))((l) (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))}{($enrq :  Q.Typ) ↦ (($enrr :  Q.Typ) ↦ ((_ : ($enrq = $enrr) :  $enrq = $enrr) ↦ (($enrr = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) → ($e...

scala> assert(bIsc.typ == (l(b(n)) =:= l(c(succ(n))) ))
```

With these preliminaries, we can prove the lemma by induction. First note the base case.

```scala
scala> val baseCase = triang(inv(s))(t) !: (lemma(0))
baseCase: provingground.functionfinder.QField.PosWit = ((triangle-inequality) ((inv) (s))) (t) : (Pos((((prod) (-1)) ((l) (((mul) ((inv) (s))) (t))) + (l) ((inv) (s)) + (l) (t))))
```

The induction step takes more work.
We first assume the induction hypothesis.

```scala
scala> val hyp = "hyp" :: lemma(n)
hyp: provingground.functionfinder.QField.PosWit with provingground.HoTT.Subs[provingground.functionfinder.QField.PosWit] = hyp : (Pos((((l) (z) * (<function1>) (n)) + (l) (t) + (l) ((inv) (s)) + ((<function1>) (n) * (l) (y)) + ((prod) (-1)) ((l) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n)))))))))
```

Next, we bound $l(b(n))$ (which we know is $l(c(n+ 1)))$ in terms of $l(c(n))$.

```scala
scala> val lbnBoundedlcnlylz = triang(y)(c(n)) + triang(y |+| c(n))(z)
lbnBoundedlcnlylz: provingground.functionfinder.QField.PosWitSum = PosWitSum(((triangle-inequality) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))) : (Pos((((prod) (-1)) ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))))) + (l) (y) + (l) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n)))))))),((triangle-inequality) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))))) (z) : (Pos((((prod) (-1)) ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z))))))) + (l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) ...

scala> assert(lbnBoundedlcnlylz.typ == (leq(l(b(n)))(l(c(n)) + l(y) + l(z))))
```
Now, using the induction hypothesis, we get a bound on $l(b(n))$.

```scala
scala> val lbnBounded = lbnBoundedlcnlylz + hyp
lbnBounded: provingground.functionfinder.QField.PosWitSum = PosWitSum(PosWitSum(((triangle-inequality) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))) : (Pos((((prod) (-1)) ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))))) + (l) (y) + (l) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n)))))))),((triangle-inequality) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))))) (z) : (Pos((((prod) (-1)) ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z))))))) + (l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mu...

scala> assert(lbnBounded.typ == leq(l(b(n)) )(f(succ(n))) )
```

Next, we put together $l(c(n+1)) = l(b(n))$ and the bound on $l(b(n))$ to get the required bound on $l(c(n+1))$.

```scala
scala> val bnd = "bound" :: QField.LocalTyp
bnd: provingground.functionfinder.RepTerm[spire.math.Rational] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[spire.math.Rational]] = bound

scala> val cbnd = bIsc.lift(bnd :-> (leq(bnd)(f(succ(n)) ) ))
cbnd: provingground.HoTT.Func[provingground.functionfinder.QField.PosWit,provingground.functionfinder.QField.PosWit] = (ind{($abhmp :  Q.Typ) ↦ (($abhmq :  Q.Typ) ↦ ($abhmp = $abhmq))((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))))((l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))}{($gykv :  Q.Typ) ↦ (($gykw :  Q.Typ) ↦ ((_ : ($gykv = $gykw) :  $gykv = $gykw) ↦ ((Pos(((l) (z) + ((l) (z) * (<function1>) (n)) + ((prod) (-1)) ($gykv) + (l) (y) + (l) (t) + (l) ((inv) (s)) + ((<function1>) (n) * (l) (y))))) → (Pos(((l) (z) + ((l) (z) * (<function1>) (n)) + (l) (y) + (l) (t) + (l) ((inv) (s)) +...

scala> val step = cbnd(lbnBounded)
step: provingground.functionfinder.QField.PosWit = ((ind{($abhmp :  Q.Typ) ↦ (($abhmq :  Q.Typ) ↦ ($abhmp = $abhmq))((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))))((l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))}{($gykv :  Q.Typ) ↦ (($gykw :  Q.Typ) ↦ ((_ : ($gykv = $gykw) :  $gykv = $gykw) ↦ ((Pos(((l) (z) + ((l) (z) * (<function1>) (n)) + ((prod) (-1)) ($gykv) + (l) (y) + (l) (t) + (l) ((inv) (s)) + ((<function1>) (n) * (l) (y))))) → (Pos(((l) (z) + ((l) (z) * (<function1>) (n)) + (l) (y) + (l) (t) + (l) ((inv) (s)) + ((<function1>) (n) * (l) (y)) + ((prod) (-1)) ($gykw)))))))}(($gyk...

scala> assert(step.typ == lemma(succ(n)))
```

Finally, the lemma is proved by induction.

```scala
scala> val lemmaProof = Induc(lemma, baseCase, n :~> (hyp :-> step))
lemmaProof: provingground.functionfinder.NatRing.Induc[provingground.functionfinder.QField.PosWit with provingground.HoTT.Subs[provingground.functionfinder.QField.PosWit]] = <function1>

scala> assert(lemmaProof.typ == (n ~>: (lemma(n))) )
```

## Rest of the proof of the theorem

Some work remains, essentially to use symbolic algebra for equations such as $x^{2n} = x^nx^n$
and to put together equations and inequalities. We introduce terms witnessing the hypotheses $x=swys^{-1}$ and $x=tzw^{-1}t^{-1}$

```scala
scala> val x = "x" :: FreeGroup
x: provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word]] = x

scala> val g = "g" :: FreeGroup
g: provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word]] = g

scala> val pown = g :-> FreeGroup.power(g)(n)
pown: provingground.HoTT.Func[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word]],provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word]] = (g :  FreeGroup) ↦ ((<function1>) (n))

scala> val c1 = "x ~ wy" :: (x =:= (s |+| w |+| y |+| s.inverse))
c1: provingground.HoTT.Equality[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word]]] with provingground.HoTT.Subs[provingground.HoTT.Equality[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word]]]] = x ~ wy : (x = ((mul) (s)) (((mul) (w)) (((mul) (y)) ((inv) (s)))))

scala> val c2 = "x ~ zw^{-1}" :: (x =:= (t |+| z |+| w.inverse |+| t.inverse))
c2: provingground.HoTT.Equality[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word]]] with provingground.HoTT.Subs[provingground.HoTT.Equality[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word]]]] = x ~ zw^{-1} : (x = ((mul) (t)) (((mul) (z)) (((mul) ((inv) (w))) ((inv) (t)))))
```

We deduce using a theorem (in our code) about powers of conjugates that $x^n = s(wy)^ns^{-1} = t(zw^{-1})^nt^{-1}$.
```scala
scala> val xnConjwyn = (pown *: c1) && ConjPower.pf(s)(wy)(n)
xnConjwyn: provingground.HoTT.Equality[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word]]] = ((ind{($afbza :  FreeGroup) ↦ (($afbzb :  FreeGroup) ↦ ($afbza = $afbzb))((<function1>) (n))((<function1>) (n))}{($aeujl :  FreeGroup) ↦ (($aeujm :  FreeGroup) ↦ ((_ : ($aeujl = $aeujm) :  $aeujl = $aeujm) ↦ (($aeujm = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))) → ($aeujl = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))))))}(($aeujl :  FreeGroup) ↦ (($aeush : ($aeujl = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))) :  $aeujl = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))) ↦ ($aeush : ($aeujl = (...

scala> val xnConjzwbarn= (pown *: c2) && ConjPower.pf(t)(zwbar)(n)
xnConjzwbarn: provingground.HoTT.Equality[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] with provingground.HoTT.Subs[provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word]]] = ((ind{($afwcq :  FreeGroup) ↦ (($afwcr :  FreeGroup) ↦ ($afwcq = $afwcr))((<function1>) (n))((<function1>) (n))}{($afojz :  FreeGroup) ↦ (($afoka :  FreeGroup) ↦ ((_ : ($afojz = $afoka) :  $afojz = $afoka) ↦ (($afoka = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))) → ($afojz = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))))))}(($afojz :  FreeGroup) ↦ (($afosv : ($afojz = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))) :  $afojz = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))) ↦ ($afosv : ($afojz ...

scala> assert(xnConjwyn.typ == (pown(x) =:= (s |+| pown(wy)  |+| s.inverse  ) ) )

scala> assert(xnConjzwbarn.typ == (pown(x) =:= (t |+| pown(zwbar)  |+| t.inverse  ) ) )
```

We use the above equations to show that $x^nx^n = s(wy)^ns^{-1}t(zw^{-1})^nt^{-1}$.
```scala
scala> val t1 = s |+| pown(wy)  |+| s.inverse
t1: provingground.functionfinder.FreeGroup.LocalTerm = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))

scala> val t2 = t |+| pown(zwbar)  |+| t.inverse
t2: provingground.functionfinder.FreeGroup.LocalTerm = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))

scala> val xnxnExpr = (FreeGroup.rm(pown(x)) *: xnConjwyn) && (FreeGroup.lm(t1) *: xnConjzwbarn)
xnxnExpr: provingground.HoTT.Equality[provingground.functionfinder.FreeGroup.LocalTerm with provingground.HoTT.Subs[provingground.functionfinder.FreeGroup.LocalTerm]] = ((ind{($akpis :  FreeGroup) ↦ (($akpit :  FreeGroup) ↦ ($akpis = $akpit))(((mul) ((<function1>) (n))) ((<function1>) (n)))(((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ((<function1>) (n)))))}{($aiyxu :  FreeGroup) ↦ (($aiyxv :  FreeGroup) ↦ ((_ : ($aiyxu = $aiyxv) :  $aiyxu = $aiyxv) ↦ (($aiyxv = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) → ($aiyxu = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))))))}(($aiyxu :  FreeGroup) ↦ (($ai...

scala> assert(xnxnExpr.typ == ((pown(x) |+| pown(x)) =:= (t1 |+| t2 )   ))
```

Using $x^nx^n = x^{2n}$, we get the formula $x^{2n} = s(wy)^ns^{-1}t(zw^{-1})^nt^{-1}$.
```scala
scala> val x2nExpr =PowerDistributive.pf(x)(n)(n).sym && xnxnExpr
x2nExpr: provingground.HoTT.Equality[provingground.functionfinder.FreeGroup.LocalTerm with provingground.HoTT.Subs[provingground.functionfinder.FreeGroup.LocalTerm] with provingground.HoTT.Subs[provingground.functionfinder.FreeGroup.LocalTerm with provingground.HoTT.Subs[provingground.functionfinder.FreeGroup.LocalTerm]]] = ((ind{($amblw :  FreeGroup) ↦ (($amblx :  FreeGroup) ↦ ($amblw = $amblx))((<function1>) (((prod) (2)) (n)))(((mul) ((<function1>) (n))) ((<function1>) (n)))}{($alhpk :  FreeGroup) ↦ (($alhpl :  FreeGroup) ↦ ((_ : ($alhpk = $alhpl) :  $alhpk = $alhpl) ↦ (($alhpl = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) → ($alhpk = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul)...

scala> assert(x2nExpr.typ == (FreeGroup.power(x)(NatRing.prod(n)(nat(2))) =:= (s |+| c(n) |+| t.inverse)))
```

We now bound the length of the right hand side $s(wy)^ns^{-1}t(zw^{-1})^nt^{-1}$.

```scala
scala> val thmBound = f(n) + l(s) + l(t.inverse)
thmBound: provingground.functionfinder.QField.LocalTerm = ((l) (s) + ((l) (z) * (<function1>) (n)) + (l) (t) + (l) ((inv) (s)) + ((<function1>) (n) * (l) (y)) + (l) ((inv) (t)))

scala> val exprBound = lemmaProof(n) + triang(s)(c(n)) + triang(s |+| c(n))(t.inverse)
exprBound: provingground.functionfinder.QField.PosWitSum = PosWitSum(PosWitSum((<function1>) (n) : (Pos((((l) (z) * (<function1>) (n)) + (l) (t) + (l) ((inv) (s)) + ((<function1>) (n) * (l) (y)) + ((prod) (-1)) ((l) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))))))),((triangle-inequality) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))) : (Pos((((prod) (-1)) ((l) (((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))))) + (l) (s) + (l) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))))))),((triangle-inequality) (((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n)...

scala> assert(exprBound.typ == leq(l(s |+| c(n) |+| t.inverse ))(thmBound))
```

We easily deduce the bound on $l(x^{2n})$ to complete the proof.

```scala
scala> val thmProof = x2nExpr.sym.lift (g :-> leq(l(g))(thmBound))(exprBound)
thmProof: provingground.functionfinder.QField.PosWit = ((ind{($bpxkw :  FreeGroup) ↦ (($bpxkx :  FreeGroup) ↦ ($bpxkw = $bpxkx))(((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))))))((<function1>) (((prod) (2)) (n)))}{($apcui :  FreeGroup) ↦ (($apcuj :  FreeGroup) ↦ ((_ : ($apcui = $apcuj) :  $apcui = $apcuj) ↦ ((Pos(((l) (s) + ((l) (z) * (<function1>) (n)) + ((prod) (-1)) ((l) ($apcui)) + (l) (t) + (l) ((inv) (s)) + ((<function1>) (n) * (l) (y)) + (l) ((inv) (t))))) → (Pos(((l) (s) + ((l) (z) * (<function1>) (n)) + (l) (t) + (l) ((inv) (s)) + ((<function1>) (n) * (l) (y)) + ((prod) (-1)) ((l) ($apcuj)) + (l) ((inv) (t))))))))}(($apcui :  FreeGroup) ↦ (($axeam : (Pos(((l) (s) + ((prod) (-1)) ((l...

scala> val x2n = FreeGroup.power(x)(NatRing.prod(n)(nat(2)))
x2n: provingground.functionfinder.RepTerm[provingground.andrewscurtis.FreeGroups.Word] = (<function1>) (((prod) (2)) (n))

scala> assert(thmProof.typ == leq(l(x2n))(thmBound ))
```
