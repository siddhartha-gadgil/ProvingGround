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
scala>    

scala> import provingground._, HoTT._ 
import provingground._, HoTT._

scala>  

scala> import scalahott._, andrewscurtis.FreeGroups._ 
import scalahott._, andrewscurtis.FreeGroups._

scala> import spire.implicits._ 
import spire.implicits._

scala> 
import NatRing.{ x=>_,  Literal => nat, _}, QField.{w => _, x =>_, y=>_, z=>_, Literal => rat, _}, FreeGroup.{Literal => elem, _} 
import NatRing.{ x=>_,  Literal => nat, _}, QField.{w => _, x =>_, y=>_, z=>_, Literal => rat, _}, FreeGroup.{Literal => elem, _}

scala> import Theorems.{PowerDistributive, ConjPower} 
import Theorems.{PowerDistributive, ConjPower}
```



## The Setup

We introduce terms for the length function $l$, as well as _witnesses_ for the assumptions on $l$.


```scala
scala> val l = "l" :: FreeGroup ->: QTyp 
l: Func[RepTerm[Word], RepTerm[spire.math.Rational]] with Subs[Func[RepTerm[Word], RepTerm[spire.math.Rational]]] = l

scala>  

scala> val g = "g" :: FreeGroup 
g: RepTerm[Word] with Subs[RepTerm[Word]] = g

scala> val h = "h" :: FreeGroup 
h: RepTerm[Word] with Subs[RepTerm[Word]] = h

scala> val n = "n" :: NatTyp 
n: RepTerm[spire.math.SafeLong] with Subs[RepTerm[spire.math.SafeLong]] = n

scala>  

scala> val triang =
         "triangle-inequality" :: (
           g ~>: (h ~>: (
             (leq(l(g |+| h))(l(g) + l(h)))
           ))
         ) 
triang: FuncLike[RepTerm[Word] with Subs[RepTerm[Word]], FuncLike[RepTerm[Word] with Subs[RepTerm[Word]], PosWit]] with Subs[FuncLike[RepTerm[Word] with Subs[RepTerm[Word]], FuncLike[RepTerm[Word] with Subs[RepTerm[Word]], PosWit]]] = triangle-inequality

scala>  

scala> val conjInv =
         "conjugacy-invariance" :: (
           g ~>: (
             h ~>: (
               (l(h) =:= (l(g |+| h |+| g.inverse)))
             )
           )
         ) 
conjInv: FuncLike[RepTerm[Word] with Subs[RepTerm[Word]], FuncLike[RepTerm[Word] with Subs[RepTerm[Word]], Equality[RepTerm[spire.math.Rational]]]] with Subs[FuncLike[RepTerm[Word] with Subs[RepTerm[Word]], FuncLike[RepTerm[Word] with Subs[RepTerm[Word]], Equality[RepTerm[spire.math.Rational]]]]] = conjugacy-invariance

scala>
```



Next, we introduce variables for $x, y, z, s, t \in G$, where $s, t \in G$ are the elements so that the conjugacies $x \\sim wy$ and $x \\sim zw^{-1}$ are given by the equations  $x = swys^{-1}$ and $x = tzw^{-1}t^{-1}$.  


```scala
scala> val w = "w" :: FreeGroup 
w: RepTerm[Word] with Subs[RepTerm[Word]] = w

scala> val y = "y" :: FreeGroup 
y: RepTerm[Word] with Subs[RepTerm[Word]] = y

scala> val z = "z" :: FreeGroup 
z: RepTerm[Word] with Subs[RepTerm[Word]] = z

scala> val s = "s" :: FreeGroup 
s: RepTerm[Word] with Subs[RepTerm[Word]] = s

scala> val t = "t" :: FreeGroup 
t: RepTerm[Word] with Subs[RepTerm[Word]] = t
```



So far these are all independent. We shall introduce terms as witnesses for the above equations later, as we first prove a lemma not involving $x$.

## Statement of lemma

The main internal repetition trick is captured the bound

$$l((wy)^ns^{-1}t(zw^{-1})^n)\leq n(l(y) + l(z)) + l(s^{-1}) + l(t).$$

We define $c(n) = (wy)^ns^{-1}t(zw^{-1})^n$ and $f(n) = n(l(y) + l(z)) + l(s^{-1}) + l(t)$, so the inequality is $l(c(n))\leq(f(n))$. We encode this  below as the _main lemma_.



```scala
scala> val wy = w |+| y 
wy: LocalTerm = ((mul) (w)) (y)

scala> val zwbar = z |+| w.inverse 
zwbar: LocalTerm = ((mul) (z)) ((inv) (w))

scala> val wyn = FreeGroup.power(wy)(n) 
wyn: RepTerm[Word] = (<function1>) (n)

scala> val zwbarn = FreeGroup.power(zwbar)(n) 
zwbarn: RepTerm[Word] = (<function1>) (n)

scala>  

scala> val c = n :-> (wyn |+| s.inverse |+| t |+| zwbarn) // this is the function we have to bound. 
c: Func[RepTerm[spire.math.SafeLong] with Subs[RepTerm[spire.math.SafeLong]], LocalTerm] = (n :  Nat.Typ) ↦ (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n)))))

scala>  

scala> val r = incl(QField) 
r: Rec[LocalTerm] = <function1>

scala>  

scala> val f = n :-> (l(s.inverse) + l(t) + ((l(y) + l(z)) * r(n) ) ) 
f: Func[RepTerm[spire.math.SafeLong] with Subs[RepTerm[spire.math.SafeLong]], LocalTerm] = (n :  Nat.Typ) ↦ (((l) (t) + (l) ((inv) (s)) + ((l) (z) * (<function1>) (n)) + ((<function1>) (n) * (l) (y))))

scala>  

scala> val lemma = n :-> (leq (l(c(n)) )(f(n) ) ) 
lemma: Func[RepTerm[spire.math.SafeLong] with Subs[RepTerm[spire.math.SafeLong]], Pos] = (n :  Nat.Typ) ↦ (Pos((((<function1>) (n) * (l) (y)) + ((prod) (-1)) ((l) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n)))))) + ((<function1>) (n) * (l) (z)) + (l) ((inv) (s)) + (l) (t))))
```



## Proving the lemma

Let $d(n) = (wy)(wy)^ns^{-1}t(zw^{-1})^n(zw)$. Note that by definition $g^{n+1} = g^n g$, so we need to prove $d(n) = c(n+1)$.
We use $g^ng^m = g^{n+m}$ (proved in our code) to prove this. The proof is given by the term `dIsc`.


```scala
scala> val d = n :-> (wy |+| wyn |+| s.inverse |+| t |+| zwbarn  |+| zwbar) 
d: Func[RepTerm[spire.math.SafeLong] with Subs[RepTerm[spire.math.SafeLong]], LocalTerm] = (n :  Nat.Typ) ↦ (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))

scala> val dIsc = FreeGroup.rm(s.inverse |+| t |+| zwbarn |+| zwbar) *: (PowerDistributive.pf(wy)(nat(1))(n)) 
dIsc: Equality[LocalTerm] = (ind{($dpmt :  FreeGroup) ↦ (($dpmu :  FreeGroup) ↦ ($dpmt = $dpmu))(((mul) (w)) (((mul) (y)) ((<function1>) (n))))(((mul) ((<function1>) (n))) (((mul) (w)) (y)))}{($adxw :  FreeGroup) ↦ (($adxx :  FreeGroup) ↦ ((_ : ($adxw = $adxx) :  $adxw = $adxx) ↦ (((mul) ($adxw)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))) = ((mul) ($adxx)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))}(($adxw :  FreeGroup) ↦ (Refl(FreeGroup,((mul) ($adxw)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) ((<function1>) (n) : (((mul) (w)) (((mul) (y)) ((<function1>) (n))) = ((mul) ((<function1>) (n))) (((mul) (w)) (y)))) : (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))) = ((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))

scala> assert(dIsc.typ == (d(n) =:= c(succ(n))))
```



Next, let $b(n) = y(wy)^ns^{-1}t(zw^{-1})^nz$ be the conjugacy-reduced form of $d(n)$. Using $d(n) = c(n+1)$ and conjugacy invariance, we show that $l(c(n+1)) = l(b(n))$. The term `bIsc` is the proof of `l(c(n+1)) = l(b(n))`.


```scala
scala> val b = n :-> (y |+| wyn |+| s.inverse |+| t |+| zwbarn  |+| z) 
b: Func[RepTerm[spire.math.SafeLong] with Subs[RepTerm[spire.math.SafeLong]], LocalTerm] = (n :  Nat.Typ) ↦ (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z))))))

scala> val lbIsld = conjInv(w)(y |+| c(n) |+| z) 
lbIsld: Equality[RepTerm[spire.math.Rational]] = ((conjugacy-invariance) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))) : ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))) = (l) (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))

scala> val bIsc = lbIsld && (l *: dIsc) 
bIsc: Equality[RepTerm[spire.math.Rational] with Subs[RepTerm[spire.math.Rational]]] = ((ind{($stxr :  Q.Typ) ↦ (($stxs :  Q.Typ) ↦ ($stxr = $stxs))((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))))((l) (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))}{($hjcl :  Q.Typ) ↦ (($hjcm :  Q.Typ) ↦ ((_ : ($hjcl = $hjcm) :  $hjcl = $hjcm) ↦ (($hjcm = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) → ($hjcl = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))))))}(($hjcl :  Q.Typ) ↦ (($hjrb : ($hjcl = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) :  $hjcl = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) ↦ ($hjrb : ($hjcl = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))))))) (((conjugacy-invariance) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))) : ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))) = (l) (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))))) ((ind{($stxu :  FreeGroup) ↦ (($stxv :  FreeGroup) ↦ ($stxu = $stxv))(((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))(((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))}{($dtua :  FreeGroup) ↦ (($dtub :  FreeGroup) ↦ ((_ : ($dtua = $dtub) :  $dtua = $dtub) ↦ ((l) ($dtua) = (l) ($dtub))))}(($dtua :  FreeGroup) ↦ (Refl(Q.Typ,(l) ($dtua))))) ((ind{($dpmt :  FreeGroup) ↦ (($dpmu :  FreeGroup) ↦ ($dpmt = $dpmu))(((mul) (w)) (((mul) (y)) ((<function1>) (n))))(((mul) ((<function1>) (n))) (((mul) (w)) (y)))}{($adxw :  FreeGroup) ↦ (($adxx :  FreeGroup) ↦ ((_ : ($adxw = $adxx) :  $adxw = $adxx) ↦ (((mul) ($adxw)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))) = ((mul) ($adxx)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))}(($adxw :  FreeGroup) ↦ (Refl(FreeGroup,((mul) ($adxw)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) ((<function1>) (n) : (((mul) (w)) (((mul) (y)) ((<function1>) (n))) = ((mul) ((<function1>) (n))) (((mul) (w)) (y)))) : (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))) = ((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) : ((l) (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))) = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))) : ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))) = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))

scala> assert(bIsc.typ == (l(b(n)) =:= l(c(succ(n))) ))
```



With these preliminaries, we can prove the lemma by induction. First note the base case.


```scala
scala> val baseCase = triang(inv(s))(t) !: (lemma(0)) 
baseCase: PosWit = ((triangle-inequality) ((inv) (s))) (t) : (Pos((((prod) (-1)) ((l) (((mul) ((inv) (s))) (t))) + (l) ((inv) (s)) + (l) (t))))
```



The induction step takes more work.
We first assume the induction hypothesis.


```scala
scala> val hyp = "hyp" :: lemma(n) 
hyp: PosWit with Subs[PosWit] = hyp : (Pos((((prod) (-1)) ((l) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n)))))) + (l) ((inv) (s)) + (l) (t) + ((<function1>) (n) * (l) (z)) + ((<function1>) (n) * (l) (y)))))
```



Next, we bound $l(b(n))$ (which we know is $l(c(n+ 1)))$ in terms of $l(c(n))$.


```scala
scala> val lbnBoundedlcnlylz = triang(y)(c(n)) + triang(y |+| c(n))(z) 
lbnBoundedlcnlylz: PosWitSum = PosWitSum(((triangle-inequality) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))) : (Pos((((prod) (-1)) ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))))) + (l) (y) + (l) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n)))))))),((triangle-inequality) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))))) (z) : (Pos((((prod) (-1)) ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z))))))) + (l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n)))))) + (l) (z)))))

scala> assert(lbnBoundedlcnlylz.typ == (leq(l(b(n)))(l(c(n)) + l(y) + l(z))))
```


Now, using the induction hypothesis, we get a bound on $l(b(n))$.


```scala
scala> val lbnBounded = lbnBoundedlcnlylz + hyp 
lbnBounded: PosWitSum = PosWitSum(PosWitSum(((triangle-inequality) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))) : (Pos((((prod) (-1)) ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))))) + (l) (y) + (l) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n)))))))),((triangle-inequality) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))))) (z) : (Pos((((prod) (-1)) ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z))))))) + (l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n)))))) + (l) (z))))),hyp : (Pos((((prod) (-1)) ((l) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n)))))) + (l) ((inv) (s)) + (l) (t) + ((<function1>) (n) * (l) (z)) + ((<function1>) (n) * (l) (y))))))

scala> assert(lbnBounded.typ == leq(l(b(n)) )(f(succ(n))) )
```



Next, we put together $l(c(n+1)) = l(b(n))$ and the bound on $l(b(n))$ to get the required bound on $l(c(n+1))$.


```scala
scala> val bnd = "bound" :: QField.LocalTyp 
bnd: RepTerm[spire.math.Rational] with Subs[RepTerm[spire.math.Rational]] = bound

scala> val cbnd = bIsc.lift(bnd :-> (leq(bnd)(f(succ(n)) ) )) 
cbnd: Func[PosWit, PosWit] = (ind{($ciyea :  Q.Typ) ↦ (($ciyeb :  Q.Typ) ↦ ($ciyea = $ciyeb))((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))))((l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))}{($tghe :  Q.Typ) ↦ (($tghf :  Q.Typ) ↦ ((_ : ($tghe = $tghf) :  $tghe = $tghf) ↦ ((Pos(((l) (z) + (l) (y) + ((prod) (-1)) ($tghe) + (l) ((inv) (s)) + (l) (t) + ((<function1>) (n) * (l) (z)) + ((<function1>) (n) * (l) (y))))) → (Pos(((l) (z) + (l) (y) + (l) ((inv) (s)) + ((prod) (-1)) ($tghf) + (l) (t) + ((<function1>) (n) * (l) (z)) + ((<function1>) (n) * (l) (y))))))))}(($tghe :  Q.Typ) ↦ (($anlea : (Pos(((l) (z) + ((prod) (-1)) ($tghe) + (l) (y) + (l) ((inv) (s)) + (l) (t) + ((<function1>) (n) * (l) (z)) + ((<function1>) (n) * (l) (y))))) :  Pos(((l) (z) + ((prod) (-1)) ($tghe) + (l) (y) + (l) ((inv) (s)) + (l) (t) + ((<function1>) (n) * (l) (z)) + ((<function1>) (n) * (l) (y))))) ↦ ($anlea : (Pos(((l) (z) + ((prod) (-1)) ($tghe) + (l) (y) + (l) ((inv) (s)) + (l) (t) + ((<function1>) (n) * (l) (z)) + ((<function1>) (n) * (l) (y))))))))) (((ind{($stxr :  Q.Typ) ↦ (($stxs :  Q.Typ) ↦ ($stxr = $stxs))((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))))((l) (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))}{($hjcl :  Q.Typ) ↦ (($hjcm :  Q.Typ) ↦ ((_ : ($hjcl = $hjcm) :  $hjcl = $hjcm) ↦ (($hjcm = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) → ($hjcl = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))))))}(($hjcl :  Q.Typ) ↦ (($hjrb : ($hjcl = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) :  $hjcl = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) ↦ ($hjrb : ($hjcl = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))))))) (((conjugacy-invariance) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))) : ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))) = (l) (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))))) ((ind{($stxu :  FreeGroup) ↦ (($stxv :  FreeGroup) ↦ ($stxu = $stxv))(((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))(((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))}{($dtua :  FreeGroup) ↦ (($dtub :  FreeGroup) ↦ ((_ : ($dtua = $dtub) :  $dtua = $dtub) ↦ ((l) ($dtua) = (l) ($dtub))))}(($dtua :  FreeGroup) ↦ (Refl(Q.Typ,(l) ($dtua))))) ((ind{($dpmt :  FreeGroup) ↦ (($dpmu :  FreeGroup) ↦ ($dpmt = $dpmu))(((mul) (w)) (((mul) (y)) ((<function1>) (n))))(((mul) ((<function1>) (n))) (((mul) (w)) (y)))}{($adxw :  FreeGroup) ↦ (($adxx :  FreeGroup) ↦ ((_ : ($adxw = $adxx) :  $adxw = $adxx) ↦ (((mul) ($adxw)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))) = ((mul) ($adxx)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))}(($adxw :  FreeGroup) ↦ (Refl(FreeGroup,((mul) ($adxw)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) ((<function1>) (n) : (((mul) (w)) (((mul) (y)) ((<function1>) (n))) = ((mul) ((<function1>) (n))) (((mul) (w)) (y)))) : (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))) = ((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) : ((l) (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))) = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))) : ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul...

scala> val step = cbnd(lbnBounded) 
step: PosWit = ((ind{($ciyea :  Q.Typ) ↦ (($ciyeb :  Q.Typ) ↦ ($ciyea = $ciyeb))((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))))((l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))}{($tghe :  Q.Typ) ↦ (($tghf :  Q.Typ) ↦ ((_ : ($tghe = $tghf) :  $tghe = $tghf) ↦ ((Pos(((l) (z) + (l) (y) + ((prod) (-1)) ($tghe) + (l) ((inv) (s)) + (l) (t) + ((<function1>) (n) * (l) (z)) + ((<function1>) (n) * (l) (y))))) → (Pos(((l) (z) + (l) (y) + (l) ((inv) (s)) + ((prod) (-1)) ($tghf) + (l) (t) + ((<function1>) (n) * (l) (z)) + ((<function1>) (n) * (l) (y))))))))}(($tghe :  Q.Typ) ↦ (($anlea : (Pos(((l) (z) + ((prod) (-1)) ($tghe) + (l) (y) + (l) ((inv) (s)) + (l) (t) + ((<function1>) (n) * (l) (z)) + ((<function1>) (n) * (l) (y))))) :  Pos(((l) (z) + ((prod) (-1)) ($tghe) + (l) (y) + (l) ((inv) (s)) + (l) (t) + ((<function1>) (n) * (l) (z)) + ((<function1>) (n) * (l) (y))))) ↦ ($anlea : (Pos(((l) (z) + ((prod) (-1)) ($tghe) + (l) (y) + (l) ((inv) (s)) + (l) (t) + ((<function1>) (n) * (l) (z)) + ((<function1>) (n) * (l) (y))))))))) (((ind{($stxr :  Q.Typ) ↦ (($stxs :  Q.Typ) ↦ ($stxr = $stxs))((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))))((l) (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))}{($hjcl :  Q.Typ) ↦ (($hjcm :  Q.Typ) ↦ ((_ : ($hjcl = $hjcm) :  $hjcl = $hjcm) ↦ (($hjcm = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) → ($hjcl = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))))))}(($hjcl :  Q.Typ) ↦ (($hjrb : ($hjcl = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) :  $hjcl = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) ↦ ($hjrb : ($hjcl = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))))))) (((conjugacy-invariance) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))) : ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))) = (l) (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))))) ((ind{($stxu :  FreeGroup) ↦ (($stxv :  FreeGroup) ↦ ($stxu = $stxv))(((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))(((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))}{($dtua :  FreeGroup) ↦ (($dtub :  FreeGroup) ↦ ((_ : ($dtua = $dtub) :  $dtua = $dtub) ↦ ((l) ($dtua) = (l) ($dtub))))}(($dtua :  FreeGroup) ↦ (Refl(Q.Typ,(l) ($dtua))))) ((ind{($dpmt :  FreeGroup) ↦ (($dpmu :  FreeGroup) ↦ ($dpmt = $dpmu))(((mul) (w)) (((mul) (y)) ((<function1>) (n))))(((mul) ((<function1>) (n))) (((mul) (w)) (y)))}{($adxw :  FreeGroup) ↦ (($adxx :  FreeGroup) ↦ ((_ : ($adxw = $adxx) :  $adxw = $adxx) ↦ (((mul) ($adxw)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))) = ((mul) ($adxx)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))}(($adxw :  FreeGroup) ↦ (Refl(FreeGroup,((mul) ($adxw)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) ((<function1>) (n) : (((mul) (w)) (((mul) (y)) ((<function1>) (n))) = ((mul) ((<function1>) (n))) (((mul) (w)) (y)))) : (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))) = ((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) : ((l) (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))) = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))) : ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mu...

scala> assert(step.typ == lemma(succ(n)))
```



Finally, the lemma is proved by induction.


```scala
scala> val lemmaProof = Induc(lemma, baseCase, n :~> (hyp :-> step)) 
lemmaProof: Induc[PosWit with Subs[PosWit]] = <function1>

scala> assert(lemmaProof.typ == (n ~>: (lemma(n))) )
```



## Rest of the proof of the theorem

Some work remains, essentially to use symbolic algebra for equations such as $x^{2n} = x^nx^n$
and to put together equations and inequalities. We introduce terms witnessing the hypotheses $x=swys^{-1}$ and $x=tzw^{-1}t^{-1}$


```scala
scala> val x = "x" :: FreeGroup 
x: RepTerm[Word] with Subs[RepTerm[Word]] = x

scala> val g = "g" :: FreeGroup 
g: RepTerm[Word] with Subs[RepTerm[Word]] = g

scala> val pown = g :-> FreeGroup.power(g)(n) 
pown: Func[RepTerm[Word] with Subs[RepTerm[Word]], RepTerm[Word]] = (g :  FreeGroup) ↦ ((<function1>) (n))

scala>  

scala> val c1 = "x ~ wy" :: (x =:= (s |+| w |+| y |+| s.inverse)) 
c1: Equality[RepTerm[Word] with Subs[RepTerm[Word]]] with Subs[Equality[RepTerm[Word] with Subs[RepTerm[Word]]]] = x ~ wy : (x = ((mul) (s)) (((mul) (w)) (((mul) (y)) ((inv) (s)))))

scala> val c2 = "x ~ zw^{-1}" :: (x =:= (t |+| z |+| w.inverse |+| t.inverse)) 
c2: Equality[RepTerm[Word] with Subs[RepTerm[Word]]] with Subs[Equality[RepTerm[Word] with Subs[RepTerm[Word]]]] = x ~ zw^{-1} : (x = ((mul) (t)) (((mul) (z)) (((mul) ((inv) (w))) ((inv) (t)))))
```



We deduce using a theorem (in our code) about powers of conjugates that $x^n = s(wy)^ns^{-1} = t(zw^{-1})^nt^{-1}$.

```scala
scala> val xnConjwyn = (pown *: c1) && ConjPower.pf(s)(wy)(n) 
xnConjwyn: Equality[RepTerm[Word] with Subs[RepTerm[Word]]] = ((ind{($cnmhm :  FreeGroup) ↦ (($cnmhn :  FreeGroup) ↦ ($cnmhm = $cnmhn))((<function1>) (n))((<function1>) (n))}{($cmgsz :  FreeGroup) ↦ (($cmgta :  FreeGroup) ↦ ((_ : ($cmgsz = $cmgta) :  $cmgsz = $cmgta) ↦ (($cmgta = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))) → ($cmgsz = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))))))}(($cmgsz :  FreeGroup) ↦ (($cmhhp : ($cmgsz = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))) :  $cmgsz = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))) ↦ ($cmhhp : ($cmgsz = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))))))) ((ind{($cmkam :  FreeGroup) ↦ (($cmkan :  FreeGroup) ↦ ($cmkam = $cmkan))(x)(((mul) (s)) (((mul) (w)) (((mul) (y)) ((inv) (s)))))}{($ckpkx :  FreeGroup) ↦ (($ckpky :  FreeGroup) ↦ ((_ : ($ckpkx = $ckpky) :  $ckpkx = $ckpky) ↦ ((<function1>) (n) = (<function1>) (n))))}(($ckpkx :  FreeGroup) ↦ (Refl(FreeGroup,(<function1>) (n))))) (x ~ wy : (x = ((mul) (s)) (((mul) (w)) (((mul) (y)) ((inv) (s)))))) : ((<function1>) (n) = (<function1>) (n)))) ((<function1>) (n) : ((<function1>) (n) = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s))))) : ((<function1>) (n) = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s))))

scala> val xnConjzwbarn= (pown *: c2) && ConjPower.pf(t)(zwbar)(n) 
xnConjzwbarn: Equality[RepTerm[Word] with Subs[RepTerm[Word]]] = ((ind{($cpikk :  FreeGroup) ↦ (($cpikl :  FreeGroup) ↦ ($cpikk = $cpikl))((<function1>) (n))((<function1>) (n))}{($cobrd :  FreeGroup) ↦ (($cobre :  FreeGroup) ↦ ((_ : ($cobrd = $cobre) :  $cobrd = $cobre) ↦ (($cobre = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))) → ($cobrd = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))))))}(($cobrd :  FreeGroup) ↦ (($cocft : ($cobrd = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))) :  $cobrd = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))) ↦ ($cocft : ($cobrd = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))))))) ((ind{($coeyq :  FreeGroup) ↦ (($coeyr :  FreeGroup) ↦ ($coeyq = $coeyr))(x)(((mul) (t)) (((mul) (z)) (((mul) ((inv) (w))) ((inv) (t)))))}{($cnmhp :  FreeGroup) ↦ (($cnmhq :  FreeGroup) ↦ ((_ : ($cnmhp = $cnmhq) :  $cnmhp = $cnmhq) ↦ ((<function1>) (n) = (<function1>) (n))))}(($cnmhp :  FreeGroup) ↦ (Refl(FreeGroup,(<function1>) (n))))) (x ~ zw^{-1} : (x = ((mul) (t)) (((mul) (z)) (((mul) ((inv) (w))) ((inv) (t)))))) : ((<function1>) (n) = (<function1>) (n)))) ((<function1>) (n) : ((<function1>) (n) = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))) : ((<function1>) (n) = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))

scala> assert(xnConjwyn.typ == (pown(x) =:= (s |+| pown(wy)  |+| s.inverse  ) ) ) 


scala> assert(xnConjzwbarn.typ == (pown(x) =:= (t |+| pown(zwbar)  |+| t.inverse  ) ) )
```



We use the above equations to show that $x^nx^n = s(wy)^ns^{-1}t(zw^{-1})^nt^{-1}$.

```scala
scala> val t1 = s |+| pown(wy)  |+| s.inverse 
t1: LocalTerm = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))

scala> val t2 = t |+| pown(zwbar)  |+| t.inverse 
t2: LocalTerm = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))

scala> val xnxnExpr = (FreeGroup.rm(pown(x)) *: xnConjwyn) && (FreeGroup.lm(t1) *: xnConjzwbarn) 
xnxnExpr: Equality[LocalTerm with Subs[LocalTerm]] = ((ind{($cyhqn :  FreeGroup) ↦ (($cyhqo :  FreeGroup) ↦ ($cyhqn = $cyhqo))(((mul) ((<function1>) (n))) ((<function1>) (n)))(((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ((<function1>) (n)))))}{($ctiow :  FreeGroup) ↦ (($ctiox :  FreeGroup) ↦ ((_ : ($ctiow = $ctiox) :  $ctiow = $ctiox) ↦ (($ctiox = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) → ($ctiow = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))))))}(($ctiow :  FreeGroup) ↦ (($ctjdm : ($ctiow = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) :  $ctiow = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) ↦ ($ctjdm : ($ctiow = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))))))) ((ind{($ctpfz :  FreeGroup) ↦ (($ctpga :  FreeGroup) ↦ ($ctpfz = $ctpga))((<function1>) (n))(((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s))))}{($cpipx :  FreeGroup) ↦ (($cpipy :  FreeGroup) ↦ ((_ : ($cpipx = $cpipy) :  $cpipx = $cpipy) ↦ (((mul) ($cpipx)) ((<function1>) (n)) = ((mul) ($cpipy)) ((<function1>) (n)))))}(($cpipx :  FreeGroup) ↦ (Refl(FreeGroup,((mul) ($cpipx)) ((<function1>) (n)))))) (((ind{($cnmhm :  FreeGroup) ↦ (($cnmhn :  FreeGroup) ↦ ($cnmhm = $cnmhn))((<function1>) (n))((<function1>) (n))}{($cmgsz :  FreeGroup) ↦ (($cmgta :  FreeGroup) ↦ ((_ : ($cmgsz = $cmgta) :  $cmgsz = $cmgta) ↦ (($cmgta = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))) → ($cmgsz = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))))))}(($cmgsz :  FreeGroup) ↦ (($cmhhp : ($cmgsz = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))) :  $cmgsz = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))) ↦ ($cmhhp : ($cmgsz = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))))))) ((ind{($cmkam :  FreeGroup) ↦ (($cmkan :  FreeGroup) ↦ ($cmkam = $cmkan))(x)(((mul) (s)) (((mul) (w)) (((mul) (y)) ((inv) (s)))))}{($ckpkx :  FreeGroup) ↦ (($ckpky :  FreeGroup) ↦ ((_ : ($ckpkx = $ckpky) :  $ckpkx = $ckpky) ↦ ((<function1>) (n) = (<function1>) (n))))}(($ckpkx :  FreeGroup) ↦ (Refl(FreeGroup,(<function1>) (n))))) (x ~ wy : (x = ((mul) (s)) (((mul) (w)) (((mul) (y)) ((inv) (s)))))) : ((<function1>) (n) = (<function1>) (n)))) ((<function1>) (n) : ((<function1>) (n) = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s))))) : ((<function1>) (n) = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s))))) : (((mul) ((<function1>) (n))) ((<function1>) (n)) = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ((<function1>) (n))))))) ((ind{($cyhqq :  FreeGroup) ↦ (($cyhqr :  FreeGroup) ↦ ($cyhqq = $cyhqr))((<function1>) (n))(((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))}{($cqxma :  FreeGroup) ↦ (($cqxmb :  FreeGroup) ↦ ((_ : ($cqxma = $cqxmb) :  $cqxma = $cqxmb) ↦ (((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ($cqxma))) = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ($cqxmb))))))}(($cqxma :  FreeGroup) ↦ (Refl(FreeGroup,((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ($cqxma))))))) (((ind{($cpikk :  FreeGroup) ↦ (($cpikl :  FreeGroup) ↦ ($cpikk = $cpikl))((<function1>) (n))((<function1>) (n))}{($cobrd :  FreeGroup) ↦ (($cobre :  FreeGroup) ↦ ((_ : ($cobrd = $cobre) :  $cobrd = $cobre) ↦ (($cobre = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))) → ($cobrd = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))))))}(($cobrd :  FreeGroup) ↦ (($cocft : ($cobrd = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))) :  $cobrd = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))) ↦ ($cocft : ($cobrd = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))))))) ((ind{($coeyq :  FreeGroup) ↦ (($coeyr :  FreeGroup) ↦ ($coeyq = $coeyr))(x)(((mul) (t)) (((mul) (z)) (((mul) ((inv) (w))) ((inv) (t)))))}{($cnmhp :  FreeGroup) ↦ (($cnmhq :  FreeGroup) ↦ ((_ : ($cnmhp = $cnmhq) :  $cnmhp = $cnmhq) ↦ ((<function1>) (n) = (<function1>) (n))))}(($cnmhp :  FreeGroup) ↦ (Refl(FreeGroup,(<function1>) (n))))) (x ~ zw^{-1} : (x = ((mul) (t)) (((mul) (z)) (((mul) ((inv) (w))) ((inv) (t)))))) : ((<function1>) (n) = (<function1>) (n)))) ((<function1>) (n) : ((<function1>) (n) = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))) : ((<function1>) (n) = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))) : (((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ((<function1>) (n)))) = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))))))) : (((mul) ((<function1>) (n))) ((<function1>) (n)) = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))))))

scala> assert(xnxnExpr.typ == ((pown(x) |+| pown(x)) =:= (t1 |+| t2 )   ))
```



Using $x^nx^n = x^{2n}$, we get the formula $x^{2n} = s(wy)^ns^{-1}t(zw^{-1})^nt^{-1}$.

```scala
scala> val x2nExpr =PowerDistributive.pf(x)(n)(n).sym && xnxnExpr 
x2nExpr: Equality[LocalTerm with Subs[LocalTerm] with Subs[LocalTerm with Subs[LocalTerm]]] = ((ind{($dawxw :  FreeGroup) ↦ (($dawxx :  FreeGroup) ↦ ($dawxw = $dawxx))((<function1>) (((prod) (2)) (n)))(((mul) ((<function1>) (n))) ((<function1>) (n)))}{($cywvb :  FreeGroup) ↦ (($cywvc :  FreeGroup) ↦ ((_ : ($cywvb = $cywvc) :  $cywvb = $cywvc) ↦ (($cywvc = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) → ($cywvb = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))))))}(($cywvb :  FreeGroup) ↦ (($cyxjr : ($cywvb = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) :  $cywvb = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) ↦ ($cyxjr : ($cywvb = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))))))) ((ind{($czaqz :  FreeGroup) ↦ (($czara :  FreeGroup) ↦ ($czaqz = $czara))(((mul) ((<function1>) (n))) ((<function1>) (n)))((<function1>) (((prod) (2)) (n)))}{($cykyp :  FreeGroup) ↦ (($cykyq :  FreeGroup) ↦ ((_ : ($cykyp = $cykyq) :  $cykyp = $cykyq) ↦ ($cykyq = $cykyp)))}(($cykyp :  FreeGroup) ↦ (Refl(FreeGroup,$cykyp)))) ((<function1>) (n) : (((mul) ((<function1>) (n))) ((<function1>) (n)) = (<function1>) (((prod) (2)) (n)))) : ((<function1>) (((prod) (2)) (n)) = ((mul) ((<function1>) (n))) ((<function1>) (n))))) (((ind{($cyhqn :  FreeGroup) ↦ (($cyhqo :  FreeGroup) ↦ ($cyhqn = $cyhqo))(((mul) ((<function1>) (n))) ((<function1>) (n)))(((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ((<function1>) (n)))))}{($ctiow :  FreeGroup) ↦ (($ctiox :  FreeGroup) ↦ ((_ : ($ctiow = $ctiox) :  $ctiow = $ctiox) ↦ (($ctiox = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) → ($ctiow = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))))))}(($ctiow :  FreeGroup) ↦ (($ctjdm : ($ctiow = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) :  $ctiow = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) ↦ ($ctjdm : ($ctiow = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))))))) ((ind{($ctpfz :  FreeGroup) ↦ (($ctpga :  FreeGroup) ↦ ($ctpfz = $ctpga))((<function1>) (n))(((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s))))}{($cpipx :  FreeGroup) ↦ (($cpipy :  FreeGroup) ↦ ((_ : ($cpipx = $cpipy) :  $cpipx = $cpipy) ↦ (((mul) ($cpipx)) ((<function1>) (n)) = ((mul) ($cpipy)) ((<function1>) (n)))))}(($cpipx :  FreeGroup) ↦ (Refl(FreeGroup,((mul) ($cpipx)) ((<function1>) (n)))))) (((ind{($cnmhm :  FreeGroup) ↦ (($cnmhn :  FreeGroup) ↦ ($cnmhm = $cnmhn))((<function1>) (n))((<function1>) (n))}{($cmgsz :  FreeGroup) ↦ (($cmgta :  FreeGroup) ↦ ((_ : ($cmgsz = $cmgta) :  $cmgsz = $cmgta) ↦ (($cmgta = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))) → ($cmgsz = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))))))}(($cmgsz :  FreeGroup) ↦ (($cmhhp : ($cmgsz = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))) :  $cmgsz = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))) ↦ ($cmhhp : ($cmgsz = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))))))) ((ind{($cmkam :  FreeGroup) ↦ (($cmkan :  FreeGroup) ↦ ($cmkam = $cmkan))(x)(((mul) (s)) (((mul) (w)) (((mul) (y)) ((inv) (s)))))}{($ckpkx :  FreeGroup) ↦ (($ckpky :  FreeGroup) ↦ ((_ : ($ckpkx = $ckpky) :  $ckpkx = $ckpky) ↦ ((<function1>) (n) = (<function1>) (n))))}(($ckpkx :  FreeGroup) ↦ (Refl(FreeGroup,(<function1>) (n))))) (x ~ wy : (x = ((mul) (s)) (((mul) (w)) (((mul) (y)) ((inv) (s)))))) : ((<function1>) (n) = (<function1>) (n)))) ((<function1>) (n) : ((<function1>) (n) = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s))))) : ((<function1>) (n) = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s))))) : (((mul) ((<function1>) (n))) ((<function1>) (n)) = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ((<function1>) (n))))))) ((ind{($cyhqq :  FreeGroup) ↦ (($cyhqr :  FreeGroup) ↦ ($cyhqq = $cyhqr))((<function1>) (n))(((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))}{($cqxma :  FreeGroup) ↦ (($cqxmb :  FreeGroup) ↦ ((_ : ($cqxma = $cqxmb) :  $cqxma = $cqxmb) ↦ (((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ($cqxma))) = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ($cqxmb))))))}(($cqxma :  FreeGroup) ↦ (Refl(FreeGroup,((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ($cqxma))))))) (((ind{($cpikk :  FreeGroup) ↦ (($cpikl :  FreeGroup) ↦ ($cpikk = $cpikl))((<function1>) (n))((<function1>) (n))}{($...

scala> assert(x2nExpr.typ == (FreeGroup.power(x)(NatRing.prod(n)(nat(2))) =:= (s |+| c(n) |+| t.inverse)))
```



We now bound the length of the right hand side $s(wy)^ns^{-1}t(zw^{-1})^nt^{-1}$.


```scala
scala> val thmBound = f(n) + l(s) + l(t.inverse) 
thmBound: LocalTerm = ((l) (s) + (l) ((inv) (t)) + (l) ((inv) (s)) + (l) (t) + ((<function1>) (n) * (l) (z)) + ((<function1>) (n) * (l) (y)))

scala> val exprBound = lemmaProof(n) + triang(s)(c(n)) + triang(s |+| c(n))(t.inverse) 
exprBound: PosWitSum = PosWitSum(PosWitSum((<function1>) (n) : (Pos((((prod) (-1)) ((l) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n)))))) + (l) ((inv) (s)) + (l) (t) + ((<function1>) (n) * (l) (z)) + ((<function1>) (n) * (l) (y))))),((triangle-inequality) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))) : (Pos((((prod) (-1)) ((l) (((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))))) + (l) (s) + (l) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))))))),((triangle-inequality) (((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))))) ((inv) (t)) : (Pos((((prod) (-1)) ((l) (((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))))))) + (l) (((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n)))))) + (l) ((inv) (t))))))

scala> assert(exprBound.typ == leq(l(s |+| c(n) |+| t.inverse ))(thmBound))
```



We easily deduce the bound on $l(x^{2n})$ to complete the proof.


```scala
scala> val thmProof = x2nExpr.sym.lift (g :-> leq(l(g))(thmBound))(exprBound) 
thmProof: PosWit = ((ind{($fqtdr :  FreeGroup) ↦ (($fqtds :  FreeGroup) ↦ ($fqtdr = $fqtds))(((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))))))((<function1>) (((prod) (2)) (n)))}{($detqw :  FreeGroup) ↦ (($detqx :  FreeGroup) ↦ ((_ : ($detqw = $detqx) :  $detqw = $detqx) ↦ ((Pos(((l) (s) + (l) ((inv) (t)) + ((prod) (-1)) ((l) ($detqw)) + (l) ((inv) (s)) + (l) (t) + ((<function1>) (n) * (l) (z)) + ((<function1>) (n) * (l) (y))))) → (Pos(((l) (s) + (l) ((inv) (t)) + ((prod) (-1)) ((l) ($detqx)) + (l) ((inv) (s)) + (l) (t) + ((<function1>) (n) * (l) (z)) + ((<function1>) (n) * (l) (y))))))))}(($detqw :  FreeGroup) ↦ (($ecgvs : (Pos(((l) (s) + (l) ((inv) (t)) + (l) ((inv) (s)) + ((prod) (-1)) ((l) ($detqw)) + (l) (t) + ((<function1>) (n) * (l) (z)) + ((<function1>) (n) * (l) (y))))) :  Pos(((l) (s) + (l) ((inv) (t)) + (l) ((inv) (s)) + ((prod) (-1)) ((l) ($detqw)) + (l) (t) + ((<function1>) (n) * (l) (z)) + ((<function1>) (n) * (l) (y))))) ↦ ($ecgvs : (Pos(((l) (s) + (l) ((inv) (t)) + (l) ((inv) (s)) + ((prod) (-1)) ((l) ($detqw)) + (l) (t) + ((<function1>) (n) * (l) (z)) + ((<function1>) (n) * (l) (y))))))))) ((ind{($fhavn :  FreeGroup) ↦ (($fhavo :  FreeGroup) ↦ ($fhavn = $fhavo))((<function1>) (((prod) (2)) (n)))(((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))))))}{($dbhxy :  FreeGroup) ↦ (($dbhxz :  FreeGroup) ↦ ((_ : ($dbhxy = $dbhxz) :  $dbhxy = $dbhxz) ↦ ($dbhxz = $dbhxy)))}(($dbhxy :  FreeGroup) ↦ (Refl(FreeGroup,$dbhxy)))) (((ind{($dawxw :  FreeGroup) ↦ (($dawxx :  FreeGroup) ↦ ($dawxw = $dawxx))((<function1>) (((prod) (2)) (n)))(((mul) ((<function1>) (n))) ((<function1>) (n)))}{($cywvb :  FreeGroup) ↦ (($cywvc :  FreeGroup) ↦ ((_ : ($cywvb = $cywvc) :  $cywvb = $cywvc) ↦ (($cywvc = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) → ($cywvb = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))))))}(($cywvb :  FreeGroup) ↦ (($cyxjr : ($cywvb = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) :  $cywvb = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) ↦ ($cyxjr : ($cywvb = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))))))) ((ind{($czaqz :  FreeGroup) ↦ (($czara :  FreeGroup) ↦ ($czaqz = $czara))(((mul) ((<function1>) (n))) ((<function1>) (n)))((<function1>) (((prod) (2)) (n)))}{($cykyp :  FreeGroup) ↦ (($cykyq :  FreeGroup) ↦ ((_ : ($cykyp = $cykyq) :  $cykyp = $cykyq) ↦ ($cykyq = $cykyp)))}(($cykyp :  FreeGroup) ↦ (Refl(FreeGroup,$cykyp)))) ((<function1>) (n) : (((mul) ((<function1>) (n))) ((<function1>) (n)) = (<function1>) (((prod) (2)) (n)))) : ((<function1>) (((prod) (2)) (n)) = ((mul) ((<function1>) (n))) ((<function1>) (n))))) (((ind{($cyhqn :  FreeGroup) ↦ (($cyhqo :  FreeGroup) ↦ ($cyhqn = $cyhqo))(((mul) ((<function1>) (n))) ((<function1>) (n)))(((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ((<function1>) (n)))))}{($ctiow :  FreeGroup) ↦ (($ctiox :  FreeGroup) ↦ ((_ : ($ctiow = $ctiox) :  $ctiow = $ctiox) ↦ (($ctiox = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) → ($ctiow = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))))))}(($ctiow :  FreeGroup) ↦ (($ctjdm : ($ctiow = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) :  $ctiow = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) ↦ ($ctjdm : ($ctiow = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))))))) ((ind{($ctpfz :  FreeGroup) ↦ (($ctpga :  FreeGroup) ↦ ($ctpfz = $ctpga))((<function1>) (n))(((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s))))}{($cpipx :  FreeGroup) ↦ (($cpipy :  FreeGroup) ↦ ((_ : ($cpipx = $cpipy) :  $cpipx = $cpipy) ↦ (((mul) ($cpipx)) ((<function1>) (n)) = ((mul) ($cpipy)) ((<function1>) (n)))))}(($cpipx :  FreeGroup) ↦ (Refl(FreeGroup,((mul) ($cpipx)) ((<function1>) (n)))))) (((ind{($cnmhm :  FreeGroup) ↦ (($cnmhn :  FreeGroup) ↦ ($cnmhm = $cnmhn))((<function1>) (n))((<function1>) (n))}{($cmgsz :  FreeGroup) ↦ (($cmgta :  FreeGroup) ↦ ((_ : ($cmgsz = $cmgta) :  $cmgsz = $cmgta) ↦ (($cmgta = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))) → ($cmgsz = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))))))}(($cmgsz :  FreeGroup) ↦ (($cmhhp : ($cmgsz = ((mul) (s)) (((mul) (...

scala> val x2n = FreeGroup.power(x)(NatRing.prod(n)(nat(2))) 
x2n: RepTerm[Word] = (<function1>) (((prod) (2)) (n))

scala> assert(thmProof.typ == leq(l(x2n))(thmBound )) 


scala>
```



