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

scala> import functionfinder._, andrewscurtis.FreeGroups._ 
import functionfinder._, andrewscurtis.FreeGroups._

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
f: Func[RepTerm[spire.math.SafeLong] with Subs[RepTerm[spire.math.SafeLong]], LocalTerm] = (n :  Nat.Typ) ↦ (((l) (t) + (l) ((inv) (s)) + ((<function1>) (n) * (l) (z)) + ((<function1>) (n) * (l) (y))))

scala>  

scala> val lemma = n :-> (leq (l(c(n)) )(f(n) ) ) 
lemma: Func[RepTerm[spire.math.SafeLong] with Subs[RepTerm[spire.math.SafeLong]], Pos] = (n :  Nat.Typ) ↦ (Pos((((<function1>) (n) * (l) (y)) + ((<function1>) (n) * (l) (z)) + (l) (t) + (l) ((inv) (s)) + ((prod) (-1)) ((l) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n)))))))))
```



## Proving the lemma

Let $d(n) = (wy)(wy)^ns^{-1}t(zw^{-1})^n(zw)$. Note that by definition $g^{n+1} = g^n g$, so we need to prove $d(n) = c(n+1)$.
We use $g^ng^m = g^{n+m}$ (proved in our code) to prove this. The proof is given by the term `dIsc`.


```scala
scala> val d = n :-> (wy |+| wyn |+| s.inverse |+| t |+| zwbarn  |+| zwbar) 
d: Func[RepTerm[spire.math.SafeLong] with Subs[RepTerm[spire.math.SafeLong]], LocalTerm] = (n :  Nat.Typ) ↦ (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))

scala> val dIsc = FreeGroup.rm(s.inverse |+| t |+| zwbarn |+| zwbar) *: (PowerDistributive.pf(wy)(nat(1))(n)) 
dIsc: Equality[LocalTerm] = (ind{($dplp :  FreeGroup) ↦ (($dplq :  FreeGroup) ↦ ($dplp = $dplq))(((mul) (w)) (((mul) (y)) ((<function1>) (n))))(((mul) ((<function1>) (n))) (((mul) (w)) (y)))}{($adws :  FreeGroup) ↦ (($adwt :  FreeGroup) ↦ ((_ : ($adws = $adwt) :  $adws = $adwt) ↦ (((mul) ($adws)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))) = ((mul) ($adwt)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))}(($adws :  FreeGroup) ↦ (Refl(FreeGroup,((mul) ($adws)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) ((<function1>) (n) : (((mul) (w)) (((mul) (y)) ((<function1>) (n))) = ((mul) ((<function1>) (n))) (((mul) (w)) (y)))) : (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))) = ((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))

scala> assert(dIsc.typ == (d(n) =:= c(succ(n))))
```



Next, let $b(n) = y(wy)^ns^{-1}t(zw^{-1})^nz$ be the conjugacy-reduced form of $d(n)$. Using $d(n) = c(n+1)$ and conjugacy invariance, we show that $l(c(n+1)) = l(b(n))$. The term `bIsc` is the proof of `l(c(n+1)) = l(b(n))`.


```scala
scala> val b = n :-> (y |+| wyn |+| s.inverse |+| t |+| zwbarn  |+| z) 
b: Func[RepTerm[spire.math.SafeLong] with Subs[RepTerm[spire.math.SafeLong]], LocalTerm] = (n :  Nat.Typ) ↦ (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z))))))

scala> val lbIsld = conjInv(w)(y |+| c(n) |+| z) 
lbIsld: Equality[RepTerm[spire.math.Rational]] = ((conjugacy-invariance) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))) : ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))) = (l) (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))

scala> val bIsc = lbIsld && (l *: dIsc) 
bIsc: Equality[RepTerm[spire.math.Rational] with Subs[RepTerm[spire.math.Rational]]] = ((ind{($stwn :  Q.Typ) ↦ (($stwo :  Q.Typ) ↦ ($stwn = $stwo))((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))))((l) (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))}{($hjbh :  Q.Typ) ↦ (($hjbi :  Q.Typ) ↦ ((_ : ($hjbh = $hjbi) :  $hjbh = $hjbi) ↦ (($hjbi = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) → ($hjbh = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))))))}(($hjbh :  Q.Typ) ↦ (($hjpx : ($hjbh = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) :  $hjbh = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) ↦ ($hjpx : ($hjbh = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))))))) (((conjugacy-invariance) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))) : ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))) = (l) (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))))) ((ind{($stwq :  FreeGroup) ↦ (($stwr :  FreeGroup) ↦ ($stwq = $stwr))(((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))(((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))}{($dtsw :  FreeGroup) ↦ (($dtsx :  FreeGroup) ↦ ((_ : ($dtsw = $dtsx) :  $dtsw = $dtsx) ↦ ((l) ($dtsw) = (l) ($dtsx))))}(($dtsw :  FreeGroup) ↦ (Refl(Q.Typ,(l) ($dtsw))))) ((ind{($dplp :  FreeGroup) ↦ (($dplq :  FreeGroup) ↦ ($dplp = $dplq))(((mul) (w)) (((mul) (y)) ((<function1>) (n))))(((mul) ((<function1>) (n))) (((mul) (w)) (y)))}{($adws :  FreeGroup) ↦ (($adwt :  FreeGroup) ↦ ((_ : ($adws = $adwt) :  $adws = $adwt) ↦ (((mul) ($adws)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))) = ((mul) ($adwt)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))}(($adws :  FreeGroup) ↦ (Refl(FreeGroup,((mul) ($adws)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) ((<function1>) (n) : (((mul) (w)) (((mul) (y)) ((<function1>) (n))) = ((mul) ((<function1>) (n))) (((mul) (w)) (y)))) : (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))) = ((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) : ((l) (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))) = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))) : ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))) = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))

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
hyp: PosWit with Subs[PosWit] = hyp : (Pos((((<function1>) (n) * (l) (y)) + ((<function1>) (n) * (l) (z)) + ((prod) (-1)) ((l) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n)))))) + (l) (t) + (l) ((inv) (s)))))
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
lbnBounded: PosWitSum = PosWitSum(PosWitSum(((triangle-inequality) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))) : (Pos((((prod) (-1)) ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))))) + (l) (y) + (l) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n)))))))),((triangle-inequality) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))))) (z) : (Pos((((prod) (-1)) ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z))))))) + (l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n)))))) + (l) (z))))),hyp : (Pos((((<function1>) (n) * (l) (y)) + ((<function1>) (n) * (l) (z)) + ((prod) (-1)) ((l) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n)))))) + (l) (t) + (l) ((inv) (s))))))

scala> assert(lbnBounded.typ == leq(l(b(n)) )(f(succ(n))) )
```



Next, we put together $l(c(n+1)) = l(b(n))$ and the bound on $l(b(n))$ to get the required bound on $l(c(n+1))$.


```scala
scala> val bnd = "bound" :: QField.LocalTyp 
bnd: RepTerm[spire.math.Rational] with Subs[RepTerm[spire.math.Rational]] = bound

scala> val cbnd = bIsc.lift(bnd :-> (leq(bnd)(f(succ(n)) ) )) 
cbnd: Func[PosWit, PosWit] = (ind{($budme :  Q.Typ) ↦ (($budmf :  Q.Typ) ↦ ($budme = $budmf))((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))))((l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))}{($tdgm :  Q.Typ) ↦ (($tdgn :  Q.Typ) ↦ ((_ : ($tdgm = $tdgn) :  $tdgm = $tdgn) ↦ ((Pos(((l) (z) + ((<function1>) (n) * (l) (y)) + ((<function1>) (n) * (l) (z)) + (l) (y) + (l) (t) + (l) ((inv) (s)) + ((prod) (-1)) ($tdgm)))) → (Pos(((l) (z) + ((<function1>) (n) * (l) (y)) + ((<function1>) (n) * (l) (z)) + ((prod) (-1)) ($tdgn) + (l) (y) + (l) (t) + (l) ((inv) (s))))))))}(($tdgm :  Q.Typ) ↦ (($aiqem : (Pos(((l) (z) + ((<function1>) (n) * (l) (y)) + ((<function1>) (n) * (l) (z)) + (l) (y) + (l) (t) + (l) ((inv) (s)) + ((prod) (-1)) ($tdgm)))) :  Pos(((l) (z) + ((<function1>) (n) * (l) (y)) + ((<function1>) (n) * (l) (z)) + (l) (y) + (l) (t) + (l) ((inv) (s)) + ((prod) (-1)) ($tdgm)))) ↦ ($aiqem : (Pos(((l) (z) + ((<function1>) (n) * (l) (y)) + ((<function1>) (n) * (l) (z)) + (l) (y) + (l) (t) + (l) ((inv) (s)) + ((prod) (-1)) ($tdgm)))))))) (((ind{($stwn :  Q.Typ) ↦ (($stwo :  Q.Typ) ↦ ($stwn = $stwo))((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))))((l) (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))}{($hjbh :  Q.Typ) ↦ (($hjbi :  Q.Typ) ↦ ((_ : ($hjbh = $hjbi) :  $hjbh = $hjbi) ↦ (($hjbi = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) → ($hjbh = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))))))}(($hjbh :  Q.Typ) ↦ (($hjpx : ($hjbh = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) :  $hjbh = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) ↦ ($hjpx : ($hjbh = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))))))) (((conjugacy-invariance) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))) : ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))) = (l) (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))))) ((ind{($stwq :  FreeGroup) ↦ (($stwr :  FreeGroup) ↦ ($stwq = $stwr))(((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))(((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))}{($dtsw :  FreeGroup) ↦ (($dtsx :  FreeGroup) ↦ ((_ : ($dtsw = $dtsx) :  $dtsw = $dtsx) ↦ ((l) ($dtsw) = (l) ($dtsx))))}(($dtsw :  FreeGroup) ↦ (Refl(Q.Typ,(l) ($dtsw))))) ((ind{($dplp :  FreeGroup) ↦ (($dplq :  FreeGroup) ↦ ($dplp = $dplq))(((mul) (w)) (((mul) (y)) ((<function1>) (n))))(((mul) ((<function1>) (n))) (((mul) (w)) (y)))}{($adws :  FreeGroup) ↦ (($adwt :  FreeGroup) ↦ ((_ : ($adws = $adwt) :  $adws = $adwt) ↦ (((mul) ($adws)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))) = ((mul) ($adwt)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))}(($adws :  FreeGroup) ↦ (Refl(FreeGroup,((mul) ($adws)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) ((<function1>) (n) : (((mul) (w)) (((mul) (y)) ((<function1>) (n))) = ((mul) ((<function1>) (n))) (((mul) (w)) (y)))) : (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))) = ((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) : ((l) (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))) = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))) : ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul...

scala> val step = cbnd(lbnBounded) 
step: PosWit = ((ind{($budme :  Q.Typ) ↦ (($budmf :  Q.Typ) ↦ ($budme = $budmf))((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))))((l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))}{($tdgm :  Q.Typ) ↦ (($tdgn :  Q.Typ) ↦ ((_ : ($tdgm = $tdgn) :  $tdgm = $tdgn) ↦ ((Pos(((l) (z) + ((<function1>) (n) * (l) (y)) + ((<function1>) (n) * (l) (z)) + (l) (y) + (l) (t) + (l) ((inv) (s)) + ((prod) (-1)) ($tdgm)))) → (Pos(((l) (z) + ((<function1>) (n) * (l) (y)) + ((<function1>) (n) * (l) (z)) + ((prod) (-1)) ($tdgn) + (l) (y) + (l) (t) + (l) ((inv) (s))))))))}(($tdgm :  Q.Typ) ↦ (($aiqem : (Pos(((l) (z) + ((<function1>) (n) * (l) (y)) + ((<function1>) (n) * (l) (z)) + (l) (y) + (l) (t) + (l) ((inv) (s)) + ((prod) (-1)) ($tdgm)))) :  Pos(((l) (z) + ((<function1>) (n) * (l) (y)) + ((<function1>) (n) * (l) (z)) + (l) (y) + (l) (t) + (l) ((inv) (s)) + ((prod) (-1)) ($tdgm)))) ↦ ($aiqem : (Pos(((l) (z) + ((<function1>) (n) * (l) (y)) + ((<function1>) (n) * (l) (z)) + (l) (y) + (l) (t) + (l) ((inv) (s)) + ((prod) (-1)) ($tdgm)))))))) (((ind{($stwn :  Q.Typ) ↦ (($stwo :  Q.Typ) ↦ ($stwn = $stwo))((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))))((l) (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))}{($hjbh :  Q.Typ) ↦ (($hjbi :  Q.Typ) ↦ ((_ : ($hjbh = $hjbi) :  $hjbh = $hjbi) ↦ (($hjbi = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) → ($hjbh = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))))))}(($hjbh :  Q.Typ) ↦ (($hjpx : ($hjbh = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) :  $hjbh = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) ↦ ($hjpx : ($hjbh = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))))))) (((conjugacy-invariance) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))) : ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (z)))))) = (l) (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))))) ((ind{($stwq :  FreeGroup) ↦ (($stwr :  FreeGroup) ↦ ($stwq = $stwr))(((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))(((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))}{($dtsw :  FreeGroup) ↦ (($dtsx :  FreeGroup) ↦ ((_ : ($dtsw = $dtsx) :  $dtsw = $dtsx) ↦ ((l) ($dtsw) = (l) ($dtsx))))}(($dtsw :  FreeGroup) ↦ (Refl(Q.Typ,(l) ($dtsw))))) ((ind{($dplp :  FreeGroup) ↦ (($dplq :  FreeGroup) ↦ ($dplp = $dplq))(((mul) (w)) (((mul) (y)) ((<function1>) (n))))(((mul) ((<function1>) (n))) (((mul) (w)) (y)))}{($adws :  FreeGroup) ↦ (($adwt :  FreeGroup) ↦ ((_ : ($adws = $adwt) :  $adws = $adwt) ↦ (((mul) ($adws)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))) = ((mul) ($adwt)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))}(($adws :  FreeGroup) ↦ (Refl(FreeGroup,((mul) ($adws)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) ((<function1>) (n) : (((mul) (w)) (((mul) (y)) ((<function1>) (n))) = ((mul) ((<function1>) (n))) (((mul) (w)) (y)))) : (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))) = ((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w)))))))))) : ((l) (((mul) (w)) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))) = (l) (((mul) ((<function1>) (n))) (((mul) (w)) (((mul) (y)) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) (((mul) (z)) ((inv) (w))))))))))) : ((l) (((mul) (y)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mu...

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
xnConjwyn: Equality[RepTerm[Word] with Subs[RepTerm[Word]]] = ((ind{($byozu :  FreeGroup) ↦ (($byozv :  FreeGroup) ↦ ($byozu = $byozv))((<function1>) (n))((<function1>) (n))}{($bxjlh :  FreeGroup) ↦ (($bxjli :  FreeGroup) ↦ ((_ : ($bxjlh = $bxjli) :  $bxjlh = $bxjli) ↦ (($bxjli = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))) → ($bxjlh = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))))))}(($bxjlh :  FreeGroup) ↦ (($bxjzx : ($bxjlh = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))) :  $bxjlh = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))) ↦ ($bxjzx : ($bxjlh = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))))))) ((ind{($bxmsu :  FreeGroup) ↦ (($bxmsv :  FreeGroup) ↦ ($bxmsu = $bxmsv))(x)(((mul) (s)) (((mul) (w)) (((mul) (y)) ((inv) (s)))))}{($bvsdf :  FreeGroup) ↦ (($bvsdg :  FreeGroup) ↦ ((_ : ($bvsdf = $bvsdg) :  $bvsdf = $bvsdg) ↦ ((<function1>) (n) = (<function1>) (n))))}(($bvsdf :  FreeGroup) ↦ (Refl(FreeGroup,(<function1>) (n))))) (x ~ wy : (x = ((mul) (s)) (((mul) (w)) (((mul) (y)) ((inv) (s)))))) : ((<function1>) (n) = (<function1>) (n)))) ((<function1>) (n) : ((<function1>) (n) = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s))))) : ((<function1>) (n) = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s))))

scala> val xnConjzwbarn= (pown *: c2) && ConjPower.pf(t)(zwbar)(n) 
xnConjzwbarn: Equality[RepTerm[Word] with Subs[RepTerm[Word]]] = ((ind{($calcs :  FreeGroup) ↦ (($calct :  FreeGroup) ↦ ($calcs = $calct))((<function1>) (n))((<function1>) (n))}{($bzejl :  FreeGroup) ↦ (($bzejm :  FreeGroup) ↦ ((_ : ($bzejl = $bzejm) :  $bzejl = $bzejm) ↦ (($bzejm = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))) → ($bzejl = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))))))}(($bzejl :  FreeGroup) ↦ (($bzeyb : ($bzejl = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))) :  $bzejl = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))) ↦ ($bzeyb : ($bzejl = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))))))) ((ind{($bzhqy :  FreeGroup) ↦ (($bzhqz :  FreeGroup) ↦ ($bzhqy = $bzhqz))(x)(((mul) (t)) (((mul) (z)) (((mul) ((inv) (w))) ((inv) (t)))))}{($byozx :  FreeGroup) ↦ (($byozy :  FreeGroup) ↦ ((_ : ($byozx = $byozy) :  $byozx = $byozy) ↦ ((<function1>) (n) = (<function1>) (n))))}(($byozx :  FreeGroup) ↦ (Refl(FreeGroup,(<function1>) (n))))) (x ~ zw^{-1} : (x = ((mul) (t)) (((mul) (z)) (((mul) ((inv) (w))) ((inv) (t)))))) : ((<function1>) (n) = (<function1>) (n)))) ((<function1>) (n) : ((<function1>) (n) = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))) : ((<function1>) (n) = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))

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
xnxnExpr: Equality[LocalTerm with Subs[LocalTerm]] = ((ind{($cjkiv :  FreeGroup) ↦ (($cjkiw :  FreeGroup) ↦ ($cjkiv = $cjkiw))(((mul) ((<function1>) (n))) ((<function1>) (n)))(((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ((<function1>) (n)))))}{($celhe :  FreeGroup) ↦ (($celhf :  FreeGroup) ↦ ((_ : ($celhe = $celhf) :  $celhe = $celhf) ↦ (($celhf = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) → ($celhe = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))))))}(($celhe :  FreeGroup) ↦ (($celvu : ($celhe = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) :  $celhe = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) ↦ ($celvu : ($celhe = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))))))) ((ind{($ceryh :  FreeGroup) ↦ (($ceryi :  FreeGroup) ↦ ($ceryh = $ceryi))((<function1>) (n))(((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s))))}{($calif :  FreeGroup) ↦ (($calig :  FreeGroup) ↦ ((_ : ($calif = $calig) :  $calif = $calig) ↦ (((mul) ($calif)) ((<function1>) (n)) = ((mul) ($calig)) ((<function1>) (n)))))}(($calif :  FreeGroup) ↦ (Refl(FreeGroup,((mul) ($calif)) ((<function1>) (n)))))) (((ind{($byozu :  FreeGroup) ↦ (($byozv :  FreeGroup) ↦ ($byozu = $byozv))((<function1>) (n))((<function1>) (n))}{($bxjlh :  FreeGroup) ↦ (($bxjli :  FreeGroup) ↦ ((_ : ($bxjlh = $bxjli) :  $bxjlh = $bxjli) ↦ (($bxjli = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))) → ($bxjlh = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))))))}(($bxjlh :  FreeGroup) ↦ (($bxjzx : ($bxjlh = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))) :  $bxjlh = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))) ↦ ($bxjzx : ($bxjlh = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))))))) ((ind{($bxmsu :  FreeGroup) ↦ (($bxmsv :  FreeGroup) ↦ ($bxmsu = $bxmsv))(x)(((mul) (s)) (((mul) (w)) (((mul) (y)) ((inv) (s)))))}{($bvsdf :  FreeGroup) ↦ (($bvsdg :  FreeGroup) ↦ ((_ : ($bvsdf = $bvsdg) :  $bvsdf = $bvsdg) ↦ ((<function1>) (n) = (<function1>) (n))))}(($bvsdf :  FreeGroup) ↦ (Refl(FreeGroup,(<function1>) (n))))) (x ~ wy : (x = ((mul) (s)) (((mul) (w)) (((mul) (y)) ((inv) (s)))))) : ((<function1>) (n) = (<function1>) (n)))) ((<function1>) (n) : ((<function1>) (n) = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s))))) : ((<function1>) (n) = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s))))) : (((mul) ((<function1>) (n))) ((<function1>) (n)) = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ((<function1>) (n))))))) ((ind{($cjkiy :  FreeGroup) ↦ (($cjkiz :  FreeGroup) ↦ ($cjkiy = $cjkiz))((<function1>) (n))(((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))}{($ccaei :  FreeGroup) ↦ (($ccaej :  FreeGroup) ↦ ((_ : ($ccaei = $ccaej) :  $ccaei = $ccaej) ↦ (((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ($ccaei))) = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ($ccaej))))))}(($ccaei :  FreeGroup) ↦ (Refl(FreeGroup,((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ($ccaei))))))) (((ind{($calcs :  FreeGroup) ↦ (($calct :  FreeGroup) ↦ ($calcs = $calct))((<function1>) (n))((<function1>) (n))}{($bzejl :  FreeGroup) ↦ (($bzejm :  FreeGroup) ↦ ((_ : ($bzejl = $bzejm) :  $bzejl = $bzejm) ↦ (($bzejm = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))) → ($bzejl = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))))))}(($bzejl :  FreeGroup) ↦ (($bzeyb : ($bzejl = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))) :  $bzejl = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))) ↦ ($bzeyb : ($bzejl = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))))))) ((ind{($bzhqy :  FreeGroup) ↦ (($bzhqz :  FreeGroup) ↦ ($bzhqy = $bzhqz))(x)(((mul) (t)) (((mul) (z)) (((mul) ((inv) (w))) ((inv) (t)))))}{($byozx :  FreeGroup) ↦ (($byozy :  FreeGroup) ↦ ((_ : ($byozx = $byozy) :  $byozx = $byozy) ↦ ((<function1>) (n) = (<function1>) (n))))}(($byozx :  FreeGroup) ↦ (Refl(FreeGroup,(<function1>) (n))))) (x ~ zw^{-1} : (x = ((mul) (t)) (((mul) (z)) (((mul) ((inv) (w))) ((inv) (t)))))) : ((<function1>) (n) = (<function1>) (n)))) ((<function1>) (n) : ((<function1>) (n) = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))) : ((<function1>) (n) = ((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))) : (((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ((<function1>) (n)))) = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))))))) : (((mul) ((<function1>) (n))) ((<function1>) (n)) = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))))))

scala> assert(xnxnExpr.typ == ((pown(x) |+| pown(x)) =:= (t1 |+| t2 )   ))
```



Using $x^nx^n = x^{2n}$, we get the formula $x^{2n} = s(wy)^ns^{-1}t(zw^{-1})^nt^{-1}$.

```scala
scala> val x2nExpr =PowerDistributive.pf(x)(n)(n).sym && xnxnExpr 
x2nExpr: Equality[LocalTerm with Subs[LocalTerm] with Subs[LocalTerm with Subs[LocalTerm]]] = ((ind{($clzqe :  FreeGroup) ↦ (($clzqf :  FreeGroup) ↦ ($clzqe = $clzqf))((<function1>) (((prod) (2)) (n)))(((mul) ((<function1>) (n))) ((<function1>) (n)))}{($cjznj :  FreeGroup) ↦ (($cjznk :  FreeGroup) ↦ ((_ : ($cjznj = $cjznk) :  $cjznj = $cjznk) ↦ (($cjznk = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) → ($cjznj = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))))))}(($cjznj :  FreeGroup) ↦ (($ckabz : ($cjznj = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) :  $cjznj = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) ↦ ($ckabz : ($cjznj = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))))))) ((ind{($ckdjh :  FreeGroup) ↦ (($ckdji :  FreeGroup) ↦ ($ckdjh = $ckdji))(((mul) ((<function1>) (n))) ((<function1>) (n)))((<function1>) (((prod) (2)) (n)))}{($cjnqx :  FreeGroup) ↦ (($cjnqy :  FreeGroup) ↦ ((_ : ($cjnqx = $cjnqy) :  $cjnqx = $cjnqy) ↦ ($cjnqy = $cjnqx)))}(($cjnqx :  FreeGroup) ↦ (Refl(FreeGroup,$cjnqx)))) ((<function1>) (n) : (((mul) ((<function1>) (n))) ((<function1>) (n)) = (<function1>) (((prod) (2)) (n)))) : ((<function1>) (((prod) (2)) (n)) = ((mul) ((<function1>) (n))) ((<function1>) (n))))) (((ind{($cjkiv :  FreeGroup) ↦ (($cjkiw :  FreeGroup) ↦ ($cjkiv = $cjkiw))(((mul) ((<function1>) (n))) ((<function1>) (n)))(((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ((<function1>) (n)))))}{($celhe :  FreeGroup) ↦ (($celhf :  FreeGroup) ↦ ((_ : ($celhe = $celhf) :  $celhe = $celhf) ↦ (($celhf = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) → ($celhe = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))))))}(($celhe :  FreeGroup) ↦ (($celvu : ($celhe = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) :  $celhe = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) ↦ ($celvu : ($celhe = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))))))) ((ind{($ceryh :  FreeGroup) ↦ (($ceryi :  FreeGroup) ↦ ($ceryh = $ceryi))((<function1>) (n))(((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s))))}{($calif :  FreeGroup) ↦ (($calig :  FreeGroup) ↦ ((_ : ($calif = $calig) :  $calif = $calig) ↦ (((mul) ($calif)) ((<function1>) (n)) = ((mul) ($calig)) ((<function1>) (n)))))}(($calif :  FreeGroup) ↦ (Refl(FreeGroup,((mul) ($calif)) ((<function1>) (n)))))) (((ind{($byozu :  FreeGroup) ↦ (($byozv :  FreeGroup) ↦ ($byozu = $byozv))((<function1>) (n))((<function1>) (n))}{($bxjlh :  FreeGroup) ↦ (($bxjli :  FreeGroup) ↦ ((_ : ($bxjlh = $bxjli) :  $bxjlh = $bxjli) ↦ (($bxjli = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))) → ($bxjlh = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))))))}(($bxjlh :  FreeGroup) ↦ (($bxjzx : ($bxjlh = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))) :  $bxjlh = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))) ↦ ($bxjzx : ($bxjlh = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))))))) ((ind{($bxmsu :  FreeGroup) ↦ (($bxmsv :  FreeGroup) ↦ ($bxmsu = $bxmsv))(x)(((mul) (s)) (((mul) (w)) (((mul) (y)) ((inv) (s)))))}{($bvsdf :  FreeGroup) ↦ (($bvsdg :  FreeGroup) ↦ ((_ : ($bvsdf = $bvsdg) :  $bvsdf = $bvsdg) ↦ ((<function1>) (n) = (<function1>) (n))))}(($bvsdf :  FreeGroup) ↦ (Refl(FreeGroup,(<function1>) (n))))) (x ~ wy : (x = ((mul) (s)) (((mul) (w)) (((mul) (y)) ((inv) (s)))))) : ((<function1>) (n) = (<function1>) (n)))) ((<function1>) (n) : ((<function1>) (n) = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s))))) : ((<function1>) (n) = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s))))) : (((mul) ((<function1>) (n))) ((<function1>) (n)) = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ((<function1>) (n))))))) ((ind{($cjkiy :  FreeGroup) ↦ (($cjkiz :  FreeGroup) ↦ ($cjkiy = $cjkiz))((<function1>) (n))(((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))}{($ccaei :  FreeGroup) ↦ (($ccaej :  FreeGroup) ↦ ((_ : ($ccaei = $ccaej) :  $ccaei = $ccaej) ↦ (((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ($ccaei))) = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ($ccaej))))))}(($ccaei :  FreeGroup) ↦ (Refl(FreeGroup,((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ($ccaei))))))) (((ind{($calcs :  FreeGroup) ↦ (($calct :  FreeGroup) ↦ ($calcs = $calct))((<function1>) (n))((<function1>) (n))}{($...

scala> assert(x2nExpr.typ == (FreeGroup.power(x)(NatRing.prod(n)(nat(2))) =:= (s |+| c(n) |+| t.inverse)))
```



We now bound the length of the right hand side $s(wy)^ns^{-1}t(zw^{-1})^nt^{-1}$.


```scala
scala> val thmBound = f(n) + l(s) + l(t.inverse) 
thmBound: LocalTerm = (((<function1>) (n) * (l) (y)) + (l) ((inv) (t)) + ((<function1>) (n) * (l) (z)) + (l) (s) + (l) (t) + (l) ((inv) (s)))

scala> val exprBound = lemmaProof(n) + triang(s)(c(n)) + triang(s |+| c(n))(t.inverse) 
exprBound: PosWitSum = PosWitSum(PosWitSum((<function1>) (n) : (Pos((((<function1>) (n) * (l) (y)) + ((<function1>) (n) * (l) (z)) + ((prod) (-1)) ((l) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n)))))) + (l) (t) + (l) ((inv) (s))))),((triangle-inequality) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))) : (Pos((((prod) (-1)) ((l) (((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))))) + (l) (s) + (l) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))))))),((triangle-inequality) (((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n))))))) ((inv) (t)) : (Pos((((prod) (-1)) ((l) (((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))))))) + (l) (((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) ((<function1>) (n)))))) + (l) ((inv) (t))))))

scala> assert(exprBound.typ == leq(l(s |+| c(n) |+| t.inverse ))(thmBound))
```



We easily deduce the bound on $l(x^{2n})$ to complete the proof.


```scala
scala> val thmProof = x2nExpr.sym.lift (g :-> leq(l(g))(thmBound))(exprBound) 
thmProof: PosWit = ((ind{($ekysn :  FreeGroup) ↦ (($ekyso :  FreeGroup) ↦ ($ekysn = $ekyso))(((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))))))((<function1>) (((prod) (2)) (n)))}{($cpuhc :  FreeGroup) ↦ (($cpuhd :  FreeGroup) ↦ ((_ : ($cpuhc = $cpuhd) :  $cpuhc = $cpuhd) ↦ ((Pos((((<function1>) (n) * (l) (y)) + (l) ((inv) (t)) + ((<function1>) (n) * (l) (z)) + (l) (s) + (l) (t) + (l) ((inv) (s)) + ((prod) (-1)) ((l) ($cpuhc))))) → (Pos((((<function1>) (n) * (l) (y)) + (l) ((inv) (t)) + ((<function1>) (n) * (l) (z)) + (l) (s) + (l) (t) + (l) ((inv) (s)) + ((prod) (-1)) ((l) ($cpuhd))))))))}(($cpuhc :  FreeGroup) ↦ (($dfcjy : (Pos((((<function1>) (n) * (l) (y)) + (l) ((inv) (t)) + ((<function1>) (n) * (l) (z)) + (l) (s) + ((prod) (-1)) ((l) ($cpuhc)) + (l) (t) + (l) ((inv) (s))))) :  Pos((((<function1>) (n) * (l) (y)) + (l) ((inv) (t)) + ((<function1>) (n) * (l) (z)) + (l) (s) + ((prod) (-1)) ((l) ($cpuhc)) + (l) (t) + (l) ((inv) (s))))) ↦ ($dfcjy : (Pos((((<function1>) (n) * (l) (y)) + (l) ((inv) (t)) + ((<function1>) (n) * (l) (z)) + (l) (s) + ((prod) (-1)) ((l) ($cpuhc)) + (l) (t) + (l) ((inv) (s))))))))) ((ind{($ebkuv :  FreeGroup) ↦ (($ebkuw :  FreeGroup) ↦ ($ebkuv = $ebkuw))((<function1>) (((prod) (2)) (n)))(((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t)))))))}{($cmkco :  FreeGroup) ↦ (($cmkcp :  FreeGroup) ↦ ((_ : ($cmkco = $cmkcp) :  $cmkco = $cmkcp) ↦ ($cmkcp = $cmkco)))}(($cmkco :  FreeGroup) ↦ (Refl(FreeGroup,$cmkco)))) (((ind{($clzqe :  FreeGroup) ↦ (($clzqf :  FreeGroup) ↦ ($clzqe = $clzqf))((<function1>) (((prod) (2)) (n)))(((mul) ((<function1>) (n))) ((<function1>) (n)))}{($cjznj :  FreeGroup) ↦ (($cjznk :  FreeGroup) ↦ ((_ : ($cjznj = $cjznk) :  $cjznj = $cjznk) ↦ (($cjznk = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) → ($cjznj = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))))))}(($cjznj :  FreeGroup) ↦ (($ckabz : ($cjznj = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) :  $cjznj = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) ↦ ($ckabz : ($cjznj = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))))))) ((ind{($ckdjh :  FreeGroup) ↦ (($ckdji :  FreeGroup) ↦ ($ckdjh = $ckdji))(((mul) ((<function1>) (n))) ((<function1>) (n)))((<function1>) (((prod) (2)) (n)))}{($cjnqx :  FreeGroup) ↦ (($cjnqy :  FreeGroup) ↦ ((_ : ($cjnqx = $cjnqy) :  $cjnqx = $cjnqy) ↦ ($cjnqy = $cjnqx)))}(($cjnqx :  FreeGroup) ↦ (Refl(FreeGroup,$cjnqx)))) ((<function1>) (n) : (((mul) ((<function1>) (n))) ((<function1>) (n)) = (<function1>) (((prod) (2)) (n)))) : ((<function1>) (((prod) (2)) (n)) = ((mul) ((<function1>) (n))) ((<function1>) (n))))) (((ind{($cjkiv :  FreeGroup) ↦ (($cjkiw :  FreeGroup) ↦ ($cjkiv = $cjkiw))(((mul) ((<function1>) (n))) ((<function1>) (n)))(((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) ((<function1>) (n)))))}{($celhe :  FreeGroup) ↦ (($celhf :  FreeGroup) ↦ ((_ : ($celhe = $celhf) :  $celhe = $celhf) ↦ (($celhf = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) → ($celhe = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))))))}(($celhe :  FreeGroup) ↦ (($celvu : ($celhe = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) :  $celhe = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))) ↦ ($celvu : ($celhe = ((mul) (s)) (((mul) ((<function1>) (n))) (((mul) ((inv) (s))) (((mul) (t)) (((mul) ((<function1>) (n))) ((inv) (t))))))))))) ((ind{($ceryh :  FreeGroup) ↦ (($ceryi :  FreeGroup) ↦ ($ceryh = $ceryi))((<function1>) (n))(((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s))))}{($calif :  FreeGroup) ↦ (($calig :  FreeGroup) ↦ ((_ : ($calif = $calig) :  $calif = $calig) ↦ (((mul) ($calif)) ((<function1>) (n)) = ((mul) ($calig)) ((<function1>) (n)))))}(($calif :  FreeGroup) ↦ (Refl(FreeGroup,((mul) ($calif)) ((<function1>) (n)))))) (((ind{($byozu :  FreeGroup) ↦ (($byozv :  FreeGroup) ↦ ($byozu = $byozv))((<function1>) (n))((<function1>) (n))}{($bxjlh :  FreeGroup) ↦ (($bxjli :  FreeGroup) ↦ ((_ : ($bxjlh = $bxjli) :  $bxjlh = $bxjli) ↦ (($bxjli = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))) → ($bxjlh = ((mul) (s)) (((mul) ((<function1>) (n))) ((inv) (s)))))))}(($bxjlh :  FreeGroup) ↦ (($bxjzx : ($bxjlh = ((mul) (s)) (((mul) (...

scala> val x2n = FreeGroup.power(x)(NatRing.prod(n)(nat(2))) 
x2n: RepTerm[Word] = (<function1>) (((prod) (2)) (n))

scala> assert(thmProof.typ == leq(l(x2n))(thmBound )) 


scala>
```



