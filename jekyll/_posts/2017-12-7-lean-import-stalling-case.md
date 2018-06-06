---
title: Stalling in the lean import - the problem case.
date: 2017-12-07
layout: post
---

## The setup within lean

By working through the lean import in steps, I pinned down the function application that causes stuff to stall. Extracting inductive definitions from lean is quite clean. Here is the code for extracting the argument `x`. Before posting, I should also extract the function `f`. Below they are ddenoted `ff` and `xx`

```scala
import trepplein._
import interface._, LeanInterface._
val mods = getMods("data/group.export")
val parser = new LeanParser(mods)
import monix.execution.Scheduler.Implicits.global
val fG = parser.parse(Const("group", Vector()), Vector()).runAsync

val fgmul = parser.parse(Const(Name("group", "mul"), Vector()), Vector()).runAsync

// val fgone = parser.parse(Const(Name("group", "one"), Vector()), Vector()).runAsync // not necessary

// val fgmass = parser.parse(Const(Name("group", "mul_assoc"), Vector()), Vector()).runAsync // may not be needed

// val fgmone = parser.parse(Const(Name("group", "mul_one"), Vector()), Vector()).runAsync // this is the one that causes a crash

val A = "A" :: Type

import Fold._

val semigroupIndMod = parser.termIndModMap(Name("semigroup"))
val semigroup = parser.defnMap(Name("semigroup"))
val semigroupMk = parser.defnMap(Name("semigroup", "mk"))

val groupMul = parser.defnMap(Name("group", "mul"))
val group = parser.defnMap(Name("group"))

val gp = "group(A)" :: toTyp(group(A))
val wit = "_" :: domain(semigroupMk(A)(groupMul(A)(gp)))
val xx = semigroupMk(A)(groupMul(A)(gp))(wit)
```


Indeed, we can define `ff` with
```scala
@ val ff = parser.defnMap(Name("semigroup", "to_has_mul"))(A)
```

Some extra code, for building from `rec` more directly:

```scala
val groupIndMod = parser.termIndModMap(Name("group"))
val groupIndA = groupIndMod.asInstanceOf[SimpleIndMod].getInd(Vector(A))
groupIndA.getInd(Vector(A ->: A ->: A))

val gr = groupIndA.recE(A ->: A ->: A)

val m = "m" :: A ->: A ->: A
```

### More direct definitions

For the record, here are the inductive definitions

```scala
@ semigroupIndA
res168: ConstructorSeqTL[_1.SS, Term, _1.Intros] = ConstructorSeqTL(
  Cons(
    (semigroup.mk : ($aoe : 𝒰 _0 ~> $aoi : (($aoe : 𝒰 _0) → (($aoe : 𝒰 _0) → ($aoe : 𝒰 _0))) ~> ($aoj : ($aoe : 𝒰 _0) ~> $aok : ($aoe : 𝒰 _0) ~> $aol : ($aoe : 𝒰 _0) ~> (((eq : ($j : 𝒰 _0 ~> ($j : 𝒰 _0) → (($j : 𝒰 _0) → (Prop)))) ($aoe : 𝒰 _0) : (($aoe : 𝒰 _0) → (($aoe : 𝒰 _0) → (Prop)))) ((($aoi : (($aoe : 𝒰 _0) → (($aoe : 𝒰 _0) → ($aoe : 𝒰 _0)))) ((($aoi : (($aoe : 𝒰 _0) → (($aoe : 𝒰 _0) → ($aoe : 𝒰 _0)))) ($aoj : ($aoe : 𝒰 _0)) : (($aoe : 𝒰 _0) → ($aoe : 𝒰 _0))) ($aok : ($aoe : 𝒰 _0)) : ($aoe : 𝒰 _0)) : (($aoe : 𝒰 _0) → ($aoe : 𝒰 _0))) ($aol : ($aoe : 𝒰 _0)) : ($aoe : 𝒰 _0)) : (($aoe : 𝒰 _0) → (Prop))) ((($aoi : (($aoe : 𝒰 _0) → (($aoe : 𝒰 _0) → ($aoe : 𝒰 _0)))) ($aoj : ($aoe : 𝒰 _0)) : (($aoe : 𝒰 _0) → ($aoe : 𝒰 _0))) ((($aoi : (($aoe : 𝒰 _0) → (($aoe : 𝒰 _0) → ($aoe : 𝒰 _0)))) ($aok : ($aoe : 𝒰 _0)) : (($aoe : 𝒰 _0) → ($aoe : 𝒰 _0))) ($aol : ($aoe : 𝒰 _0)) : ($aoe : 𝒰 _0)) : ($aoe : 𝒰 _0))) → ((semigroup : ((𝒰 _0) → (𝒰 _0))) ($aoe : 𝒰 _0) : 𝒰 _0))) (A : 𝒰 _0),
    CnstDepFuncConsShape(
      (A → (A → A)),
      provingground.induction.ConstructorShape$$Lambda$7658/1930236645@1c8ae6b1
    ),
    Empty()
  ),
  semigroup(A)
)

@ groupIndA
res169: ConstructorSeqTL[_1.SS, Term, _1.Intros] = ConstructorSeqTL(
  Cons(
    (group.mk : ($b : 𝒰 _0 ~> $f : (($b : 𝒰 _0) → (($b : 𝒰 _0) → ($b : 𝒰 _0))) ~> ($g : ($b : 𝒰 _0) ~> $h : ($b : 𝒰 _0) ~> $i : ($b : 𝒰 _0) ~> (((eq : ($j : 𝒰 _0 ~> ($j : 𝒰 _0) → (($j : 𝒰 _0) → (Prop)))) ($b : 𝒰 _0) : (($b : 𝒰 _0) → (($b : 𝒰 _0) → (Prop)))) ((($f : (($b : 𝒰 _0) → (($b : 𝒰 _0) → ($b : 𝒰 _0)))) ((($f : (($b : 𝒰 _0) → (($b : 𝒰 _0) → ($b : 𝒰 _0)))) ($g : ($b : 𝒰 _0)) : (($b : 𝒰 _0) → ($b : 𝒰 _0))) ($h : ($b : 𝒰 _0)) : ($b : 𝒰 _0)) : (($b : 𝒰 _0) → ($b : 𝒰 _0))) ($i : ($b : 𝒰 _0)) : ($b : 𝒰 _0)) : (($b : 𝒰 _0) → (Prop))) ((($f : (($b : 𝒰 _0) → (($b : 𝒰 _0) → ($b : 𝒰 _0)))) ($g : ($b : 𝒰 _0)) : (($b : 𝒰 _0) → ($b : 𝒰 _0))) ((($f : (($b : 𝒰 _0) → (($b : 𝒰 _0) → ($b : 𝒰 _0)))) ($h : ($b : 𝒰 _0)) : (($b : 𝒰 _0) → ($b : 𝒰 _0))) ($i : ($b : 𝒰 _0)) : ($b : 𝒰 _0)) : ($b : 𝒰 _0))) → ($anq : ($b : 𝒰 _0) ~> ($anr : ($b : 𝒰 _0) ~> (((eq : ($j : 𝒰 _0 ~> ($j : 𝒰 _0) → (($j : 𝒰 _0) → (Prop)))) ($b : 𝒰 _0) : (($b : 𝒰 _0) → (($b : 𝒰 _0) → (Prop)))) ((($f : (($b : 𝒰 _0) → (($b : 𝒰 _0) → ($b : 𝒰 _0)))) ($anq : ($b : 𝒰 _0)) : (($b : 𝒰 _0) → ($b : 𝒰 _0))) ($anr : ($b : 𝒰 _0)) : ($b : 𝒰 _0)) : (($b : 𝒰 _0) → (Prop))) ($anr : ($b : 𝒰 _0))) → (($dtzv : ($b : 𝒰 _0) ~> (((eq : ($j : 𝒰 _0 ~> ($j : 𝒰 _0) → (($j : 𝒰 _0) → (Prop)))) ($b : 𝒰 _0) : (($b : 𝒰 _0) → (($b : 𝒰 _0) → (Prop)))) ((($f : (($b : 𝒰 _0) → (($b : 𝒰 _0) → ($b : 𝒰 _0)))) ($dtzv : ($b : 𝒰 _0)) : (($b : 𝒰 _0) → ($b : 𝒰 _0))) ($anq : ($b : 𝒰 _0)) : ($b : 𝒰 _0)) : (($b : 𝒰 _0) → (Prop))) ($dtzv : ($b : 𝒰 _0))) → ($fmtc : (($b : 𝒰 _0) → ($b : 𝒰 _0)) ~> ($fmtd : ($b : 𝒰 _0) ~> (((eq : ($j : 𝒰 _0 ~> ($j : 𝒰 _0) → (($j : 𝒰 _0) → (Prop)))) ($b : 𝒰 _0) : (($b : 𝒰 _0) → (($b : 𝒰 _0) → (Prop)))) ((($f : (($b : 𝒰 _0) → (($b : 𝒰 _0) → ($b : 𝒰 _0)))) (($fmtc : (($b : 𝒰 _0) → ($b : 𝒰 _0))) ($fmtd : ($b : 𝒰 _0)) : ($b : 𝒰 _0)) : (($b : 𝒰 _0) → ($b : 𝒰 _0))) ($fmtd : ($b : 𝒰 _0)) : ($b : 𝒰 _0)) : (($b : 𝒰 _0) → (Prop))) ($anq : ($b : 𝒰 _0))) → ((group : ((𝒰 _0) → (𝒰 _0))) ($b : 𝒰 _0) : 𝒰 _0)))))) (A : 𝒰 _0),
    CnstDepFuncConsShape(
      (A → (A → A)),
      provingground.induction.ConstructorShape$$Lambda$7658/1930236645@5f38386d
    ),
    Empty()
  ),
  group(A)
)
```


A more readable form is to just look at the types.

```scala
@ semigroupMk(A).typ
res173: Typ[U] = ∏($aoi : (A → (A → A))){ (∏($aoj : A){ ∏($aok : A){ ∏($aol : A){ eq(A)($aoi($aoi($aoj)($aok))($aol))($aoi($aoj)($aoi($aok)($aol))) } } } → semigroup(A)) }

@ groupMk(A).typ
res174: Typ[U] = ∏($f : (A → (A → A))){ (∏($g : A){ ∏($h : A){ ∏($i : A){ eq(A)($f($f($g)($h))($i))($f($g)($f($h)($i))) } } } → ∏($anq : A){ (∏($anr : A){ eq(A)($f($anq)($anr))($anr) } → (∏($dtzv : A){ eq(A)($f($dtzv)($anq))($dtzv) } → ∏($fmtc : (A → A)){ (∏($fmtd : A){ eq(A)($f($fmtc($fmtd))($fmtd))($anq) } → group(A)) })) }) }
```

## The culprit: Equality

When the equality from lean is replaced by a formal one, the result parses. We setup

```scala
@ val eqA = "eql(A)" :: A ->: A ->: Prop
eqA: Func[Term, Func[Term, Typ[Term]]] with Subs[Func[Term, Func[Term, Typ[Term]]]] = eql(A)

@ val eql = parser.defnMap(Name("eq"))
eql: Term = eq
```

We can then get

```scala
@ val xxx = xx.replace(eql(A), eqA)
xxx: Term with Subs[Term] = semigroup.mk(A)(rec(group(A))((A → (A → A)))(($exahig : (A → (A → A))) ↦ ($exaigb : ∏($exahih : A){ ∏($exahii : A){ ∏($exahij : A){ eql(A)($exahig($exahig($exahih)($exahii))($exahij))($exahig($exahih)($exahig($exahii)($exahij))) } } }) ↦ ($exaigc : A) ↦ ($excazi : ∏($exaigd : A){ eql(A)($exahig($exaigc)($exaigd))($exaigd) }) ↦ ($exdtso : ∏($excazj : A){ eql(A)($exahig($excazj)($exaigc))($excazj) }) ↦ ($exdtsq : (A → A)) ↦ ($faopho : ∏($exdtsr : A){ eql(A)($exahig($exdtsq($exdtsr))($exdtsr))($exaigc) }) ↦ $exahig)(group(A)))(_)

@ val fff = ff.replace(eql(A), eqA)
fff: Term with Subs[Term] = ($btj : semigroup(A)) ↦ has_mul.mk(A)(rec(semigroup(A))((A → (A → A)))(($btt : (A → (A → A))) ↦ ($cro : ∏($btu : A){ ∏($btv : A){ ∏($btw : A){ eql(A)($btt($btt($btu)($btv))($btw))($btt($btu)($btt($btv)($btw))) } } }) ↦ $btt)($btj))

@ fff(xxx)
res182: Term = has_mul.mk(A)(rec(semigroup(A))((A → (A → A)))(($btt : (A → (A → A))) ↦ ($cro : ∏($btu : A){ ∏($btv : A){ ∏($btw : A){ eql(A)($btt($btt($btu)($btv))($btw))($btt($btu)($btt($btv)($btw))) } } }) ↦ $btt)(semigroup.mk(A)(rec(group(A))((A → (A → A)))(($exahig : (A → (A → A))) ↦ ($exaigb : ∏($exahih : A){ ∏($exahii : A){ ∏($exahij : A){ eql(A)($exahig($exahig($exahih)($exahii))($exahij))($exahig($exahih)($exahig($exahii)($exahij))) } } }) ↦ ($exaigc : A) ↦ ($excazi : ∏($exaigd : A){ eql(A)($exahig($exaigc)($exaigd))($exaigd) }) ↦ ($exdtso : ∏($excazj : A){ eql(A)($exahig($excazj)($exaigc))($excazj) }) ↦ ($exdtsq : (A → A)) ↦ ($faopho : ∏($exdtsr : A){ eql(A)($exahig($exdtsq($exdtsr))($exdtsr))($exaigc) }) ↦ $exahig)(group(A)))(_)))

@ fff(xxx).typ
res189: Typ[U] = has_mul(A)
```
