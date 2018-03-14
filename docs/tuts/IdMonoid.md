---
title: Left and Right Identities
layout: page
---

We demonstrate how the system can discover a simple proof by purely forward reasoning. The steps here are tailor-made for this proof,
and in practice there will be many blind allies.

We start with some axioms for a Monoid, except that the left and right identities are not assumed equal. We show that they are equal.

First, some imports

```scala
scala> import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD, _}
import provingground.{FiniteDistribution=>FD, ProbabilityDistribution=>PD, _}

scala> import library._, MonoidSimple._
import library._
import MonoidSimple._

scala> import learning._
import learning._
```

A Monoid is axiomatized as a type `M` with equality `eqM` and an operation `op := _*_`. We consider the relevant objects and axioms.

```scala
scala> dist1.entropyVec.mkString("\n", "\n", "\n")
res0: String =
"
Weighted(_*_,1.5849625007211563)
Weighted(eqM,1.5849625007211563)
Weighted(axiom[eqM(_*_(a)(e_r))(a)],4.392317422778761)
Weighted(axiom[eqM(a)(a)],4.392317422778761)
Weighted(axiom[(eqM(a)(b) → eqM(b)(a))],4.392317422778761)
Weighted(e_l,4.392317422778761)
Weighted(axiom[eqM(_*_(e_l)(a))(a)],4.392317422778761)
Weighted(e_r,4.392317422778761)
Weighted(axiom[(eqM(a)(b) → (eqM(b)(c) → eqM(a)(c)))],4.392317422778761)
"
```

We use a `TermEvolver`, We first generate the types, which includes the theorems.
The time has not been optimized here to avoid accidental biases.
```scala
scala> val tv = new TermEvolver(lambdaWeight = 0.0, piWeight = 0.0)
tv: provingground.learning.TermEvolver = provingground.learning.TermEvolver@78e1cfda

scala> val fdT = Truncate(tv.baseEvolveTyps(dist1), math.pow(0.1, 8))
fdT: provingground.FiniteDistribution[provingground.HoTT.Typ[provingground.HoTT.Term]] = [((eqM) (e_l)) (e_l) : 0.16770029826173277, ((eqM) (e_r)) (e_r) : 0.16770029826173272, ((eqM) (e_r)) (e_l) : 0.16770029826173272, ((eqM) (e_l)) (e_r) : 0.16770029826173272, ((eqM) (e_r)) (((_*_) (e_r)) (e_r)) : 0.01811765815858783, ((eqM) (e_r)) (((_*_) (e_l)) (e_r)) : 0.018117658158587832, ((eqM) (e_l)) (((_*_) (e_r)) (e_r)) : 0.01811765815858783, ((eqM) (e_r)) (((_*_) (e_r)) (e_l)) : 0.018117658158587832, ((eqM) (e_r)) (((_*_) (e_l)) (e_l)) : 0.018117658158587832, ((eqM) (e_l)) (((_*_) (e_l)) (e_l)) : 0.01811765815858783, ((eqM) (e_l)) (((_*_) (e_l)) (e_r)) : 0.018117658158587832, ((eqM) (e_l)) (((_*_) (e_r)) (e_l)) : 0.01811765815858783, ((eqM) (((_*_) (e_l)) (e_r))) (e_...
```

We shall generate terms. Some experiments show that it is enough to generate with truncation `10^{-5}`.
```scala
scala> val fd = Truncate(tv.baseEvolve(dist1), math.pow(0.1, 5))
fd: provingground.FiniteDistribution[provingground.HoTT.Term] = [_*_ : 0.27, eqM : 0.27, axiom[eqM(_*_(a)(e_r))(a)] : 0.03857142857142857, axiom[eqM(a)(a)] : 0.03857142857142857, axiom[(eqM(a)(b) → eqM(b)(a))] : 0.03857142857142857, e_l : 0.03857142857142857, axiom[eqM(_*_(e_l)(a))(a)] : 0.03857142857142857, e_r : 0.03857142857142857, axiom[(eqM(a)(b) → (eqM(b)(c) → eqM(a)(c)))] : 0.03857142857142857, (eqM) (e_r) : 0.026428977882084353, (_*_) (e_l) : 0.026428977882084353, (_*_) (e_r) : 0.026428977882084353, (eqM) (e_l) : 0.026428977882084353, (axiom[(eqM(a)(b) → eqM(b)(a))]) (e_r) : 0.003940271597629893, (axiom[eqM(_*_(e_l)(a))(a)]) (e_l) : 0.003940271597629893, (axiom[(eqM(a)(b) → (eqM(b)(c) → eqM(a)(...

scala> fd.filter(_.typ == eqM(l)(op(l)(r)))
res1: provingground.FiniteDistribution[provingground.HoTT.Term] = [(((axiom[(eqM(a)(b) → eqM(b)(a))]) (((_*_) (e_l)) (e_r))) (e_l)) ((axiom[eqM(_*_(a)(e_r))(a)]) (e_l)) : 2.492918527842087E-4]
```
We see that wee get a proof of a key lemma. Criteria, based on probabilities of statements and proofs,
tell us that this is one of the best results proved, along with one related by symmetry and a pair that are not useful.

A quick way to explore consequences of this discovered lemma is to use the derivative of the evolution.
We see that we get the proof.
```scala
scala> val pf = fd.filter(_.typ == eqM(l)(op(l)(r))).supp.head
pf: provingground.HoTT.Term = (((axiom[(eqM(a)(b) → eqM(b)(a))]) (((_*_) (e_l)) (e_r))) (e_l)) ((axiom[eqM(_*_(a)(e_r))(a)]) (e_l))

scala> val initt = TangVec(dist1, FD.unif(pf))
initt: provingground.learning.TangVec[provingground.FiniteDistribution[provingground.HoTT.Term]] = TangVec([eqM : 0.2857142857142857, _*_ : 0.2857142857142857, e_l : 0.047619047619047616, e_r : 0.047619047619047616, _*_ : 0.047619047619047616, eqM : 0.047619047619047616, axiom[eqM(a)(a)] : 0.047619047619047616, axiom[(eqM(a)(b) → eqM(b)(a))] : 0.047619047619047616, axiom[(eqM(a)(b) → (eqM(b)(c) → eqM(a)(c)))] : 0.047619047619047616, axiom[eqM(_*_(e_l)(a))(a)] : 0.047619047619047616, axiom[eqM(_*_(a)(e_r))(a)] : 0.047619047619047616],[(((axiom[(eqM(a)(b) → eqM(b)(a))]) (((_*_) (e_l)) (e_r))) (e_l)) ((axiom[eqM(_*_(a)(e_r))(a)]) (e_l)) : 1.0])

scala> val fdt = Truncate(tv.evolve(initt).vec , math.pow(0.1, 4))
fdt: provingground.FiniteDistribution[provingground.HoTT.Term] = [(((axiom[(eqM(a)(b) → eqM(b)(a))]) (((_*_) (e_l)) (e_r))) (e_l)) ((axiom[eqM(_*_(a)(e_r))(a)]) (e_l)) : 0.81, ((((axiom[(eqM(a)(b) → (eqM(b)(c) → eqM(a)(c)))]) (e_l)) (((_*_) (e_l)) (e_r))) (e_l)) ((((axiom[(eqM(a)(b) → eqM(b)(a))]) (((_*_) (e_l)) (e_r))) (e_l)) ((axiom[eqM(_*_(a)(e_r))(a)]) (e_l))) : 0.06628023644707162, ((((axiom[(eqM(a)(b) → (eqM(b)(c) → eqM(a)(c)))]) (e_l)) (((_*_) (e_l)) (e_r))) (e_r)) ((((axiom[(eqM(a)(b) → eqM(b)(a))]) (((_*_) (e_l)) (e_r))) (e_l)) ((axiom[eqM(_*_(a)(e_r))(a)]) (e_l))) : 0.06628023644707162, ($kusvq :  M) ↦ (((((axiom[(eqM(a)(b) → (eqM(b)(c) → eqM(a)(c)))]) (e_l)) (((...

scala> val tqs = fdt.map(_.typ).filter(fdT(_) > 0).flatten
tqs: provingground.FiniteDistribution[provingground.HoTT.Typ[U] forSome { type U >: x$1.type <: provingground.HoTT.Term with provingground.HoTT.Subs[U]; val x$1: provingground.HoTT.Term }] = [((eqM) (e_l)) (((_*_) (e_l)) (e_r)) : 0.8113066811853465, ((eqM) (((_*_) (e_l)) (e_r))) (e_l) : 0.025244354566902594, ((eqM) (e_l)) (e_l) : 0.0013066811853465034, ((eqM) (e_l)) (e_r) : 0.0013066811853465034]

scala> tqs(eqM(l)(r))
res2: Double = 0.0013066811853465034
```

The steps of the proof in this case took less than `0.1` seconds. Of course in practice we assume other axioms and follow other paths.
But hopefully the time taken is just a few seconds.
