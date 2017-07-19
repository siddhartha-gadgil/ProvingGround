---
title: Left and Right Identities
layout: page
---

We demonstrate how the system can discover a simple proof by purely forward reasoning. The steps here are tailor-made for this proof,
and in practice there will be many blind allies.

We start with some axioms for a Monoid, except that the left and right identities are not assumed equal. We show that they are equal.

First, some imports

```tut
import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD, _}
import library._, MonoidSimple._
import learning._
```

A Monoid is axiomatized as a type `M` with equality `eqM` and an operation `op := _*_`. We consider the relevant objects and axioms.

```tut
dist1.entropyVec.mkString("\n", "\n", "\n")
```

We use a `TermEvolver`, We first generate the types, which includes the theorems.
The time has not been optimized here to avoid accidental biases.
```tut
val tv = new TermEvolver(lambdaWeight = 0.0, piWeight = 0.0)
val fdT = Truncate(tv.baseEvolveTyps(dist1), math.pow(0.1, 8))
```

We shall generate terms. Some experiments show that it is enough to generate with truncation `10^{-5}`.
```tut
val fd = Truncate(tv.baseEvolve(dist1), math.pow(0.1, 5))
fd.map(_.typ).filter(_.typ == eqM(l)(op(l)(r)))
```
We see that wee get a proof of a key lemma. Criteria, based on probabilities of statements and proofs,
tell us that this is one of the best results proved, along with one related by symmetry and a pair that are not useful.

A quick way to explore consequences of this discovered lemma is to use the derivative of the evolution.
We see that we get the proof.
```tut
val pf = fd.filter(_.typ == eqM(l)(op(l)(r))).supp.head
val initt = TangVec(dist1, FD.unif(pf))
val fdt = Truncate(tv.evolve(initt).vec , math.pow(0.1, 4))
val tqs = fdt.map(_.typ).filter(fdT(_) > 0).flatten
tqs(eqM(l)(r))
```

The steps of the proof in this case took less than `0.1` seconds. Of course in practice we assume other axioms and follow other paths.
But hopefully the time taken is just a few seconds.
