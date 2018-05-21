---
title: Left and Right Identities
layout: page
---

We demonstrate how the system can discover a simple proof by purely forward reasoning. The steps here are tailor-made for this proof,
and in practice there will be many blind allies.

We start with some axioms for a Monoid, except that the left and right identities are not assumed equal. We show that they are equal.

First, some imports


```scala
scala>    

scala> import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD, _} 
import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD, _}

scala> import library._, MonoidSimple._ 
import library._, MonoidSimple._

scala> import learning._ 
import learning._
```



A Monoid is axiomatized as a type `M` with equality `eqM` and an operation `op := _*_`. We consider the relevant objects and axioms.


```scala
scala> dist1.entropyVec.mkString("\n", "\n", "\n") 
res9: String = """
Weighted(_*_,1.5849625007211563)
Weighted(eqM,1.5849625007211563)
Weighted(axiom[eqM(_*_(a)(e_r))(a)],4.392317422778761)
Weighted(axiom[eqM(a)(a)],4.392317422778761)
Weighted(axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))],4.392317422778761)
Weighted(e_l,4.392317422778761)
Weighted(axiom[eqM(_*_(e_l)(a))(a)],4.392317422778761)
Weighted(e_r,4.392317422778761)
Weighted(axiom[(eqM(a)(b) [91m→[39m (eqM(b)(c) [91m→[39m eqM(a)(c)))],4.392317422778761)
"""
```



We use a `TermEvolver`, We first generate the types, which includes the theorems.
The time has not been optimized here to avoid accidental biases.

```scala
scala> val tv = new TermEvolver(lambdaWeight = 0.0, piWeight = 0.0) 
tv: TermEvolver = provingground.learning.TermEvolver@2fc67206

scala> val fdT = Truncate(tv.baseEvolveTyps(dist1), math.pow(0.1, 8)) 
fdT: FiniteDistribution[HoTT.Typ[HoTT.Term]] = FiniteDistribution(
  Vector(
    Weighted(
      ((eqM) (((_*_) (e_l)) (e_l))) (((_*_) (((_*_) (e_r)) (e_r))) (e_l)),
      1.9112879403319106E-5
    ),
    Weighted(((eqM) (((_*_) (e_l)) (e_l))) (((_*_) (e_r)) (e_l)), 0.0010982705587242983),
    Weighted(
      ((eqM) (((_*_) (e_l)) (e_l))) (((_*_) (((_*_) (e_l)) (e_l))) (e_l)),
      1.9112879403319106E-5
    ),
    Weighted(((eqM) (((_*_) (e_l)) (e_r))) (((_*_) (e_r)) (e_r)), 0.0010982705587242983),
    Weighted(
      ((eqM) (e_r)) (((_*_) (((_*_) (e_r)) (e_r))) (((_*_) (e_l)) (e_l))),
      9.113892294053897E-6
    ),
    Weighted(
      ((eqM) (((_*_) (e_r)) (e_l))) (((_*_) (((_*_) (e_r)) (e_l))) (e_r)),
      1.9112879403319106E-5
    ),
    Weighted(((eqM) (((_*_) (e_l)) (e_r))) (((_*_) (e_r)) (e_l)), 0.001098270558724298),
    Weighted(
      ((eqM) (e_l)) (((_*_) (e_r)) (((_*_) (e_r)) (((_*_) (e_r)) (e_r)))),
      2.208091209772835E-6
    ),
    Weighted(
      ((eqM) (((_*_) (e_r)) (e_r))) (((_*_) (e_l)) (((_*_) (e_r)) (e_l))),
      1.3226352305986222E-5
    ),
    Weighted(((eqM) (e_l)) (((_*_) (e_l)) (((_*_) (e_l)) (e_r))), 2.2243076188824701E-4),
    Weighted(
      ((eqM) (e_l)) (((_*_) (e_l)) (((_*_) (((_*_) (e_l)) (e_r))) (e_l))),
      5.863557578778822E-7
    ),
    Weighted(
      ((eqM) (((_*_) (e_l)) (((_*_) (e_l)) (e_l)))) (((_*_) (e_r)) (e_l)),
      1.0504450768866332E-5
    ),
    Weighted(
      ((eqM) (e_l)) (((_*_) (e_l)) (((_*_) (e_l)) (((_*_) (e_r)) (e_r)))),
      2.208091209772835E-6
    ),
    Weighted(((eqM) (e_r)) (e_r), 0.16770029826173277),
    Weighted(((eqM) (((_*_) (e_l)) (e_l))) (((_*_) (e_l)) (e_r)), 0.0010982705587242983),
    Weighted(
      ((eqM) (((_*_) (((_*_) (e_l)) (e_l))) (e_l))) (((_*_) (e_l)) (e_r)),
      3.003288307213682E-6
    ),
    Weighted(
...
```



We shall generate terms. Some experiments show that it is enough to generate with truncation `10^{-5}`.

```scala
scala> val fd = Truncate(tv.baseEvolve(dist1), math.pow(0.1, 5)) 
fd: FiniteDistribution[HoTT.Term] = FiniteDistribution(
  Vector(
    Weighted(axiom[eqM(_*_(a)(e_r))(a)], 0.03857142857142856),
    Weighted(((eqM) (e_r)) (e_r), 0.0038068219681510434),
    Weighted((eqM) (((_*_) (e_r)) (e_r)), 8.855801048572368E-4),
    Weighted(((_*_) (e_r)) (e_l), 0.0038068219681510426),
    Weighted(axiom[eqM(a)(a)], 0.03857142857142856),
    Weighted(((eqM) (e_r)) (e_l), 0.0038068219681510434),
    Weighted(axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))], 0.03857142857142856),
    Weighted((_*_) (((_*_) (e_l)) (e_l)), 8.855801048572368E-4),
    Weighted(_*_, 0.26999999999999996),
    Weighted(((eqM) (e_l)) (e_l), 0.0038068219681510434),
    Weighted((axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))]) (e_r), 0.003940271597629894),
    Weighted(
      (((axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))]) (e_r)) (e_r)) ((axiom[eqM(a)(a)]) (e_r)),
      2.4929185278420903E-4
    ),
    Weighted(
      (((axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))]) (e_l)) (e_l)) ((axiom[eqM(a)(a)]) (e_l)),
      2.4929185278420903E-4
    ),
    Weighted(((axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))]) (e_r)) (e_l), 5.438317097358634E-4),
    Weighted((eqM) (e_r), 0.02642897788208435),
    Weighted((axiom[eqM(_*_(e_l)(a))(a)]) (e_l), 0.003940271597629894),
    Weighted(((axiom[(eqM(a)(b) [91m→[39m (eqM(b)(c) [91m→[39m eqM(a)(c)))]) (e_l)) (e_r), 5.438317097358634E-4),
    Weighted((axiom[(eqM(a)(b) [91m→[39m (eqM(b)(c) [91m→[39m eqM(a)(c)))]) (e_l), 0.003940271597629894),
    Weighted(
      ($cvhau :  M) ↦ (((((axiom[(eqM(a)(b) [91m→[39m (eqM(b)(c) [91m→[39m eqM(a)(c)))]) (e_l)) (e_l)) ($cvhau)) ((axiom[eqM(a)(a)]) (e_l))),
      2.4929185278420903E-4
    ),
    Weighted(
      ($cvgys :  M) ↦ (((((axiom[(eqM(a)(b) [91m→[39m (eqM(b)(c) [91m→[39m eqM(a)(c)))]) (((_*_) (e_l)) (e_r))) (e_r)) ($cvgys)) ((axiom[eqM(_*_(e_l)(a))(a)]) (e_r))),
      2.4929185278420903E-4
    ),
    Weighted((eqM) (((_*_) (e_r)) (e_l)), 8.855801048572368E-4),
    Weighted(e_l, 0.03857142857142856),
    Weighted(
      (((axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))]) (((_*_) (e_l)) (e_l))) (e_l)) ((axiom[eqM(_*_(e_l)(a))(a)]) (e_l)),
      2.4929185278420903E-4
    ),
    Weighted(((axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))]) (e_l)) (e_l), 5.438317097358634E-4),
    Weighted((_*_) (((_*_) (e_r)) (e_r)), 8.855801048572368E-4),
    Weighted(
      (((axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))]) (((_*_) (e_r)) (e_r))) (e_r)) ((axiom[eqM(_*_(a)(e_r))(a)]) (e_r)),
      2.4929185278420903E-4
...

scala> fd.filter(_.typ == eqM(l)(op(l)(r))) 
res13: FiniteDistribution[HoTT.Term] = FiniteDistribution(
  Vector(
    Weighted(
      (((axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))]) (((_*_) (e_l)) (e_r))) (e_l)) ((axiom[eqM(_*_(a)(e_r))(a)]) (e_l)),
      2.4929185278420903E-4
    )
  )
)
```


We see that wee get a proof of a key lemma. Criteria, based on probabilities of statements and proofs,
tell us that this is one of the best results proved, along with one related by symmetry and a pair that are not useful.

A quick way to explore consequences of this discovered lemma is to use the derivative of the evolution.
We see that we get the proof.

```scala
scala> val pf = fd.filter(_.typ == eqM(l)(op(l)(r))).supp.head 
pf: HoTT.Term = (((axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))]) (((_*_) (e_l)) (e_r))) (e_l)) ((axiom[eqM(_*_(a)(e_r))(a)]) (e_l))

scala> val initt = TangVec(dist1, FD.unif(pf)) 
initt: TangVec[FiniteDistribution[HoTT.Term]] = TangVec(
  FiniteDistribution(
    Vector(
      Weighted(e_l, 0.047619047619047616),
      Weighted(e_r, 0.047619047619047616),
      Weighted(_*_, 0.047619047619047616),
      Weighted(eqM, 0.047619047619047616),
      Weighted(axiom[eqM(a)(a)], 0.047619047619047616),
      Weighted(axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))], 0.047619047619047616),
      Weighted(axiom[(eqM(a)(b) [91m→[39m (eqM(b)(c) [91m→[39m eqM(a)(c)))], 0.047619047619047616),
      Weighted(axiom[eqM(_*_(e_l)(a))(a)], 0.047619047619047616),
      Weighted(axiom[eqM(_*_(a)(e_r))(a)], 0.047619047619047616),
      Weighted(eqM, 0.2857142857142857),
      Weighted(_*_, 0.2857142857142857)
    )
  ),
  FiniteDistribution(
    Vector(
      Weighted(
        (((axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))]) (((_*_) (e_l)) (e_r))) (e_l)) ((axiom[eqM(_*_(a)(e_r))(a)]) (e_l)),
        1.0
      )
    )
  )
)

scala> val fdt = Truncate(tv.evolve(initt).vec , math.pow(0.1, 4)) 
fdt: FiniteDistribution[HoTT.Term] = FiniteDistribution(
  Vector(
    Weighted(
      ((((axiom[(eqM(a)(b) [91m→[39m (eqM(b)(c) [91m→[39m eqM(a)(c)))]) (e_l)) (((_*_) (e_l)) (e_r))) (e_l)) ((((axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))]) (((_*_) (e_l)) (e_r))) (e_l)) ((axiom[eqM(_*_(a)(e_r))(a)]) (e_l))),
      0.06628023644707164
    ),
    Weighted(
      (_ :  ((eqM) (e_r)) ($cvptf)) ↦ ((((((axiom[(eqM(a)(b) [91m→[39m (eqM(b)(c) [91m→[39m eqM(a)(c)))]) (e_r)) (e_l)) (((_*_) (e_l)) (e_r))) ($cvptp)) ((((axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))]) (((_*_) (e_l)) (e_r))) (e_l)) ((axiom[eqM(_*_(a)(e_r))(a)]) (e_l)))),
      0.0017240932306655272
    ),
    Weighted(
      ((((axiom[(eqM(a)(b) [91m→[39m (eqM(b)(c) [91m→[39m eqM(a)(c)))]) (e_l)) (((_*_) (e_l)) (e_r))) (e_r)) ((((axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))]) (((_*_) (e_l)) (e_r))) (e_l)) ((axiom[eqM(_*_(a)(e_r))(a)]) (e_l))),
      0.06628023644707164
    ),
    Weighted(
      ($cvplj :  M) ↦ (((((axiom[(eqM(a)(b) [91m→[39m (eqM(b)(c) [91m→[39m eqM(a)(c)))]) (((_*_) (e_l)) (e_r))) (e_l)) ($cvplj)) ((((axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))]) (e_l)) (((_*_) (e_l)) (e_r))) ((((axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))]) (((_*_) (e_l)) (e_r))) (e_l)) ((axiom[eqM(_*_(a)(e_r))(a)]) (e_l))))),
      0.0013066811853465036
    ),
    Weighted(
      (((axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))]) (((_*_) (e_l)) (e_r))) (e_l)) ((((axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))]) (e_l)) (((_*_) (e_l)) (e_r))) ((((axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))]) (((_*_) (e_l)) (e_r))) (e_l)) ((axiom[eqM(_*_(a)(e_r))(a)]) (e_l)))),
      0.0013066811853465036
    ),
    Weighted(
      ($cvpjc :  M) ↦ (((((axiom[(eqM(a)(b) [91m→[39m (eqM(b)(c) [91m→[39m eqM(a)(c)))]) (e_l)) (((_*_) (e_l)) (e_r))) ($cvpjc)) ((((axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))]) (((_*_) (e_l)) (e_r))) (e_l)) ((axiom[eqM(_*_(a)(e_r))(a)]) (e_l)))),
      0.025244354566902594
    ),
    Weighted(
      (((axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))]) (((_*_) (e_l)) (e_r))) (e_l)) ((axiom[eqM(_*_(a)(e_r))(a)]) (e_l)),
      0.81
    ),
    Weighted(
      (((((axiom[(eqM(a)(b) [91m→[39m (eqM(b)(c) [91m→[39m eqM(a)(c)))]) (e_l)) (((_*_) (e_l)) (e_r))) (e_r)) ((((axiom[(eqM(a)(b) [91m→[39m eqM(b)(a))]) (((_*_) (e_l)) (e_r))) (e_l)) ((axiom[eqM(_*_(a)(e_r))(a)]) (e_l)))) ((axiom[eqM(_*_(e_l)(a))(a)]) (e_r)),
      0.0013066811853465043
    ),
    Weighted(
      (((((axiom[(eqM(a)(b) [91m→[39m (eqM(b)(c) [91m→[39m eqM(a)(c)))]) (e_l)) (((_*_) (e_l)) (e_r))) (e_l)) ((((ax...

scala> val tqs = fdt.map(_.typ).filter(fdT(_) > 0).flatten 
tqs: FiniteDistribution[HoTT.Typ[U] forSome { type U >: x$1 <: HoTT.Term with HoTT.Subs[U]; val x$1: HoTT.Term }] = FiniteDistribution(
  Vector(
    Weighted(((eqM) (((_*_) (e_l)) (e_r))) (e_l), 0.025244354566902594),
    Weighted(((eqM) (e_l)) (e_l), 0.0013066811853465043),
    Weighted(((eqM) (e_l)) (e_r), 0.0013066811853465043),
    Weighted(((eqM) (e_l)) (((_*_) (e_l)) (e_r)), 0.8113066811853465)
  )
)

scala> tqs(eqM(l)(r)) 
res18: Double = 0.0013066811853465043
```



The steps of the proof in this case took less than `0.1` seconds. Of course in practice we assume other axioms and follow other paths.
But hopefully the time taken is just a few seconds.

