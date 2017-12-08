---
title: Lean Import and Propositions
date: 2017-12-06
layout: post  
---

Lean import takes enormously long to a large extent because of the burden of proof awareness.

## The Example code

For instance, in mapping a `group` to a `semigroup`, we halted with the function application of:

```scala
@ f
res93: Term =
  (s : semigroup(A)) ↦
    hes_mul.mk(A)(
      rec(semigroup(A))((A → (A → A)))(
        (m : (A → (A → A)))
          ↦ (_ : ∏(x : A){ ∏(y : A){ ∏(z : A){ eq(A)(m(m(y)(z))(z))(m(y)(m(z)(z))) } } }) ↦
            m
            )(s)
      )
```

on the argument (partly messed up during cleaning)

```scala
@ x
res94: Term =
  semigroup.mk(A)(
    rec(group(A))(
      (A → (A → A))
    )(
      (m : (A → (A → A))) ↦
        (z : ∏(x : A){ ∏(y : A){ ∏(z : A){ eq(A)(m(m(x)(y))(z))(m(x)(m(y)(z))) } } }) ↦
          (e : A) ↦
            lid : ∏(b : A){ eq(A)(m(e)(b))(b) }) ↦
              (rid : ∏(c : A){ eq(A)(m(c)(e))(c) }) ↦
                (inv : (A → A)) ↦
                  (invAxiom : ∏(w : A){ eq(A)(m(inv(w))(w))(e) }) ↦ m
                ) (gp)
              ) // multiplication for the semigroup
                    (
                      induc(group(A))
                      ((gp : group(A)) ↦
                        ∏(z : A){ ∏(x : A){ ∏(y : A){
                          eq(A)(rec(group(A))((A → (A → A)))((m : (A → (A → A))) ↦
                            (assoc : ∏(x : A){ ∏(y : A){ ∏(z : A){ eq(A)(m(m(x)(y))(z))(m(x)(m(y)(z))) } } }) ↦
                              (e : A) ↦
                                lid : ∏(b : A){ eq(A)(m(e)(b))(b) }) ↦
                                  (rid : ∏(c : A){ eq(A)(m(c)(e))(c) }) ↦
                                    (inv : (A → A)) ↦
                                      (invAxiom : ∏(w : A){ eq(A)(m(inv(w))(w))(e) }) ↦ m)(gp)(
                                        rec(group(A))((A → (A → A)))((m : (A → (A → A))) ↦
                                          (assoc : ∏(x : A){ ∏(y : A){ ∏(z : A){ eq(A)(m(m(x)(y))(z))(m(x)(m(y)(z))) } } }) ↦
                                            (e : A) ↦ lid : ∏(b : A){ eq(A)(m(e)(b))(b) }) ↦
                                              (rid : ∏(c : A){ eq(A)(m(c)(e))(c) }) ↦ (inv : (A → A)) ↦
                                                (invAxiom : ∏(w : A){ eq(A)(m(inv(w))(w))(e) }) ↦ m)(gp)(z)(x))
                                                (y)
                      )
                        (
                          rec(group(A))((A → (A → A)))((m : (A → (A → A))) ↦ (z : ∏(x : A){ ∏(y : A){ ∏(z : A){ eq(A)(m(m(x)(y))(z))(m(x)(m(y)(z))) } } }) ↦ (e : A) ↦ lid : ∏(b : A){ eq(A)(m(e)(b))(b) }) ↦ (rid : ∏(c : A){ eq(A)(m(c)(e))(c) }) ↦ (inv : (A → A)) ↦ (invAxiom : ∏(w : A){ eq(A)(m(inv(w))(w))(e) }) ↦ m
                          )
                            (gp)(z)
                              (
                                rec(group(A))((A → (A → A)))((m : (A → (A → A))) ↦ (z : ∏(x : A){ ∏(y : A){ ∏(z : A){ eq(A)(m(m(x)(y))(z))(m(x)(m(y)(z))) } } }) ↦ (e : A) ↦ lid : ∏(b : A){ eq(A)(m(e)(b))(b) }) ↦ (rid : ∏(c : A){ eq(A)(m(c)(e))(c) }) ↦ (inv : (A → A)) ↦ (invAxiom : ∏(w : A){ eq(A)(m(inv(w))(w))(e) }) ↦ m)(gp)(x)(y))) } } })(
                                  (m : (A → (A → A))) ↦
                                    (assoc :
                                      ∏(x : A){ ∏(y : A){ ∏(z : A){ eq(A)(m(m(x)(y))(z))(m(x)(m(y)(z))) } } }) ↦
                                        (e : A) ↦ (lid : ∏(b : A){ eq(A)(m(e)(b))(b) }) ↦
                                          (rid : ∏(c : A){ eq(A)(m(c)(e))(c) }) ↦
                                            (inv : (A → A)) ↦ (invAxiom : ∏(w : A){ eq(A)(m(inv(w))(w))(e) }) ↦ assoc)(gp)
                                            )
```

Note that the type of the semigroup `mk` is:

```scala
@ parser.defnMap(Name("semigroup", "mk"))(A).typ
res26: Typ[U] = ∏($f : (A → (A → A))){ (∏($g : A){ ∏($h : A){ ∏($i : A){ eq(A)($f($f($g)($h))($i))($f($g)($f($h)($i))) } } } → semigroup(A)) }
```

## Why so complicated

* In Lean, all the structures are given as inductive types with a single introduction rule `mk`.
* To form the semigroup, we give to make a multiplication `m` and a proof of associativity of `m`.
* The multiplication is recursively defined, forgetting many parts of the structure.
* On the other hand, the associativity depends on the group structure (via `m`), so needs to be defined inductively.
* Even the family for the induction is complicated, as much as `m`.

## Speeding up by witnesses

* Associativity is propositional, as it is an iterated function ending in a proposition.
* Hence it is enough to have a witness.
* While parsing, can save precise terms but just return witnesses for propositions.

## Update

* Witnesses did not solve the problem.
* As data, here are the raw terms.

```scala
@ f
res77: Term = ($btk : semigroup($amuwvdd)) ↦ has_mul.mk($amuwvdd)(rec(semigroup($amuwvdd))(($amuwvdd → ($amuwvdd → $amuwvdd)))(($btu : ($amuwvdd → ($amuwvdd → $amuwvdd))) ↦ ($crp : ∏($btv : $amuwvdd){ ∏($btw : $amuwvdd){ ∏($btx : $amuwvdd){ eq($amuwvdd)($btu($btu($btv)($btw))($btx))($btu($btv)($btu($btw)($btx))) } } }) ↦ $btu)($btk))

@ x
res88: Term = semigroup.mk($amuwvdd)(rec(group($amuwvdd))(($amuwvdd → ($amuwvdd → $amuwvdd)))(($eybvwn : ($amuwvdd → ($amuwvdd → $amuwvdd))) ↦ ($eybwui : ∏($eybvwo : $amuwvdd){ ∏($eybvwp : $amuwvdd){ ∏($eybvwq : $amuwvdd){ eq($amuwvdd)($eybvwn($eybvwn($eybvwo)($eybvwp))($eybvwq))($eybvwn($eybvwo)($eybvwn($eybvwp)($eybvwq))) } } }) ↦ ($eybwuj : $amuwvdd) ↦ ($eydpnp : ∏($eybwuk : $amuwvdd){ eq($amuwvdd)($eybvwn($eybwuj)($eybwuk))($eybwuk) }) ↦ ($eyfigv : ∏($eydpnq : $amuwvdd){ eq($amuwvdd)($eybvwn($eydpnq)($eybwuj))($eydpnq) }) ↦ ($eyfigx : ($amuwvdd → $amuwvdd)) ↦ ($fbqdvv : ∏($eyfigy : $amuwvdd){ eq($amuwvdd)($eybvwn($eyfigx($eyfigy))($eyfigy))($eybwuj) }) ↦ $eybvwn)($amuwvdf))(_)

@ groupMk(A).typ
res92: Typ[U] = ∏($g : (A → (A → A))){ (∏($h : A){ ∏($i : A){ ∏($j : A){ eq(A)($g($g($h)($i))($j))($g($h)($g($i)($j))) } } } → ∏($anr : A){ (∏($ans : A){ eq(A)($g($anr)($ans))($ans) } → (∏($dtzw : A){ eq(A)($g($dtzw)($anr))($dtzw) } → ∏($fmtd : (A → A)){ (∏($fmte : A){ eq(A)($g($fmtd($fmte))($fmte))($anr) } → group(A)) })) }) }

@ semigroupMk(A).typ
res94: Typ[U] = ∏($aoj : (A → (A → A))){ (∏($aok : A){ ∏($aol : A){ ∏($aom : A){ eq(A)($aoj($aoj($aok)($aol))($aom))($aoj($aok)($aoj($aol)($aom))) } } } → semigroup(A)) }
```

Recall that equality is defined with the based path induction principle, so even the first point is a parameter, not an index.

```scala
@ val eqMod = parser.termIndModMap (Name("eq"))
eqMod: TermIndMod = IndexedIndMod(Str(, "eq"), eq, Vector(eq.refl), 2, true)

@ eqRefl.typ
res97: Typ[U] = ∏($o : 𝒰 ){ ∏($p : $o){ eq($o)($p)($p) } }
```

* Should see if the definitions in this form give trouble, or it is a perculiarity of lean parsing (e.g., running out of memory).
