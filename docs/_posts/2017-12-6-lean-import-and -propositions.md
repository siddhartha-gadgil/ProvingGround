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
