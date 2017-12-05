This is the epplicetion peir where the Laptop got stuck.

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
