# Lean non-bug

While using lean parser server for `eq_of_heq` (mapping heterogeneous to ordinary equality), we get an error. This turns out to be another case where we should use equality of terms with the same type if it is a proposition.

As this uses `heq.rec_on`, for reference:

```scala
@ heqRecOn
res11: Term =
  ('t : 𝒰 ) ↦
    ('u : 't) ↦
      ('v : ∏('v : 𝒰 ){ ('v → 𝒰 ) }) ↦
        ('w : 𝒰 ) ↦
          ('x : 'w) ↦
            ('y : heq('t)('u)('w)('x)) ↦
              ('z : 'v('t)('u)) ↦
                induc_{
                   (A : 𝒰 ) ↦
                      heq('t)('u)(A) ;
                        (B : 𝒰 ) ↦
                          (b : B) ↦
                            (p : heq('t)('u)(B)(b)) ↦
                            'v(B)(b)
                   }('z)('y)

@ heqRecOn.typ
res12: Typ[U] = ∏('t : 𝒰 ){ ∏('u : 't){ ∏('v : ∏('v : 𝒰 ){ ('v → 𝒰 ) }){ ∏('w : 𝒰 ){ ∏('x : 'w){ (heq('t)('u)('w)('x) → ('v('t)('u) → 'v('w)('x))) } } } } }
```

The variables in context are:

```scala
@ fail2.vars.map(t => t ->t.typ) 
res9: Vector[(Term, Typ[U]) forSome { type U >: t <: Term with Subs[U]; val t: Term }] = Vector(
  ('ag, heq('aa)('ab)('ae)('af)),
  ('af, 'ae),
  ('ae, 𝒰 ),
  ('ad, heq('aa)('ab)('aa)('ac)),
  ('ac, 'aa),
  ('ab, 'aa),
  ('aa, 𝒰 )
)
```

We get an application failure, with the relevant expressions and functions being:

```scala
val fail2.fe.toString =
  @heq.rec_on.{0 u} #6 #5
    (λ (x : Sort u) (x_0 : x),
      ∀ (h : @eq.{u+1} Sort u #8 x),
        @eq.{u} ((λ (x_1 : Sort u), x_1) x)
        (@eq.rec_on.{u u+1} Sort u #9 (λ (x_1 : Sort u), x_1) x h #8) x_0
    ) #2 #1 #0


val fail2.ae.toString =
  λ (h : @eq.{u+1} Sort u #6 #6),
      @rfl.{u} ((λ (x : Sort u), x) #7)
  (@eq.rec_on.{u u+1} Sort u #7 (λ (x : Sort u), x) #7 h #6)

@ fail2.func
res13: Term = 
('z : ∏('aj : eq(𝒰 )('aa)('aa)){ eq('aa)(induc_{ eq(𝒰 )('aa) ; ($ch : 𝒰 ) ↦ ($ci : eq(𝒰 )('aa)($ch)) ↦ $ch }('ab)('aj))('ab) }) ↦ //.the type of this matters
    induc_{ (A : 𝒰 ) ↦ heq('aa)('ab)(A) ; (B : 𝒰 ) ↦ (b : B) ↦ (p : heq('aa)('ab)(B)(b)) ↦ ∏('aj : eq(𝒰 )('aa)(B)){ eq(B)(induc_{ eq(𝒰 )('aa) ; ($ch : 𝒰 ) ↦ ($ci : eq(𝒰 )('aa)($ch)) ↦ $ch }('ab)('aj))(b) } }('z)('ag)

@ fail2.arg
res14: Term = 
  ('ah : eq(𝒰 )('aa)('aa)) ↦ eq.refl('aa)(induc_{ eq(𝒰 )('aa) ; ($ch : 𝒰 ) ↦ ($ci : eq(𝒰 )('aa)($ch)) ↦ $ch }('ab)
    ('ah) // `'ah` has type eq(𝒰 )('aa)('aa) so is equal to reflexivity; the expression resolves to `'ab`
    )

@ fail2.domOpt.get
res15: Typ[Term] = ∏('aj : eq(𝒰 )('aa)('aa)){ eq('aa)(induc_{ eq(𝒰 )('aa) ; ($ch : 𝒰 ) ↦ ($ci : eq(𝒰 )('aa)($ch)) ↦ $ch }('ab)('aj))('ab) }

@ fail2.arg.typ
res16: Typ[U] = ∏('ah : eq(𝒰 )('aa)('aa)){ eq('aa)(induc_{ eq(𝒰 )('aa) ; ($ch : 𝒰 ) ↦ ($ci : eq(𝒰 )('aa)($ch)) ↦ $ch }('ab)('ah))(induc_{ eq(𝒰 )('aa) ; ($ch : 𝒰 ) ↦ ($ci : eq(𝒰 )('aa)($ch)) ↦ $ch }('ab)('ah)) }
```

Finally, here is the full description of `eq_of_heq`.

```scala
λ {α : Sort u} {a a_0 : α} (h : @heq.{u} α a α a_0),
(λ (this :
      (∀ (α_0 : Sort u) (a_1 : α_0) (h_0 : @heq.{u} α a α_0 a_1)
        (h_1 : @eq.{u+1} Sort u α α_0),
      @eq.{u} ((λ (x : Sort u), x) α_0)
        (@eq.rec_on.{u u+1} Sort u α (λ (x : Sort u), x) α_0 h_1 a) a_1)),
  (λ (this_0 :
        @eq.{u} ((λ (x : Sort u), x) α)
          (@eq.rec_on.{u u+1} Sort u α (λ (x : Sort u), x) α
            (@eq.refl.{u+1} Sort u α) a) a_0),
    this_0) (this α a_0 h (@eq.refl.{u+1} Sort u α)))
  (λ (α_0 : Sort u) (a_1 : α_0) (h_0 : @heq.{u} α a α_0 a_1),
  @heq.rec_on.{0 u} α a // the function begins here
    (λ (x : Sort u) (x_0 : x),
    ∀ (h_1 : @eq.{u+1} Sort u α x),
    @eq.{u} ((λ (x_1 : Sort u), x_1) x)
      (@eq.rec_on.{u u+1} Sort u α (λ (x_1 : Sort u), x_1) x h_1 a
      ) x_0) α_0 a_1
        h_0 // the function ends, argument below
    (λ (h_1 : @eq.{u+1} Sort u α α),
      @rfl.{u} ((λ (x : Sort u), x) α)
        (@eq.rec_on.{u u+1} Sort u α (λ (x : Sort u), x) α h_1 a))) // the argument ends
```

```scala
val argExp =
Lam(
  Binding(
    Str(, h₂),
    App(
      App(App(Const(Str(, eq), Vector(Succ(Param(Str(, u))))), Sort(Param(Str(, u)))), Var(6)),
      Var(6)
    ),
    Default
  ),
  App(
    App(
      Const(Str(, rfl), Vector(Param(Str(, u)))),
      App(Lam(Binding(Str(, _x), Sort(Param(Str(, u))), Default), Var(0)), Var(7))
    ),
    App(
      App(
        App(
          App(
            App(
              App(
                Const(Str(Str(, eq), rec_on), Vector(Param(Str(, u)), Succ(Param(Str(, u))))),
                Sort(Param(Str(, u)))
              ),
              Var(7)
            ),
            Lam(Binding(Str(, _x), Sort(Param(Str(, u))), Default), Var(0))
          ),
          Var(7)
        ),
        Var(0)
      ),
      Var(6)
    )
  )
)
```
