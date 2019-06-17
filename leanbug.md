# Lean bug


While using lean parser server for `decidable.rec_false`, which has lean code as follows:

```haskell
def rec_on_false [h : decidable p] {h₁ : p → Sort u} {h₂ : ¬p → Sort u} (h₃ : ¬p) (h₄ : h₂ h₃)
      : decidable.rec_on h h₂ h₁ :=
  decidable.rec_on h (λ h, h₄) (λ h, false.rec _ (h₃ h))
```

where `decidable.rec_on` parses to:

```scala
val decidable.rec_on =
    (`'n : Prop) ↦
        (
            (`'o : ((decidable) (`'n)) → (𝒰 _0)) ↦
                ((`'p : (decidable) (`'n)) ↦
                    (
                        (```'q : (`'q : (`'n) → (false) ) ~> ((`'o) (((decidable.is_false) (`'n)) (`'q)))) ↦
                            (
                                (```'r : (_ : `'n ) ~> ((`'o) (((decidable.is_true) (`'n)) (_)))) ↦
                                    ((ind((decidable) (`'n))((``$eo : (decidable) (`'n)) ↦
                                        ((`'o) (``$eo)))(```'q)(```'r)) (`'p)))
                            )))
```

we get the following session:

```scala
provingground.interface.LeanParser#withMod x$83:640 s"Defined $name": "Defined decidable"
provingground.interface.LeanParser#withMod x$83:640 s"Defined $name": "Defined false"
provingground.interface.LeanParser#withDefn x$69:587 s"Defined $name": "Defined not"
provingground.interface.LeanParser#foldAxiomSeq x$78:623 s"Defined $name": "Defined decidable.is_false"
provingground.interface.LeanParser#foldAxiomSeq x$78:623 s"Defined $name": "Defined decidable.is_true"
provingground.interface.LeanParser#parse resTask:466 s"Seeking RecIterAp $name, $args, $vars": "Seeking RecIterAp decidable, Vector(#4, #3, #1, #0, #2), Vector('r, 'q, 'p, 'o, 'n)"
provingground.interface.LeanParser#parse resTask:467 s"${vars.headOption.map(isWitness)}": "Some(false)"
provingground.interface.LeanInterface.introShuffle:47 s"${data.typ.fansi}, $flag, ${intro.typ.fansi}": "\u001b[36m\u220f\u001b[93m(\u001b[39m'q\u001b[33m : \u001b[39m('n \u001b[91m\u2192\u001b[39m false)\u001b[93m)\u001b[91m{ \u001b[39m'o(decidable.is_false('n)('q))\u001b[91m }\u001b[39m, Vector(false), \u001b[36m\u220f\u001b[93m(\u001b[39m'd\u001b[33m : \u001b[39mProp\u001b[93m)\u001b[91m{ \u001b[39m(('d \u001b[91m\u2192\u001b[39m false) \u001b[91m\u2192\u001b[39m decidable('d))\u001b[91m }\u001b[39m"
∏('d : Prop){ (('d → false) → decidable('d)) }
'q
∏('q : ('n → false)){ 'o(decidable.is_false('n)('q)) }
provingground.interface.LeanInterface.introShuffle:47 s"${data.typ.fansi}, $flag, ${intro.typ.fansi}": "\u001b[36m\u220f\u001b[93m(\u001b[39m_\u001b[33m : \u001b[39m'n\u001b[93m)\u001b[91m{ \u001b[39m'o(decidable.is_true('n)(_))\u001b[91m }\u001b[39m, Vector(false), \u001b[36m\u220f\u001b[93m(\u001b[39m'c\u001b[33m : \u001b[39mProp\u001b[93m)\u001b[91m{ \u001b[39m('c \u001b[91m\u2192\u001b[39m decidable('c))\u001b[91m }\u001b[39m"
∏('c : Prop){ ('c → decidable('c)) }
'r
∏(_ : 'n){ 'o(decidable.is_true('n)(_)) }
provingground.interface.LeanParser#withDefn x$69:587 s"Defined $name": "Defined decidable.rec_on"
provingground.interface.LeanRoutes.parse:224 message: """while parsing decidable.rec_on_false, got provingground.interface.LeanParser$ParseException: provingground.HoTT$ApplnFailException: function (`````'q :  (`'q : ('u) → (false) ) ~> (('w) (_))) ↦ ((`````'r :  (_ : 'u ) ~> (('w) (_))) ↦ ((ind((decidable) ('u))((```$eo :  (decidable) ('u)) ↦ (('w) (_)))(`````'q)(`````'r)) ('v))) with domain(optional) Some((`'q : ('u) → (false) ) ~> (('w) (_))) cannot act on given term (_ :  ('u) → (false)) ↦ ('z) with type (('u) → (false)) → (('x) ('y))
 Modifier: 
 Some(λ {p : Prop} [h : @decidable p] {h_0 : (∀ (a : p), Sort u)}
  {h_1 : (∀ (a : @not p), Sort u)} (h_2 : @not p) (h_3 : h_1 h_2),
@decidable.rec_on.{u} p
  (λ (x : @decidable p),
  @decidable.rec_on.{u+1} p (λ (x_0 : @decidable p), Sort u) x h_1 h_0) h
  (λ (h_4 : @not p), h_3)
  (λ (h_4 : p),
  @false.rec.{u}
    (@decidable.rec_on.{u+1} p (λ (x : @decidable p), Sort u)
      (@decidable.is_true p h_4) h_1 h_0) (h_2 h_4)))"""
provingground.interface.LeanRoutes.parse:225 p.findDefMod(
                trepplein.Name(name.split("\\."): _*)).map(_.value):
```

isolating the culprit:

```scala
λ {p : Prop} [h : @decidable p] {h_0 : (∀ (a : p), Sort u)}{h_1 : (∀ (a : @not p), Sort u)} (h_2 : @not p) (h_3 : h_1 h_2),
    @decidable.rec_on.{u} p
        (λ (x : @decidable p),
            @decidable.rec_on.{u+1} p (λ (x_0 : @decidable p), Sort u) x h_1 h_0)
        h
        (λ (h_4 : @not p), h_3)
        (λ (h_4 : p),
            @false.rec.{u}
                (@decidable.rec_on.{u+1} p (λ (x : @decidable p), Sort u)
                    (@decidable.is_true p h_4) h_1 h_0
                ) (h_2 h_4))
```

With the final detailed line:

```scala
Some(
  Lam(
    Binding(Str(, "p"), Sort(Zero), Implicit),
    Lam(
      Binding(Str(, "h"), App(Const(Str(, "decidable"), Vector()), Var(0)), InstImplicit),
      Lam(
        Binding(
          Str(, "h\u2081"),
          Pi(Binding(Str(, "a"), Var(1), Default), Sort(Param(Str(, "u")))),
          Implicit
        ),
        Lam(
          Binding(
            Str(, "h\u2082"),
            Pi(
              Binding(Str(, "a"), App(Const(Str(, "not"), Vector()), Var(2)), Default),
              Sort(Param(Str(, "u")))
            ),
            Implicit
          ),
          Lam(
            Binding(Str(, "h\u2083"), App(Const(Str(, "not"), Vector()), Var(3)), Default),
            Lam(
              Binding(Str(, "h\u2084"), App(Var(1), Var(0)), Default),
              App(
                App(
                  App(
                    App(
                      App(
                        Const(Str(Str(, "decidable"), "rec_on"), Vector(Param(Str(, "u")))),
                        Var(5)
                      ),
                      Lam(
                        Binding(
                          Str(, "_x"),
                          App(Const(Str(, "decidable"), Vector()), Var(5)),
                          Default
                        ),
                        App(
                          App(
                            App(
                              App(
                                App(
                                  Const(
                                    Str(Str(, "decidable"), "rec_on"),
                                    Vector(Succ(Param(Str(, "u"))))
                                  ),
                                  Var(6)
                                ),
                                Lam(
                                  Binding(
                                    Str(, "_x"),
                                    App(Const(Str(, "decidable"), Vector()), Var(6)),
                                    Default
                                  ),
                                  Sort(Param(Str(, "u")))
                                )
                              ),
                              Var(0)
                            ),
                            Var(3)
                          ),
                          Var(4)
                        )
                      )
                    ),
                    Var(4)
                  ),
                  Lam(
                    Binding(Str(, "h"), App(Const(Str(, "not"), Vector()), Var(5)), Default),
                    Var(1)
                  )
                ),
                Lam(
                  Binding(Str(, "h"), Var(5), Default),
                  App(
                    App(
                      Const(Str(Str(, "false"), "rec"), Vector(Param(Str(, "u")))),
                      App(
                        App(
                          App(
                            App(
                              App(
                                Const(
                                  Str(Str(, "decidable"), "rec_on"),
                                  Vector(Succ(Param(Str(, "u"))))
                                ),
                                Var(6)
                              ),
                              Lam(
                                Binding(
                                  Str(, "_x"),
                                  App(Const(Str(, "decidable"), Vector()), Var(6)),
                                  Default
                                ),
                                Sort(Param(Str(, "u")))
                              )
                            ),
                            App(
                              App(Const(Str(Str(, "decidable"), "is_true"), Vector()), Var(6)),
                              Var(0)
                            )
                          ),
                          Var(3)
                        ),
                        Var(4)
                      )
                    ),
                    App(Var(2), Var(0))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)
```