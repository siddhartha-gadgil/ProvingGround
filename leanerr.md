## The error in `drec` again

```scala
while parsing nat.less_than_or_equal.drec, got provingground.interface.LeanParser$ParseException:
provingground.interface.RecFoldException: Failure to fold recursive Function for nat.less_than_or_equal,
  recursion function (_ :  (('r) ('q)) (_)) ↦ ((InducDataSym((nat.less_than_or_equal.step) ('q)) :  ('u : nat ) ~> ((_ : ((nat.less_than_or_equal) ('q)) ('u) ) ~> (((('r) ('u)) (_)) → ((('r) ((nat.succ) ('u))) (_))))) ↦ (($gsbtc :  nat) ↦ (ind{(nat.less_than_or_equal) ('q)($gsbtc)}{($gsacz :  nat) ↦ ((_ :  ((nat.less_than_or_equal) ('q)) ($gsacz)) ↦ ((('r) ($gsacz)) (_)))}(_)(InducDataSym((nat.less_than_or_equal.step) ('q))))))
with error provingground.HoTT$ApplnFailException:
  function (InducDataSym((nat.less_than_or_equal.step) ('q)) :  ('u : nat ) ~> ((_ : ((nat.less_than_or_equal) ('q)) ('u) ) ~> (((('r) ('u)) (_)) → ((('r) ((nat.succ) ('u))) (_))))) ↦ (($gsbtc :  nat) ↦ (ind{(nat.less_than_or_equal) ('q)($gsbtc)}{($gsacz :  nat) ↦ ((_ :  ((nat.less_than_or_equal) ('q)) ($gsacz)) ↦ ((('r) ($gsacz)) (_)))}(_)(InducDataSym((nat.less_than_or_equal.step) ('q)))))
with domain(optional) Some(
  ('u : nat ) ~>
    ((_ : ((nat.less_than_or_equal) ('q)) ('u) ) ~>
    (
      ((('r) ('u)) (_)) →
        ((('r) ((nat.succ) ('u))) (_))
      )
    )
)
cannot act on given term
  ('u :  nat) ↦
    ((_ :  ((nat.less_than_or_equal) ('q)) ('u)) ↦
      (
        ('v :  (_ : ((nat.less_than_or_equal) ('q)) ('u) ) ~> ((('r) ('u)) (_))) ↦
        ((_ :  ((nat.less_than_or_equal) ('q)) ((nat.succ) ('u))) ↦ (_))
      )
    )
with type
  ('u : nat ) ~>
    (
      (((nat.less_than_or_equal) ('q)) ('u)) →
        (((_ : ((nat.less_than_or_equal) ('q)) ('u) ) ~> // extra, witness
          ((('r) ('u)) (_))) → 
            ((((nat.less_than_or_equal) ('q)) ((nat.succ) ('u))) → // extra, witness
            ((('r) ((nat.succ) ('u))) (_)))
        )
      )

Modifier:
 Some(λ {a : @nat} {C : (∀ (a_1 : @nat) (h : @nat.less_than_or_equal a a_1), Prop)}
  (e_1 : C a (@nat.less_than_or_equal.refl a))
  (e_2 :
    (∀ {b : @nat} (a_1 : @nat.less_than_or_equal a b) (ih : C b a_1),
    C (@nat.succ b) (@nat.less_than_or_equal.step a b a_1))) {a_0 : @nat}
  (n : @nat.less_than_or_equal a a_0),
@nat.less_than_or_equal.rec a
  (λ (a_1 : @nat), ∀ (h : @nat.less_than_or_equal a a_1), C a_1 h)
  (λ (a_1 : @nat.less_than_or_equal a a), e_1)
  (λ {b : @nat} (a_1 : @nat.less_than_or_equal a b)
    (ih : (∀ (h : @nat.less_than_or_equal a b), C b h))
    (a_2 : @nat.less_than_or_equal a (@nat.succ b)),
  e_2 b a_1 (ih a_1)) a_0 n n)"""
provingground.interface.LeanRoutes.parse:222 p.findDefMod(
                trepplein.Name(name.split("\\."): _*)).map(_.value): Some(
  Lam(
    Binding(Str(, "a"), Const(Str(, "nat"), Vector()), Implicit),
    Lam(
      Binding(
        Str(, "C"),
        Pi(
          Binding(Str(, "a"), Const(Str(, "nat"), Vector()), Default),
          Pi(
            Binding(
              Str(, "h"),
              App(App(Const(Str(Str(, "nat"), "less_than_or_equal"), Vector()), Var(1)), Var(0)),
              Default
            ),
            Sort(Zero)
          )
        ),
        Implicit
      ),
      Lam(
        Binding(
          Str(, "e_1"),
          App(
            App(Var(0), Var(1)),
            App(Const(Str(Str(Str(, "nat"), "less_than_or_equal"), "refl"), Vector()), Var(1))
          ),
          Default
        ),
        Lam(
          Binding(
            Str(, "e_2"),
            Pi(
              Binding(Str(, "b"), Const(Str(, "nat"), Vector()), Implicit),
              Pi(
                Binding(
                  Str(, "a"),
                  App(App(Const(Str(Str(, "nat"), "less_than_or_equal"), Vector()), Var(3)), Var(0)),
                  Default
                ),
                Pi(
                  Binding(Str(, "ih"), App(App(Var(3), Var(1)), Var(0)), Default),
                  App(
                    App(Var(4), App(Const(Str(Str(, "nat"), "succ"), Vector()), Var(2))),
                    App(
                      App(
                        App(
                          Const(Str(Str(Str(, "nat"), "less_than_or_equal"), "step"), Vector()),
                          Var(5)
                        ),
                        Var(2)
                      ),
                      Var(1)
                    )
                  )
                )
              )
            ),
            Default
          ),
          Lam(
            Binding(Str(, "a"), Const(Str(, "nat"), Vector()), Implicit),
            Lam(
              Binding(
                Str(, "n"),
                App(App(Const(Str(Str(, "nat"), "less_than_or_equal"), Vector()), Var(4)), Var(0)),
                Default
              ),
              App(
                App(
                  App(
                    App(
                      App(
                        App(
                          App(
                            Const(Str(Str(Str(, "nat"), "less_than_or_equal"), "rec"), Vector()),
                            Var(5)
                          ),
                          Lam(
                            Binding(Str(, "a"), Const(Str(, "nat"), Vector()), Default),
                            Pi(
                              Binding(
                                Str(, "h"),
                                App(
                                  App(
                                    Const(Str(Str(, "nat"), "less_than_or_equal"), Vector()),
                                    Var(6)
                                  ),
                                  Var(0)
                                ),
                                Default
                              ),
                              App(App(Var(6), Var(1)), Var(0))
                            )
                          )
                        ),
                        Lam(
                          Binding(
                            Str(, "_"),
                            App(
                              App(Const(Str(Str(, "nat"), "less_than_or_equal"), Vector()), Var(5)),
                              Var(5)
                            ),
                            Default
                          ),
                          Var(4)
                        )
                      ),
                      Lam(
                        Binding(Str(, "b"), Const(Str(, "nat"), Vector()), Implicit),
                        Lam(
                          Binding(
                            Str(, "a"),
                            App(
                              App(Const(Str(Str(, "nat"), "less_than_or_equal"), Vector()), Var(6)),
                              Var(0)
                            ),
                            Default
                          ),
                          Lam(
                            Binding(
                              Str(, "ih"),
                              Pi(
                                Binding(
                                  Str(, "_h"),
                                  App(
                                    App(
                                      Const(Str(Str(, "nat"), "less_than_or_equal"), Vector()),
                                      Var(7)
                                    ),
                                    Var(1)
                                  ),
                                  Default
                                ),
                                App(App(Var(7), Var(2)), Var(0))
                              ),
                              Default
                            ),
                            Lam(
                              Binding(
                                Str(, "_"),
                                App(
                                  App(
                                    Const(Str(Str(, "nat"), "less_than_or_equal"), Vector()),
                                    Var(8)
                                  ),
                                  App(Const(Str(Str(, "nat"), "succ"), Vector()), Var(2))
                                ),
                                Default
                              ),
                              App(App(App(Var(6), Var(3)), Var(2)), App(Var(1), Var(2)))
                            )
                          )
                        )
                      )
                    ),
                    Var(1)
                  ),
                  Var(0)
                ),
                Var(0)
              )
            )
          )
        )
      )
    )
  )
)

```
