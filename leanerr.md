```scala
DefMod(
  nat.pred_lt,
  Vector(),
  ∀ {n : @nat} (a : @ne.{1} @nat n (@has_zero.zero.{0} @nat @nat.has_zero)),
  @has_lt.lt.{0} @nat @nat.has_lt (@nat.pred n) n,λ {n : @nat} (a : @ne.{1} @nat n (@has_zero.zero.{0} @nat @nat.has_zero)),
    @nat.cases_on.{0}
      (λ {n_0 : @nat},
      ∀ (a_0 : @ne.{1} @nat n_0 (@has_zero.zero.{0} @nat @nat.has_zero)),
      @has_lt.lt.{0} @nat @nat.has_lt (@nat.pred n_0) n_0) n
      (λ (a_0 : @ne.{1} @nat @nat.zero (@has_zero.zero.{0} @nat @nat.has_zero)),
      @id_rhs.{0}
        (@has_lt.lt.{0} @nat @nat.has_lt
          (@nat.pred (@has_zero.zero.{0} @nat @nat.has_zero))
          (@has_zero.zero.{0} @nat @nat.has_zero))
        (@absurd.{0}
          (@eq.{1} @nat (@has_zero.zero.{0} @nat @nat.has_zero)
            (@has_zero.zero.{0} @nat @nat.has_zero))
          (@has_lt.lt.{0} @nat @nat.has_lt
            (@nat.pred (@has_zero.zero.{0} @nat @nat.has_zero))
            (@has_zero.zero.{0} @nat @nat.has_zero))
          (@rfl.{1} @nat (@has_zero.zero.{0} @nat @nat.has_zero)) a_0))
      (λ (n_0 : @nat)
        (a_0 :
          @ne.{1} @nat (@nat.succ n_0) (@has_zero.zero.{0} @nat @nat.has_zero)),
      @id_rhs.{0}
        (@has_lt.lt.{0} @nat @nat.has_lt (@nat.pred (@nat.succ n_0))
          (@nat.succ n_0))
        (@nat.lt_succ_of_le (@nat.pred (@nat.succ n_0)) n_0
          (@nat.less_than_or_equal.refl (@nat.pred (@nat.succ n_0))))) a
)
```

__Clarification__ The value is

```cpp
λ {n : @nat} (a : @ne.{1} @nat n (@has_zero.zero.{0} @nat @nat.has_zero)),
  @nat.cases_on.{0}
    (λ {n_0 : @nat},
      ∀ (a_0 : @ne.{1} @nat n_0 (@has_zero.zero.{0} @nat @nat.has_zero)), @has_lt.lt.{0} @nat @nat.has_lt (@nat.pred n_0) n_0)
       n
        (λ (a_0 : @ne.{1} @nat @nat.zero (@has_zero.zero.{0} @nat @nat.has_zero)), @id_rhs.{0} (@has_lt.lt.{0} @nat @nat.has_lt (@nat.pred (@has_zero.zero.{0} @nat @nat.has_zero)) (@has_zero.zero.{0} @nat @nat.has_zero)) (@absurd.{0} (@eq.{1} @nat (@has_zero.zero.{0} @nat @nat.has_zero) (@has_zero.zero.{0} @nat @nat.has_zero)) (@has_lt.lt.{0} @nat @nat.has_lt (@nat.pred (@has_zero.zero.{0} @nat @nat.has_zero)) (@has_zero.zero.{0} @nat @nat.has_zero)) (@rfl.{1} @nat (@has_zero.zero.{0} @nat @nat.has_zero)) a_0))
          (λ (n_0 : @nat) (a_0 : @ne.{1} @nat (@nat.succ n_0) (@has_zero.zero.{0} @nat @nat.has_zero)), @id_rhs.{0} (@has_lt.lt.{0} @nat @nat.has_lt (@nat.pred (@nat.succ n_0)) (@nat.succ n_0)) (@nat.lt_succ_of_le (@nat.pred (@nat.succ n_0)) n_0 (@nat.less_than_or_equal.refl (@nat.pred (@nat.succ n_0)))))
            a
```

with error message

```scala
while parsing nat.pred_lt, got provingground.interface.LeanParser$ParseException: provingground.HoTT$ApplnFailException:
  function (_ :  ((nat.less_than_or_equal) ((nat.succ) (_))) ((nat.succ) ('w))) ↦ (_)
with domain(optional)
  Some(
    ((nat.less_than_or_equal) ((nat.succ) (_))) ((nat.succ) ('w)))
cannot act on given term
     (_ :  ((nat.less_than_or_equal) (_)) ('w)) ↦ (_)
with type
  (((nat.less_than_or_equal) (_)) ('w)) → (((nat.less_than_or_equal) ((nat.succ) (_))) ((nat.succ) ('w)))
```


## In structured scala form

```scala
Lam(
  Binding(Str(, "n"), Const(Str(, "nat"), Vector()), Implicit),
  Lam(
    Binding(
      Str(, "a"),
      App(
        App(App(Const(Str(, "ne"), Vector(Succ(Zero))), Const(Str(, "nat"), Vector())), Var(0)),
        App(
          App(Const(Str(Str(, "has_zero"), "zero"), Vector(Zero)), Const(Str(, "nat"), Vector())),
          Const(Str(Str(, "nat"), "has_zero"), Vector())
        )
      ),
      Default
    ),
    App(
      App(
        App(
          App(
            App(
              Const(Str(Str(, "nat"), "cases_on"), Vector(Zero)),
              Lam(
                Binding(Str(, "n"), Const(Str(, "nat"), Vector()), Implicit),
                Pi(
                  Binding(
                    Str(, "a"),
                    App(
                      App(
                        App(Const(Str(, "ne"), Vector(Succ(Zero))), Const(Str(, "nat"), Vector())),
                        Var(0)
                      ),
                      App(
                        App(
                          Const(Str(Str(, "has_zero"), "zero"), Vector(Zero)),
                          Const(Str(, "nat"), Vector())
                        ),
                        Const(Str(Str(, "nat"), "has_zero"), Vector())
                      )
                    ),
                    Default
                  ),
                  App(
                    App(
                      App(
                        App(
                          Const(Str(Str(, "has_lt"), "lt"), Vector(Zero)),
                          Const(Str(, "nat"), Vector())
                        ),
                        Const(Str(Str(, "nat"), "has_lt"), Vector())
                      ),
                      App(Const(Str(Str(, "nat"), "pred"), Vector()), Var(1))
                    ),
                    Var(1)
                  )
                )
              )
            ),
            Var(1)
          ),
          Lam(
            Binding(
              Str(, "a"),
              App(
                App(
                  App(Const(Str(, "ne"), Vector(Succ(Zero))), Const(Str(, "nat"), Vector())),
                  Const(Str(Str(, "nat"), "zero"), Vector())
                ),
                App(
                  App(
                    Const(Str(Str(, "has_zero"), "zero"), Vector(Zero)),
                    Const(Str(, "nat"), Vector())
                  ),
                  Const(Str(Str(, "nat"), "has_zero"), Vector())
                )
              ),
              Default
            ),
            App(
              App(
                Const(Str(, "id_rhs"), Vector(Zero)),
                App(
                  App(
                    App(
                      App(
                        Const(Str(Str(, "has_lt"), "lt"), Vector(Zero)),
                        Const(Str(, "nat"), Vector())
                      ),
                      Const(Str(Str(, "nat"), "has_lt"), Vector())
                    ),
                    App(
                      Const(Str(Str(, "nat"), "pred"), Vector()),
                      App(
                        App(
                          Const(Str(Str(, "has_zero"), "zero"), Vector(Zero)),
                          Const(Str(, "nat"), Vector())
                        ),
                        Const(Str(Str(, "nat"), "has_zero"), Vector())
                      )
                    )
                  ),
                  App(
                    App(
                      Const(Str(Str(, "has_zero"), "zero"), Vector(Zero)),
                      Const(Str(, "nat"), Vector())
                    ),
                    Const(Str(Str(, "nat"), "has_zero"), Vector())
                  )
                )
              ),
              App(
                App(
                  App(
                    App(
                      Const(Str(, "absurd"), Vector(Zero)),
                      App(
                        App(
                          App(
                            Const(Str(, "eq"), Vector(Succ(Zero))),
                            Const(Str(, "nat"), Vector())
                          ),
                          App(
                            App(
                              Const(Str(Str(, "has_zero"), "zero"), Vector(Zero)),
                              Const(Str(, "nat"), Vector())
                            ),
                            Const(Str(Str(, "nat"), "has_zero"), Vector())
                          )
                        ),
                        App(
                          App(
                            Const(Str(Str(, "has_zero"), "zero"), Vector(Zero)),
                            Const(Str(, "nat"), Vector())
                          ),
                          Const(Str(Str(, "nat"), "has_zero"), Vector())
                        )
                      )
                    ),
                    App(
                      App(
                        App(
                          App(
                            Const(Str(Str(, "has_lt"), "lt"), Vector(Zero)),
                            Const(Str(, "nat"), Vector())
                          ),
                          Const(Str(Str(, "nat"), "has_lt"), Vector())
                        ),
                        App(
                          Const(Str(Str(, "nat"), "pred"), Vector()),
                          App(
                            App(
                              Const(Str(Str(, "has_zero"), "zero"), Vector(Zero)),
                              Const(Str(, "nat"), Vector())
                            ),
                            Const(Str(Str(, "nat"), "has_zero"), Vector())
                          )
                        )
                      ),
                      App(
                        App(
                          Const(Str(Str(, "has_zero"), "zero"), Vector(Zero)),
                          Const(Str(, "nat"), Vector())
                        ),
                        Const(Str(Str(, "nat"), "has_zero"), Vector())
                      )
                    )
                  ),
                  App(
                    App(Const(Str(, "rfl"), Vector(Succ(Zero))), Const(Str(, "nat"), Vector())),
                    App(
                      App(
                        Const(Str(Str(, "has_zero"), "zero"), Vector(Zero)),
                        Const(Str(, "nat"), Vector())
                      ),
                      Const(Str(Str(, "nat"), "has_zero"), Vector())
                    )
                  )
                ),
                Var(0)
              )
            )
          )
        ),
        Lam(
          Binding(Str(, "n"), Const(Str(, "nat"), Vector()), Default),
          Lam(
            Binding(
              Str(, "a"),
              App(
                App(
                  App(Const(Str(, "ne"), Vector(Succ(Zero))), Const(Str(, "nat"), Vector())),
                  App(Const(Str(Str(, "nat"), "succ"), Vector()), Var(0))
                ),
                App(
                  App(
                    Const(Str(Str(, "has_zero"), "zero"), Vector(Zero)),
                    Const(Str(, "nat"), Vector())
                  ),
                  Const(Str(Str(, "nat"), "has_zero"), Vector())
                )
              ),
              Default
            ),
            App(
              App(
                Const(Str(, "id_rhs"), Vector(Zero)),
                App(
                  App(
                    App(
                      App(
                        Const(Str(Str(, "has_lt"), "lt"), Vector(Zero)),
                        Const(Str(, "nat"), Vector())
                      ),
                      Const(Str(Str(, "nat"), "has_lt"), Vector())
                    ),
                    App(
                      Const(Str(Str(, "nat"), "pred"), Vector()),
                      App(Const(Str(Str(, "nat"), "succ"), Vector()), Var(1))
                    )
                  ),
                  App(Const(Str(Str(, "nat"), "succ"), Vector()), Var(1))
                )
              ),
              App(
                App(
                  App(
                    Const(Str(Str(, "nat"), "lt_succ_of_le"), Vector()),
                    App(
                      Const(Str(Str(, "nat"), "pred"), Vector()),
                      App(Const(Str(Str(, "nat"), "succ"), Vector()), Var(1))
                    )
                  ),
                  Var(1)
                ),
                App(
                  Const(Str(Str(Str(, "nat"), "less_than_or_equal"), "refl"), Vector()),
                  App(
                    Const(Str(Str(, "nat"), "pred"), Vector()),
                    App(Const(Str(Str(, "nat"), "succ"), Vector()), Var(1))
                  )
                )
              )
            )
          )
        )
      ),
      Var(0)
    )
  )
)


```
