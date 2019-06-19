# Lean bug

While using lean parser server for `and.dcases_on`. This can be narrowed down to the following expression involving `and` with the application of the second `n`.

```scala
λ {a b : Prop} {C : (∀ (h : @and a b), Sort l)} (n : @and a b)
  (e_1 : (∀ (left : a) (right : b), C (@and.intro a b left right))), // variables a, b, C, n and e_1
    @and.rec.{l} a b (∀ (h : @and a b), C h) // parameters and family for rec
        (λ (left : a) (right : b) (a_0 : @and a b), e_1 left right)
        n n
```

which in full form is:

```scala
Lam(
  Binding(Str(, a), Sort(Zero), Implicit),
  Lam(
    Binding(Str(, b), Sort(Zero), Implicit),
    Lam(
      Binding(
        Str(, C),
        Pi(
          Binding(Str(, h), App(App(Const(Str(, and), Vector()), Var(1)), Var(0)), Default),
          Sort(Param(Str(, l)))
        ),
        Implicit
      ),
      Lam(
        Binding(Str(, n), App(App(Const(Str(, and), Vector()), Var(2)), Var(1)), Default),
        Lam(
          Binding(
            Str(, e_1),
            Pi(
              Binding(Str(, left), Var(3), Default),
              Pi(
                Binding(Str(, right), Var(3), Default),
                App(
                  Var(3),
                  App(
                    App(App(App(Const(Str(Str(, and), intro), Vector()), Var(5)), Var(4)), Var(1)),
                    Var(0)
                  )
                )
              )
            ),
            Default
          ),
          App(
            App(
              App(
                App(
                  App(App(Const(Str(Str(, and), rec), Vector(Param(Str(, l)))), Var(4)), Var(3)),
                  Pi(
                    Binding(
                      Str(, h),
                      App(App(Const(Str(, and), Vector()), Var(4)), Var(3)),
                      Default
                    ),
                    App(Var(3), Var(0))
                  )
                ),
                Lam(
                  Binding(Str(, left), Var(4), Default),
                  Lam(
                    Binding(Str(, right), Var(4), Default),
                    Lam(
                      Binding(
                        Str(, _),
                        App(App(Const(Str(, and), Vector()), Var(6)), Var(5)),
                        Default
                      ),
                      App(App(Var(3), Var(2)), Var(1))
                    )
                  )
                )
              ),
              Var(1)
            ),
            Var(1)
          )
        )
      )
    )
  )
)

```

For contrast, a case with a bug earlier that now works (and also has the peculiar double `n`):

```scala
λ {a : @nat} {C : (∀ (a_1 : @nat) (h : @nat.less_than_or_equal a a_1), Prop)}
  {a_0 : @nat} (n : @nat.less_than_or_equal a a_0)
  (e_1 : C a (@nat.less_than_or_equal.refl a))
  (e_2 :
    (∀ {b : @nat} (a_1 : @nat.less_than_or_equal a b),
    C (@nat.succ b) (@nat.less_than_or_equal.step a b a_1))),
        @nat.less_than_or_equal.rec a
           (λ (a_1 : @nat), ∀ (h : @nat.less_than_or_equal a a_1), C a_1 h)
              (λ (a_1 : @nat.less_than_or_equal a a), e_1)
            (λ {b : @nat} (a_1 : @nat.less_than_or_equal a b)
              (ih : (∀ (h : @nat.less_than_or_equal a b), C b h))
              (a_2 : @nat.less_than_or_equal a (@nat.succ b)),
                 e_2 b a_1) a_0 n n
  ```
