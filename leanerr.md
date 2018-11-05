## This time an error in `bin_tree`

Note the source already before shuffling:
```scala
Vector('r, 's,
  ('u : bin_tree('o)) ↦ (_ : 'p('u)) ↦ ('v : bin_tree('o)) ↦ (_ : 'p('v)) ↦ 't('u)('v), // the term
  'q)
Vector('r, 's,
  ('u : bin_tree('o)) ↦ ('v : bin_tree('o)) ↦ ('w : 'p('u)) ↦ ('x : 'p('v)) ↦ 't('u)('v), // the term
  'q)
Vector('p(bin_tree.empty('o)), ∏('s : 'o){ 'p(bin_tree.leaf('o)('s)) },
  ∏('u : bin_tree('o)){
    ('p('u) → ∏('v : bin_tree('o)){ ('p('v) → 'p(bin_tree.node('o)('v)('v))) }) }, // the type giving error
    bin_tree('o))
Vector('p(bin_tree.empty('o)), ∏('s : 'o){ 'p(bin_tree.leaf('o)('s)) },
  ∏('u : bin_tree('o)){
    ∏('v : bin_tree('o)){ ('p('u) → ('p('v) → 'p(bin_tree.node('o)('v)('v)))) } },
    bin_tree('o))
```
* the term `'t('u)'v` unexpectedly has type `'p(bin_tree.node('o)('v)('v))` which gives an error.
* this is even before shuffling
* the full error and the raw expression are below.

```scala
while parsing bin_tree.cases_on, got provingground.interface.LeanParser$ParseException:
provingground.interface.RecFoldException: Failure to fold recursive Function for bin_tree,
recursion function
  (InducDataSym((bin_tree.empty) ('o)) :  ('p) ((bin_tree.empty) ('o))) ↦ ((InducDataSym((bin_tree.leaf) ('o)) :  ($bzryq : 'o ) ~> (('p) (((bin_tree.leaf) ('o)) ($bzryq)))) ↦ ((InducDataSym((bin_tree.node) ('o)) :  ($bzsdp : (bin_tree) ('o) ) ~> ((('p) ($bzsdp)) → (($bzstd : (bin_tree) ('o) ) ~> ((('p) ($bzstd)) → (('p) ((((bin_tree.node) ('o)) ($bzsdp)) ($bzstd))))))) ↦ (ind((bin_tree) ('o))(($bzqod :  (bin_tree) ('o)) ↦ (('p) ($bzqod)))(InducDataSym((bin_tree.empty) ('o)))(InducDataSym((bin_tree.leaf) ('o)))(InducDataSym((bin_tree.node) ('o))))))
with error provingground.HoTT$ApplnFailException:
function
  (InducDataSym((bin_tree.node) ('o)) :  ($bzsdp : (bin_tree) ('o) ) ~> ((('p) ($bzsdp)) → (($bzstd : (bin_tree) ('o) ) ~> ((('p) ($bzstd)) → (('p) ((((bin_tree.node) ('o)) ($bzsdp)) ($bzstd))))))) ↦ (ind((bin_tree) ('o))(($bzqod :  (bin_tree) ('o)) ↦ (('p) ($bzqod)))('r)('s)(InducDataSym((bin_tree.node) ('o))))
with domain(optional)
Some(
  ($bzsdp : (bin_tree) ('o) ) ~> ((('p) ($bzsdp)) → (($bzstd : (bin_tree) ('o) ) ~> ((('p) ($bzstd)) → (('p) ((((bin_tree.node) ('o)) ($bzsdp)) ($bzstd)))))))
cannot act on given term
  ('u :  (bin_tree) ('o)) ↦ ((_ :  ('p) ('u)) ↦ (('v :  (bin_tree) ('o)) ↦ ((_ :  ('p) ('v)) ↦ ((('t) ('u)) ('v)))))
with type
  ('u : (bin_tree) ('o) ) ~> ((('p) ('u)) → (('v : (bin_tree) ('o) ) ~> ((('p) ('v)) → (('p) ((((bin_tree.node) ('o)) ('v)) ('v))))))

Modifier:
Some(
  λ {α : Type u} {C : (∀ (n_0 : @bin_tree.{u} α), Sort l)}
  (n : @bin_tree.{u} α)
  (e_1 : C (@bin_tree.empty.{u} α))
  (e_2 : (∀ (val : α), C (@bin_tree.leaf.{u} α val)))
  (e_3 :
    (∀ (left right : @bin_tree.{u} α), C (@bin_tree.node.{u} α left right))),
    @bin_tree.rec.{l u} α C e_1 (λ (val : α), e_2 val)
      (λ (left right : @bin_tree.{u} α) (ih_left : C left) (ih_right : C right),
        e_3 left right) n)
```

* In terms of the modifier, `'o` is `\alpha`, `'p` is `C`, `'u` and `'v` are _left_ and _right_ and `'t` is `e_3`.
* If that is so, then getting witnesses is suspicious, and can explain not distinguishing left and right.

```scala
Some(
  Lam(
    Binding(Str(, "\u03b1"), Sort(Succ(Param(Str(, "u")))), Implicit),
    Lam(
      Binding(
        Str(, "C"),
        Pi(
          Binding(
            Str(, "n"),
            App(Const(Str(, "bin_tree"), Vector(Param(Str(, "u")))), Var(0)),
            Default
          ),
          Sort(Param(Str(, "l")))
        ),
        Implicit
      ),
      Lam(
        Binding(
          Str(, "n"),
          App(Const(Str(, "bin_tree"), Vector(Param(Str(, "u")))), Var(1)),
          Default
        ),
        Lam(
          Binding(
            Str(, "e_1"),
            App(
              Var(1),
              App(Const(Str(Str(, "bin_tree"), "empty"), Vector(Param(Str(, "u")))), Var(2))
            ),
            Default
          ),
          Lam(
            Binding(
              Str(, "e_2"),
              Pi(
                Binding(Str(, "val"), Var(3), Default),
                App(
                  Var(3),
                  App(
                    App(Const(Str(Str(, "bin_tree"), "leaf"), Vector(Param(Str(, "u")))), Var(4)),
                    Var(0)
                  )
                )
              ),
              Default
            ),
            Lam(
              Binding(
                Str(, "e_3"),
                Pi(
                  Binding(
                    Str(, "left"),
                    App(Const(Str(, "bin_tree"), Vector(Param(Str(, "u")))), Var(4)),
                    Default
                  ),
                  Pi(
                    Binding(
                      Str(, "right"),
                      App(Const(Str(, "bin_tree"), Vector(Param(Str(, "u")))), Var(5)),
                      Default
                    ),
                    App(
                      Var(5),
                      App(
                        App(
                          App(
                            Const(Str(Str(, "bin_tree"), "node"), Vector(Param(Str(, "u")))),
                            Var(6)
                          ),
                          Var(1)
                        ),
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
                      App(
                        App(
                          Const(
                            Str(Str(, "bin_tree"), "rec"),
                            Vector(Param(Str(, "l")), Param(Str(, "u")))
                          ),
                          Var(5)
                        ),
                        Var(4)
                      ),
                      Var(2)
                    ),
                    Lam(Binding(Str(, "val"), Var(5), Default), App(Var(2), Var(0)))
                  ),
                  Lam(
                    Binding(
                      Str(, "left"),
                      App(Const(Str(, "bin_tree"), Vector(Param(Str(, "u")))), Var(5)),
                      Default
                    ),
                    Lam(
                      Binding(
                        Str(, "right"),
                        App(Const(Str(, "bin_tree"), Vector(Param(Str(, "u")))), Var(6)),
                        Default
                      ),
                      Lam(
                        Binding(Str(, "ih_left"), App(Var(6), Var(1)), Default),
                        Lam(
                          Binding(Str(, "ih_right"), App(Var(7), Var(1)), Default),
                          App(App(Var(4), Var(3)), Var(2))
                        )
                      )
                    )
                  )
                ),
                Var(3)
              )
            )
          )
        )
      )
    )
  )
)


```


## Later error

```scala
while parsing bin_tree.leaf.inj, got provingground.interface.LeanParser$ParseException: provingground.interface.RecFoldException:
Failure to fold recursive Function for bin_tree, recursion function
  (InducDataSym((bin_tree.empty) ('o)) :  ('p) ((bin_tree.empty) ('o))) ↦ ((InducDataSym((bin_tree.leaf) ('o)) :  ($lxb : 'o ) ~> (('p) (((bin_tree.leaf) ('o)) ($lxb)))) ↦ ((InducDataSym((bin_tree.node) ('o)) :  ('u : (bin_tree) ('o) ) ~> ((('p) ('u)) → (('v : (bin_tree) ('o) ) ~> ((('p) ('v)) → (('p) ((((bin_tree.node) ('o)) ('u)) ('v))))))) ↦ (ind((bin_tree) ('o))(($kmo :  (bin_tree) ('o)) ↦ (('p) ($kmo)))(InducDataSym((bin_tree.empty) ('o)))(InducDataSym((bin_tree.leaf) ('o)))(InducDataSym((bin_tree.node) ('o))))))
with error provingground.HoTT$ApplnFailException:
function
  (InducDataSym((bin_tree.node) ('o)) :  ('u : (bin_tree) ('o) ) ~> ((('p) ('u)) → (('v : (bin_tree) ('o) ) ~> ((('p) ('v)) → (('p) ((((bin_tree.node) ('o)) ('u)) ('v))))))) ↦ (ind((bin_tree) ('o))(($kmo :  (bin_tree) ('o)) ↦ (('p) ($kmo)))('r)('s)(InducDataSym((bin_tree.node) ('o))))
with domain(optional)
Some(
  ('u : (bin_tree) ('o) ) ~> ((('p) ('u)) → (('v : (bin_tree) ('o) ) ~> ((('p) ('v)) → (('p) ((((bin_tree.node) ('o)) ('u)) ('v))))))
) cannot act on given term
  ('u :  (bin_tree) ('o)) ↦ ((_ :  ('p) ('u)) ↦ (('v :  (bin_tree) ('o)) ↦ ((_ :  ('p) ('v)) ↦ ((('t) ('u)) ('v)))))
with type
  ('u : (bin_tree) ('o) ) ~> ((('p) ('u)) → (('v : (bin_tree) ('o) ) ~> ((('p) ('v)) → (('p) ((((bin_tree.node) ('o)) ('v)) ('v))))))

provingground.interface.LeanRoutes.parse:222 p.findDefMod(
                trepplein.Name(name.split("\\."): _*)).map(_.value): Some(
  Lam(
    Binding(Str(, "\u03b1"), Sort(Succ(Param(Str(, "u")))), Implicit),
    Lam(
      Binding(Str(, "val"), Var(0), Implicit),
      Lam(
        Binding(Str(, "val"), Var(1), Implicit),
        Lam(
          Binding(
            Str(, "a"),
            App(
              App(
                App(
                  Const(Str(, "eq"), Vector(Succ(Param(Str(, "u"))))),
                  App(Const(Str(, "bin_tree"), Vector(Param(Str(, "u")))), Var(2))
                ),
                App(
                  App(Const(Str(Str(, "bin_tree"), "leaf"), Vector(Param(Str(, "u")))), Var(2)),
                  Var(1)
                )
              ),
              App(
                App(Const(Str(Str(, "bin_tree"), "leaf"), Vector(Param(Str(, "u")))), Var(2)),
                Var(0)
              )
            ),
            Default
          ),
          App(
            App(
              App(
                App(
                  App(
                    App(
                      Const(Str(Str(, "bin_tree"), "no_confusion"), Vector(Zero, Param(Str(, "u")))),
                      Var(3)
                    ),
                    App(
                      App(App(Const(Str(, "eq"), Vector(Succ(Param(Str(, "u"))))), Var(3)), Var(2)),
                      Var(1)
                    )
                  ),
                  App(
                    App(Const(Str(Str(, "bin_tree"), "leaf"), Vector(Param(Str(, "u")))), Var(3)),
                    Var(2)
                  )
                ),
                App(
                  App(Const(Str(Str(, "bin_tree"), "leaf"), Vector(Param(Str(, "u")))), Var(3)),
                  Var(1)
                )
              ),
              Var(0)
            ),
            Lam(
              Binding(
                Str(, "val_eq"),
                App(
                  App(App(Const(Str(, "eq"), Vector(Succ(Param(Str(, "u"))))), Var(3)), Var(2)),
                  Var(1)
                ),
                Default
              ),
              Var(0)
            )
          )
        )
      )
    )
  )
)

```
