## This time an error in `bin_tree`

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
