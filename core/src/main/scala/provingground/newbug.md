```scala
@ pprint.log(failAp.domOpt.get)
cmd47.sc:1 failAp.domOpt.get: SymbTyp(
  name = ApplnSym(
    func = SymbolicFunc(
      name = ApplnSym(
        func = SymbolicFunc(
          name = ApplnSym(
            func = PiSymbolicFunc(
              name = Name(name = "@a"),
              variable = SymbTyp(name = `@a, level = 0),
              value = PiDefn(
                variable = Universe(level = 0),
                value = FuncTyp(dom = SymbTyp(name = `@a, level = 0), codom = Universe(level = 0))
              )
            ),
            arg = Unit
          ),
          dom = Universe(level = 1),
          codom = FuncTyp(dom = Unit, codom = Universe(level = 0))
        ),
        arg = Universe(level = 0)
      ),
      dom = Unit,
      codom = Universe(level = 0)
    ),
    arg = Star
  ),
  level = 0
)
res47: Typ[Term] = @a(Unit)(𝒰 )(Star)
```

```scala
@ pprint.log(failAp.argType)
cmd48.sc:1 failAp.argType: SymbTyp(
  name = ApplnSym(
    func = SymbolicFunc(
      name = ApplnSym(
        func = PiSymbolicFunc(
          name = ApplnSym(
            func = PiSymbolicFunc(
              name = Name(name = "@a"),
              variable = SymbTyp(name = `@a, level = 0),
              value = PiDefn(
                variable = SymbTyp(name = `@b, level = 0),
                value = FuncTyp(
                  dom = SymbTyp(name = `@a, level = 0),
                  codom = SymbTyp(name = `@b, level = 0)
                )
              )
            ),
            arg = Unit
          ),
          variable = SymbTyp(name = `@b, level = 0),
          value = FuncTyp(dom = Unit, codom = SymbTyp(name = `@b, level = 0))
        ),
        arg = Universe(level = 0)
      ),
      dom = Unit,
      codom = Universe(level = 0)
    ),
    arg = Star
  ),
  level = 0
)
res48: Typ[Term] = @a(Unit)(𝒰 )(Star)
```

```scala
@ ff(fffff(Unit)(Type)(Star))(Type) == failAp.func
res65: Boolean = true

@ pprint.log(ff(fffff(Unit)(Type)(Star)))
cmd66.sc:1 ff(fffff(Unit)(Type)(Star)): PiSymbolicFunc(
  name = ApplnSym(
    func = PiSymbolicFunc(
      name = Name(name = "@a"),
      variable = SymbTyp(name = `@a, level = 0),
      value = PiDefn(
        variable = SymbTyp(name = `@b, level = 0),
        value = FuncTyp(
          dom = SymbTyp(name = `@a, level = 0),
          codom = SymbTyp(name = `@b, level = 0)
        )
      )
    ),
    arg = SymbTyp(
      name = ApplnSym(
        func = SymbolicFunc(
          name = ApplnSym(
            func = PiSymbolicFunc(
              name = ApplnSym(
                func = PiSymbolicFunc(
                  name = Name(name = "@a"),
                  variable = SymbTyp(name = `@a, level = 0),
                  value = PiDefn(
                    variable = SymbTyp(name = `@b, level = 0),
                    value = FuncTyp(
                      dom = SymbTyp(name = `@a, level = 0),
                      codom = SymbTyp(name = `@b, level = 0)
                    )
                  )
                ),
                arg = Unit
              ),
              variable = SymbTyp(name = `@b, level = 0),
              value = FuncTyp(dom = Unit, codom = SymbTyp(name = `@b, level = 0))
            ),
            arg = Universe(level = 0)
          ),
          dom = Unit,
          codom = Universe(level = 0)
        ),
        arg = Star
      ),
      level = 0
    )
  ),
  variable = SymbTyp(name = `@b, level = 0),
  value = FuncTyp(
    dom = SymbTyp(
      name = ApplnSym(
        func = SymbolicFunc(
          name = ApplnSym(
            func = PiSymbolicFunc(
              name = ApplnSym(
                func = PiSymbolicFunc(
                  name = Name(name = "@a"),
                  variable = SymbTyp(name = `@a, level = 0),
                  value = PiDefn(
                    variable = SymbTyp(name = `@b, level = 0),
                    value = FuncTyp(
                      dom = SymbTyp(name = `@a, level = 0),
                      codom = SymbTyp(name = `@b, level = 0)
                    )
                  )
                ),
                arg = Unit
                ),
              variable = SymbTyp(name = `@b, level = 0),
              value = FuncTyp(dom = Unit, codom = SymbTyp(name = `@b, level = 0))
            ),
            arg = Universe(level = 0)
          ),
          dom = Unit,
          codom = Universe(level = 0)
        ),
        arg = Star
      ),
      level = 0
    ),
    codom = SymbTyp(name = `@b, level = 0)
  )
)
res66: Term = @a(@a(Unit)(𝒰 )(Star))
```

```scala
@ pprint.log(fffff(Unit)(Type)(Star))
cmd79.sc:1 fffff(Unit)(Type)(Star): SymbTyp(
  name = ApplnSym(
    func = SymbolicFunc(
      name = ApplnSym(
        func = PiSymbolicFunc(
          name = ApplnSym(
            func = PiSymbolicFunc(
              name = Name(name = "@a"),
              variable = SymbTyp(name = `@a, level = 0),
              value = PiDefn(
                variable = SymbTyp(name = `@b, level = 0),
                value = FuncTyp(
                  dom = SymbTyp(name = `@a, level = 0),
                  codom = SymbTyp(name = `@b, level = 0)
                )
              )
            ),
            arg = Unit
          ),
          variable = SymbTyp(name = `@b, level = 0),
          value = FuncTyp(dom = Unit, codom = SymbTyp(name = `@b, level = 0))
        ),
        arg = Universe(level = 0)
      ),
      dom = Unit,
      codom = Universe(level = 0)
    ),
    arg = Star
  ),
  level = 0
)
res79: Term = @a(Unit)(𝒰 )(Star)
```

```scala
@ pprint.log(fffff(Unit)(Type))
cmd85.sc:1 fffff(Unit)(Type): SymbolicFunc(
  name = ApplnSym(
    func = PiSymbolicFunc(
      name = ApplnSym(
        func = PiSymbolicFunc(
          name = Name(name = "@a"),
          variable = SymbTyp(name = `@a, level = 0),
          value = PiDefn(
            variable = SymbTyp(name = `@b, level = 0),
            value = FuncTyp(
              dom = SymbTyp(name = `@a, level = 0),
              codom = SymbTyp(name = `@b, level = 0)
            )
          )
        ),
        arg = Unit
      ),
      variable = SymbTyp(name = `@b, level = 0),
      value = FuncTyp(dom = Unit, codom = SymbTyp(name = `@b, level = 0))
    ),
    arg = Universe(level = 0)
  ),
  dom = Unit,
  codom = Universe(level = 0)
)
res85: Term = @a(Unit)(𝒰 )
```
