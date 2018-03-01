```scala
@ val af = fail.apl.get._1
af: trepplein.Expr = App(
  App(
    App(App(Const(Str(Str(, "monoid"), "mk"), Vector(Param(Str(, "u")))), Var(8)), Var(6)),
    Var(5)
  ),
  Var(4)
)

@ val af = fail.apl.get._3.func
af: Term = monoid.mk('a)('c)('d)('e)

@ val af = fail.apl.get._3.arg
af: Term = 'f

@ val af = fail.apl.get._3.argType
af: Typ[U] = ∏('f : 'a){ eq('a)('c('e)('f))('f) }

@ val af = fail.apl.get._3.domOpt.get
af: Typ[u] = ∏('e : 'a){ eq('a)('c('e)('e))('e) }

@ parser.get("monoid.mk").value
res13: Option[scala.util.Try[Term]] = Some(Success(monoid.mk))

@ val mmk = parser.get("monoid.mk").value.get.get  
mmk: Term = monoid.mk

@ mmk.typ
res15: Typ[U] = ∏('a : 𝒰 )
{ ∏('b : ('a → ('a → 'a))){ ∏('c : ∏('c : 'a){ ∏('d : 'a){ ∏('e : 'a){ eq('a)('b('b('c)('d))('e))('b('c)('b('d)('e))) } } }
){ ∏('d : 'a){
   (∏('e : 'a){ eq('a)('b('d)('e))('e) } → (∏('f : 'a){ eq('a)('b('f)('d))('f) } → monoid('a))) } } } }
```
