### Expanded form of  `natnoct`

```scala
val natnoct0 =
        A :~> {
          p :~> {
            q :~> {
              (P :~> {
          n :~> {
            x :~> {
              g :~> {
                (indNP(x)(m :~> (("_" :: P(m)) :-> (g(m)))))(n)
              }
            }
          }
        } )(("_" :: Nat) :-> (Type: Typ[Term]))(q) {
                (P :~> {
          n :~> {
            x :~> {
              g :~> {
                (indNP(x)(m :~> (("_" :: P(m)) :-> (g(m)))))(n)
              }
            }
          }
        } )(("_" :: Nat) :-> (Type: Typ[Term]))(p)(A)(
                  l :-> (A ->: A))
              } {
                k :-> {
                  (P :~> {
          n :~> {
            x :~> {
              g :~> {
                (indNP(x)(m :~> (("_" :: P(m)) :-> (g(m)))))(n)
              }
            }
          }
        } ) (("_" :: Nat) :-> (Type: Typ[Term]))(p)(
                    A
                  )(l :-> ((eql(k)(l) ->: A) ->: A))
                }
              }
            }
          }
        }
```

### Generated code
```scala
lmbda("A" :: Type)(lmbda("p" :: "Nat" :: Type)(lmbda("q" :: "Nat" :: Type)(({
  val rxyz = NatInd.induc(lmbda("_" :: "Nat" :: Type)(Type))
  rxyz
})(({
  val rxyz = NatInd.induc(lmbda("_" :: "Nat" :: Type)(Type))
  rxyz
})("A" :: Type)(lambda("m" :: "Nat" :: Type)(lmbda("_" :: Type)(FuncTyp("A" :: Type, "A" :: Type))))("p" :: "Nat" :: Type))(lambda("m" :: "Nat" :: Type)(lmbda("_" :: Type)(({
  val rxyz = NatInd.induc(lmbda("_" :: "Nat" :: Type)(Type))
  rxyz
})("A" :: Type)(lambda("m" :: "Nat" :: Type)(lmbda("_" :: Type)(FuncTyp(FuncTyp(("eq" :: FuncTyp("Nat" :: Type, FuncTyp("Nat" :: Type, Type)))("m" :: "Nat" :: Type)("m" :: "Nat" :: Type), "A" :: Type), "A" :: Type))))("p" :: "Nat" :: Type))))("q" :: "Nat" :: Type))))
```


### Correct codegen

```scala
CodeGen()(natcases).get.toString
res46: String = """lambda("P" :: FuncTyp("Nat" :: Type, Type))(lambda("n" :: "Nat" :: Type)(lmbda("x" :: ("P" :: FuncTyp("Nat" :: Type, Type))("0" :: "Nat" :: Type))(lmbda("g" :: piDefn("n" :: "Nat" :: Type)(("P" :: FuncTyp("Nat" :: Type, Type))(("succ" :: FuncTyp("Nat" :: Type, "Nat" :: Type))("n" :: "Nat" :: Type))))(({
  val rxyz = NatInd.induc("P" :: FuncTyp("Nat" :: Type, Type))
  rxyz
})("x" :: ("P" :: FuncTyp("Nat" :: Type, Type))("0" :: "Nat" :: Type))(lambda("m" :: "Nat" :: Type)(lmbda("_" :: ("P" :: FuncTyp("Nat" :: Type, Type))("m" :: "Nat" :: Type))(("g" :: piDefn("n" :: "Nat" :: Type)(("P" :: FuncTyp("Nat" :: Type, Type))(("succ" :: FuncTyp("Nat" :: Type, "Nat" :: Type))("n" :: "Nat" :: Type))))("m" :: "Nat" :: Type))))("n" :: "Nat" :: Type)))))"""
```

### Making smaller

```scala
@ val nc = nc
nc: FuncLike[Term with Subs[Term], FuncLike[Term with Subs[Term], FuncLike[FuncLike[Term with Subs[Term], Term] with Subs[FuncLike[Term with Subs[Term], Term]], Term]]] = (n : Nat) ↦ (x : 𝒰 ) ↦ (g : ∏(n : Nat){ 𝒰  }) ↦ induc_{ Nat ; (_ : Nat) ↦ 𝒰  }(x)((m : Nat) ↦ (_ : 𝒰 ) ↦ g(m))(n)

@ val natnoct1 =
  A :~> {
    p :~> {
      q :~> {
        nc(q) {
          nc(p)(A)(
            l :-> (A ->: A))
        } {
          k :-> {
            nc(p)(
              A
            )(l :-> ((eql(k)(l) ->: A) ->: A))
          }
        }
      }
    }
  }
```

with partially generated code
```scala
val nind = NatInd.induc(lmbda("_" :: "Nat" :: Type)(Type))

val natoct00 =
A :~> {
  p :~> {
    q :~> {
      (
        lambda("n" :: "Nat" :: Type)(
          lmbda("x" :: Type)(
            lmbda("g" :: piDefn("n" :: "Nat" :: Type)(Type))(
              ({
              val rxyz = NatInd.induc(lmbda("_" :: "Nat" :: Type)(Type))
              rxyz
              }
            )("x" :: Type)(lambda("m" :: "Nat" :: Type)(lmbda("_" :: Type)(("g" :: piDefn("n" :: "Nat" :: Type)(Type))("m" :: "Nat" :: Type))))("n" :: "Nat" :: Type)))))(q) {
        (lambda("n" :: "Nat" :: Type)(lmbda("x" :: Type)(lmbda("g" :: piDefn("n" :: "Nat" :: Type)(Type))(({
  val rxyz = NatInd.induc(lmbda("_" :: "Nat" :: Type)(Type))
  rxyz
})("x" :: Type)(lambda("m" :: "Nat" :: Type)(lmbda("_" :: Type)(("g" :: piDefn("n" :: "Nat" :: Type)(Type))("m" :: "Nat" :: Type))))("n" :: "Nat" :: Type)))))(p)(A)(
          l :-> (A ->: A))
      } {
        k :-> {
          (lambda("n" :: "Nat" :: Type)(lmbda("x" :: Type)(lmbda("g" :: piDefn("n" :: "Nat" :: Type)(Type))(({
  val rxyz = NatInd.induc(lmbda("_" :: "Nat" :: Type)(Type))
  rxyz
})("x" :: Type)(lambda("m" :: "Nat" :: Type)(lmbda("_" :: Type)(("g" :: piDefn("n" :: "Nat" :: Type)(Type))("m" :: "Nat" :: Type))))("n" :: "Nat" :: Type)))))(p)(
            A
          )(l :-> ((eql(k)(l) ->: A) ->: A))
        }
      }
    }
  }
}

natoct00 == natoct
```

## Simplifying generated code:
```scala
val wrong0 = lmbda(A)(lmbda(p)(lmbda(q)(({
  val rxyz = NatInd.induc(lmbda("_" :: "Nat" :: Type)(Type))
  rxyz
})(({
  val rxyz = NatInd.induc(lmbda("_" :: "Nat" :: Type)(Type))
  rxyz
})(A)(lambda(m)(lmbda("_" :: Type)(FuncTyp(A, A))))(p))(lambda(m)(lmbda("_" :: Type)(({
  val rxyz = NatInd.induc(lmbda("_" :: "Nat" :: Type)(Type))
  rxyz
})(A)(lambda(m)(lmbda("_" :: Type)(FuncTyp(FuncTyp(("eq" :: FuncTyp("Nat" :: Type, FuncTyp("Nat" :: Type, Type)))(m)(m), A), A))))(p))))(q))))

val nind = NatInd.induc(lmbda("_" :: "Nat" :: Type)(Type))

val wrong00 = lmbda(A)(lmbda(p)(lmbda(q)(nind(nind(A)(lambda(m)(lmbda("_" :: Type)(FuncTyp(A, A))))(p))(lambda(m)(lmbda("_" :: Type)(nind(A)(lambda(m)(lmbda("_" :: Type)(FuncTyp(FuncTyp(eql(m)(m), A), A))))(p))))(q))))
```

## Back to older bug

The bug due to escaped variables was fixed by using a hash with names. However, the original bug seems to be incomplete substitution.
```scala
@ val t = parser.defnMap(name)
t: Term = ('n : Prop) ↦ ('o : (decidable('n) → 𝒰 )) ↦ ('p : decidable('n)) ↦ ('q : ∏('q : ('n → false)){ 'o(decidable.is_false('n)('q)) }) ↦ ('r : ∏(_ : 'n){ 'o(decidable.is_true('n)(_)) }) ↦ induc_{ decidable('n) ; ($clwnj : decidable('n)) ↦ 'o($clwnj) }('q)('r)('p)

val check = Try{lambda("'n_1881520718" :: Prop)(lambda("'o_67280897" :: FuncTyp(("decidable" :: FuncTyp(Prop, Type))("'n_1881520718" :: Prop), Type))(lambda("'p_609050670" :: ("decidable" :: FuncTyp(Prop, Type))("'n_1881520718" :: Prop))(lmbda("'q_1177881189" :: piDefn("'q_-1045036105" :: FuncTyp("'n_1881520718" :: Prop, "false" :: Prop))(("'o_67280897" :: FuncTyp(("decidable" :: FuncTyp(Prop, Type))("'n_1881520718" :: Prop), Type))(("decidable.is_false" :: piDefn("'d" :: Prop)(FuncTyp(FuncTyp("'d" :: Prop, "false" :: Prop), ("decidable" :: FuncTyp(Prop, Type))("'d" :: Prop))))("'n_1881520718" :: Prop)("'q_-1045036105" :: FuncTyp("'n_1881520718" :: Prop, "false" :: Prop)))))(lmbda("'r_1393788225" :: piDefn("_" :: "'n_1881520718" :: Prop)(("'o_67280897" :: FuncTyp(("decidable" :: FuncTyp(Prop, Type))("'n_1881520718" :: Prop), Type))(("decidable.is_true" :: piDefn("'c" :: Prop)(FuncTyp("'c" :: Prop, ("decidable" :: FuncTyp(Prop, Type))("'c" :: Prop))))("'n_1881520718" :: Prop)("_" :: "'n_1881520718" :: Prop))))(({
  val rxyz = decidableInd.value("'n_1881520718" :: Prop).induc(lmbda("$clwnj_-138920855" :: ("decidable" :: FuncTyp(Prop, Type))("'n_1881520718" :: Prop))(("'o_67280897" :: FuncTyp(("decidable" :: FuncTyp(Prop, Type))("'n_1881520718" :: Prop), Type))("$clwnj_-138920855" :: ("decidable" :: FuncTyp(Prop, Type))("'n_1881520718" :: Prop))))
  rxyz
})("'q_1177881189" :: piDefn("'q_-1045036105" :: FuncTyp("'n_1881520718" :: Prop, "false" :: Prop))(("'o_67280897" :: FuncTyp(("decidable" :: FuncTyp(Prop, Type))("'n_1881520718" :: Prop), Type))(("decidable.is_false" :: piDefn("'d" :: Prop)(FuncTyp(FuncTyp("'d" :: Prop, "false" :: Prop), ("decidable" :: FuncTyp(Prop, Type))("'d" :: Prop))))("'n_1881520718" :: Prop)("'q_-1045036105" :: FuncTyp("'n_1881520718" :: Prop, "false" :: Prop)))))("'r_1393788225" :: piDefn("_" :: "'n_1881520718" :: Prop)(("'o_67280897" :: FuncTyp(("decidable" :: FuncTyp(Prop, Type))("'n_1881520718" :: Prop), Type))(("decidable.is_true" :: piDefn("'c" :: Prop)(FuncTyp("'c" :: Prop, ("decidable" :: FuncTyp(Prop, Type))("'c" :: Prop))))("'n_1881520718" :: Prop)("_" :: "'n_1881520718" :: Prop))))("'p_609050670" :: ("decidable" :: FuncTyp(Prop, Type))("'n_1881520718" :: Prop)))))))}
check: Try[FuncLike[Typ[Term] with Subs[Typ[Term]], FuncLike[Func[Term, Typ[Term]] with Subs[Func[Term, Typ[Term]]], FuncLike[Term with Subs[Term], Func[FuncLike[Func[Term, Term] with Subs[Func[Term, Term]], Term] with Subs[FuncLike[Func[Term, Term] with Subs[Func[Term, Term]], Term]], Func[FuncLike[Term with Subs[Term], Term] with Subs[FuncLike[Term with Subs[Term], Term]], Term]]]]]] = Failure(
  provingground.HoTT$ApplnFailException: function func  cannot act on given term
)

@ val fail = check.failed.get.asInstanceOf[ApplnFailException]
fail: ApplnFailException = provingground.HoTT$ApplnFailException: function func  cannot act on given term

@ fail.domOpt.get
res55: Typ[u] = ∏($fgkqz : ('n_1881520718 → false)){ 'o_67280897((decidable.is_false) ($cwtyw)($fgkqz)) }

@ fail.argType
res56: Typ[U] = ∏('q_-1045036105 : ('n_1881520718 → false)){ 'o_67280897(decidable.is_false('n_1881520718)('q_-1045036105)) }

@ fail.func
res58: Term = (InducDataSym((decidable.is_false) ($cwtyw)) : ∏($fgkqz : ('n_1881520718 → false)){ 'o_67280897((decidable.is_false) ($cwtyw)($fgkqz)) }) ↦ (InducDataSym((decidable.is_true) ($cwtyw)) : ∏(_ : 'n_1881520718){ 'o_67280897((decidable.is_true) ($cwtyw)(_)) }) ↦ induc_{ decidable('n_1881520718) ; ($clwnj_-138920855 : decidable('n_1881520718)) ↦ 'o_67280897($clwnj_-138920855) }(InducDataSym((decidable.is_false) ($cwtyw)))(InducDataSym((decidable.is_true) ($cwtyw)))
```

The escaped variable is one internal to the inductive type : it does not figure in the code, but does figure in the `InducDataSym`

### Minimizing this bug:

```scala
@ val t5 = value(value(value(value(value(t)))))
t5: Term = induc_{ decidable('n) ; ($clwnj : decidable('n)) ↦ 'o($clwnj) }('q)('r)('p)

@
val check5= Try{
({
  val rxyz = decidableInd.value("'n_1881520718" :: Prop).induc(lmbda("$clwnj_-138920855" :: ("decidable" :: FuncTyp(Prop, Type))("'n_1881520718" :: Prop))(("'o_67280897" :: FuncTyp(("decidable" :: FuncTyp(Prop, Type))("'n_1881520718" :: Prop), Type))("$clwnj_-138920855" :: ("decidable" :: FuncTyp(Prop, Type))("'n_1881520718" :: Prop))))
  rxyz
})("'q_1177881189" :: piDefn("'q_-1045036105" :: FuncTyp("'n_1881520718" :: Prop, "false" :: Prop))(("'o_67280897" :: FuncTyp(("decidable" :: FuncTyp(Prop, Type))("'n_1881520718" :: Prop), Type))(("decidable.is_false" :: piDefn("'d" :: Prop)(FuncTyp(FuncTyp("'d" :: Prop, "false" :: Prop), ("decidable" :: FuncTyp(Prop, Type))("'d" :: Prop))))("'n_1881520718" :: Prop)("'q_-1045036105" :: FuncTyp("'n_1881520718" :: Prop, "false" :: Prop)))))("'r_1393788225" :: piDefn("_" :: "'n_1881520718" :: Prop)(("'o_67280897" :: FuncTyp(("decidable" :: FuncTyp(Prop, Type))("'n_1881520718" :: Prop), Type))(("decidable.is_true" :: piDefn("'c" :: Prop)(FuncTyp("'c" :: Prop, ("decidable" :: FuncTyp(Prop, Type))("'c" :: Prop))))("'n_1881520718" :: Prop)("_" :: "'n_1881520718" :: Prop))))("'p_609050670" :: ("decidable" :: FuncTyp(Prop, Type))("'n_1881520718" :: Prop))
}
check5: Try[Term] = Failure(
  provingground.HoTT$ApplnFailException: function func  cannot act on given term
)

 val fail5 = check5.failed.get.asInstanceOf[ApplnFailException]
fail5: ApplnFailException = provingground.HoTT$ApplnFailException: function func  cannot act on given term

@ fail5.domOpt.get
res79: Typ[u] = ∏($fjaak : ('n_1881520718 → false)){ 'o_67280897((decidable.is_false) ($cwtyw)($fjaak)) }

@ fail5.argType
res80: Typ[U] = ∏('q_-1045036105 : ('n_1881520718 → false)){ 'o_67280897(decidable.is_false('n_1881520718)('q_-1045036105)) }

@ fail5.func
res81: Term = (InducDataSym((decidable.is_false) ($cwtyw)) : ∏($fjaak : ('n_1881520718 → false)){ 'o_67280897((decidable.is_false) ($cwtyw)($fjaak)) }) ↦ (InducDataSym((decidable.is_true) ($cwtyw)) : ∏(_ : 'n_1881520718){ 'o_67280897((decidable.is_true) ($cwtyw)(_)) }) ↦ induc_{ decidable('n_1881520718) ; ($clwnj_-138920855 : decidable('n_1881520718)) ↦ 'o_67280897($clwnj_-138920855) }(InducDataSym((decidable.is_false) ($cwtyw)))(InducDataSym((decidable.is_true) ($cwtyw)))
```


```scala
@ cg.consSeq(decidableInd.value("'n_1881520718" :: Prop)).get.toString
res99: String = "ConstructorSeqTL(ConstructorSeqDom.Cons(HoTT.Name("(decidable.is_false) ($cwtyw)"), ConstructorShape.CnstFuncConsShape(FuncTyp("'n_1881520718" :: Prop, "false" :: Prop), ConstructorShape.IdShape.byTyp(("decidable" :: FuncTyp(Prop, Type))("'n_1881520718" :: Prop))), ConstructorSeqDom.Cons(HoTT.Name("(decidable.is_true) ($cwtyw)"), ConstructorShape.CnstFuncConsShape("'n_1881520718" :: Prop, ConstructorShape.IdShape.byTyp(("decidable" :: FuncTyp(Prop, Type))("'n_1881520718" :: Prop))), ConstructorSeqDom.Empty.byTyp(("decidable" :: FuncTyp(Prop, Type))("'n_1881520718" :: Prop)))), ("decidable" :: FuncTyp(Prop, Type))("'n_1881520718" :: Prop))"
```
