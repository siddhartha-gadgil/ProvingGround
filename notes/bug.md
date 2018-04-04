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
