import library._, Nats._

val n = "n" :: Nat
val m = "m" :: Nat
val P = "P" :: Nat ->: Type
val x = "x" :: P(zero)
val g = "g" :: (n ~>: P(succ(n)))

val indNP = NatInd.induc(P)
val natcases =
  P :~> {
    n :~> {
      x :~> {
        g :~> {
          (indNP(x)(m :~> (("_" :: P(m)  ) :-> (g(m)))))(n)}
        }
      }
    }

val A = "A" :: Type
val eql = "eq" :: Nat ->: Nat ->: Type

val k = "k" :: Nat
val l = "l" :: Nat

val p = "p" :: Nat
val q = "q" :: Nat

val natnoct =
  A :~> {
    p :~> {
      q :~> {
        natcases(("_" :: Nat) :-> (Type: Typ[Term])  )(q){
          natcases(("_" :: Nat) :-> (Type : Typ[Term]) )(p)(
            A)(
              l :-> (A ->: A))
        }{k :->
          {
          natcases(("_" :: Nat) :-> (Type : Typ[Term]) )(p)(
             A
           )(
             l :->  ((eql(k)(l) ->: A) ->: A)  )
          }
        }
        }
      }
    }

val bug = natnoct(succ(p))(succ(q))
