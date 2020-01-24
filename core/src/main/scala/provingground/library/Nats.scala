package provingground.library

import provingground._

import HoTT._

import induction.TLImplicits._

import shapeless._

object Nats {
  val Nat                  = "Nat" :: Type
  val NatInd               = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
  val zero :: succ :: HNil = NatInd.intros

  val N = LazyList.iterate(zero)(succ)

  val n = "n" :: Nat
  val m = "m" :: Nat

  val recNN  = NatInd.rec(Nat)
  val double = recNN(zero)(m :-> (n :-> (succ(succ(n)))))

  val recNNN = NatInd.rec(Nat ->: Nat)
  val addn   = "add(n)" :: Nat ->: Nat
  val add    = recNNN(m :-> m)(n :-> (addn :-> (m :-> (succ(addn(m))))))

  lazy val sumTo = recNN(zero)(m :-> (n :-> (add(succ(m))(n))))

  val ackm = "ack(m)" :: Nat ->: Nat

  val ackmp1n = "ack(m+1)(n)" :: Nat

  val ack = recNNN(succ)(
    m :-> (ackm :-> recNN(ackm(N(1)))(n :-> (ackmp1n :-> (ackm(ackmp1n))))))
}
