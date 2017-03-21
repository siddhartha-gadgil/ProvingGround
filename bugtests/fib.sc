import provingground._
import HoTT._

import TLImplicits._
import shapeless._
val Nat = "Nat" :: Type
val NatInd = ("0" ::: Nat) |: ("succ" ::: Nat -->>: Nat) =: Nat
val zero :: succ :: HNil = NatInd.intros
val one = succ(zero)
val two = succ(one)
val three = succ(two)
val four = succ(three)
val five = succ(four)
val six = succ(five)
val n = "n" :: Nat
val m = "m" :: Nat
val m1 = "m1" :: Nat
val m2 = "m2" :: Nat
val recNNN = NatInd.rec(Nat ->: Nat)
val addn = "add(n)" :: Nat ->: Nat
val add = recNNN(m :-> m)(n :-> (addn :-> (m :-> succ(addn(m)) )))
val recNNNN = NatInd.rec(Nat ->: Nat ->: Nat)
val fibn = "fib_aux(n,_,_)" :: Nat ->: Nat ->: Nat
val fib_aux = recNNNN(m1 :-> (m2 :-> m1))(n :-> (fibn :-> (m1 :-> (m2 :-> fibn(m2)(add(m1)(m2)) ))))
val fib = n :-> fib_aux(n)(zero)(one)
val stepData = (n :-> (fibn :-> (m1 :-> (m2 :-> fibn(m2)(add(m1)(m2)) ))))
