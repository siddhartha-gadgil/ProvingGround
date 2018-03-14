package provingground.library

import provingground._

import HoTT._

import induction._, implicits._

import shapeless._

object Lists {
  val A = "A" :: Type

  val ListA               = "List(A)" :: Type
  val ListAInd            = ("nil" ::: ListA) |: ("cons" ::: A ->>: ListA -->>: ListA) =: ListA
  val nil :: cons :: HNil = ListAInd.intros

  import Nats._

  val ListInd = A ~->: ListAInd

  val recLN = ListAInd.rec(Nat)
  val a     = "a" :: A
  val l     = "l" :: ListA
  val size  = recLN(zero)(a :-> (l :-> (n :-> (succ(n)))))
}
