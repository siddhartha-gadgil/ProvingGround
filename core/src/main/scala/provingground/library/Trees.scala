package provingground.library

import provingground._

import HoTT._

import induction.TLImplicits._

import shapeless._

object Trees {
  val T                    = "Tree" :: Type
  val TInd                 = ("leaf" ::: T) |: ("node" ::: T -->>: T -->>: T) =: T
  val leaf :: node :: HNil = TInd.intros

  import Nats._
  val recTN = TInd.rec(Nat)

  val t1 = "t1" :: T
  val t2 = "t2" :: T

  val vertices = recTN(N(1))(t1 :-> (m :-> (t2 :-> (n :-> (succ(add(n)(m)))))))
}

object BinTrees {
  import library.Bools._
  val BT                   = "BinTree" :: Type
  val BTInd                = ("leaf" ::: BT) |: ("node" ::: (Bool -|>: BT) -->>: BT) =: BT
  val leaf :: node :: HNil = BTInd.intros

  import Nats._

  val recBTN = BTInd.rec(Nat)
  val f      = "f" :: Bool ->: BT
  val g      = "g" :: Bool ->: Nat
  val leaves = recBTN(N(1))(f :-> (g :-> (add(g(ff))(g(tt)))))
}
