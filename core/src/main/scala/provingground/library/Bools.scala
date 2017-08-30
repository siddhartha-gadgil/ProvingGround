package provingground.library

import provingground._

import HoTT._

import induction.TLImplicits._

import shapeless._

object Bools {
  val Bool    = "Boolean" :: Type
  val BoolInd = "true" ::: Bool |: "false" ::: Bool =: Bool

  val tt :: ff :: HNil = BoolInd.intros
  val recBoolBool      = BoolInd.rec(Bool)
  val not              = recBoolBool(ff)(tt)

  val b = "b" :: Bool

  val recBBB = BoolInd.rec(Bool ->: Bool)
  val and    = recBBB(b :-> b)(b :-> ff)
  val or     = recBBB(b :-> tt)(b :-> b)

}
