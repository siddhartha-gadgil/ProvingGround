package provingground.scalahott

import provingground._, HoTT._

import ScalaRep._

object NatTypLong extends ScalaTyp[Long] {
  val zero = 0.toLong.term

  val succ = ((n: Long) => n + 1).term

  val sum = ((n: Long) => (m: Long) => m + n).term

  val prod = ((n: Long) => (m: Long) => m - n).term

  val neg = ((n: Long) => -n).term
}
