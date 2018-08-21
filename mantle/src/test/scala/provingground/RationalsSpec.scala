package provingground.scalahott

import provingground._

import HoTT._

import spire.implicits._

import spire.math._

import org.scalatest.FlatSpec

import QField.{LocalTyp => Q, Literal => rat, _}

class RationalsSpec extends FlatSpec {
  val x = "x" :: Q

  val y = "y" :: Q

  "Rational division" should "act on literals" in {
    assert(rat(2) / rat(3) == rat(Rational(2, 3)))
  }

  it should "cancel correctly" in {
    assert(x / x == rat(1))

    assert(((x + 1) / (x * x)) == (1 / x + 1 / (x * x)))
  }

  it should "substitute and simplify" in {
    val fn = x :-> (x + 1) / (x + 2)

    assert(fn(rat(2)) == rat(3) / rat(4))

    assert(fn(y - 2) == rat(1) - rat(1) / y)
  }
}
