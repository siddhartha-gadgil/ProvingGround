package provingground.scalahott

import provingground._

import HoTT._

import SymbolicCRing._

import SymbolicCRing._

// import spire.math._

import spire.algebra._

import spire.implicits._

import spire.syntax._

import org.scalatest.FlatSpec

import NatRing._

object NatRingSpec {

  val x = "x " :: NatRing.LocalTyp

  val y = "y" :: NatRing.LocalTyp

  val z = "z" :: NatRing.LocalTyp

  val nr = implicitly[CRing[NatRing.LocalTerm]]

  nr.plus(x, y)

  val xPy = x + y

  lazy val xPyPz = x + (y + z)

  lazy val xPyPzl = (x + y) + z
}

class NatRingSpec extends FlatSpec {
  import NatRingSpec._

  "Addition" should "be commutative and associative" in {
    assert(x + y == y + x)

    assert((x + y) + z == x + (y + z))

    assert((x + 3) + y == (x + y) + 3)
  }

  it should "group terms correctly" in {
    assert(x + (y + x) == (x + x) + y)

    assert((x + x) + y == (x + y) + x)

    assert((x + y) - x == y)

    assert((-x) + Literal(0) == -x)
    assert(2 - x == 2 - x + Literal(0))
  }

  "Symbolic Algebra" should "satisfy the tut tests" in {
    val n = "n" :: NatTyp
    val m = "m" :: NatTyp
    val k = "k" :: NatTyp

    assert(n + m == m + n)
    assert((n + m) + k == n + (m + k))

    assert { (n + n) + m == (n + m) + n }

    assert { n * m == m * n }

    assert(n * (m + k) == n * m + n * k)

    assert(1 + (n + 2) == n + 3)

    val fn = lmbda(n)(n * n)

    assert(fn(3) == (9: Nat))

    val mm        = lmbda(n)(prod(n + 1))
    val factorial = Rec(1: Nat, mm)

    assert(factorial(5) == (120: Nat))

    assert { factorial(k + 2) == factorial(k) * (k + 2) * (k + 1) }
  }
}
