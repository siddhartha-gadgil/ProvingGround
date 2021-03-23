package provingground.scalahott

import provingground._
import HoTT._
import spire.algebra._
import spire.implicits._
import org.scalatest._, flatspec._
import IntRing._
import spire.math.SafeLong

object IntRingSpec {

  val x: ScalaTerm[SafeLong] = "x " :: IntRing.LocalTyp

  val y: ScalaTerm[SafeLong] = "y" :: IntRing.LocalTyp

  val z: ScalaTerm[SafeLong] = "z" :: IntRing.LocalTyp

  val nr: CRing[IntRing.LocalTerm] = implicitly[CRing[IntRing.LocalTerm]]

  nr.plus(x, y)

  val xPy: ScalaTerm[SafeLong] = x + y

  lazy val xPyPz: ScalaTerm[SafeLong] = x + (y + z)

  lazy val xPyPzl: ScalaTerm[SafeLong] = (x + y) + z
}

class IntRingSpec extends flatspec.AnyFlatSpec {
  import IntRingSpec._

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
    val n = "n" :: IntTyp
    val m = "m" :: IntTyp
    val k = "k" :: IntTyp

    assert(n + m == m + n)
    assert((n + m) + k == n + (m + k))

    assert { (n + n) + m == (n + m) + n }

    assert { n * m == m * n }

    assert(n * (m + k) == n * m + n * k)

    assert(1 + (n + 2) == n + 3)

    val fn = lmbda(n)(n * n)

    assert(fn(3) == (9: Ints))

  }
}
