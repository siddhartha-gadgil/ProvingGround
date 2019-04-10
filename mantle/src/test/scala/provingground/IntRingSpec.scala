package provingground.scalahott

import provingground._
import HoTT._
import spire.algebra._
import spire.implicits._
import org.scalatest.FlatSpec
import IntRing._
import spire.math.SafeLong

object IntRingSpec {

  val x: RepTerm[SafeLong] = "x " :: IntRing.LocalTyp

  val y: RepTerm[SafeLong] = "y" :: IntRing.LocalTyp

  val z: RepTerm[SafeLong] = "z" :: IntRing.LocalTyp

  val nr: CRing[IntRing.LocalTerm] = implicitly[CRing[IntRing.LocalTerm]]

  nr.plus(x, y)

  val xPy: RepTerm[SafeLong] = x + y

  lazy val xPyPz: RepTerm[SafeLong] = x + (y + z)

  lazy val xPyPzl: RepTerm[SafeLong] = (x + y) + z
}

class IntRingSpec extends FlatSpec {
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

//    val fn = lmbda(n)(n * n)
//
//    assert(fn(3) == (9: SafeLong))

  }
}
