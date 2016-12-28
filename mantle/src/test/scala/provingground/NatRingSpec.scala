package provingground

import HoTT._

import SymbolicCRing._

import SymbolicCRig._

import spire.math._

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
  }
}
