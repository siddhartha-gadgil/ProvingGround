package provingground.scalahott

import provingground._

import HoTT._

// import spire.implicits._

import FreeGroup._

import cats.implicits._

// import andrewscurtis.FreeGroups._

import org.scalatest.FlatSpec

object FreeGroupSpec {
  val g = "g" :: FreeGroup

  val h = "h" :: FreeGroup

  val k = "k" :: FreeGroup

  val l = word("aab!aba")
}

class FreeGroupSpec extends FlatSpec {
  import FreeGroupSpec._

  "Free group operations" should "operate on literals" in {
    assert { (word("ab") |+| word("b!a")) == word("aa") }
    assert { (word("ab!a").inverse) == word("a!ba!") }
  }

  they should "be associative" in {
    assert { ((g |+| h) |+| k) == (g |+| (h |+| k)) }

    assert { ((l |+| h) |+| k) == (l |+| (h |+| k)) }

    assert { ((g |+| l) |+| k) == (g |+| (l |+| k)) }

    assert { ((g |+| h) |+| l) == (g |+| (h |+| l)) }

    assert { ((l |+| h) |+| word("ab")) == (l |+| (h |+| word("ab"))) }
  }

  they should "satisfy identity axioms" in {
    assert((e |+| g) == g)

    assert((g |+| e) == g)
  }

  they should "cancel recursively" in {
    assert((g.inverse |+| l.inverse |+| h |+| h.inverse |+| l |+| g) == e)
  }

  they should "have corect inverse" in {
    assert((g |+| h).inverse == (h.inverse |+| g.inverse))

    assert((g |+| l).inverse == (l.inverse |+| g.inverse))

    assert((l |+| h).inverse == (h.inverse |+| l.inverse))
  }

  "Power operation" should "simplify" in {
    import NatRing.{NatTyp, succ}
    val n = "n" :: NatTyp
    assert((power(g)(n) |+| g) == (power(g)(succ(n))))
  }

  "Theorems for powers" should "have valid proofs" in {
    import NatRing.{NatTyp}
    val n = "n" :: NatTyp

    import Theorems._

    assert(ConjPower.pf.typ == ConjPower.thm)

    assert(PowerDistributive.pf.typ == PowerDistributive.thm)
  }

}
