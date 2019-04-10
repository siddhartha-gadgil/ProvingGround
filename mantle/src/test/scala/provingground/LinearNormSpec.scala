package provingground

import scalahott._, andrewscurtis.FreeGroups._
import LinNormBound._

import org.scalatest.FlatSpec

import NatRing.{Literal => nat, leq => leqNat, _}, QField.{Literal => rat, _},
FreeGroup.{Literal => elem, _}

class LinearNormSpec extends FlatSpec {
  "Homogeneous length functions" should "satisy expected bounds" in {
    val pf = (1 *: ((1 *: (Gen(2))) ++ ((-2) *: (Gen(1) ++ Gen(1))))) ++ Gen(-2)

    val w = Word("aaba!b!")

    assert(pf.word == w ++ w)

    val lem = PowerBound(w, 2, pf)

    assert(lem.word == w &&
             lem.bound == 2 &&
             lem.theorem == leq(l(elem(w)))(rat(2)) &&
             lem.proof.typ == lem.theorem,
           "Incorrect theorem proved")
  }
}
