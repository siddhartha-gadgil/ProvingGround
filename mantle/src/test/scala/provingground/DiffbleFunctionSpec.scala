package provingground

import provingground._
import Collections._; import FiniteDistribution._; import provingground._
import org.scalatest.FlatSpec
import FiniteDistribution._
import AdjDiffbleFunction._
import LinearStructure._

class AdjDiffbleFunctionSpec extends FlatSpec {
  val double =
    AdjDiffbleFunction((x: Double) => 2 * x)((x: Double) => (y: Double) => 2 * y)

  val square =
    AdjDiffbleFunction((x: Double) => x * x)((x: Double) => (y: Double) => x * y)

  "A Differentiable function" should "evaluate by apply" in {
    val fn = AdjDiffbleFunction((x: Double) => 2 * x)((x: Double) =>
      (y: Double) => 2 * y)

    assert(double.func(2) == 4)

    assert(square.func(3) == 9)
  }

  it should "apply adjDerient" in {
    val fn = AdjDiffbleFunction((x: Double) => x * x)((x: Double) =>
      (y: Double) => x * y)

    assert(square.adjDer(2)(3) == 6)

    assert(double.adjDer(3)(2) == 4)
  }

  it should "have expected derivatives and values for inclusion" in {
    val fn1 = Incl1[Double, Double]

    val fn2 = Incl2[Double, Double]

    assert(fn1.func(1.5) == (1.5, 0))
    assert(fn1.adjDer(1.5)(2.5, 3.5) == 2.5)

    assert(fn2.func(1.5) == (0, 1.5))
    assert(fn2.adjDer(1.5)(2.5, 3.5) == 3.5)
  }

  it should "have expected derivatives and values for projection" in {
    val fn1 = Proj1[Double, Double]

    val fn2 = Proj2[Double, Double]

    assert(fn1.func((1.5, 2.5)) == 1.5)
    assert(fn1.adjDer(1.5, 2.5)(3) == (3, 0))

    assert(fn2.func((1.5, 2.5)) == 2.5)
    assert(fn2.adjDer(1.5, 2.5)(3) == (0, 3))
  }
}
