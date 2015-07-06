import provingground._
import Collections._
import org.scalatest.FlatSpec
import FiniteDistribution._
import DiffbleFunction._

class DiffbleFunctionSpec extends FlatSpec{
  "A Differentiable function" should "evaluate by apply" in {
    val fn = DiffbleFunction((x: Double) => 2 * x)((x: Double) => (y: Double) => 2 *y)

    assert(fn.func(2) == 4)
  }

  it should "apply gradient" in {
    val fn = DiffbleFunction((x: Double) => x * x)((x: Double) => (y: Double) => x * y)

    assert(fn.grad(2)(3) == 6)
  }

  it should "have expected derivatives and values for inclusion" in {
    val fn1 = Incl1[Double, Double]

    val fn2 = Incl2[Double, Double]

    assert(fn1.func(1.5) == (1.5, 0))
    assert(fn1.grad(1.5)(2.5, 3.5) == 2.5)

    assert(fn2.func(1.5) == (0, 1.5))
    assert(fn2.grad(1.5)(2.5, 3.5) == 3.5)
  }

  it should "have expected derivatives and values for projection" in {
    val fn1 = Proj1[Double, Double]

    val fn2 = Proj2[Double, Double]

    assert(fn1.func((1.5, 2.5)) == 1.5)
    assert(fn1.grad(1.5, 2.5)(3) == (3, 0))

    assert(fn2.func((1.5, 2.5)) == 2.5)
    assert(fn2.grad(1.5, 2.5)(3) == (0, 3))
  }
}
