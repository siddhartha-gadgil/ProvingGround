package provingground

// import Collections._
import org.scalatest._, flatspec._
import FiniteDistribution._
import LinearStructure._
import Weighted._

trait FiniteDistributionBehaviours { this: AnyFlatSpec =>

  def intFiniteDistribution(
      finDist: Iterable[Weighted[Int]] => FiniteDistribution[Int]) = {
    val x = finDist(
      weights(-1 -> 0.2, 1 -> 0.2, 3 -> 0.25, 4 -> 0.25, 7 -> 0.1))

    val y = finDist(weights(0 -> 0.3, 1 -> 0.4, 5 -> 0.3))

    val square = (x: Int) => x * x

    val epsilon = 0.001

    import math._

    def eqls(a: Double, b: Double) = assert(abs(a - b) < epsilon)

    it should "return mass on apply" in {

      assert(x(1) == 0.2)

      assert(x(7) == 0.1)

      assert(x(0) == 0.0)
    }

    it should "have correct vector operations" in {

      val mean = (x * 0.5) ++ (y * 0.5)

      eqls(mean(1), 0.3)

      assert(mean(2) == 0)

      eqls(mean(4), 0.125)

      assert(mean(5) > 0.149)

      assert(mean(5) < 0.151)
    }

    it should "map correctly under (not necessarily injective) functions" in {
      val fd = FiniteDistribution.unif(-1, 1, 2, 3)

      val image = x map (square)

      eqls(image(1), 0.4)

      assert(image(2) == 0)

      eqls(image(4), 0)

      eqls(image(9), 0.25)
    }

    it should "map correctly under optional functions" in {

      val image = x mapOpt ((x: Int) => if (x < 0) None else Some(x % 2))

      assert(image(1) == 0.55)

      assert(image(0) == 0.25)
    }

    it should "filter correctly" in {
      val fd = x filter ((x: Int) => x > 1)

      assert(fd(1) == 0)

      assert(fd(3) == 0.25)
    }

    it should "correctly add elements even if in support" in {
      val fd = x .+ (3, 0.25)

      assert(fd(1) == 0.2)

      assert(fd(3) == 0.5)
    }

    it should "correctly normalize with pruning" in {
      val fd = x.normalized(0.15)

      eqls(fd(1), 0.2 / 0.9)

      eqls(fd(3), 0.25 / 0.9)

      assert(fd(7) == 0)
    }

    it should "have empty distribution as identity" in {

      val addL: FiniteDistribution[Int] = FiniteDistribution.empty[Int] ++ x

      val empty: FiniteDistribution[Int] = FiniteDistribution.empty[Int]

      val addR: FiniteDistribution[Int] = x ++ empty

      for (j <- -5 to 5) assert(addL(j) == x(j))

      for (j <- -5 to 5) assert(addR(j) == x(j))
    }
  }
}
