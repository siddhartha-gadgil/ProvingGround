package provingground

// import Collections._
import org.scalatest.FlatSpec
import FiniteDistribution._
import LinearStructure._

class FiniteDistributionVecSpec
    extends FlatSpec
    with FiniteDistributionBehaviours {
  def finDist =
    (pmf: Traversable[Weighted[Int]]) => FiniteDistribution(pmf.toVector)

  "Finite Distributions implemented as Vectors" should behave like intFiniteDistribution(
    finDist)
}
