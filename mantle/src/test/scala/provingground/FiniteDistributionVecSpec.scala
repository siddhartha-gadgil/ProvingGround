package provingground

// import Collections._
import org.scalatest._, flatspec._
import FiniteDistribution._
import LinearStructure._

class FiniteDistributionVecSpec
    extends AnyFlatSpec
    with FiniteDistributionBehaviours {
  def finDist =
    (pmf: Iterable[Weighted[Int]]) => FiniteDistribution(pmf.toVector)

  "Finite Distributions implemented as Vectors" should behave like intFiniteDistribution(
    finDist)
}
