package provingground

// import Collections._
import org.scalatest.FlatSpec
import FiniteDistribution._
import LinearStructure._

import scala.collection.parallel.immutable.ParVector

class FiniteDistributionParVecSpec
    extends FlatSpec
    with FiniteDistributionBehaviours {
  // def finDist = (pmf: Traversable[Weighted[Int]]) => FiniteDistributionParVec(pmf.toVector.par)
  //
  // "Finite Distributions implemented as Parallel Vectors" should behave like intFiniteDistribution(finDist)
}
