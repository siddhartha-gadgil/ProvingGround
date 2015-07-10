import provingground._
import Collections._ ; import FiniteDistribution._; import provingground._
import org.scalatest.FlatSpec
import FiniteDistribution._

class FiniteDistributionSpec extends FlatSpec{
  "Finite Distributions" should "return mass on apply" in {
    val fd = FiniteDistribution.fromWts(1 -> 0.5, 2 -> 0.3, 1 -> 0.2)

    assert(fd(1) == 0.7)
  }

  it should "have correct vector operations" in {
    val first = FiniteDistribution.unif(1, 2)

    val second = FiniteDistribution.fromWts(1 -> 0.5, 3 -> 0.25, 4 -> 0.25)
    
    val mean = (first * 0.5) ++ (second * 0.5)

    assert(mean(1) == 0.5)
    
    assert(mean(2) == 0.25)
    
    assert(mean(4) == 0.125)
    
  }

  it should "map correctly under (not necessarily injective) functions" in {
    val fd = FiniteDistribution.unif(-1, 1, 2, 3)

    val image = fd map ((x: Int) => x * x)

    assert(image(1) == 0.5)
    
    assert(image(2) == 0)

    assert(image(4) == 0.25)
    
    assert(image(9) == 0.25)
    
  }

  it should "map correctly under optional functions" in {
    val fd = FiniteDistribution.unif(-1, 1, 2, 3)

    val image = fd mapOpt ((x: Int) =>  if (x <0) None else Some(x % 2))

    assert(image(1) == 0.5)

    assert(image(0) == 0.25)
  }

  it should "filter correctly" in {
    val fd = FiniteDistribution.unif(1, 2, 3, 4) filter ((x: Int) => x >1)

    assert(fd(1) == 0)

    assert( fd(2) == 0.25)
  }

  it should "correctly add elements even if in support" in {
    val fd = FiniteDistribution.unif(1, 2, 3, 4) + (2, 0.25)

    assert(fd(2) == 0.5)

    assert(fd(3) == 0.25)
  }

  it should "correctly normalize with pruning" in {
    val fd = FiniteDistribution.fromWts(1 -> 0.4, 2 -> 0.2, 3 -> 0.2, 7 -> 0.001).normalized(0.1)

    assert(fd(1) == 0.5)

    assert(fd(3) == 0.25)

    assert(fd(7) == 0)
  }
}
