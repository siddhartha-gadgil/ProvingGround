import provingground._
import FiniteDistributionLearner._
import Collections._
import org.scalatest.FlatSpec
import FiniteDistribution._

import scala.util._

/**
 * @author gadgil
 */
class DiffFnFromMoveSpec extends FlatSpec{
  behavior of "the differentiable function induced by a move" 
  
  it should "map uniform distributions forward correctly" in {
    val fn = moveFn ((x: Double) => Some(2 * x))
      
    val at12 = unif(1.0, 2.0)
    
    assert(fn(at12) == unif(2.0, 4.0))
  }
  
  it should "map non-uniform distributions correctly" in {
    val fn = moveFn ((x: Double) => Some(2 * x))
    
    val d = FiniteDistribution((1 to 10).toSet map ((i: Int) => Weighted(i.toDouble, i.toDouble))).flatten
    
    val dd = FiniteDistribution((1 to 10).toSet map ((i : Int) => Weighted(i.toDouble * 2.0, i.toDouble))).flatten
    
    assert(fn(d) == dd)
  }
  
  it should "fold together elements and flatten" in {
    val fn = moveFn ((x: Double) => Some(x * x))
    
    assert(fn(unif(1.0, 2.0, -1.0, -2.0)) == unif(1.0, 4.0))
  }
  
  it should "ignore elements that are not acted on" in {
    def f(x: Double) = if (x >= 0) Some(math.sqrt(x)) else None
    
    val fn = moveFn(f)
    
    assert(fn(unif(1.0, 4.0, -1.0, -3.0)) == FiniteDistribution(Set(Weighted(1.0, 0.25), Weighted(2.0, 0.25))))
  }
}