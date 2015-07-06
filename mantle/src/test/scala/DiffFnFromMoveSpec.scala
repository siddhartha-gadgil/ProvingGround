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
    val fn = MoveFn ((x: Double) => Some(2 * x))
      
    val at12 = unif(1.0, 2.0)
    
    assert(fn.func(at12) == unif(2.0, 4.0))
  }
  
  it should "map non-uniform distributions correctly" in {
    val fn = MoveFn ((x: Double) => Some(2 * x))
    
    val d = FiniteDistribution((1 to 10).toSet map ((i: Int) => Weighted(i.toDouble, i.toDouble))).flatten
    
    val dd = FiniteDistribution((1 to 10).toSet map ((i : Int) => Weighted(i.toDouble * 2.0, i.toDouble))).flatten
    
    assert(fn.func(d) == dd)
  }
  
  it should "fold together elements and flatten" in {
    val fn = MoveFn ((x: Double) => Some(x * x))
    
    assert(fn.func(unif(1.0, 2.0, -1.0, -2.0)) == unif(1.0, 4.0))
  }
  
  it should "ignore elements that are not acted on" in {
    def f(x: Double) = if (x >= 0) Some(math.sqrt(x)) else None
    
    val fn = MoveFn(f)
    
    assert(fn.func(unif(1.0, 4.0, -1.0, -3.0)) == FiniteDistribution(1.0 -> 0.25, 2.0 -> 0.25))
  }
  
  it should "have expected gradient" in {
    val fn = MoveFn ((x: Int) => Some(2 * x))
    
    val p = FiniteDistribution.unif(1, 3, 4)
    
    val w = FiniteDistribution(2 -> 0.5, 6 -> 0.25, 10 -> 0.25)
    
    assert(fn.grad(p)(w) == FiniteDistribution(1 -> 0.5, 3 -> 0.25))
  }
}