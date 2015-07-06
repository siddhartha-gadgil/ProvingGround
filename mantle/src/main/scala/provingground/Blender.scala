package provingground

import scala.concurrent._
import ExecutionContext.Implicits.global
import Collections._
//import provingground.FiniteDistributionLearner.IterDynSys

/**
 * @author gadgil
 * Runs dynamilac system with futures, blending results.
 */
class Blender[A](dyn: A => A)(implicit ls : LinearStructure[A]) {

  def asyncDynLoop(a : A, n: Int) = Future(IterateDyn(a, dyn, n))

  def futDynLoop(fut: Future[A], n: Int) = fut flatMap ((a: A) => asyncDynLoop(a, n))



  def iterFut(init: Future[A], copies: Int, loops: Int) = {
    val results = (1 to copies) map ((i) => futDynLoop(init, loops))
    val termfut = Future.sequence(results)
    termfut map ((terms) => vAverage(terms))
  }

  def iter(init: A, copies: Int, loops: Int) = iterFut(Future.successful(init), copies, loops)
}
