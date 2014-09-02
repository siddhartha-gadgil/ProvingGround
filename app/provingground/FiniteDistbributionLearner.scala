package provingground

import LearningSystem._
import Collections._

object FiniteDistbributionLearner {
	type FD[V] = FiniteDistribution[V] 
	
	type DF[X, Y] = DiffbleFunction[X, Y]
	
	/**
	 * A combined differentiable function from ones on FD[V] labeled by elements of M.
	 * This has domain arrays with labels in M (temporarily just maps) as well as an element of FD[V].
	 * It maps to just FD[V], but this can be further composed.
	 */
	def combined[M, V](terms: Map[M, DF[FD[V], FD[V]]] ) ={
	  val func = (wd : (M => Double, FD[V])) => {
	  	val wtd = for ((m, fn) <- terms.toSeq) yield (wd._1(m), fn(wd._2))
	  	FiniteDistribution.linearCombination(wtd)	  			
	  }
	  /**
	   * the adjoint has first component the inner product of the linear combination of pullbacks with the original vector.
	   */
	  def adj(wd : (M => Double, FD[V]))(v : FD[V]) = {
	   val wtd = for ((m, fn) <- terms.toSeq) yield (wd._1(m), fn.grad(wd._2)(v))
	   val pullback = FiniteDistribution.linearCombination(wtd) 
	  }
	}
	
	
}