package provingground

import DiffbleFunction._

import Collections._

import LinearStructure._

import annotation._

import FiniteDistribution._

/**
 * A combinator for learning systems with state finite distributions on vertices.
 * Systems are built from components labeled by elements of a set M.
 * The state also has weights for these components.
 * The components are built from: moves (partial functions), partial combinations and islands.
 *
 */
object FiniteDistributionLearner {
	/**
	 * An atom for a finite distribution
	 */
  case class Atom[V](x: V) extends DiffbleFunction[Double, FiniteDistribution[V]] {
	  val func = (w: Double) => FiniteDistribution[V](Set(Weighted(x, w)))

	  val grad = (w: Double) => (p: FiniteDistribution[V]) => p(x)
	}

	/**
	 * Evaluation at a point for a finite distribution
	 */
  case class Evaluate[V](x: V) extends DiffbleFunction[FiniteDistribution[V], Double] {
	  val func = (p: FiniteDistribution[V]) => p(x)

	  val grad = (p: FiniteDistribution[V]) => (w: Double) => FiniteDistribution[V](Set(Weighted(x, w)))

	}

  case class PtwiseProd[V](sc: V => Double) extends DiffbleFunction[FiniteDistribution[V], FiniteDistribution[V]] {
	  val func = (p: FiniteDistribution[V]) => {
	    val pmf = for (Weighted(x, w) <- p.pmf) yield Weighted(x, w * sc(x))
	    FiniteDistribution(pmf)
	  }

	  val grad = (q: FiniteDistribution[V]) => (p: FiniteDistribution[V]) => {
		val pmf = for (Weighted(x, w) <- p.pmf) yield Weighted(x, w * sc(x))
	    FiniteDistribution(pmf)
	  }

	}



  case class Sample[X](N: Double) extends
    FormalExtension[FiniteDistribution[X]]{
    val func = (d : FiniteDistribution[X]) =>
      d.flatten filter ((x : X) => random.nextDouble < d(x) * N) normalized()
  }

  def sample[X](N: Double) : DiffbleFunction[FiniteDistribution[X], FiniteDistribution[X]] =
    Sample(N)

	import DiffbleFunction._

  /**
   * Normalizing a finite distribution.
   */
  case class NormalizeFD[V]() extends DiffbleFunction[FiniteDistribution[V], FiniteDistribution[V]] {
   val func = (d: FiniteDistribution[V]) => d.normalized()

   val grad = (d: FiniteDistribution[V]) => (w: FiniteDistribution[V]) => w * (1 / d.norm)
  }

  /**
   * purging (randomly) a finite distribution.
   * @param size upper bound on the expected size of the support.
   */
  def purgeFD[V](size: Int)(fd : FiniteDistribution[V]) = fd filter ((x : V) => fd(x) * size > random.nextDouble())

		/**
	 * smooth function applying move wherever applicable
	 */
  case class MoveFn[V, W](f: V => Option[W]) extends DiffbleFunction[FiniteDistribution[V], FiniteDistribution[W]] {
	  val func =  (d: FiniteDistribution[V]) => {
//	    val rawpmf = for (x<- d.support.toSeq; y<- f(x)) yield Weighted(y, d(x))
//	    FiniteDistribution(Weighted.flatten(rawpmf).toSet).flatten
      d mapOpt (f)
	  }

	  val grad = (d: FiniteDistribution[V]) => (w: FiniteDistribution[W]) => {
//	    val rawpmf = for (x<- d.support; y<- f(x)) yield Weighted(x, w(y))
//	    FiniteDistribution(rawpmf).flatten
      w.invmapOpt(f, d.supp)
	  }


	}

  case class CombinationFn[V](f: (V, V) => Option[V]) extends DiffbleFunction[FiniteDistribution[V], FiniteDistribution[V]] {
	  val func =  (d: FiniteDistribution[V]) => {
//	    val rawpmf = for (a <- d.support.toSeq; b <- d.support.toSeq; y <- f(a, b)) yield
//	    		Weighted(y, d(a) * d(b))
//	    FiniteDistribution(Weighted.flatten(rawpmf).toSet).flatten
      d flatMap ((v: V) =>
        d mapOpt((w: V) => f(v, w)))
	  }

	  val grad = (d: FiniteDistribution[V]) => (w: FiniteDistribution[V]) => {
   //   val fstDists =
   //     d.supp map(
    //      (a: V) => (w.invmapOpt((b: V) => f(a, b), d.supp)) * d(a))
      val fstsum =
        invFlatMap((a: V) => (w.invmapOpt((b: V) => f(a, b), d.supp)) * d(a), d.supp)
        //(fstDists :\ FiniteDistribution.empty[V])(_++_)

      val scndDists = d.supp map(
          (a: V) => (w.invmapOpt((b: V) => f(a, b), d.supp)) * d(a))
      val scndsum =
        invFlatMap((a: V) => (w.invmapOpt((b: V) => f(b, a), d.supp)) * d(a), d.supp)
    //    (scndDists :\ FiniteDistribution.empty[V])(_++_)

      fstsum ++ scndsum
      /*
	    val rawpmf = (for (a <- d.support; b <- d.support; y <- f(a, b)) yield
	    		Set(Weighted(a, w(y) * d(b)), Weighted(b, w(y) * d(b)))).flatten
	    FiniteDistribution(rawpmf).flatten*/
	  }

	}

	/**
	 * Add a new vertex, mainly for lambdas
	 */
  case class NewVertex[V](v: V) extends DiffbleFunction[(Double, FiniteDistribution[V]), FiniteDistribution[V]] {
	  val func = (wp : (Double,  FiniteDistribution[V])) => wp._2 * (1 - wp._1) + (v, wp._1)

	  val grad = (wp: (Double, FiniteDistribution[V])) => (q: FiniteDistribution[V]) => {
	    val nov = q filter ((x: V) => x != v)
	    (q(v), nov * wp._1)
	  }

	}


	/**
	 * Returns a smooth function (FD[M], X) => X, given a parameter index m : M and a dynamical system f: X => X;
   * the system f should correspond to m. For a distribution p in FD[M], if p(m) denotes the value at m,
		* the smooth function being defined is p(m)f.
	 */
	def weightedDyn[M, X :  LinearStructure : InnerProduct]: (
	        M,  DiffbleFunction[X, X]
	        ) =>  DiffbleFunction[(FiniteDistribution[M], X), X] = (m, fn) => {
	  val pm = Proj1[FiniteDistribution[M], X]
	  val scm = Evaluate(m)
	  val atM = pm andthen scm andthen Incl1[Double, X]
	  val pv = Proj2[FiniteDistribution[M], X]
	  val fv = pv andthen fn andthen Incl2[Double, X]
	  val fnsum = vsum[ DiffbleFunction[(FiniteDistribution[M], X), (Double, X)]]
	  fnsum(atM, fv) andthen ScProd[X]
	}


  case class ExtendM[M, X](
      fn:  DiffbleFunction[(FiniteDistribution[M], X), X]) extends
      DiffbleFunction[(FiniteDistribution[M], X), (FiniteDistribution[M], X)]  {
	  val func =  (mv: (FiniteDistribution[M], X)) => (mv._1, fn.func(mv))

	  val grad = (mv: (FiniteDistribution[M], X)) => (
        mw: (FiniteDistribution[M], X)) =>
          (mw._1 ++ fn.grad(mv)(mw._2)._1, fn.grad(mv)(mw._2)._2)

        }
	/**
	 * Extend differentiable function by identity on M.
	 */
	def extendM[M, X](
      fn:  DiffbleFunction[(FiniteDistribution[M], X), X]) :
      DiffbleFunction[(FiniteDistribution[M], X), (FiniteDistribution[M], X)] = {
        ExtendM(fn)

	}


  case class ProjectV[M, V]() extends DiffbleFunction[
    (FiniteDistribution[M], FiniteDistribution[V]), FiniteDistribution[V]] {
  	  val func =  (mv: (FiniteDistribution[M], FiniteDistribution[V])) => mv._2

  	  val grad = (mv: (FiniteDistribution[M], FiniteDistribution[V])) =>
        (v : FiniteDistribution[V]) => (FiniteDistribution.empty[M], v)

  	}

    def projectV[M, V] : DiffbleFunction[
    (FiniteDistribution[M], FiniteDistribution[V]), FiniteDistribution[V]] = ProjectV[M, V]


    def sampleV[M, V](N: Double) : DiffbleFunction[
    (FiniteDistribution[M], FiniteDistribution[V]),
    (FiniteDistribution[M], FiniteDistribution[V])] =
      block(id[FiniteDistribution[M]], sample(N))


}
