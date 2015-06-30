package provingground

import DiffbleFunction._
import Collections._
import annotation._


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
	def atom[V](x: V) = {
	  def func(w: Double) = FiniteDistribution[V](List(Weighted(x, w)))

	  def grad(w: Double)(p: FiniteDistribution[V]) = p(x)

	  DiffbleFunction(func)(grad)
	}

	/**
	 * Evaluation at a point for a finite distribution
	 */
	def eval[V](x: V) = {
	  def func(p: FiniteDistribution[V]) = p(x)

	  def grad(p: FiniteDistribution[V])(w: Double) = FiniteDistribution[V](List(Weighted(x, w)))

	  DiffbleFunction(func)(grad)
	}

	def ptwiseProd[V](sc: V => Double) = {
	  def func(p: FiniteDistribution[V]) = {
	    val pmf = for (Weighted(x, w) <- p.pmf) yield Weighted(x, w * sc(x))
	    FiniteDistribution(pmf)
	  }

	  def grad(q: FiniteDistribution[V])(p: FiniteDistribution[V]) = {
		val pmf = for (Weighted(x, w) <- p.pmf) yield Weighted(x, w * sc(x))
	    FiniteDistribution(pmf)
	  }

	  DiffbleFunction(func)(grad)
	}

	import DiffbleFunction._

  /**
   * Normalizing a finite distribution.
   */
  def normalizeFD[V] = {
   def func(d: FiniteDistribution[V]) = d.normalized()

   def grad(d: FiniteDistribution[V])(w: FiniteDistribution[V]) = w * (1 / d.norm)

   DiffbleFunction(func)(grad)
  }

  /**
   * purging (randomly) a finite distribution.
   * @param size upper bound on the expected size of the support.
   */
  def purgeFD[V](size: Int)(fd : FiniteDistribution[V]) = fd filter ((x : V) => fd(x) * size > random.nextDouble())

		/**
	 * smooth function applying move wherever applicable
	 */
	def moveFn[V, W](f: V => Option[W]) = {
	  def func(d: FiniteDistribution[V]) = {
	    val rawpmf = for (x<- d.support.toSeq; y<- f(x)) yield Weighted(y, d(x))
	    FiniteDistribution(rawpmf).flatten
	  }

	  def grad(d: FiniteDistribution[V])(w: FiniteDistribution[W]) = {
	    val rawpmf = for (x<- d.support.toSeq; y<- f(x)) yield Weighted(x, w(y))
	    FiniteDistribution(rawpmf).flatten
	  }

	  DiffbleFunction(func)(grad)
	}

	def combinationFn[V](f: (V, V) => Option[V]) = {
	  def func(d: FiniteDistribution[V]) = {
	    val rawpmf = for (a <- d.support.toSeq; b <- d.support.toSeq; y <- f(a, b)) yield
	    		Weighted(y, d(a) * d(b))
	    FiniteDistribution(rawpmf).flatten
	  }

	  def grad(d: FiniteDistribution[V])(w: FiniteDistribution[V]) = {
	    val rawpmf = (for (a <- d.support.toSeq; b <- d.support.toSeq; y <- f(a, b)) yield
	    		Seq(Weighted(a, w(y) * d(b)), Weighted(b, w(y) * d(b)))).flatten
	    FiniteDistribution(rawpmf).flatten
	  }

	  DiffbleFunction(func)(grad)
	}

	/**
	 * Add a new vertex, mainly for lambdas
	 */
	def newVertex[V](v: V) = {
	  def func(wp : (Double,  FiniteDistribution[V])) = wp._2 * (1 - wp._1) + (v, wp._1)

	  def grad(wp: (Double, FiniteDistribution[V]))(q: FiniteDistribution[V]) = {
	    val nov = q filter ((x: V) => x != v)
	    (q(v), nov * wp._1)
	  }

	  DiffbleFunction(func)(grad)
	}


	/**
	 * Returns a smooth function (FD[M], X) => X, given a parameter index m : M and a dynamical system f: X => X;
   * the system f should correspond to m. For a distribution p in FD[M], if p(m) denotes the value at m,
		* the smooth function being defined is p(m)f.
	 */
	def weightedDyn[M, X :  LinearStructure : InnerProduct]: (
	        M,  DiffbleFunction[X, X]
	        ) =>  DiffbleFunction[(FiniteDistribution[M], X), X] = (m, fn) => {
	  val pm = proj1[FiniteDistribution[M], X]
	  val scm = eval(m)
	  val atM = pm andthen scm andthen incl1[Double, X]
	  val pv = proj2[FiniteDistribution[M], X]
	  val fv = pv andthen fn andthen incl2[Double, X]
	  val fnsum = vsum[ DiffbleFunction[(FiniteDistribution[M], X), (Double, X)]]
	  fnsum(atM, fv) andthen scprod[X]
	}


	/**
	 * Extend differentiable function by identity on M.
	 */
	def extendM[M, X](fn:  DiffbleFunction[(FiniteDistribution[M], X), X]) :DiffbleFunction[(FiniteDistribution[M], X), (FiniteDistribution[M], X)] = {
	  def func(mv: (FiniteDistribution[M], X)) = (mv._1, fn(mv))

	  def grad(mv: (FiniteDistribution[M], X))(
        mw: (FiniteDistribution[M], X)) =
          (mw._1 ++ fn.grad(mv)(mw._2)._1, fn.grad(mv)(mw._2)._2)

	  DiffbleFunction(func)(grad)
	}


  def matchFlow[A, B](fn : A => Option[B], target :  B => Double) = (d: FiniteDistribution[A]) => {
    val push = moveFn(fn)

    val shiftb = (bs : FiniteDistribution[B]) => bs.feedback(target)

    shiftb ^: push
  }

  @tailrec def flow[A : LinearStructure](init : A, shift: A => A, epsilon : Double, n: Int): A = {
    lazy val sum = vsum[A]
    lazy val scprod = vprod[A]
    if (n <1) init
    else flow(sum(init, scprod(epsilon, shift(init))), shift, epsilon, n-1)
  }





















	// Most of the below is to be deprecated.

  @tailrec def goalFlow[A : LinearStructure, R](init : A, shift: A => A, epsilon : Double, n: Int,
      result: A => Option[R]): Either[A, R] = {
    lazy val sum = vsum[A]
    lazy val scprod = vprod[A]
    result(init) match{
      case Some(r) => Right(r)
      case None =>
        if (n <1) Left(init)
        else goalFlow(sum(init, scprod(epsilon, shift(init))), shift, epsilon, n-1, result)
    }
  }

  case class ResultState[A, R](state: A, results: Set[R])

  @tailrec def resultsFlow[A : LinearStructure, R](init : ResultState[A, R], shift: A => A, epsilon : Double, n: Int,
      results : A => Set[R]): ResultState[A, R] = {
    lazy val sum = vsum[A]
    lazy val scprod = vprod[A]
    if (n <1) init
    else {
      val nxt = sum(init.state, scprod(epsilon, shift(init.state)))
      resultsFlow(
        ResultState(nxt, init.results union results(nxt)),
            shift, epsilon, n-1, results)
    }
  }

	 def dynsum[M : LinearStructure, V : LinearStructure] =
     vsum[ DiffbleFunction[(FiniteDistribution[M], FiniteDistribution[V]), FiniteDistribution[V]]]


	// Generic helpers

	/**
	 * identity smooth function.
	 */



	/**
	 * differentiable function x -> (x, x)

	def diagonal[V] = {
	  def func(d: FiniteDistribution[V]) = {
	    val pmf= (for (x <- d.support; y <- d.support) yield Weighted((x, y), d(x) * d(y))).toSeq
	    FiniteDistribution(pmf)
	  }

	  def grad(d: FiniteDistribution[V])(pd: FiniteDistribution[(V, V)]) = {
	    val rawpmf = pd.pmf.flatMap(ab => Seq(Weighted(ab.elem._1, d(ab.elem._2)), Weighted(ab.elem._2, d(ab.elem._1))))
	    FiniteDistribution(rawpmf).flatten
	  }

	  DiffbleFunction(func)(grad)
	}
	*
	*/

	/**
	 * declare the gradient to be the identity - to be used for approximations, pruning, sampling.
	 */
	def formalSmooth[A](f: A => A) = DiffbleFunction(f)((a : A) => {(b : A) => b})



	// Helpers for operations on pairs of distributions.


	private def dstsum[M, V](fst: (FiniteDistribution[M], FiniteDistribution[V]), scnd: (FiniteDistribution[M], FiniteDistribution[V])) = (fst._1 ++ scnd._1, fst._2++ scnd._2)

	private def dstmult[M, V](fst: (FiniteDistribution[M], FiniteDistribution[V]), scnd: Double) = (fst._1 * scnd, fst._2 * scnd)

	private def dstdot[M, V](fst: (FiniteDistribution[M], FiniteDistribution[V]), scnd: (FiniteDistribution[M], FiniteDistribution[V])) = (fst._1 dot scnd._1) + (fst._2 dot scnd._2)

	private def dstzero[M, V] = (FiniteDistribution.empty[M], FiniteDistribution.empty[V])

	// Building dynamics on FiniteDistribution[V]



	// Building dynamics (FiniteDistribution[M], FiniteDistribution[V]) => FiniteDistribution[V]

	private type DynFn[M, V] =  DiffbleFunction[(FiniteDistribution[M], FiniteDistribution[V]), FiniteDistribution[V]]


	/**
	 * smooth function corresponding to adding the V distributions for two given smooth functions.
	 *
	 */
	def sumFn[M, V](fst: => DynFn[M, V], scnd : => DynFn[M, V]) : DynFn[M, V] = {
	  def func(st : (FiniteDistribution[M], FiniteDistribution[V])) = fst(st) ++ scnd(st)

	  def grad(st: (FiniteDistribution[M], FiniteDistribution[V]))(w: FiniteDistribution[V]) ={
	    dstsum(fst.grad(st)(w), scnd.grad(st)(w))
	  }

	  DiffbleFunction(func)(grad)
	}

	/**
	 * differentiable function from a given one by composing with scalar multiplication.
	 * Namely, (s, x) -> s * f(x), with gradient having weights on both s and x.
	 */
	def scProdFn[V](fn :  DiffbleFunction[FiniteDistribution[V], FiniteDistribution[V]]) = {
	  def func(cv: (Double, FiniteDistribution[V])) = fn(cv._2) * cv._1

	  def grad(cv: (Double, FiniteDistribution[V]))(w: FiniteDistribution[V]) = {
	    val x = fn.grad(cv._2)(w)
	    (x dot cv._2, x * cv._1)
	  }

	  DiffbleFunction(func)(grad)
	}



	/**
	 * Creates an atom at a parameter with given weight.
	 */
	private def atM[M, V](cv: (Double, FiniteDistribution[V]), m: M) = {
	  val dstM = FiniteDistribution.empty[M] + (m, cv._1)
	  (dstM, cv._2)
	}

		/**
	 * binds a scalar (multiplication) constant to a particular parameter.
	 */
	def bindFn[M, V](m: M, fn:  DiffbleFunction[(Double, FiniteDistribution[V]), FiniteDistribution[V]]) ={
	  def func(csv: (FiniteDistribution[M], FiniteDistribution[V])) = {
	    val c = csv._1(m)
	    val v = csv._2
	    fn((c, v))
	  }

	  def grad(csv: (FiniteDistribution[M], FiniteDistribution[V]))(w: FiniteDistribution[V]) = {
	    val c = csv._1(m)
	    val v = csv._2
	    val cterm = fn.grad((c, v))(w)
	    atM(cterm, m)
	  }

	  DiffbleFunction(func)(grad)
	}

	/**
	 * The weighted term typically corresponding to a move or a combination
	 */
	def wtdDyn[M, V](m: M,fn :  DiffbleFunction[FiniteDistribution[V], FiniteDistribution[V]]) : DynFn[M, V] = bindFn(m, scProdFn(fn))

	/**
	 * the zero differentiable function
	 */
	private def zeroMVV[M, V] : DynFn[M, V] = {
	  def func(a: (FiniteDistribution[M], FiniteDistribution[V])) = FiniteDistribution.empty[V]

	  def grad(a: (FiniteDistribution[M], FiniteDistribution[V]))(b: FiniteDistribution[V]) = (FiniteDistribution.empty[M], FiniteDistribution.empty[V])

	  DiffbleFunction(func)(grad)
	}

	/**
	 * Linear combination of dynamical systems
	 */
	def linComb[M, V](dyns: Map[M,  DiffbleFunction[FiniteDistribution[V], FiniteDistribution[V]]]) = {
	  val syslst = for ((m, f) <- dyns) yield wtdDyn(m, f)
	  (zeroMVV[M, V] /: syslst)((a, b) => sumFn(a, b))
	}





	// Building dynamics on (FiniteDistribution[M], FiniteDistribution[V])

	private type DS[M, V] =  DiffbleFunction[(FiniteDistribution[M], FiniteDistribution[V]), (FiniteDistribution[M], FiniteDistribution[V])]

	def sumDF[M, V](fst: => DS[M, V], scnd : => DS[M, V]) = {
	  def func(st : (FiniteDistribution[M], FiniteDistribution[V])) = dstsum(fst(st), scnd(st))

	  def grad(st: (FiniteDistribution[M], FiniteDistribution[V]))(w: (FiniteDistribution[M], FiniteDistribution[V])) ={
	    dstsum(fst.grad(st)(w), scnd.grad(st)(w))
	  }

	  DiffbleFunction(func)(grad)
	}

	def foldDF[M, V](base: DS[M, V], comps: Map[V, DS[M, V]]) = {
	  def func(arg: (FiniteDistribution[M], FiniteDistribution[V])) = {
	    val l = for ((v, f) <- comps) yield dstmult(f(arg), arg._2(v))
	    (base(arg) /: l)(dstsum)
	  }

	  def grad(arg: (FiniteDistribution[M], FiniteDistribution[V]))(vect: (FiniteDistribution[M], FiniteDistribution[V])) = {
	    val l = for ((v, f) <- comps) yield {
	      val prob = dstdot(f.grad(arg)(vect), arg)
	      val term = dstmult(f.grad(arg)(vect), arg._2(v))
	      (term._1, term._2 + (v, prob))
	    }
	    (base.grad(arg)(vect) /: l)(dstsum)
	  }

	  DiffbleFunction(func)(grad)
	}




	def projectV[M, V] = {
	  def func(mv: (FiniteDistribution[M], FiniteDistribution[V])) = mv._2

	  def grad(mv: (FiniteDistribution[M], FiniteDistribution[V]))(v : FiniteDistribution[V]) = (FiniteDistribution.empty[M], v)

	  DiffbleFunction(func)(grad)
	}

	/**
	 * prune the distribution on vertices without changing the one on M.
	 */
	def pruneV[M, V](t: Double) = formalSmooth((mv: (FiniteDistribution[M], FiniteDistribution[V])) => (mv._1, mv._2 normalized(t)))

	/**
	 * prune a distribution
	 */
	def prune[V](t: Double) = formalSmooth((d: FiniteDistribution[V]) => d normalized(t))


	/**
	 * Linear combination of dynamical systems with coefficients in V
	 */
	def linComb[M, V](dyns: V => Option[DS[M, V]]) = {
	  def func(arg: (FiniteDistribution[M], FiniteDistribution[V])) = {
	    val vdst = arg._2
	    val vs = vdst.support
	    val terms = for (v <- vs; f <- dyns(v)) yield dstmult(f(arg), vdst(v))
	    (dstzero[M, V] /: terms)(dstsum)
	  }

	  def grad(arg: (FiniteDistribution[M], FiniteDistribution[V]))(w : (FiniteDistribution[M], FiniteDistribution[V])) = {
	    val vdst = arg._2
	    val vs = vdst.support
	    val vectterms = for (v <- vs; f <- dyns(v)) yield dstmult(f.grad(arg)(w), vdst(v))
	    val scatoms = for (v <- vs; f <- dyns(v)) yield Weighted(v, dstdot(f.grad(arg)(w), arg))
	    val scdst = (FiniteDistribution.empty[M], FiniteDistribution(scatoms.toSeq))
	    dstsum(scdst, (dstzero[M, V] /: vectterms)(dstsum))
	  }

	  DiffbleFunction(func)(grad)
	}





	// Various attempts at islands


	/**
	 * function and gradient after scalar multiplication by the parameter of the isle,
	 * (p_M, p_V) -> (p_M(m) * f(p_M, p_V))
	 */
	def isleMultFn[M, V](fn: DynFn[M, V], m: M) = {
	  def func(csv: (FiniteDistribution[M], FiniteDistribution[V])) = {
	    val c = csv._1(m)
	    val v = csv._2
	    fn((csv._1, v)) * c
	  }

	  def grad(csv: (FiniteDistribution[M], FiniteDistribution[V]))(w: FiniteDistribution[V]) = {
	    val c = csv._1(m)
	    val v = csv._2
	    val mv = fn.grad((csv._1, v))(w)
	    val shift = mv._2 dot v
	    (mv._1 + (m, shift), mv._2)
	  }

	  DiffbleFunction(func)(grad)
	}



	/**
	 * smooth function creating initial distribution for isle creating an object v with weight given by parameter t.
	 * To actually create an isle, we compose this with the corresponding mixin.
	 */
	def simpleIsleInitFn[M, V](v: V, t: M) = {
	  def func(ds: (FiniteDistribution[M], FiniteDistribution[V])) ={
	    val p = ds._1(t)
	    (ds._1, ds._2 * (1.0 - p) + (v, p))
	  }

	  def grad(ds: (FiniteDistribution[M], FiniteDistribution[V]))(ws: (FiniteDistribution[M], FiniteDistribution[V])) ={
	    val p = ds._1(t)
	    val c = ws._1(t)
	    val shift = (ds._2 filter ((x: V) => x != v)) * (1.0 - p)
	    (ws._1 + (t, c), shift)
	  }

	  DiffbleFunction(func)(grad)
	}



	/**
	 * Spawns an island, that can be mixed in with other dynamics
	 * One should call this with f modified by updating depth, which should affect number of steps.
	 *
	 */
	def spawnSimpleIsle[M, V](v: V, t: M, m : M, f : => DynFn[M, V]) = {
	  val g = simpleIsleInitFn(v, t) andthen f
	  isleMultFn(g, m)
	}



	/**
	 * step for learning based on feedback.
	 */
	def learnstep[M, V](f:  DiffbleFunction[(FiniteDistribution[M], FiniteDistribution[V]), (FiniteDistribution[M], FiniteDistribution[V])], epsilon: Double, feedback: (FiniteDistribution[M], FiniteDistribution[V]) => (FiniteDistribution[M], FiniteDistribution[V])) = {
	  (init : (FiniteDistribution[M], FiniteDistribution[V])) =>
	    val fb = feedback(f(init)._1, f(init)._2)
	    (init._1 ++ (fb._1 * epsilon), init._2 ++ (fb._2 * epsilon))
	}





	/**
	 *  Iterate a diffble function given depth
	 */
	@tailrec def iterateDiffbleDepth[X](fn: =>  DiffbleFunction[X, X], steps: Int, depth: Int, accum: =>  DiffbleFunction[X, X] = id[X]):  DiffbleFunction[X, X] = {
		if (steps<math.pow(2, depth)) accum else iterateDiffbleDepth(fn, steps - math.pow(2, depth).toInt, depth, accum andthen fn)
	}


	class IterDynSys[M, V](dyn : => DynFn[M, V], steps: Int, depth: Int){
	  def iter(d: Int) = iterateDiffbleDepth(next, steps, d)

	  def iterdeeper(d: Int) = if (steps < math.pow(2, depth + 1)) id[(FiniteDistribution[M], FiniteDistribution[V])] else iterateDiffbleDepth(next, steps, depth+1)

	  def iterV(d: Int) = iter(d) andthen projectV[M, V]

	  /**
	   * add a simple island one step deeper and create a system adding depth.
	   *
	   * @param v new variable created in import.
	   * @param t coefficient of the new variable created.
	   * @param m weight for island formation.
	   * @param export exporting from the island.
	   */
	  def withIsle(v: V, t: M, m : M, export : =>  DiffbleFunction[FiniteDistribution[V], FiniteDistribution[V]], d: Int = depth, isleDepth: Int => Int) : DynFn[M, V] = {
	    def iternext = iterateDiffbleDepth(extendM(withIsle(v, t, m, export, d+1, isleDepth)), steps, isleDepth(d))
	    def isle = spawnSimpleIsle[M, V](v: V, t: M, m : M, iternext andthen projectV[M, V]) andthen export
	    sumFn(dyn, isle)
	  }

	  /**
	   * make a simple island one step deeper.
	   *
	   * @param v new variable created in import.
	   * @param t coefficient of the new variable created.
	   * @param m weight for island formation.
	   * @param export exporting from the island.
	   */
	  def mkIsle(v: V, t: M, m : M, export : =>  DiffbleFunction[FiniteDistribution[V], FiniteDistribution[V]], d: Int = depth, isleDepth: Int => Int) : DynFn[M, V] = {
	    def iternext = iterateDiffbleDepth(iter(isleDepth(d)), steps, isleDepth(d))
	    spawnSimpleIsle[M, V](v: V, t: M, m : M, iternext andthen projectV[M, V]) andthen export
	  }

	  def addIsle(v: V, t: M, m : M, export : =>  DiffbleFunction[FiniteDistribution[V], FiniteDistribution[V]], isleDepth: Int => Int = (n) => n + 1) = new IterDynSys(
	      withIsle(v: V, t: M, m : M, export, depth, isleDepth), steps, depth)

	  def next = extendM(dyn)
	}

	type DynF[M, V] =  DiffbleFunction[(FiniteDistribution[M], FiniteDistribution[V]), (FiniteDistribution[M], FiniteDistribution[V])]


	def sumF[M, V](fst: => DynF[M, V], scnd : => DynF[M, V]) = {
	  def func(st : (FiniteDistribution[M], FiniteDistribution[V])) = dstsum(fst(st), scnd(st))

	  def grad(st: (FiniteDistribution[M], FiniteDistribution[V]))(w: (FiniteDistribution[M], FiniteDistribution[V])) ={
	    dstsum(fst.grad(st)(w), scnd.grad(st)(w))
	  }

	  DiffbleFunction(func)(grad)
	}

	/*
	 * Isle independent of state, does not include lambda's
	 * We are given a dynamical system base at each depth n,
	 * and an isle function depending on a given dynamical system and dpeth.
	 * We get another one f at each depth n by the recurence
	 * f(n) = base(n) + isle(n+1)(f(n+1))
	 *
	 *
	 */
	def mixinIsle[M, V](base : => Int => DynFn[M, V], isle: Int => DynFn[M, V] => DynFn[M, V]): Int => DynFn[M, V] = {
	  def rec(g : => Int => DynFn[M, V])(n: Int) = sumFn(base(n), isle(n+1)(g(n+1)))
	  def f: Int => DynFn[M, V] = rec(f)
	  f
	}


	/*
	 * Mix in isles corresponding to elements of V, for instance lambdas
	 * Should really have optional isles.
	  */
	def mixinAllIsles[M, V](base : => Int => DynF[M, V], isles: Map[V, Int => DynF[M, V] => DynF[M, V]]): Int => DynF[M, V] = {
	  def rec(g : => Int => DynF[M, V])(n: Int) = {
	    val isllst = for ((v, f) <- isles) yield (v, isles(v)(n)(g(n+1)))
	    foldDF[M, V](sumF(base(n), g(n)), isllst)
	  }
	  def f: Int => DynF[M, V] = rec(f)
	  f
	}



}
