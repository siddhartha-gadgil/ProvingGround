package provingground

import DiffbleFunction._
import Collections._; import FiniteDistribution._; import provingground._
import annotation._

import FiniteDistributionLearner._
import LinearStructure._

object FiniteDistributionLearnerExtras {
  @tailrec
  def goalFlow[A: LinearStructure, R](init: A,
                                      shift: A => A,
                                      epsilon: Double,
                                      n: Int,
                                      result: A => Option[R]): Either[A, R] = {
    lazy val sum = vsum[A]
    lazy val ScProd = vprod[A]
    result(init) match {
      case Some(r) => Right(r)
      case None =>
        if (n < 1) Left(init)
        else
          goalFlow(sum(init, ScProd(epsilon, shift(init))),
                   shift,
                   epsilon,
                   n - 1,
                   result)
    }
  }

  case class ResultState[A, R](state: A, results: Set[R])

  @tailrec
  def resultsFlow[A: LinearStructure, R](
      init: ResultState[A, R],
      shift: A => A,
      epsilon: Double,
      n: Int,
      results: A => Set[R]): ResultState[A, R] = {
    lazy val sum = vsum[A]
    lazy val ScProd = vprod[A]
    if (n < 1) init
    else {
      val nxt = sum(init.state, ScProd(epsilon, shift(init.state)))
      resultsFlow(ResultState(nxt, init.results union results(nxt)),
                  shift,
                  epsilon,
                  n - 1,
                  results)
    }
  }

  def dynsum[M: LinearStructure, V: LinearStructure] =
    vsum[DiffbleFunction[(FiniteDistribution[M], FiniteDistribution[V]),
                         FiniteDistribution[V]]]

  // Generic helpers

  /**
    * identity smooth function.
    */
  /**
    * differentiable function x -> (x, x)

  def diagonal[V] = {
    val func =  d: FiniteDistribution[V]) = {
      val pmf= (for (x <- d.support; y <- d.support) yield Weighted((x, y), d(x) * d(y)))
      FiniteDistribution(pmf)
    }

    val grad(d: FiniteDistribution[V])(pd: FiniteDistribution[(V, V)]) = {
      val rawpmf = pd.pmf.flatMap(ab => Set(Weighted(ab.elem._1, d(ab.elem._2)), Weighted(ab.elem._2, d(ab.elem._1))))
      FiniteDistribution(rawpmf).flatten
    }

    DiffbleFunction(func)(grad)
  }
    *
    */
  /**
    * declare the gradient to be the identity - to be used for approximations, pruning, sampling.
    */
  def formalSmooth[A](f: A => A) =
    DiffbleFunction(f)((a: A) => { (b: A) =>
      b
    })

  // Helpers for operations on pairs of distributions.

  private def dstsum[M, V](
      fst: (FiniteDistribution[M], FiniteDistribution[V]),
      scnd: (FiniteDistribution[M], FiniteDistribution[V])) =
    (fst._1 ++ scnd._1, fst._2 ++ scnd._2)

  private def dstmult[M, V](
      fst: (FiniteDistribution[M], FiniteDistribution[V]), scnd: Double) =
    (fst._1 * scnd, fst._2 * scnd)

  private def dstdot[M, V](
      fst: (FiniteDistribution[M], FiniteDistribution[V]),
      scnd: (FiniteDistribution[M], FiniteDistribution[V])) =
    (fst._1 dot scnd._1) + (fst._2 dot scnd._2)

  private def dstzero[M, V] =
    (FiniteDistribution.empty[M], FiniteDistribution.empty[V])

  // Building dynamics on FiniteDistribution[V]

  // Building dynamics (FiniteDistribution[M], FiniteDistribution[V]) => FiniteDistribution[V]

  private type DynFn[M, V] = DiffbleFunction[
      (FiniteDistribution[M], FiniteDistribution[V]), FiniteDistribution[V]]

  /**
    * smooth function corresponding to adding the V distributions for two given smooth functions.
    *
    */
  def sumFn[M, V](fst: => DynFn[M, V], scnd: => DynFn[M, V]): DynFn[M, V] = {
    val func = (st: (FiniteDistribution[M], FiniteDistribution[V])) =>
      fst.func(st) ++ scnd.func(st)

    val grad = (st: (FiniteDistribution[M], FiniteDistribution[V])) =>
      (w: FiniteDistribution[V]) => {
        dstsum(fst.grad(st)(w), scnd.grad(st)(w))
    }

    DiffbleFunction(func)(grad)
  }

  /**
    * differentiable function from a given one by composing with scalar multiplication.
    * Namely, (s, x) -> s * f(x), with gradient having weights on both s and x.
    */
  def scProdFn[V](
      fn: DiffbleFunction[FiniteDistribution[V], FiniteDistribution[V]]) = {
    val func = (cv: (Double, FiniteDistribution[V])) => fn.func(cv._2) * cv._1

    val grad = (cv: (Double, FiniteDistribution[V])) =>
      (w: FiniteDistribution[V]) => {
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
  def bindFn[M, V](
      m: M,
      fn: DiffbleFunction[
          (Double, FiniteDistribution[V]), FiniteDistribution[V]]) = {
    val func = (csv: (FiniteDistribution[M], FiniteDistribution[V])) => {
      val c = csv._1(m)
      val v = csv._2
      fn.func((c, v))
    }

    val grad = (csv: (FiniteDistribution[M], FiniteDistribution[V])) =>
      (w: FiniteDistribution[V]) => {
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
  def wtdDyn[M, V](
      m: M, fn: DiffbleFunction[FiniteDistribution[V], FiniteDistribution[V]])
    : DynFn[M, V] = bindFn(m, scProdFn(fn))

  /**
    * the zero differentiable function
    */
  private def zeroMVV[M, V]: DynFn[M, V] = {
    val func = (a: (FiniteDistribution[M], FiniteDistribution[V])) =>
      FiniteDistribution.empty[V]

    val grad = (a: (FiniteDistribution[M], FiniteDistribution[V])) =>
      (b: FiniteDistribution[V]) =>
        (FiniteDistribution.empty[M], FiniteDistribution.empty[V])

    DiffbleFunction(func)(grad)
  }

  /**
    * Linear combination of dynamical systems
    */
  def linComb[M, V](
      dyns: Map[
          M,
          DiffbleFunction[FiniteDistribution[V], FiniteDistribution[V]]]) = {
    val syslst = for ((m, f) <- dyns) yield wtdDyn(m, f)
    (zeroMVV[M, V] /: syslst)((a, b) => sumFn(a, b))
  }

  // Building dynamics on (FiniteDistribution[M], FiniteDistribution[V])

  private type DS[M, V] =
    DiffbleFunction[(FiniteDistribution[M], FiniteDistribution[V]),
                    (FiniteDistribution[M], FiniteDistribution[V])]

  def sumDF[M, V](fst: => DS[M, V], scnd: => DS[M, V]) = {
    val func = (st: (FiniteDistribution[M], FiniteDistribution[V])) =>
      dstsum(fst.func(st), scnd.func(st))

    val grad = (st: (FiniteDistribution[M], FiniteDistribution[V])) =>
      (w: (FiniteDistribution[M], FiniteDistribution[V])) => {
        dstsum(fst.grad(st)(w), scnd.grad(st)(w))
    }

    DiffbleFunction(func)(grad)
  }

  def foldDF[M, V](base: DS[M, V], comps: Map[V, DS[M, V]]) = {
    val func = (arg: (FiniteDistribution[M], FiniteDistribution[V])) => {
      val l = for ((v, f) <- comps) yield dstmult(f.func(arg), arg._2(v))
      (base.func(arg) /: l)(dstsum)
    }

    val grad = (arg: (FiniteDistribution[M], FiniteDistribution[V])) =>
      (vect: (FiniteDistribution[M], FiniteDistribution[V])) => {
        val l = for ((v, f) <- comps) yield {
          val prob = dstdot(f.grad(arg)(vect), arg)
          val term = dstmult(f.grad(arg)(vect), arg._2(v))
          (term._1, term._2 + (v, prob))
        }
        (base.grad(arg)(vect) /: l)(dstsum)
    }

    DiffbleFunction(func)(grad)
  }

  /**
    * prune the distribution on vertices without changing the one on M.
    */
  def pruneV[M, V](t: Double) =
    formalSmooth((mv: (FiniteDistribution[M], FiniteDistribution[V])) =>
          (mv._1, mv._2 normalized (t)))

  /**
    * prune a distribution
    */
  def prune[V](t: Double) =
    formalSmooth((d: FiniteDistribution[V]) => d normalized (t))

  /**
    * Linear combination of dynamical systems with coefficients in V
    */
  def linComb[M, V](dyns: V => Option[DS[M, V]]) = {
    val func = (arg: (FiniteDistribution[M], FiniteDistribution[V])) => {
      val vdst = arg._2
      val vs = vdst.support
      val terms = for (v <- vs; f <- dyns(v)) yield
        dstmult(f.func(arg), vdst(v))
      (dstzero[M, V] /: terms)(dstsum)
    }

    val grad = (arg: (FiniteDistribution[M], FiniteDistribution[V])) =>
      (w: (FiniteDistribution[M], FiniteDistribution[V])) => {
        val vdst = arg._2
        val vs = vdst.support
        val vectterms = for (v <- vs; f <- dyns(v)) yield
          dstmult(f.grad(arg)(w), vdst(v))
        val scatoms = for (v <- vs; f <- dyns(v)) yield
          Weighted(v, dstdot(f.grad(arg)(w), arg))
        val scdst = (FiniteDistribution.empty[M], FiniteDistribution(scatoms))
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
    val func = (csv: (FiniteDistribution[M], FiniteDistribution[V])) => {
      val c = csv._1(m)
      val v = csv._2
      fn.func((csv._1, v)) * c
    }

    val grad = (csv: (FiniteDistribution[M], FiniteDistribution[V])) =>
      (w: FiniteDistribution[V]) => {
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
    val func = (ds: (FiniteDistribution[M], FiniteDistribution[V])) => {
      val p = ds._1(t)
      (ds._1, ds._2 * (1.0 - p) + (v, p))
    }

    val grad = (ds: (FiniteDistribution[M], FiniteDistribution[V])) =>
      (ws: (FiniteDistribution[M], FiniteDistribution[V])) => {
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
  def spawnSimpleIsle[M, V](v: V, t: M, m: M, f: => DynFn[M, V]) = {
    val g = simpleIsleInitFn(v, t) andthen f
    isleMultFn(g, m)
  }

  /**
    * step for learning based on feedback.
    */
  def learnstep[M, V](
      f: DiffbleFunction[(FiniteDistribution[M], FiniteDistribution[V]),
                         (FiniteDistribution[M], FiniteDistribution[V])],
      epsilon: Double,
      feedback: (FiniteDistribution[M],
                 FiniteDistribution[V]) => (FiniteDistribution[M],
                                            FiniteDistribution[V])) = {
    (init: (FiniteDistribution[M], FiniteDistribution[V])) =>
      val fb = feedback(f.func(init)._1, f.func(init)._2)
      (init._1 ++ (fb._1 * epsilon), init._2 ++ (fb._2 * epsilon))
  }

  /**
    *  Iterate a diffble function given depth
    */
  @tailrec
  def iterateDiffbleDepth[X](fn: => DiffbleFunction[X, X],
                             steps: Int,
                             depth: Int,
                             accum: => DiffbleFunction[X, X] =
                               id[X]): DiffbleFunction[X, X] = {
    if (steps < math.pow(2, depth)) accum
    else
      iterateDiffbleDepth(
          fn, steps - math.pow(2, depth).toInt, depth, accum andthen fn)
  }

  class IterDynSys[M, V](dyn: => DynFn[M, V], steps: Int, depth: Int) {
    def iter(d: Int) = iterateDiffbleDepth(next, steps, d)

    def iterdeeper(d: Int) =
      if (steps < math.pow(2, depth + 1))
        id[(FiniteDistribution[M], FiniteDistribution[V])]
      else iterateDiffbleDepth(next, steps, depth + 1)

    def iterV(d: Int) = iter(d) andthen projectV[M, FiniteDistribution[V]]

    /**
      * add a simple island one step deeper and create a system adding depth.
      *
      * @param v new variable created in import.
      * @param t coefficient of the new variable created.
      * @param m weight for island formation.
      * @param export exporting from the island.
      */
    def withIsle(v: V,
                 t: M,
                 m: M,
                 export: => DiffbleFunction[
                     FiniteDistribution[V], FiniteDistribution[V]],
                 d: Int = depth,
                 isleDepth: Int => Int): DynFn[M, V] = {
      def iternext =
        iterateDiffbleDepth(
            extendM(withIsle(v, t, m, export, d + 1, isleDepth)),
            steps,
            isleDepth(d))
      def isle =
        spawnSimpleIsle[M, V](
            v: V,
            t: M,
            m: M,
            iternext andthen projectV[M, FiniteDistribution[V]]) andthen export
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
    def mkIsle(v: V,
               t: M,
               m: M,
               export: => DiffbleFunction[
                   FiniteDistribution[V], FiniteDistribution[V]],
               d: Int = depth,
               isleDepth: Int => Int): DynFn[M, V] = {
      def iternext =
        iterateDiffbleDepth(iter(isleDepth(d)), steps, isleDepth(d))
      spawnSimpleIsle[M, V](
          v: V,
          t: M,
          m: M,
          iternext andthen projectV[M, FiniteDistribution[V]]) andthen export
    }

    def addIsle(v: V,
                t: M,
                m: M,
                export: => DiffbleFunction[
                    FiniteDistribution[V], FiniteDistribution[V]],
                isleDepth: Int => Int = (n) => n + 1) =
      new IterDynSys(
          withIsle(v: V, t: M, m: M, export, depth, isleDepth), steps, depth)

    def next = extendM(dyn)
  }

  type DynF[M, V] =
    DiffbleFunction[(FiniteDistribution[M], FiniteDistribution[V]),
                    (FiniteDistribution[M], FiniteDistribution[V])]

  def sumF[M, V](fst: => DynF[M, V], scnd: => DynF[M, V]) = {
    val func = (st: (FiniteDistribution[M], FiniteDistribution[V])) =>
      dstsum(fst.func(st), scnd.func(st))

    val grad = (st: (FiniteDistribution[M], FiniteDistribution[V])) =>
      (w: (FiniteDistribution[M], FiniteDistribution[V])) => {
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
  def mixinIsle[M, V](
      base: => Int => DynFn[M, V],
      isle: Int => DynFn[M, V] => DynFn[M, V]): Int => DynFn[M, V] = {
    def rec(g: => Int => DynFn[M, V])(n: Int) =
      sumFn(base(n), isle(n + 1)(g(n + 1)))
    def f: Int => DynFn[M, V] = rec(f)
    f
  }

  /*
   * Mix in isles corresponding to elements of V, for instance lambdas
   * Should really have optional isles.
   */
  def mixinAllIsles[M, V](
      base: => Int => DynF[M, V],
      isles: Map[V, Int => DynF[M, V] => DynF[M, V]]): Int => DynF[M, V] = {
    def rec(g: => Int => DynF[M, V])(n: Int) = {
      val isllst = for ((v, f) <- isles) yield (v, isles(v)(n)(g(n + 1)))
      foldDF[M, V](sumF(base(n), g(n)), isllst)
    }
    def f: Int => DynF[M, V] = rec(f)
    f
  }
}
