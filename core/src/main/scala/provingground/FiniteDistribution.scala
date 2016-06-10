package provingground

import scala.language.implicitConversions

import Collections._

//import LinearStructure._

import scala.collection.parallel.immutable.ParVector

/**
  * Finite distributions, often supposed to be probability distributions, but may also be tangents to this or intermediates.
  *
  * @param pmf probability mass function, may have same object split.
  *
  * @param epsilon cutoff below which some methods ignore objects. should be very small to allow for split objects.
  */
sealed trait GenFiniteDistribution[T]
    extends Any
    with ProbabilityDistribution[T] /*ProbabilityDistribution[T] with LabelledVector[T]*/ {
  def pmf: Traversable[Weighted[T]]

//  val injective: Boolean

//  val epsilon: Double

//  def next = ProbabilityDistribution.choose(pmf)

  /**
    * flatten distribution collating elements.
    */
  def flatten: GenFiniteDistribution[T]

  /**
    * add together all probabilities for
    */
  def getsum(label: T): Double

  def filter(p: T => Boolean): GenFiniteDistribution[T]

  /**
    * normalized so all probabilities are positive and the total is 1.
    */
  def normalized(t: Double = 0.0): GenFiniteDistribution[T]

  /**
    * add another distribution without normalizing
    */
  def ++(that: GenFiniteDistribution[T]): GenFiniteDistribution[T]

  /**
    * scale the distribution
    */
  def *(sc: Double): GenFiniteDistribution[T]

  def elems = pmf map (_.elem)

  /**
    * support of the distribution.
    */
  def support = (pmf filter (_.weight > 0) map (_.elem)).toSet

  /**
    * A more efficient support
    */
  def supp =
    //if (injective) elems else
    flatten.elems

  /**
    * l^1-norm
    */
  def norm = (pmf.toSeq map (_.weight.abs)).sum

  /**
    * next instance of a random variable with the given distribution
    */
  def next = Weighted.pick(posmf(), random.nextDouble * postotal())

  /**
    * get weight.
    */
  def get(label: T) = Some(getsum(label))

  /**
    * weight of the label.
    */
  def apply(label: T) = getsum(label)

  /**
    * distribution as set with collation
    */
  def flatdist = supp map ((l) => Weighted(l, getsum(l)))

  /**
    * probability of elements satisfying a predicate
    */
  def prob(p: T => Boolean) = {
    (supp filter p map getsum).sum
  }

  /**
    * weights of the quotient distribution, for feedback.
    * Note that we cannot use equality in the `quotient' distribution as quotienting actually involves picking a set.
    */
  def weight(equiv: (T, T) => Boolean)(t: T) =
    (supp filter (equiv(t, _)) map getsum).sum

  /**
    * objects with positive probability (or bounded below by a threshhold)
    */
  def posmf(t: Double = 0.0) = pmf filter (_.weight > t)

  /**
    * total of the positive weights
    */
  def postotal(t: Double = 0.0) =
    ((posmf(t).toSeq map (_.weight))).sum

  /**
    * add weighted element without normalizing
    */
  def +(elem: T, prob: Double) =
    this ++ FiniteDistribution(Vector(Weighted(elem, prob)))

  /**
    * subtract distribution
    */
  def --(that: GenFiniteDistribution[T]) = this ++ (that * (-1))

  /**
    * map distribution without normalizing, concretely implemented to avoid
    * probability distribution map (with weaker type) overriding this.
    */
  override def map[S](f: T => S): GenFiniteDistribution[S] = {
    val newpmf = for (Weighted(elem, wt) <- pmf) yield Weighted(f(elem), wt)
    FiniteDistribution(newpmf)
  }

  def mapOpt[S](f: T => Option[S]): GenFiniteDistribution[S]

  def invmap[S](f: S => T, support: Traversable[S]): GenFiniteDistribution[S]

  def invmapOpt[S](
      f: S => Option[T], support: Traversable[S]): GenFiniteDistribution[S]

  def expectation(implicit ls: LinearStructure[T]): T = {
    val wtdelems = for (Weighted(a, p) <- pmf) yield ls.mult(p, a)
    (wtdelems :\ ls.zero)(ls.sum)
  }

//  @deprecated("should move to concrete implementation", "now")

  def purge(epsilon: Double) = filter((t: T) => (apply(t) > epsilon))

  override def toString = {
    val sortedpmf = pmf.toSeq.sortBy(1 - _.weight)
    val terms = (for (Weighted(elem, wt) <- sortedpmf) yield
      (elem.toString + " : " + wt.toString + ", ")).foldLeft("")(_ + _)
    "[" + terms.dropRight(2) + "]"
  }

  def entropy(elem: T) = -math.log(apply(elem)) / math.log(2)

  def entropyView =
    supp.toList.map((x) => Weighted(x.toString, entropy(x))).sortBy(_.weight)

  def split(groups: Int) = {
    val rand = new scala.util.Random

    pmf groupBy ((_) => rand.nextInt(groups - 1)) mapValues { x =>
      FiniteDistribution(x)
    }
  }
}

object FiniteDistribution {
  // choose default implementation
//  def apply[T](pmf: Traversable[Weighted[T]], epsilon: Double = 0.0) : FiniteDistribution[T] = FiniteDistributionSet(Weighted.flatten(pmf.toSeq).toSet, epsilon)
  def apply[T](pmf: Traversable[Weighted[T]]): FiniteDistribution[T] =
    FiniteDistribution(pmf.toVector)

  def collect[T](fd: FiniteDistribution[Option[T]]) =
    FiniteDistribution(
        fd.pmf.filter(wo => !(wo.elem.isEmpty)).map((wo) => wo.map(_.get)))

  implicit def finiteDistInnerProd[X] =
    InnerProduct[FiniteDistribution[X]](_ dot _)

  def uniform[A](s: Traversable[A]): FiniteDistribution[A] = {
    val prob = 1.0 / s.size
    val pmf = (s map (Weighted(_, prob)))
    FiniteDistribution(pmf)
  }

  def rawUnif[A](s: Traversable[A]) =
    FiniteDistribution(s map ((Weighted(_, 1.0))))

  def unif[A](as: A*) = uniform(as.toSet)

  def empty[T]: FiniteDistribution[T] =
    FiniteDistribution[T](Vector(): Vector[Weighted[T]]) //Empty[T]

  @deprecated("use empty method", "to remove")
  case class Empty[T]() extends GenFiniteDistribution[T] {
    val pmf: Traversable[Weighted[T]] = Traversable.empty

    val injective: Boolean = true

    val epsilon: Double = 0

    /**
      * flatten distribution collating elements.
      */
    def flatten: GenFiniteDistribution[T] = this

    /**
      * add together all probabilities for
      */
    def getsum(label: T): Double = 0

    def filter(p: T => Boolean): GenFiniteDistribution[T] = this

    /**
      * normalized so all probabilities are positive and the total is 1.
      */
    def normalized(t: Double = 0.0): GenFiniteDistribution[T] =
      throw new IllegalArgumentException(
          "can only normailze non-empty distributions")

    /**
      * add another distribution without normalizing
      */
    def ++(that: GenFiniteDistribution[T]): GenFiniteDistribution[T] = that

    /**
      * scale the distribution
      */
    def *(sc: Double): GenFiniteDistribution[T] = this

    /** As seen from class Empty, the missing signatures are as follows.
      *  *  For convenience, these are usable as stub implementations.  */
    def invmap[S](
        f: S => T,
        support: Traversable[S]): provingground.GenFiniteDistribution[S] =
      empty

    def invmapOpt[S](
        f: S => Option[T],
        support: Traversable[S]): provingground.GenFiniteDistribution[S] =
      empty

    override def map[S](f: T => S): provingground.GenFiniteDistribution[S] =
      empty

    def mapOpt[S](f: T => Option[S]): provingground.GenFiniteDistribution[S] =
      empty
  }

  def linearCombination[T](terms: Seq[(Double, FiniteDistribution[T])]) = {
    val scaled = for ((a, d) <- terms) yield d * a
    (scaled :\ (empty[T]))(_ ++ _)
  }

  def invFlatMap[S, T](
      f: S => FiniteDistribution[T], support: Traversable[S]) = {
    val dists = support map ((s: S) => f(s))
    (dists :\ FiniteDistribution.empty[T])(_ ++ _)
  }

  implicit def FiniteDistVec[T] =
    LinearStructure[FiniteDistribution[T]](
        FiniteDistribution.empty, _ ++ _, (w, d) => d * w)
}

case class FiniteDistributionSet[T](
    pmf: Set[Weighted[T]], epsilon: Double = 0.0)
    extends GenFiniteDistribution[T] {
  val injective = true

  def flatten = this

  def getsum(label: T) =
    (pmf.toSeq filter (_.elem == label) map (_.weight)).sum

  def normalized(t: Double = 0.0) =
    FiniteDistributionSet((posmf(t) map (_.scale(1.0 / postotal(t)))).toSet)

  def filter(p: T => Boolean) =
    FiniteDistributionSet(pmf filter (wt => p(wt.elem)))

  def *(sc: Double) = FiniteDistributionSet(pmf map (_.scale(sc)))

  def ++(that: GenFiniteDistribution[T]) = {
    val combined = (for (k <- support union that.support) yield
      Weighted(k, apply(k) + that(k)))
    FiniteDistributionSet(combined, epsilon)
  }

  def --(that: FiniteDistribution[T]) = this ++ (that * (-1))

  override def map[S](f: T => S) = {
    val newpmf = for (Weighted(elem, wt) <- pmf.toSeq) yield
      Weighted(f(elem), wt)
    FiniteDistributionSet(Weighted.flatten(newpmf).toSet, epsilon)
  }

  def mapOpt[S](f: T => Option[S]) = {
    val newpmf = for (Weighted(elem, wt) <- pmf.toSeq;
                      felem <- f(elem)) yield Weighted(felem, wt)
    FiniteDistributionSet(Weighted.flatten(newpmf).toSet, epsilon)
  }

  def invmap[S](f: S => T, support: Traversable[S]) = {
    val pmf = support.toSet map ((s: S) => Weighted(s, apply(f(s))))
    FiniteDistributionSet(pmf)
  }

  def invmapOpt[S](f: S => Option[T], support: Traversable[S]) = {
    val pmf =
      support.toSet map ((s: S) => Weighted(s, f(s).map(apply).getOrElse(0)))
    FiniteDistributionSet(pmf)
  }

  /** quotient by an equivalence relation
    *
    */
  def quotient(equiv: (T, T) => Boolean) = {
    val supp = transversal(support.toList, equiv)
    val quotpmf = for (x <- supp) yield Weighted(x, prob(equiv(x, _)))
    FiniteDistributionSet(quotpmf.toSet, epsilon)
  }
}

case class FiniteDistribution[T](pmf: Vector[Weighted[T]])
    extends AnyVal
    with GenFiniteDistribution[T] {

  def flatten: FiniteDistribution[T] =
    FiniteDistribution(Weighted.flatten(pmf).toVector)

  def sort: FiniteDistribution[T] =
    FiniteDistribution(pmf.sortBy((wt) => 1 - wt.weight))

  override def supp: Vector[T] = (Weighted.flatten(pmf) map (_.elem)).toVector

//  lazy val decryFlat = FiniteDistribution(pmf, true, epsilon)

  def getsum(label: T) = (pmf filter (_.elem == label) map (_.weight)).sum

  def normalized(t: Double = 0.0): FiniteDistribution[T] =
    FiniteDistribution((posmf(t) map (_.scale(1.0 / postotal(t)))).toVector)

  def prunedPMF(epsilon: Double) =
    flatten.pmf filter ((x) => math.abs(x.weight) > epsilon)

  def pruneMap[S](f: => (T => S), epsilon: Double) = {
    if (prunedPMF(epsilon).isEmpty) FiniteDistribution.empty[S]
    else FiniteDistribution(prunedPMF(epsilon)) map (f)
  }

  def filter(p: T => Boolean) =
    FiniteDistribution(pmf filter (wt => p(wt.elem)))

  def *(sc: Double): FiniteDistribution[T] =
    FiniteDistribution(pmf map (_.scale(sc)))

  def ++(that: GenFiniteDistribution[T]): FiniteDistribution[T] = {
    FiniteDistribution(pmf ++ that.pmf)
  }

  def ++(that: FiniteDistribution[T]): FiniteDistribution[T] = {
    FiniteDistribution(pmf ++ that.pmf)
  }

  override def +(elem: T, prob: Double): FiniteDistribution[T] =
    this ++ FiniteDistribution(Vector(Weighted(elem, prob)))

  override def map[S](f: T => S): FiniteDistribution[S] = {
    val newpmf = for (Weighted(elem, wt) <- pmf) yield Weighted(f(elem), wt)
    FiniteDistribution(newpmf)
  }

  def mapOpt[S](f: T => Option[S]): FiniteDistribution[S] = {
    val newpmf = for (Weighted(elem, wt) <- pmf;
                      felem <- f(elem)) yield Weighted(felem, wt)
    FiniteDistribution(newpmf)
  }

  def invmap[S](f: S => T, support: Traversable[S]): FiniteDistribution[S] = {
    val mem = memo
    def memFn = (x: T) => mem.get(x).getOrElse(0.0)
    val pmf = support.toVector map ((s: S) => Weighted(s, memFn(f(s))))
    FiniteDistribution(pmf)
  }

  def invmapOpt[S](
      f: S => Option[T], support: Traversable[S]): FiniteDistribution[S] = {
    val mem = memo
    def memFn = (x: T) => mem.get(x).getOrElse(0.0)
    val pmf =
      support.toVector map ((s: S) =>
            Weighted(s, f(s).map(memFn).getOrElse(0)))
    FiniteDistribution(pmf)
  }

  def flatMap[S](f: T => FiniteDistribution[S]) = {
    implicit val ls = FiniteDistribution.FiniteDistVec[S]
    val distdist = map(f)
    distdist.expectation
  }

  def pickle =
    flatten.map((t: T) => t.toString).pmf.toList.map((w) => (w.elem, w.weight))

  def innerProduct(that: FiniteDistribution[T]) =
    (for (l <- support; fst <- get(l); scnd <- that.get(l)) yield
      fst * scnd).sum

  def dot(that: FiniteDistribution[T]) = innerProduct(that)

  private def preMemo: Map[T, Double] = {
    (for (Weighted(x, p) <- pmf) yield (x -> p)).toMap
  }

  def memo: Map[T, Double] = flatten.preMemo

  def total = (supp map (getsum)).sum

  /**
    * entropy feedback for the finite distribution to move in the direction of the base distribution,
    * however values outside support are ignored.
    * warning: should come after ++ to ensure implementation choice.
    *
    * @param baseweights
    */
  def rawfeedback(baseweights: T => Double,
                  damp: Double = 0.1,
                  strictness: Double = 1.0) = {
    val weights = (t: T) => math.pow(baseweights(t), strictness)
    val rawdiff = for (elem <- supp) yield
      (Weighted(elem, weights(elem) / (weights(elem) * damp + apply(elem))))
    val shift = rawdiff.map(_.weight).sum / (rawdiff.size)
    val normaldiff = for (Weighted(pres, prob) <- rawdiff) yield
      Weighted(pres, prob - shift)
    FiniteDistribution(normaldiff)
  }

  /**
    * entropy feedback for the finite distribution to move in the direction of the base distribution,
    * however values outside support are ignored.
    * smoothed to ensure at most proportional to the target probability
    * warning: should come after ++ to ensure implementation choice.
    *
    * @param baseweights
    */
  def smoothedFeedback(baseweights: T => Double,
                       damp: Double = 0.1,
                       strictness: Double = 1.0) = {
    val weights = (t: T) => math.pow(baseweights(t), strictness)
    val rawdiff = for (elem <- supp) yield
      (Weighted(elem,
                weights(elem) * weights(elem) / (weights(elem) * damp +
                    apply(elem))))
    val shift = rawdiff.map(_.weight).sum / (rawdiff.size)
    val normaldiff = for (Weighted(pres, prob) <- rawdiff) yield
      Weighted(pres, prob - shift)
    FiniteDistribution(normaldiff)
  }

  /**
    * gradient w.r.t. inner product scaled by presentation weights,
    * perpendicular to the gradient (w.r.t. same inner product) of the "total weight" function.
    */
  def KLfeedback(baseweights: T => Double, strictness: Double = 1.0) = {
    val weights = (t: T) => math.pow(baseweights(t), strictness)
    val rawdiff = for (elem <- supp) yield (Weighted(elem, 1.0 / apply(elem)))
    val innerprod = rawdiff.map((x) => 1.0 / x.weight).sum // Sum(1/q))
    val normsq = rawdiff.map((x) => 1.0 / weights(x.elem)).sum // Sum (1/p)
    val normaldiff = for (Weighted(pres, prob) <- rawdiff) yield
      Weighted(pres, prob - ((1 / weights(pres)) * innerprod / normsq))
    FiniteDistribution(normaldiff)
  }
}
case class FiniteDistributionParVec[T](parpmf: ParVector[Weighted[T]],
                                       injective: Boolean = false,
                                       epsilon: Double = 0.0)
    extends GenFiniteDistribution[T] {

  val pmf = parpmf.seq

  def flatten =
    FiniteDistributionParVec(Weighted.flatten(pmf).toVector.par, true, epsilon)

  lazy val decryFlat = FiniteDistributionParVec(parpmf, true, epsilon)

  def getsum(label: T) = (parpmf filter (_.elem == label) map (_.weight)).sum

  def normalized(t: Double = 0.0) =
    FiniteDistributionParVec(
        (posmf(t) map (_.scale(1.0 / postotal(t)))).toVector.par)

  def filter(p: T => Boolean) =
    FiniteDistributionParVec(parpmf filter (wt => p(wt.elem)))

  def *(sc: Double) = FiniteDistributionParVec(parpmf map (_.scale(sc)))

  def ++(that: GenFiniteDistribution[T]) = {
    FiniteDistributionParVec(parpmf ++ (that.pmf.toVector.par), false, epsilon)
  }

  override def map[S](f: T => S) = {
    val newpmf = for (Weighted(elem, wt) <- parpmf) yield Weighted(f(elem), wt)
    FiniteDistributionParVec(newpmf, false, epsilon)
  }

  def mapOpt[S](f: T => Option[S]) = {
    val newpmf = for (Weighted(elem, wt) <- parpmf;
                      felem <- f(elem)) yield Weighted(felem, wt)
    FiniteDistributionParVec(newpmf, false, epsilon)
  }

  def invmap[S](f: S => T, support: Traversable[S]) = {
    val pmf = support.toVector.par map ((s: S) => Weighted(s, apply(f(s))))
    FiniteDistributionParVec(pmf)
  }

  def invmapOpt[S](f: S => Option[T], support: Traversable[S]) = {
    val pmf =
      support.toVector.par map ((s: S) =>
            Weighted(s, f(s).map(apply).getOrElse(0)))
    FiniteDistributionParVec(pmf)
  }
}
