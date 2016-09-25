package provingground

import breeze.linalg.{Vector => BVector, _}
import breeze.stats.distributions._

object Sampler{
  def total[A](x: Vector[(A, Int)]) = (x map (_._2)).sum

  def combine[A](x : Map[A, Int], y: Map[A, Int]) = {
    val supp =x.keySet union (y.keySet)
    (supp map ((a) => (a, x.getOrElse(a, 0) + y.getOrElse(a, 0)))).toMap
  }

  def collapse[A](ps: Vector[(A, Int)]) =
    (ps.groupBy(_._1) mapValues((x) => total(x))).toMap

  def combineAll[A](xs: Vector[Map[A, Int]]) = {
    collapse((xs map (_.toVector)).flatten)
  }

  def fromPMF[A](pmf: Vector[Weighted[A]], size: Int) : Map[A, Int] = {
    val vec = pmf map (_.elem)
    val ps = pmf map (_.weight)
    getMultinomial(vec, ps, size)
  }

  def getMultinomial[A](xs: Vector[A], ps: Vector[Double], size: Int) = {
    val mult = Multinomial(DenseVector(ps.toArray))
    val samp = mult.sample(size).groupBy(identity).mapValues(_.size)
    samp map {case (j, m) => (xs(j), m)}
  }



import ProbabilityDistribution._

  def sample[A](pd: ProbabilityDistribution[A], n: Int) : Map[A, Int] =
    if (n < 1) Map ()
    else pd match {
    case FiniteDistribution(pmf) => fromPMF(pmf, n)

    case mx: Mixin[u] =>
      val m = Binomial(n, mx.q).draw
      combine(sample(mx.first, n -m), sample(mx.second, m))

    case mx: MixinOpt[u] =>
        val m = Binomial(n, mx.q).draw
        val optSample = sample(mx.second, m)
        val secondSample = for ((xo, n) <- optSample; x <- xo) yield (x, n)
        combine(sample(mx.first, n - total(secondSample.toVector)), secondSample)

    case mx: Mixture[u] =>
      val sampSizes = getMultinomial(mx.dists, mx.ps, n)
      val polySamp = (for ((d, m) <- sampSizes) yield sample(d, m))
      combineAll(polySamp.toVector)

    case Mapped(base, f) =>
      collapse((sample(base, n) map {case (x, n) => (f(x), n)}).toVector)

    case FlatMapped(base, f) =>
      val baseSamp = sample(base, n)
      val sampsVec = (for ((a, m) <- baseSamp) yield sample(f(a), m)).toVector
      combineAll(sampsVec)

    case genFD : GenFiniteDistribution[u] =>
      fromPMF(genFD.pmf.toVector, n)
  }


}
