package provingground

import breeze.linalg.{Vector => BVector, _}
import breeze.stats.distributions._

object Sampler{
  def total[A](x: Map[A, Int]) = x.values.sum

  def combine[A](x : Map[A, Int], y: Map[A, Int]) = {
    val supp =x.keySet union (y.keySet)
    (supp map ((a) => (a, x(a) + y(a)))).toMap
  }

  def collapse[A](ps: Vector[(A, Int)]) =
    (ps.groupBy(_._1) mapValues((x) => total(x.toMap))).toMap

  def combineAll[A](xs: Vector[Map[A, Int]]) = {
    collapse((xs map (_.toVector)).flatten)
  }

  def fromPMF[A](pmf: Vector[Weighted[A]], size: Int) : Map[A, Int] = {
    val vec = pmf map (_.elem)
    val probDenseVector = DenseVector((pmf map (_.weight)).toArray)
    val mult = Multinomial(probDenseVector)
    val nums = mult.sample(size).zipWithIndex
    (for ((n, j) <- nums) yield (vec(j) -> n)).toMap
  }



import ProbabilityDistribution._

  def sample[A](pd: ProbabilityDistribution[A], n: Int) : Map[A, Int] = pd match {
    case FiniteDistribution(pmf) => fromPMF(pmf, n)

    case mx: Mixin[u] =>
      val m = Binomial(n, mx.q).draw
      combine(sample(mx.first, n -m), sample(mx.second, m))

    case mx: MixinOpt[u] =>
        val m = Binomial(n, mx.q).draw
        val optSample = sample(mx.second, m)
        val secondSample = for ((xo, n) <- optSample; x <- xo) yield (x, n)
        combine(sample(mx.first, n -m), secondSample)

    case mx: Mixture[u] =>
      val mult = Multinomial(DenseVector(mx.ps.toArray))
      val sampSizes = (mult.sample(n)).zipWithIndex
      val polySamp = (for ((m, j) <- sampSizes) yield sample(mx.dists(j), m))
      combineAll(polySamp.toVector)

    case Mapped(base, f) =>
      collapse((sample(base, n) map {case (x, n) => (f(x), n)}).toVector)

    case FlatMapped(base, f) =>
      val baseSamp = sample(base, n)
      val sampsVec = (for ((a, m) <- baseSamp) yield sample(f(a), m)).toVector
      combineAll(sampsVec)
  }


}
