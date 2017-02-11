package provingground

import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD}

import breeze.linalg.{Vector => _, _}
import breeze.stats.distributions._

import breeze.plot._

import scala.concurrent._

import scala.util.{Try, Random}

import scala.concurrent.ExecutionContext.Implicits.global

object Sampler {
  val rand = new Random

  def total[A](x: Vector[(A, Int)]) = (x map (_._2)).sum

  def combine[A](x: Map[A, Int], y: Map[A, Int]) = {
    val supp = x.keySet union (y.keySet)
    (supp map ((a) => (a, x.getOrElse(a, 0) + y.getOrElse(a, 0)))).toMap
  }

  def collapse[A](ps: Vector[(A, Int)]) =
    (ps.groupBy(_._1) mapValues ((x) => total(x))).toMap

  def combineAll[A](xs: Vector[Map[A, Int]]) = {
    collapse((xs map (_.toVector)).flatten)
  }

  def fromPMF[A](pmf: Vector[Weighted[A]], size: Int): Map[A, Int] =
    if ((pmf map (_.weight)).sum >0)
    {
    val vec = pmf map (_.elem)
    val ps = pmf map (_.weight)
    getMultinomial(vec, ps, size)
  }
  else Map()

  def getMultinomial[A](xs: Vector[A], ps: Vector[Double], size: Int) = {
    val mult = Multinomial(DenseVector(ps.toArray))
    val samp = mult.sample(size).groupBy(identity).mapValues(_.size)
    samp map { case (j, m) => (xs(j), m) }
  }

  def toFD[A](sample: Map[A, Int]) = {
    val tot = total(sample.toVector)
    FiniteDistribution(
        sample.toVector map {
      case (x, n) => Weighted(x, n.toDouble / tot)
    })
  }

  def linear[A](m: Map[A, Int]) =
    m.toVector flatMap{
      case (a, n) => Vector.fill(n)(a)
    }

  def grouped[A](vec: Vector[A]) =
    vec.groupBy(identity) mapValues (_.size)

  import ProbabilityDistribution._

  def sample[A](pd: ProbabilityDistribution[A], n: Int): Map[A, Int] =
    if (n < 1) Map()
    else
      pd match {
        case FiniteDistribution(pmf) => fromPMF(pmf, n)

        case mx: Mixin[u] =>
          val m = Binomial(n, mx.q).draw
          combine(sample(mx.first, n - m), sample(mx.second, m))

        case mx: MixinOpt[u] =>
          val m = Binomial(n, mx.q).draw
          val optSample = Try(sample(mx.second, m)).getOrElse(Map(None -> 1))
          val secondSample = for ((xo, n) <- optSample; x <- xo) yield (x, n)
          combine(
              sample(mx.first, n - total(secondSample.toVector)), secondSample)

        case mx: Mixture[u] =>
          val sampSizes = getMultinomial(mx.dists, mx.ps, n)
          val polySamp = (for ((d, m) <- sampSizes) yield sample(d, m))
          combineAll(polySamp.toVector)

        case Mapped(base, f) =>
          collapse((sample(base, n) map { case (x, n) => (f(x), n) }).toVector)

        case FlatMapped(base, f) =>
          val baseSamp = sample(base, n)
          val sampsVec = (for ((a, m) <- baseSamp) yield
            sample(f(a), m)).toVector
          combineAll(sampsVec)

        case Product(first, second) =>
          val firstSamp = sample(first, n)
          val secondSamp = sample(second, n)
          grouped(rand.shuffle(linear(firstSamp)).zip(linear(secondSamp)))

        case fp: FiberProduct[u, q, v] =>
          import fp._
          val baseSamp = sample(base, n)
          val groups = baseSamp groupBy { case (x, n) => quotient(x)}
          val sampVec =
            groups.keys.toVector.flatMap {
              (a) =>
                val size = groups(a).values.sum
                val fiberSamp = sample(fibers(a), size)
                rand.shuffle(linear(groups(a))).zip(linear(fiberSamp))
              }
          grouped(sampVec)


        case Conditioned(base, p) =>
          val firstSamp = sample(base, n) filter { case (a, n) => p(a) }
          val tot = firstSamp.values.sum
          if (tot == 0) Map()
          else if (tot == n) firstSamp
          else {
            val xs = firstSamp.keys.toVector
            val ps = xs map ((a) => firstSamp(a).toDouble / tot)
            getMultinomial(xs, ps, n)
          }
      }
}

import HoTT._

object TermSampler {
  import Sampler._

  lazy val fig = Figure("Term Sample")

  lazy val entPlot = fig.subplot(0)

  import java.awt.Color

  def plotEntsThms(thms: ThmEntropies) = {
    val X = DenseVector((thms.entropyPairs map (_._2._1)).toArray)
    val Y = DenseVector((thms.entropyPairs map (_._2._2)).toArray)
    val names = (n: Int) => thms.entropyPairs(n)._1.toString
    val colours = (n: Int) => if (X(n) < Y(n)) Color.RED else Color.BLUE
    entPlot += scatter(X, Y, (_) => 0.1, colors = colours, tips = names)
    entPlot.xlabel = "statement entropy"
    entPlot.ylabel = "proof entropy"
  }

  def plotEnts(sample: Map[Term, Int]) = plotEntsThms(thmEntropies(sample))

  def plotEnts(fd: FiniteDistribution[Term]) = plotEntsThms(ThmEntropies(fd))

  def thmEntropies(sample: Map[Term, Int]) = ThmEntropies(toFD(sample))

  def thmEntropies(sample: Map[Term, Int], d: BasicDeducer) =
    ThmEntropies(toFD(sample), d.vars, d.lambdaWeight)
}

class TermSampler(d: BasicDeducer) {
  import Sampler._
  import TermSampler._

  def flow(sampleSize: Int,
           derSampleSize: Int,
           epsilon: Double,
           sc: Double,
           inertia: Double): FD[Term] => FD[Term] =
    (p: FD[Term]) =>
      NextSample(p, sampleSize, derSampleSize, sc, epsilon, inertia).shiftedFD(
          derSampleSize, epsilon)

  def iterator(init: FD[Term],
               sampleSize: Int,
               derSampleSize: Int,
               epsilon: Double,
               sc: Double,
               inertia: Double) =
    Iterator.iterate(init)(
        flow(sampleSize, derSampleSize, epsilon, sc, inertia))

  def loggedIterator(init: FD[Term],
                     sampleSize: Int,
                     derSampleSize: Int,
                     epsilon: Double,
                     sc: Double,
                     inertia: Double) =
    Iterator.iterate(
        NextSample(init, sampleSize, derSampleSize, sc, epsilon, inertia)
    )((ns) => ns.succ)

  var live: Boolean = true
  def stop() = { live = false }

  def loggedBuffer(init: FD[Term],
                   sampleSize: Int,
                   derSampleSize: Int,
                   epsilon: Double,
                   sc: Double,
                   inertia: Double) = {
    val it = loggedIterator(init,
                            sampleSize: scala.Int,
                            derSampleSize: scala.Int,
                            epsilon: scala.Double,
                            sc: scala.Double,
                            inertia: scala.Double).takeWhile((_) => live)
    val buf = scala.collection.mutable.ArrayBuffer[NextSample]()
    Future {
      it.foreach((ns) => buf.append(ns))
    }
    buf
  }

  case class NextSample(p: FD[Term],
                        size: Int,
                        derTotalSize: Int,
                        sc: Double,
                        epsilon: Double,
                        inertia: Double) {
    lazy val init = d.hFunc(sc)(p)

    lazy val nextSamp = sample(init, size)

    lazy val nextFD = toFD(nextSamp) * (1.0 - inertia) ++ (p * inertia)

    def plotEntropies = plotEnts(nextFD)

    lazy val thmEntropies = ThmEntropies(nextFD, d.vars, d.lambdaWeight)

    def derivativePD(tang: PD[Term]): PD[Term] = d.hDerFunc(sc)(nextFD)(tang)

    /**
      * Sample sizes for tangents at atomic vectors
      */
    lazy val derSamplesSizes = sample(nextFD, derTotalSize)

    /**
      * Finite distributions as derivatives at nextFD of the atomic tangent vectors with chosen sample sizes.
      */
    lazy val derFDs =
      derSamplesSizes map {
        case (x, n) =>
          val tang = FD.unif(x) //tangent vecror, atom at `x`
          val dPD =
            d.hDerFunc(sc)(nextFD)(tang) //recursive distribution based on derivative for sampling
          val samp = sample(dPD, n)
          x -> toFD(samp)
      }

    lazy val feedBacks =
      derFDs map {
        case (x, tfd) =>
          x -> thmEntropies.feedbackTermDist(tfd)
      }

    def derivativeFD(p: PD[Term], n: Int) = toFD(sample(derivativePD(p), n))

    def vecFlow(vec: PD[Term], n: Int) =
      thmEntropies.feedbackTermDist(derivativeFD(p, n))

    def termFlow(x: Term, n: Int) = vecFlow(FD.unif(x), n)

    def totalFlow(totalSize: Int): Map[Term, Double] =
      (sample(nextFD, totalSize) map {
            case (x, n) =>
              val flow = termFlow(x, n)
              x -> flow
          }).toMap

    def shiftedFD(totalSize: Int, epsilon: Double) = {
      val tf = feedBacks // totalFlow(totalSize)
      val shift = (x: Term) => tf.getOrElse(x, 0.0)

      val pmf =
        nextFD.pmf map {
          case Weighted(x, p) =>
            Weighted(x, p * math.exp(shift(x) * epsilon))
        }

      FD(pmf).flatten.normalized()
    }

    lazy val succFD = shiftedFD(derTotalSize, epsilon)

    lazy val succ = this.copy(p = succFD)
  }
}
