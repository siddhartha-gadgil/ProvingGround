package provingground

import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD}

import breeze.linalg.{Vector => _, _}
import breeze.stats.distributions._

import breeze.plot._

import scala.concurrent._

import scala.util.Try

import scala.concurrent.ExecutionContext.Implicits.global

import HoTT._

import FineDeducer._

case class NextSample(ded : FineDeducer,
              p: FD[Term],
              vars: Vector[Term],
                      size: Int,
                      derTotalSize: Int,
                      epsilon: Double,
                      inertia: Double) {
  lazy val init = ded.evolve(p)

  import Sampler._

  lazy val nextSamp = sample(init, size)

  lazy val nextFD = toFD(nextSamp) * (1.0 - inertia) ++ (p * inertia)

  // def plotEntropies = plotEnts(nextFD)

  lazy val thmFeedback = TheoremFeedback(nextFD, vars)

  def derivativePD(tang: FD[Term]): PD[Term] = ded.Devolve(nextFD, tang)

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
          ded.Devolve(nextFD, tang) //recursive distribution based on derivative for sampling
        val samp = sample(dPD, n)
        x -> toFD(samp)
    }

  lazy val feedBacks =
    derFDs map {
      case (x, tfd) =>
        x -> thmFeedback.feedbackTermDist(tfd)
    }

  def derivativeFD(p: FD[Term], n: Int) = toFD(sample(derivativePD(p), n))

  def vecFlow(vec: FD[Term], n: Int) =
    thmFeedback.feedbackTermDist(derivativeFD(p, n))

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
