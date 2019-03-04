package provingground.experiments

import spire.math._
import spire.algebra._
import spire.implicits._

class FieldGeomDist[F](N: Int = 100, p: Double = 0.5)(implicit field: Field[F],
                                                      trig: Trig[F]) {
  val minusOne: F = field.negate(field.one)

  val q = 1 - p

  def sumErr(prob: Vector[F]) = (prob.foldRight[F](minusOne)(_ + _)).pow(2)

  def recErrTerms(prob: Vector[F]): Vector[F] = {
    val prevMul = prob.init.map { (x) =>
      x * q
    }
    prob.tail.zip(prevMul).map {
      case (a, b) => ((a - b) / (a + b)).pow(2)
    }
  }

  def totalError(prob: Vector[F]) =
    recErrTerms(prob).fold(sumErr(prob))(_ + _)

  def entropy(logProb: Vector[F]) =
    totalError(logProb.map((x) => exp(-x)))

}

class JetGeomDist(N: Int = 100, p: Double = 0.5) {
  implicit val dim = JetDim(N)

  object DistErr extends FieldGeomDist[Jet[Double]](N, p) {
    def tangent(prob: Vector[Double]) =
      prob.zipWithIndex.map {
        case (x, n) => x + Jet.h[Double](n)
      }

    def shifted(prob: Vector[Double], epsilon: Double = 0.1) = {
      val shift =
        totalError(tangent(prob)).infinitesimal.toVector
      prob.zip(shift).map {
        case (x, t) => x - (epsilon * t)
      }
    }

    def logShifted(logProb: Vector[Double], epsilon: Double = 0.1) = {
      val shift =
        entropy(tangent(logProb)).infinitesimal.toVector
      logProb.zip(shift).map {
        case (x, t) => x - (epsilon * t)
      }
    }

    @annotation.tailrec
    def flowed(prob: Vector[Double],
               steps: Int = 10,
               epsilon: Double = 0.1): Vector[Double] =
      if (steps < 1) prob
      else flowed(shifted(prob, epsilon), steps - 1, epsilon)

    @annotation.tailrec
    def logFlowed(logProb: Vector[Double],
                  steps: Int = 10,
                  epsilon: Double = 0.1): Vector[Double] =
      if (steps < 1) logProb
      else logFlowed(logShifted(logProb, epsilon), steps - 1, epsilon)

    def entropyFlowed(prob: Vector[Double],
                      steps: Int = 10,
                      epsilon: Double = 0.1): Vector[Double] =
      logFlowed(prob.map((x) => -log(x)), steps, epsilon).map((x) => exp(-x))

  }

}

object JetGeomDist {
  def distErr(N: Int = 2, p: Double = 0.5) = new JetGeomDist(N, p).DistErr
}
