package provingground.learning
import provingground._, HoTT._

import spire.algebra._
import spire.math._
import spire.implicits._
import ExpressionEval._, ExprCalc._, ExprEquations._

import scala.collection.parallel._, immutable.ParVector, collection.parallel
import GeneratorVariables._, TermRandomVars._

object SpireExprEquations {
  def toProb(x: Double) = exp(x) / (1 + exp(x))
}

class SpireExprEquations(
    initMap: Map[Expression, Double], // values specified and frozen
    equationSet: Set[Equation],
    params: TermGenParams,
    initVariables: Vector[Expression] = Vector() // values that can evolve
) extends ExprEquations(initMap, equationSet, params.coeffVal(_), initVariables) {
  import SpireExprEquations._

  // val numVars = size + initVariables.size

  implicit val jetDim: JetDim = JetDim(numVars)

  def sigmoid(x: Jet[Double]): Jet[Double] = exp(x) / (1 + exp(x))

  def indexJet(n: Int, v: parallel.ParSeq[Double]) = {
    // val x = v(n)
    // val arr = Array.fill(numVars)(0.0).updated(n, 1.0/(1.0 + exp(x)))
    // val jet =  new Jet(1.0/(1.0 + exp(-x)), arr)
    // if (jet.real.isNaN()) {
    //   pprint.log(n)
    //   pprint.log(x)
    //   pprint.log(jet.real)
    // }
    // jet
    sigmoid(v(n) + Jet.h[Double](n))
  }
    

  def prodJet(
      prod: ProdExpr,
      v: parallel.ParSeq[Double]
  ): Jet[Double] = {
    val num = prod.indices
      .map { n =>
        indexJet(n, v)
      }
      .fold(prod.constant: Jet[Double])(_ * _)
    prod.negIndices.map(n => 1 / indexJet(n, v)).fold(num)(_ * _)
  }

  def sumJet(sum: SumExpr, v: parallel.ParSeq[Double]): Jet[Double] =
    sum.terms.map(prodJet(_, v)).fold(0: Jet[Double])(_ + _)

  def matchEquationsJet(
      v: parallel.ParSeq[Double]
  ): ParVector[Jet[Double]] =
    rhsExprsPar.zipWithIndex.map { rhsN: (SumExpr, Int) =>
      val (rhs, n) = rhsN
      sumJet(rhs, v) - indexJet(n, v)
    }

  def scaledMatchEquationsJet(
      v: parallel.ParSeq[Double]
  ): ParVector[Jet[Double]] =
    rhsExprsPar.zipWithIndex.map { rhsN: (SumExpr, Int) =>
      val (rhs, n) = rhsN
      val lhsJet   = indexJet(n, v)
      val rhsJet   = sumJet(rhs, v)
      (rhsJet - lhsJet) / (rhsJet + lhsJet)
    }

  def totalProbEqnsJet(
      v: parallel.ParSeq[Double]
  ): ParVector[Jet[Double]] =
    randomVarIndices.map { gp =>
      val terms = gp.map { n =>
        indexJet(n, v)
      }
      terms.fold(-1: Jet[Double])(_ + _)
    }

  // gradient is also returned to allow flowing towards this
  def equationsOrthonormal(
      v: parallel.ParSeq[Double]
  ): (ParVector[ParVector[Double]], Jet[Double]) = {
    val eqns = (scaledMatchEquationsJet(v) ++ totalProbEqnsJet(v))
    val sse  = eqns.map(err => err * err).fold(0: Jet[Double])(_ + _)
    ParGramSchmidt.orthonormalize(eqns.map(_.infinitesimal.to(ParVector))) -> sse
  }

  def initTermsEntropy(v: parallel.ParSeq[Double]): Jet[Double] =
    initTermIndices
      .map { j =>
        val p = indexJet(j, v)
        log(p) * p * (-1)
      }
      .fold(0: Jet[Double])(_ + _)

  def initTypsEntropy(v: parallel.ParSeq[Double]): Jet[Double] =
    initTermIndices
      .map { j =>
        val p = indexJet(j, v)
        log(p) * p * (-1)
      }
      .fold(0: Jet[Double])(_ + _)

  def thmPfsJet(
      v: parallel.ParSeq[Double]
  ): Vector[(Jet[Double], Jet[Double])] =
    thmPfIndices.toVector.map {
      case (thm, pfs) =>
        (
          indexJet(thm, v),
          pfs.map(j => indexJet(j, v)).fold(0: Jet[Double])(_ + _)
        )
    }

  def thmPfsKL(v: parallel.ParSeq[Double]): Jet[Double] =
    thmPfsJet(v)
      .map {
        case (p, q) => p * log(p / q)
      }
      .fold(0: Jet[Double])(_ + _)

  def thmPfsFlattenedKL(
      v: parallel.ParSeq[Double],
      exponent: Double // for instance 0.5 - both distributions are flattened to reduce importance of a few high-probability mismatches.
  ): Jet[Double] = {
    val thmPfs     = thmPfsJet(v)
    val powerTotal = thmPfs.map(_._1.pow(exponent)).fold(0: Jet[Double])(_ + _)
    thmPfs
      .map {
        case (p, q) =>
          val p1 = p.pow(exponent) / powerTotal
          p1 * log(p / q)
      }
      .fold(0: Jet[Double])(_ + _)
  }

  def normalizedInitErrors(
      initVals: Map[Int, Double],
      v: parallel.ParSeq[Double]
  ) =
    initVals.toVector.map {
      case (j, c) =>
        val p = indexJet(j, v)
        (p - c) / (p + c)
    }

  def sseInitJet(
      initVals: Map[Int, Double],
      v: parallel.ParSeq[Double]
  ): Jet[Double] =
    (normalizedInitErrors(initVals, v) ++ scaledMatchEquationsJet(v) ++ totalProbEqnsJet(
      v
    )).map(err => err * err).fold(0: Jet[Double])(_ + _)

  // iterators mainly as demos; we may want more parameters etc and also use monix Iterant instead
  def initIterator(
      initVals: Map[Int, Double] = Map(),
      seed: ParVector[Double] = ParVector.fill(numVars)(0.0),
      scale: Double = 0.1
  ): Iterator[(ParVector[Double], Double)] = {
    def nextStep(v: ParVector[Double]): (ParVector[Double], Double) = {
      val sse   = sseInitJet(initVals, v)
      val shift = sse.infinitesimal.to(ParVector)
      v.zip(shift).map {
        case (current, derivative) => (current - (derivative * scale))
      } -> sse.real
    }
    Iterator.iterate((seed, 0.0)) { case (v, _) => nextStep(v) }
  }

  def entropyPerpIterator(
      seed: ParVector[Double],
      scale: Double
  ): Iterator[ParVector[Double]] = {
    def nextStep(v: ParVector[Double]): ParVector[Double] = {
      val (onb, eqnsShift) = equationsOrthonormal(v)
      val entJet           = initTermsEntropy(v) + initTypsEntropy(v) + thmPfsKL(v)
      val shift =
        ParGramSchmidt.makePerpFromON(onb, entJet.infinitesimal.to(ParVector))
      v.zip(shift).zip(eqnsShift.infinitesimal.to(ParVector)).map {
        case ((current, derivative), correction) =>
          current - ((derivative + correction) * scale)
      }
    }
    Iterator.iterate(seed)(nextStep(_))
  }

  def termDist(v: ParVector[Double]) =
    FiniteDistribution(termIndexVec.map {
      case (x, n) => Weighted(x, toProb(v(n)))
    })

  def typDist(v: ParVector[Double]) =
    FiniteDistribution(typIndexVec.map {
      case (x, n) => Weighted(x, toProb(v(n)))
    })

}
