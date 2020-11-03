package provingground.learning
import provingground._, HoTT._

import spire.algebra._
import spire.math._
import spire.implicits._
import ExpressionEval._, ExprCalc._, ExprEquations._

import scala.collection.parallel._, immutable.ParVector

class SpireExprEquations(
    ev: ExpressionEval,
    initMap: Map[Expression, Double],
    equationSet: Set[Equation],
    params: TermGenParams
) extends ExprEquations(ev, initMap, equationSet, params) {
  implicit val jetDim: JetDim = JetDim(size)

  def sigmoid(x: Jet[Double]): Jet[Double] = exp(x) / (1 + exp(x))

  def indexJet(n: Int, v: collection.parallel.ParSeq[Double]) =
    sigmoid(v(n) + Jet.h[Double](n))

  def prodJet(
      prod: ProdExpr,
      v: collection.parallel.ParSeq[Double]
  ): Jet[Double] = {
    val num = prod.indices
      .map { n =>
        indexJet(n, v)
      }
      .fold(prod.constant: Jet[Double])(_ * _)
    prod.negIndices.map(n => 1 / indexJet(n, v)).fold(num)(_ * _)
  }

  def sumJet(sum: SumExpr, v: collection.parallel.ParSeq[Double]): Jet[Double] =
    sum.terms.map(prodJet(_, v)).fold(0: Jet[Double])(_ + _)

  def matchEquationsJet(
      v: collection.parallel.ParSeq[Double]
  ): ParVector[Jet[Double]] =
    rhsExprsPar.zipWithIndex.map { rhsN: (SumExpr, Int) =>
      val (rhs, n) = rhsN
      sumJet(rhs, v) - indexJet(n, v)
    }

  def scaledMatchEquationsJet(
      v: collection.parallel.ParSeq[Double]
  ): ParVector[Jet[Double]] =
    rhsExprsPar.zipWithIndex.map { rhsN: (SumExpr, Int) =>
      val (rhs, n) = rhsN
      val lhsJet   = indexJet(n, v)
      val rhsJet   = sumJet(rhs, v)
      (rhsJet - lhsJet) / (rhsJet + lhsJet)
    }

  def totalProbEqnsJet(
      v: collection.parallel.ParSeq[Double]
  ): ParVector[Jet[Double]] =
    randomVarIndices.map { gp =>
      val s = gp.toSet
      val terms = (0 until (size)).map { n =>
        if (s.contains(n)) indexJet(n, v) else (0: Jet[Double])
      }
      terms.fold(-1: Jet[Double])(_ + _)
    }

  def initTermsEntropy(v: collection.parallel.ParSeq[Double]): Jet[Double] =
    initTermIndices
      .map { j =>
        val p = indexJet(j, v)
        log(p) * p * (-1)
      }
      .fold(0: Jet[Double])(_ + _)

  def initTypsEntropy(v: collection.parallel.ParSeq[Double]): Jet[Double] =
    initTermIndices
      .map { j =>
        val p = indexJet(j, v)
        log(p) * p * (-1)
      }
      .fold(0: Jet[Double])(_ + _)

  def thmPfsJet(
      v: collection.parallel.ParSeq[Double]
  ): Vector[(Jet[Double], Jet[Double])] =
    thmPfIndices.toVector.map {
      case (thm, pfs) =>
        (
          indexJet(thm, v),
          pfs.map(j => indexJet(j, v)).fold(0: Jet[Double])(_ + _)
        )
    }

  def thmPfsKL(v: collection.parallel.ParSeq[Double]): Jet[Double] =
    thmPfsJet(v)
      .map {
        case (p, q) => p * log(p / q)
      }
      .fold(0: Jet[Double])(_ + _)

  def thmPfsFlattenedKL(
      v: collection.parallel.ParSeq[Double],
      exponent: Double // for instance 1/2 - both distributions are flattened to reduce importance of a few high-probability mismatches.
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

}
