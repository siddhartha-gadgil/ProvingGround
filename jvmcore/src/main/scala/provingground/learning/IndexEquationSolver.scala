package provingground.learning

import provingground._, HoTT._, JvmUtils.logger

import spire.implicits._

import GeneratorVariables._, TermRandomVars._, Expression._

import annotation.tailrec
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable._
import scala.collection.immutable.Nil
import scala.collection.mutable
import ExpressionEquationIndexifier.vecSum
object IndexEquationSolver {
  def getGenerators(
      exps: List[Expression]
  ): Option[(Set[Term], Set[Typ[Term]])] = exps match {
    case FinalVal(Elem(x, v)) :: next =>
      getGenerators(next).flatMap {
        case (tailTerms, tailTyps) =>
          (x, v) match {
            case (typ: Typ[u], Typs) => Some((tailTerms, tailTyps + typ))
            case (t: Term, _)        => Some((tailTerms + t, tailTyps))
            case (fn: ExstFunc, _)   => Some((tailTerms + fn.func, tailTyps))
            case _                   => None
          }
      }
    case Nil => Some((Set(), Set()))
    case _   => None
  }

  def generatorSets(
      traces: Set[Set[Expression]]
  ): Set[(Set[HoTT.Term], Set[HoTT.Typ[HoTT.Term]])] =
    traces.flatMap(s => getGenerators(s.toList))
}

/**
  * Seek an approximately stable distribution for equations given in terms of indices, hence solving the equations (approximately).
  * Here stable distribution is under the iteration to solve equations viewing them as fixed points.
  * A few quantitites related to the solutions, or the equations themselves, are also computed.
  * 
  *
  * @param initMap initial values of some expressions
  * @param equationSet set of equations in expressions, to be transformed to index based ones
  * @param params values of coefficients in the equations
  * @param maxRatio bound on ratio of stabilization
  * @param resolution additive scale for stabilization and some auxiliary quantities
  * @param exponent the next step is stabilized using geometric means, weighted by the exponent
  * @param decay decay of the exponent, to allow steps to be damped progressively more
  * @param maxTime time in milliseconds for timing out stabilizations
  * @param previousMap an optional previous map to initialize
  */
class IndexEquationSolver(
    initMap: Map[Expression, Double],
    equationVec: Vector[Equation],
    params: Coeff[_] => Option[Double],
    maxRatio: Double,
    resolution: Double,
    exponent: Double,
    decay: Double,
    maxTime: Option[Long],
    previousMap: Option[Map[Expression, Double]]
) extends ExpressionEquationIndexifier(initMap, equationVec, params, Vector()) {

  def restrict(
      v: Vector[Double],
      indices: Vector[Int]
  ): Vector[Double] = {
    val base = indices.map { j =>
      v(j)
    }
    val total = base.sum
    if (total == 0) base else base.map(_ / total)
  }

  def nextVec(v: ParVector[Double], exponent: Double): ParVector[Double] = {
    // pprint.log(exponent)
    val fn: ((SumIndexExpression, Int)) => Double = {
      case (exp, j) =>
        val y = exp.eval(v)
        // if (y < 0)
        //   Logger.error(s"Equation with negative value: ${equationVec(j)}")
        val z = v(j)
        if (z > 0) {
          val gm = math.pow(z, 1 - exponent) * math.pow(y, exponent)
          if (gm.isNaN() && (!y.isNaN() & !z.isNaN()))
            logger.error(
              s"Geometric mean of $y and $z with exponent $exponent is not a number\nEquation with negative value: ${scala.util
                .Try(equationVec(j))}"
            )
          gm
        } else y
    }
    rhsExprs.zipWithIndex.par.map(fn)
  }

  def simpleNextVec(v: ParVector[Double]): ParVector[Double] = {
    val fn: ((SumIndexExpression, Int)) => Double = {
      case (exp, j) =>
        val y = exp.eval(v)
        val z = v(j)
        if (z > 0) z else y
    }
    val z = rhsExprs.zipWithIndex
    z.par.map(fn)
  }

  def equalSupport(v: Vector[Double], w: Vector[Double]) = {
    require(v.size == w.size)
    v.zip(w).forall { case (x, y) => (x == 0 && y == 0) || (x != 0 && y != 0) }
  }

  def ratioBounded(
      v: Vector[Double],
      w: Vector[Double],
      bound: Double = maxRatio
  ) = {
    val condition: (((Double, Double), Int)) => Boolean = {
      case ((x: Double, y: Double), j: Int) =>
        x == 0 || y == 0 || ((x / (y + resolution)) <= bound && y / (x + resolution) <= bound)
    }
    v.zip(w).zipWithIndex.forall(condition)
  }

  def normalizedBounded(v: Vector[Double], w: Vector[Double]) = {
    equalSupport(v, w) &&
    ratioBounded(restrict(v, termIndices), restrict(w, termIndices)) &&
    ratioBounded(restrict(v, typIndices), restrict(w, typIndices))
  }

  

  @tailrec
  final def stableVec(
      initVec: ParVector[Double],
      exponent: Double = 0.5,
      decay: Double,
      maxTime: Option[Long],
      steps: Long
  ): ParVector[Double] =
    if (maxTime.map(limit => limit < 0).getOrElse(false)) {
      logger.error(
        s"Timeout for stable vector after $steps steps; resolution $resolution"
      )
      initVec
    } else {
      if (steps % 100 == 2) logger.debug(s"completed $steps steps")
      val startTime  = System.currentTimeMillis()
      val newBaseVec = nextVec(initVec, exponent)
      val currSum = newBaseVec.sum
      val scale   = size.toDouble / currSum
      val newVec = newBaseVec.map(_ * scale)
      if (normalizedBounded(initVec.seq, newVec.seq))
        newVec
      else {
        val usedTime = System.currentTimeMillis() - startTime
        stableVec(
          newVec,
          exponent * decay,
          decay,
          maxTime.map(t => t - usedTime),
          steps + 1
        )
      }
    }

  @tailrec
  final def stableSupportVec(
      initVec: ParVector[Double],
      maxTime: Option[Long],
      steps: Long
  ): ParVector[Double] =
    if (maxTime.map(limit => limit < 0).getOrElse(false)) {
      logger.error(
        s"Timeout for stable support vector after $steps steps"
      )
      initVec
    } else {
      if (steps % 100 == 2)
        logger.debug(
          s"completed $steps steps without stable support, support size : ${initVec
            .count(_ > 0)}"
        )
      val startTime  = System.currentTimeMillis()
      val newBaseVec = simpleNextVec(initVec.par)
      val scale      = size.toDouble / newBaseVec.sum
      val newVec     = newBaseVec.map(_ * scale)
      val check = (0 until (initVec.size)).forall(
        n => (initVec(n) != 0) || (newVec(n) == 0)
      )
      if (check) {
        logger.debug(
          s"stable support with support size ${newVec.count(_ != 0)}"
        )
        newVec
      } else {
        // logger.debug("recursive call for stable support vector")
        val usedTime = System.currentTimeMillis() - startTime
        stableSupportVec(
          newVec,
          maxTime.map(t => t - usedTime),
          steps + 1
        )
      }
    }

  lazy val initVector: ParVector[Double] = previousMap
    .map { pm =>
      equationVec.zipWithIndex.par.map {
        case (equation, j) => pm.getOrElse(equation.lhs, 0.0)
      }
    }
    .getOrElse(ParVector.fill(equationVec.size)(0.0))

  /**
    * vector of values (at indices) in an approximately stable solution to the equation
    *
    * @return vector of approximate solution values
    */
  lazy val finalVec: ParVector[Double] = {
    logger.debug(
      s"Computing final vector, with maximum time $maxTime, exponent: $exponent, decay: $decay"
    )
    logger.debug(s"Number of equations: ${equationVec.size}")
    logger.debug(
      s"Computed initial vector with size: ${initVector.size}"
    ) // to avoid being part of time limit for stable vector
    val stableSupport =
      stableSupportVec(initVector, maxTime, 0L)
    logger.debug("Obtained vector with stable support")
    stableVec(
      stableSupport,
      exponent,
      decay,
      maxTime,
      0L
    )
  }

  /**
    * values of expressions in an approximately stable solution to the equation
    *
    * @return map of approximate solution values
    */
  lazy val finalMap: Map[Expression, Double] = {
    val fn: ((Double, Int)) => (Expression, Double) = {
      case (x, j) => equationVec(j).lhs -> x
    }
    finalVec.zipWithIndex.map(fn).toMap ++ initMap
  }.seq


  // various backward tracing methods

  def gradientStep(index: Int): Vector[(Int, Double)] = {
    val rhs = rhsExprs(index)
    val branches: Vector[Vector[(Int, Double)]] = rhs.terms.map { prod =>
      Vector.tabulate(prod.indices.size) { j =>
        val rest        = prod.indices.take(j) ++ prod.indices.drop(j + 1)
        val denominator = prod.negIndices.map(finalVec(_)).product
        val coeff =
          if (denominator > 0) rest.map(finalVec(_)).product / (denominator)
          else rest.map(finalVec(_)).product
        val ind = prod.indices(j)
        (j -> coeff)
      }
    }
    vecSum(branches)
  }

  def gradientNextStep(
      predecessor: Vector[(Int, Double)]
  ): Vector[(Int, Double)] = {
    val branches: Vector[Vector[(Int, Double)]] = predecessor.map {
      case (j, w) =>
        gradientStep(j).map { case (i, u) => (i, u * w) }
    }
    vecSum(branches)
  }

  // currently computed gradients up to a given depth
  val gradientTerms: Vector[mutable.ArrayBuffer[Vector[(Int, Double)]]] =
    Vector.tabulate(size)(j => mutable.ArrayBuffer(Vector(j -> 1.0)))

  def gradientUptoMemo(j: Int, depth: Int): Vector[Vector[(Int, Double)]] = {
    val memo = gradientTerms(j)
    if (depth <= memo.size) memo.take(depth).toVector
    else if (depth == memo.size + 1) {
      val predecessor = memo.last
      val result =
        vecSum(
          gradientStep(j).map {
            case (i, w) =>
              gradientUptoMemo(i, depth - 1).last.map {
                case (k, u) => (k, u * w)
              }
          }
        )
      gradientTerms(j).append(result)
      memo.toVector :+ result
    } else {
      val predecessor = gradientUptoMemo(j, depth - 1).last // also saves
      val result      = gradientNextStep(predecessor)
      gradientTerms(j).append(result)
      memo.toVector :+ result
    }
  }

  // A simplification where we do not propagate through the denominator, i.e., events. This makes things more stable.
  def gradient(
      index: Int,
      decay: Double = 1.0,
      depth: Int
  ): Vector[(Int, Double)] =
    if (depth < 1) Vector(index -> 1.0)
    else {
      val rhs = rhsExprs(index)
      val branches: Vector[Vector[(Int, Double)]] = rhs.terms.map { prod =>
        {
          val liebnitz = Vector.tabulate(prod.indices.size) { j =>
            val rest        = prod.indices.take(j) ++ prod.indices.drop(j + 1)
            val denominator = prod.negIndices.map(finalVec(_)).product
            val coeff =
              if (denominator > 0) rest.map(finalVec(_)).product / (denominator)
              else rest.map(finalVec(_)).product
            val ind = prod.indices(j)
            gradient(j, decay - 1, depth - 1).map {
              case (j, w) => j -> (w * coeff * decay)
            }
          }
          vecSum(liebnitz)
        }
      }
      vecSum(branches :+ Vector(index -> 1.0))
    }


  def track(
      exp: Expression
  ): Option[(Int, Double, SumIndexExpression, Double, Vector[Double])] =
    equationVec.zipWithIndex.find(_._1.lhs == exp).map {
      case (_, j) =>
        (
          j,
          finalVec(j),
          rhsExprs(j),
          rhsExprs(j).eval(finalVec),
          rhsExprs(j).terms.map(_.eval(finalVec))
        )
    }

  def trackOutput(exp: Expression): String =
    track(exp)
      .map {
        case (j, value, rhs, rhsValue, rhsTermsValue) =>
          s""""For expression: $exp, equation index $j
       |final value: $value
       |rhs expression: $rhs
       |rhs final value: $rhsValue
       |rhs term values: $rhsTermsValue
       |""".stripMargin
      }
      .getOrElse("No equation with lhs $exp")
}
