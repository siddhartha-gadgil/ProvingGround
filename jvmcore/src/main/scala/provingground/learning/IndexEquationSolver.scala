package provingground.learning

import provingground.{FiniteDistribution => _, _}, HoTT._

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

class IndexEquationSolver(
    initMap: Map[Expression, Double],
    equationSet: Set[Equation],
    params: Coeff[_] => Option[Double],
    maxRatio: Double,
    resolution: Double,
    exponent: Double,
    decay: Double,
    maxTime: Option[Long],
    previousMap: Option[Map[Expression, Double]]
) extends ExpressionEquationIndexifier(initMap, equationSet, params, Vector()) {
  lazy val startingMap = {
    val v = rhsExprs.zipWithIndex.filter(_._1.hasConstant)
    (v.map { case (exp, j) => j -> exp.initialValue }.toMap.filter(_._2 > 0))
  }

  lazy val constantMap = startingMap.filter {
    case (j, _) => constantEquations.contains(j)
  }

  lazy val startingSupport = {
    val indSupp = startingMap.keySet
    rhsInvolves(indSupp) union indSupp
  }

  /**
    * The next step towards a stable map with given equations
    *
    * @param m the map so far
    * @param support the set of indices where we should recompute
    * @return triple of the next map, next support and whether stable
    */
  def nextMapSupport(
      m: Map[Int, Double],
      support: Set[Int]
  ): (Map[Int, Double], Set[Int], Boolean) = {
    val lookup = support
      .map { j =>
        j -> rhsExprs(j).evaluate(m)
      }
      .filter(_._2 > 0)
      .toMap
    val newMap     = m ++ lookup
    val newIndices = lookup.keySet -- m.keySet
    val newSupport = (rhsInvolves(newIndices) union m.keySet) -- constantEquations
    (newMap, newSupport, newIndices.isEmpty)
  }

  /**
    * the next map, assuming support is stable and is the support except for terms that stay constant
    *
    * @param m the present map
    * @param support the indices to update
    * @param exponent exponent for geometric mean
    * @return a stable map
    */
  def nextMap(
      m: Map[Int, Double],
      support: Set[Int],
      exponent: Double
  ): Map[Int, Double] = {
    val base = support
      .map { j =>
        val exp = rhsExprs(j)
        val y   = exp.evaluate(m) // the new value
        val z   = m.getOrElse(j, 0.0) // the old value, if any
        val newValue = if (z > 0) {
          val gm = math.pow(z, 1 - exponent) * math.pow(y, exponent)
          if (gm.isNaN() && (!y.isNaN() & !z.isNaN()))
            JvmUtils.logger.error(
              s"Geometric mean of $y and $z with exponent $exponent is not a number\nEquation with negative value: ${scala.util
                .Try(equationVec(j))}"
            )
          gm
        } else y
        j -> newValue
      }
      .filter(_._2 > 0)
      .toMap ++ constantMap
    val scale = m.values.sum / base.values.sum
    base.map { case (n, x) => (n, x * scale) }
  }

  def proofData(typ: Typ[Term]): Vector[(Int, Equation)] =
    equationVec.zipWithIndex.collect {
      case (eq @ Equation(FinalVal(Elem(t: Term, Terms)), rhs), j)
          if t.typ == typ =>
        (j, eq)
    }

  def traceIndices(j: Int, depth: Int): Vector[Int] =
    if (depth < 1) Vector(j)
    else j +: rhsExprs(j).indices.flatMap(traceIndices(_, depth - 1))

  def nextTraceVector(current: Vector[Vector[Int]]): Vector[Vector[Int]] =
    current.flatMap { branch =>
      (0 until (branch.length)).flatMap { j =>
        val before    = branch.take(j)
        val after     = branch.drop(j + 1)
        val offspring = rhsExprs(j).terms
        offspring.map(pt => before ++ pt.indices ++ after)
      }
    }

  def nextTraceSet(
      current: Set[Set[Int]],
      relativeTo: Set[Int]
  ): Set[Set[Int]] =
    current.flatMap { branchSet =>
      val branch = branchSet.toVector
      (0 until (branch.length)).flatMap { j =>
        val rest      = branch.take(j).toSet union branch.drop(j + 1).toSet
        val offspring = rhsExprs(j).terms
        offspring.map(pt => (rest union pt.indices.toSet) -- relativeTo)
      }
    }

  @annotation.tailrec
  final def recTraceSet(
      current: Set[Set[Int]],
      depth: Int,
      relativeTo: Set[Int],
      accum: Set[Set[Int]]
  ): Set[Set[Int]] =
    if (depth < 1 || current.isEmpty) accum
    else {
      val next = nextTraceSet(current, relativeTo)
      recTraceSet(next, depth - 1, relativeTo, accum union (next))
    }

  def traceSet(
      elem: Expression,
      depth: Int,
      relativeTo: Set[Int]
  ): Set[Set[Int]] =
    indexMap
      .get(elem)
      .map(index => recTraceSet(Set(Set(index)), depth, relativeTo, Set()))
      .getOrElse(Set())

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

  def restrictMap(
      m: Map[Int, Double],
      indices: Vector[Int]
  ): Vector[Double] = {
    val base = indices.map { j =>
      m.get(j)
    }.flatten
    val total = base.sum
    if (total == 0) base else base.map(_ / total)
  }

  def nextVec(v: ParVector[Double], exponent: Double): ParVector[Double] = {
    // pprint.log(exponent)
    val fn: ((SumExpr, Int)) => Double = {
      case (exp, j) =>
        val y = exp.eval(v)
        // if (y < 0)
        //   JvmUtils.logger.error(s"Equation with negative value: ${equationVec(j)}")
        val z = v(j)
        if (z > 0) {
          val gm = math.pow(z, 1 - exponent) * math.pow(y, exponent)
          if (gm.isNaN() && (!y.isNaN() & !z.isNaN()))
            JvmUtils.logger.error(
              s"Geometric mean of $y and $z with exponent $exponent is not a number\nEquation with negative value: ${scala.util
                .Try(equationVec(j))}"
            )
          gm
        } else y
    }
    rhsExprs.zipWithIndex.par.map(fn)
  }

  def simpleNextVec(v: ParVector[Double]): ParVector[Double] = {
    // JvmUtils.logger.debug("Computing new vector")
    val fn: ((SumExpr, Int)) => Double = {
      case (exp, j) =>
        val y = exp.eval(v)
        val z = v(j)
        if (z > 0) z else y
    }
    // JvmUtils.logger.debug("Computing new vector: defined function")
    val z = rhsExprs.zipWithIndex
    // JvmUtils.logger.debug(s"Mapping ${z.size} expressions")
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

  def normalizedMapBounded(
      v: Map[Int, Double],
      w: Map[Int, Double]
  ): Boolean = {
    ratioBounded(restrictMap(v, termIndices), restrictMap(w, termIndices)) &&
    ratioBounded(restrictMap(v, typIndices), restrictMap(w, typIndices))
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
      JvmUtils.logger.error(s"Timeout for stable vector after $steps steps; resolution $resolution")
      initVec
    } else {
      if (steps % 100 == 2) JvmUtils.logger.debug(s"completed $steps steps")
      val startTime  = System.currentTimeMillis()
      val newBaseVec = nextVec(initVec, exponent)
      // val initSum = math.max(initVec.sum, 1.0)
      val currSum = newBaseVec.sum
      val scale   = size.toDouble / currSum
      // JvmUtils.logger.info(initSum.toString())
      // JvmUtils.logger.info(currSum.toString)
      // JvmUtils.logger.info(scale.toString())
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
      JvmUtils.logger.error(
        s"Timeout for stable support vector after $steps steps"
      )
      initVec
    } else {
      if (steps % 100 == 2)
        JvmUtils.logger.debug(
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
        JvmUtils.logger.debug(
          s"stable support with support size ${newVec.count(_ != 0)}"
        )
        newVec
      } else {
        // JvmUtils.logger.debug("recursive call for stable support vector")
        val usedTime = System.currentTimeMillis() - startTime
        stableSupportVec(
          newVec,
          maxTime.map(t => t - usedTime),
          steps + 1
        )
      }
    }

  @tailrec
  final def stableSupportMap(
      initMap: Map[Int, Double],
      initSupport: Set[Int],
      maxTime: Option[Long],
      steps: Long
  ): (Map[Int, Double], Set[Int]) =
    if (maxTime.map(limit => limit < 0).getOrElse(false)) {
      JvmUtils.logger.error(
        s"Timeout for stable support vector after $steps steps"
      )
      (initMap, initSupport)
    } else {
      if (steps % 100 == 2)
        JvmUtils.logger.debug(
          s"completed $steps steps without stable support, support size : ${initMap.size}"
        )
      val startTime                   = System.currentTimeMillis()
      val (newMap, newSupport, check) = nextMapSupport(initMap, initSupport)
      if (check) {
        JvmUtils.logger.debug(
          s"stable support with support size ${newMap.size}"
        )
        (newMap, newSupport)
      } else {
        val usedTime = System.currentTimeMillis() - startTime
        stableSupportMap(
          newMap,
          newSupport,
          maxTime.map(t => t - usedTime),
          steps + 1
        )
      }
    }

  @tailrec
  final def stableMap(
      initMap: Map[Int, Double],
      support: Set[Int],
      exponent: Double = 0.5,
      decay: Double,
      maxTime: Option[Long],
      steps: Long
  ): Map[Int, Double] =
    if (maxTime.map(limit => limit < 0).getOrElse(false)) {
      JvmUtils.logger.error(s"Timeout for stable map after $steps steps")
      initMap
    } else {
      if (steps % 100 == 2) JvmUtils.logger.debug(s"completed $steps steps")
      val startTime = System.currentTimeMillis()
      val newMap    = nextMap(initMap, support, exponent)
      if (normalizedMapBounded(initMap, newMap)) {
        JvmUtils.logger.debug("Obtained stable map")
        newMap
      } else {
        val usedTime = System.currentTimeMillis() - startTime
        stableMap(
          newMap,
          support,
          exponent * decay,
          decay,
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

  lazy val finalVec: ParVector[Double] = {
    JvmUtils.logger.debug(
      s"Computing final vector, with maximum time $maxTime, exponent: $exponent, decay: $decay"
    )
    JvmUtils.logger.debug(s"Number of equations: ${equationVec.size}")
    JvmUtils.logger.debug(
      s"Computed initial vector with size: ${initVector.size}"
    ) // to avoid being part of time limit for stable vector
    val stableSupport =
      stableSupportVec(initVector, maxTime, 0L)
    JvmUtils.logger.debug("Obtained vector with stable support")
    stableVec(
      stableSupport,
      exponent,
      decay,
      maxTime,
      0L
    )
  }

  lazy val finalStableMap: Map[Int, Double] = {
    JvmUtils.logger.debug(
      s"Computing final map, with maximum time $maxTime, exponent: $exponent, decay: $decay"
    )
    JvmUtils.logger.debug(s"Number of equations: ${equationVec.size}")
    val (stableM, support) =
      stableSupportMap(startingMap, startingSupport, maxTime, 0L)
    JvmUtils.logger.debug("Obtained map with stable support")
    stableMap(
      stableM,
      support,
      exponent,
      decay,
      maxTime,
      0L
    )
  }

  lazy val finalDistMap: Map[Expression, Double] = finalStableMap.map {
    case (j, p) => equationVec(j).lhs -> p
  }

  lazy val finalMap: Map[Expression, Double] = {
    val fn: ((Double, Int)) => (Expression, Double) = {
      case (x, j) => equationVec(j).lhs -> x
    }
    finalVec.zipWithIndex.map(fn).toMap ++ initMap
  }.seq

  def track(
      exp: Expression
  ): Option[(Int, Double, SumExpr, Double, Vector[Double])] =
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
