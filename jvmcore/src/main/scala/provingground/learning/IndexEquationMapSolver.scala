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

/**
  * Seek an approximately stable distribution for equations given in terms of indices, hence solving the equations (approximately).
  * Here stable distribution is under the iteration to solve equations viewing them as fixed points.
  * This is an implementation based on maps and keeping track of supports, which is not used as Vectors were more efficient.
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
class IndexEquationMapSolver(
    initMap: Map[Expression, Double],
    equationVec: Vector[Equation],
    params: Coeff[_] => Option[Double],
    maxRatio: Double,
    resolution: Double,
    exponent: Double,
    decay: Double,
    maxTime: Option[Long],
    previousMap: Option[Map[Expression, Double]]
) extends ExpressionEquationIndexifier(initMap, equationVec, params, Vector()){
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

  def normalizedMapBounded(
      v: Map[Int, Double],
      w: Map[Int, Double]
  ): Boolean = {
    ratioBounded(restrictMap(v, termIndices), restrictMap(w, termIndices)) &&
    ratioBounded(restrictMap(v, typIndices), restrictMap(w, typIndices))
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

}