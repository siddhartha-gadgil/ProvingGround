package provingground.learning

import spire.implicits._

import Expression._

import scala.collection.mutable.ArrayBuffer


class FatExprEquations(
    initMap: Map[Expression, Double],
    equationVec: Vector[Equation],
    params: Coeff[_] => Option[Double],
    initVariables: Vector[Expression] = Vector()
) extends ExpressionEquationIndexifier(
      initMap,
      equationVec,
      params,
      initVariables
    ) {

  lazy val (bilinearTerms, bilienarQuotient, linearTerms, complexTerms) = {
    val bilMatrix =
      Array.fill(size)(Array.fill(numVars)(Array.fill(numVars)(0f)))
    val divMatrix =
      Array.fill(size)(Array.fill(numVars)(Array.fill(numVars)(0f)))
    val linMatrix = Array.fill(size)(Array.fill(numVars)(0f))
    val complex   = ArrayBuffer.empty[(Int, ProductIndexExpression)]

    rhsExprs.zipWithIndex.foreach {
      case (rhs, k) =>
        rhs.terms.foreach { prod =>
          (prod.indices, prod.negIndices) match {
            case (Vector(j), Vector()) =>
              linMatrix(k)(j) = linMatrix(k)(j) + prod.constant.toFloat
            case (Vector(i, j), Vector()) =>
              bilMatrix(k)(i)(j) = bilMatrix(k)(i)(j) + prod.constant.toFloat
            case (Vector(i), Vector(j)) =>
              divMatrix(k)(i)(j) = divMatrix(k)(i)(j) + prod.constant.toFloat
            case _ => complex.append(k -> prod)
          }
        }
    }
    (bilMatrix, divMatrix, linMatrix, complex)
  }

  val totalProbMatrix = randomVarIndices.map { v =>
    (0 until numVars).map(j => if (v.contains(j)) 1f else 0f).toArray
  }.toArray

}