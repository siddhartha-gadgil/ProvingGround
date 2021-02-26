package provingground.learning

import spire.algebra._
import spire.implicits._
import scala.collection.parallel._, immutable.ParVector

object ParGramSchmidt {
  def dotProd(v1: ParVector[Double], v2: ParVector[Double]) =
    v1.zip(v2).map { case (x, y) => x * y }.sum

  def normalize(v: ParVector[Double]) = {
    val norm = math.sqrt(v.map(x => x * x).sum)
    v.map(x => x / norm)
  }

  def normalizeOpt(v: ParVector[Double], cutoff: Double = 0.0) = {
    val norm = math.sqrt(v.map(x => x * x).sum)
    if (norm > cutoff) Some(v.map(x => x / norm)) else None
  }

  def sum(v1: ParVector[Double], v2: ParVector[Double]) =
    v1.zip(v2).map { case (x, y) => x + y }

  def makePerpFromON(
      orthonormals: ParVector[ParVector[Double]],
      vec: ParVector[Double]
  ): ParVector[Double] = {
    val dots = orthonormals.map(v => dotProd(v, vec))
    vec.zipWithIndex.map { (xn: (Double, Int)) =>
      val (x, n)     = xn
      val correction = orthonormals.map(v => v(n) * dots(n)).sum
      x - correction
    }
  }

  @annotation.tailrec
  def orthonormalize(
      basis: ParVector[ParVector[Double]],
      orthonormals: ParVector[ParVector[Double]] = ParVector(),
      cutoff: Double = 0.0
  ): ParVector[ParVector[Double]] =
    if (basis.isEmpty) orthonormals
    else {
      val appended =
        normalizeOpt(makePerpFromON(orthonormals, basis.head), cutoff)
          .map { newVec =>
            orthonormals :+ newVec
          }
          .getOrElse(orthonormals)
      orthonormalize(basis.tail, appended, cutoff)
    }

}