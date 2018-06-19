package provingground.experiments

import spire.math._
import spire.algebra._
import spire.implicits._

class FieldGeomDist[F](N: Int = 100, p: Double = 0.5)(implicit field: Field[F]){
  val minusOne : F = field.negate(field.one)

  val q = 1 - p

  def sumErr(prob: Vector[F]) = (prob.foldRight[F](minusOne)(_ + _)).pow(2)

  def recErrTerms(prob: Vector[F]) : Vector[F] =
    {
      val prevMul = prob.init.map{(x) => x * q}
      prob.tail.zip(prevMul).map{
        case (a, b) => (a - b).pow(2)
      }
    }

  def totalError(prob: Vector[F]) =
    recErrTerms(prob).fold(sumErr(prob))(_ + _)

}

class JetGeomDist(N: Int = 100, p: Double =  0.5){
  implicit val dim = JetDim(N)

  object DistErr extends FieldGeomDist[Jet[Double]](N, p){
    def tangent(prob: Vector[Double]) =
      prob.zipWithIndex.map{
        case (x, n) => x + Jet.h[Double](n)
      }
  }

}

object JetGeomDist{
  def distErr(N: Int = 2, p: Double = 0.5) = new JetGeomDist(N, p).DistErr
}
