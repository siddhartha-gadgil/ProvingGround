package provingground.learning

import spire.algebra._
import spire.implicits._

object GramSchmidt{
    def makePerpFromON[V](orthonormals: Vector[V], vec: V)(implicit vs: InnerProductSpace[V, Double]) : V = 
        orthonormals match {
            case Vector() => vec
            case init :+ last =>
                val recVec = makePerpFromON(init, vec)
                val minusProj = -1.0 * (last dot recVec)
                vs.plus(recVec, (minusProj *: last)) 
        }

    def orthonormal[V](v: Vector[V])(implicit vs: InnerProductSpace[V, Double]) : Vector[V] = 
        v match {
            case Vector() => Vector()
            case init :+ last =>
                val onInit = orthonormal(init)
                val perpLast = makePerpFromON(onInit, last)
                val onLast =  perpLast.normalize
                if (perpLast.norm > 0) onInit :+ perpLast.normalize else onInit      
            }

    def onVec(vv: Vector[Vector[Double]]) = orthonormal(vv)

    def perpVec(vv: Vector[Vector[Double]], v: Vector[Double]) = makePerpFromON(onVec(vv), v)

}