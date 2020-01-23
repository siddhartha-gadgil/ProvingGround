package provingground.learning

import spire.algebra._
import spire.implicits._

object GramSchmidt {
  def makePerpFromON[V](orthonormals: Vector[V], vec: V)(
      implicit vs: InnerProductSpace[V, Double]
  ): V =
    orthonormals match {
      case Vector() => vec
      case init :+ last =>
        val recVec    = makePerpFromON(init, vec)
        val minusProj = -1.0 * (last dot recVec)
        recVec + (minusProj *: last)
    }

  def orthonormal[V](
      v: Vector[V]
  )(implicit vs: InnerProductSpace[V, Double]): Vector[V] =
    v match {
      case Vector() => Vector()
      case init :+ last =>
        val onInit   = orthonormal(init)
        val perpLast = makePerpFromON(onInit, last)
        val onLast   = perpLast.normalize
        if (perpLast.norm > 0) onInit :+ perpLast.normalize else onInit
    }

  def onVec(vv: Vector[Vector[Double]]) = orthonormal(vv)

  def perpVec(vv: Vector[Vector[Double]], v: Vector[Double]) =
    makePerpFromON(onVec(vv), v)

}

object MonixGramSchmidt {
  import monix.eval._

  def makePerpFromON[V](orthonormals: Vector[V], vec: V)(
      implicit vs: InnerProductSpace[V, Double]
  ): Task[V] =
    Task.eval(orthonormals.isEmpty) flatMap {
      case true => Task.now(vec)
      case false =>
        for {
          recVec <- makePerpFromON(orthonormals.init, vec)
          minusProj = -1.0 * (orthonormals.last dot recVec)
        } yield recVec + (minusProj *: orthonormals.last)
    }

  def orthonormal[V](
      v: Vector[V]
  )(implicit vs: InnerProductSpace[V, Double]): Task[Vector[V]] =
    Task.eval(v.isEmpty) flatMap {
      case true => Task.now(Vector())
      case false =>
        import v._
        for {
          onInit   <- orthonormal(init)
          perpLast <- makePerpFromON(onInit, last)
          onLast = perpLast.normalize
        } yield if (perpLast.norm > 0) onInit :+ perpLast.normalize else onInit
    }

  def onVec(vv: Vector[Vector[Double]]): Task[Vector[Vector[Double]]] =
    orthonormal(vv)

  def perpVec(
      vv: Vector[Vector[Double]],
      v: Vector[Double]
  ): Task[Vector[Double]] = onVec(vv).flatMap(makePerpFromON(_, v))
}

object MapVS {
//   implicit def mapVS[A]: VectorSpace[Map[A, Double], Double] = MapVS()

  def compose[A](
      base: Map[A, Double],
      step: A => Map[A, Double]
  ): Map[A, Double] = {
    // val vs     = MapVS[A]()
    val groups = base.map { case (x, p) => p *: step(x) }
    groups.map(_.toVector).flatten.groupBy(_._1).view.mapValues(v => v.map(_._2).sum).toMap
  }
}

object FieldGramSchmidt{
  def makePerpFromON[V, F](orthonormals: Vector[V], vec: V)(
      implicit vs: InnerProductSpace[V, F], field: Field[F], roots: NRoot[F]
  ): V =
    orthonormals match {
      case Vector() => vec
      case init :+ last =>
        val recVec    = makePerpFromON(init, vec)
        val minusProj = -1.0 * (last dot recVec)
        recVec + (minusProj *: last)
    }

  def orthonormal[V, F](
      v: Vector[V]
  )(implicit vs: InnerProductSpace[V, F], field: Field[F], roots: NRoot[F]): Vector[V] =
    v match {
      case Vector() => Vector()
      case init :+ last =>
        val onInit   = orthonormal(init)
        val perpLast = makePerpFromON(onInit, last)
        val onLast   = perpLast.normalize
        if (perpLast.norm != 0) onInit :+ perpLast.normalize else onInit
    }

  def onVec[F](vv: Vector[Vector[F]])(implicit field: Field[F], roots: NRoot[F]) = orthonormal(vv)

  def perpVec[F](vv: Vector[Vector[F]], v: Vector[F])(implicit field: Field[F], roots: NRoot[F]) =
    makePerpFromON(onVec(vv), v)

}

object MonixFieldGramSchmidt {
  import monix.eval._

  def makePerpFromON[V, F](orthonormals: Vector[V], vec: V)(
      implicit vs: InnerProductSpace[V, F], field: Field[F], roots: NRoot[F]
  ): Task[V] =
    Task.eval(orthonormals.isEmpty) flatMap {
      case true => Task.now(vec)
      case false =>
        for {
          recVec <- makePerpFromON(orthonormals.init, vec)
          minusProj = -1.0 * (orthonormals.last dot recVec)
        } yield recVec + (minusProj *: orthonormals.last)
    }

  def orthonormal[V, F](
      v: Vector[V]
  )(implicit vs: InnerProductSpace[V, F], field: Field[F], roots: NRoot[F]): Task[Vector[V]] =
    Task.eval(v.isEmpty) flatMap {
      case true => Task.now(Vector())
      case false =>
        import v._
        for {
          onInit   <- orthonormal(init)
          perpLast <- makePerpFromON(onInit, last)
          onLast = perpLast.normalize
        } yield if (perpLast.norm != 0) onInit :+ perpLast.normalize else onInit
    }

  def onVec[F](vv: Vector[Vector[F]])(implicit field: Field[F], roots: NRoot[F]) : Task[Vector[Vector[F]]] =
    orthonormal(vv)

  def perpVec[F](
      vv: Vector[Vector[F]],
      v: Vector[F]
  )(implicit field: Field[F], roots: NRoot[F]): Task[Vector[F]] = onVec(vv).flatMap(makePerpFromON(_, v))
}