package provingground.learning

object IndexedBacktrace {
  type DistMap = Map[(Int, Int), Double]

  def bestMap(v: Vector[((Int, Int), Double)]): DistMap =
    v.groupMapReduce(_._1)(_._2)(math.min(_, _))

  def symMap(v: Vector[((Int, Int), Double)]): DistMap = {
    val symVec = v ++ v.map { case ((i, j), d) => ((j, i), d) }
    bestMap(symVec)
  }

  def triangleInequalityStep(
      m: DistMap
  ): DistMap = {
    val v = m.toVector
    val transVec = for {
      ((i, j), d1) <- v
      ((l, k), d2) <- v
      if j == l
    } yield ((i, k) -> (d1 + d2))
    bestMap(v ++ transVec)
  }

  def distancesAreImproved(
      newDist: DistMap,
      oldDist: DistMap,
      cutoff: Double
  ): Boolean =
    (newDist.keySet -- oldDist.keySet).nonEmpty ||
      newDist.exists { case ((i, j), d) => d < oldDist((i, j)) - cutoff }

  @annotation.tailrec
  final def stableDistance(
      baseDistance: DistMap,
      refine: DistMap => DistMap,
      resolution: Double
  ): DistMap = {
    val newDist = refine(baseDistance)
    if (distancesAreImproved(newDist, baseDistance, resolution))
      stableDistance(newDist, refine, resolution)
    else baseDistance
  }

  def triangleStableDistance(
      baseDistance: DistMap,
      maxDistance: Double,
      resolution: Double
  ) =
    stableDistance(
      baseDistance,
      d => triangleInequalityStep(d).filter(_._2 < maxDistance),
      resolution
    )

  type IndexMatch = (Set[(Int, Int)], Vector[Int], Vector[Int]) // (pairs, umatchedInFirst, unmatchedInSecond)

  def allMatches(
      indices1: Vector[Int],
      indices2: Vector[Int],
      permitted: Int => Int => Boolean
  ): Vector[IndexMatch] =
    (indices1.flatMap(
      i =>
        indices2
          .filter(j => permitted(i)(j))
          .flatMap(
            j =>
              allMatches(
                indices1.filter(_ != i),
                indices2.filter(_ != j),
                permitted
              ).map { case (m, i1, i2) => (m + ((i, j)), i1, i2) }
          )
    ) :+ (Set.empty[(Int, Int)], indices1, indices2)).distinct
}

class IndexedBacktrace(
    rhsExprs: Vector[SumIndexExpression],
    probVec: Vector[Double]
) {
  import IndexedBacktrace._

  def numeratorComplement(index: Int, exp: SumIndexExpression): Double =
    exp.indices
      .filter(_ != index)
      .map { j =>
        probVec(j)
      }
      .fold(exp.constantTerm)(_ * _)

  def traceBackWeighted(
      index: Int,
      cutoff: Double,
      headWeight: Double
  ): Vector[(Int, Double)] =
    if (headWeight < cutoff) Vector()
    else {
      val rhs: SumIndexExpression = rhsExprs(index)
      val children: Vector[(Int, Double)] = rhs.indices
        .map(i => i -> (headWeight * probVec(i)))
        .filter(_._2 > cutoff)
      children
        .flatMap {
          case (i, p) =>
            ((i, headWeight * numeratorComplement(i, rhs))) +: traceBackWeighted(
              i,
              cutoff,
              headWeight * p
            )
        }
        .filter(_._2 > cutoff)
    }

  lazy val baseDistances: DistMap = symMap(
    Vector.tabulate(rhsExprs.size)(i => ((i, i), 0.0)) ++ rhsExprs.zipWithIndex
      .flatMap {
        case (rhs, i) =>
          rhs.indices.map(
            j => (i, j) -> (-math.log(numeratorComplement(j, rhs)))
          )
      }
  )

  lazy val norms: Vector[Double] = probVec.map(p => -math.log(p))

  def derivedDistance(
      exp1: SumIndexExpression,
      exp2: SumIndexExpression,
      baseDistance: DistMap
  ): Double = {
    allMatches(
      exp1.indices,
      exp2.indices,
      (i) => (j) => baseDistance.keySet.contains((i, j))
    ).map {
      case (m, i1s, i2s) =>
        m.map { case (i, j) => baseDistance(i, j) }.sum + (i1s ++ i2s)
          .map(norms(_))
          .sum
    }
  }.min

  def allDerivedDistances(
      baseDistance: DistMap,
      maxDistance: Double
  ): Vector[((Int, Int), Double)] =
    rhsExprs.zipWithIndex.flatMap {
      case (rhs1, i1) =>
        (rhsExprs.zipWithIndex
          .map {
            case (rhs2, i2) =>
              val der = derivedDistance(rhs1, rhs2, baseDistance)
              (i1, i2) -> baseDistance
                .get((i1, i2))
                .map(d => math.min(d, der))
                .getOrElse(der)
          })
          .filter(_._2 < maxDistance)
    }

  def derivedStep(
      baseDistance: DistMap,
      maxDistance: Double,
      resolution: Double
  ) = {
    def refine(d: DistMap) =
      triangleStableDistance(
        symMap(d.toVector ++ allDerivedDistances(d, maxDistance)),
        maxDistance,
        resolution
      )
    stableDistance(baseDistance, refine, resolution)
  }

  def finalDistance(maxDistance: Double, resolution: Double): DistMap =
    stableDistance(
      triangleStableDistance(baseDistances, maxDistance, resolution),
      derivedStep(_, maxDistance, resolution),
      resolution
    )
}
