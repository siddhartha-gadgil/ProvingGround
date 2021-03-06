package provingground.learning

object IndexedBacktrace {

  /**
    * known upper bounds on distances, typically only those below a maximum value
    */
  type DistBoundMap = Map[(Int, Int), Double]

  /**
    * map of upper bounds, best from the data
    *
    * @param v vector of upper bounds
    * @return map of best upper bounds
    */
  def bestBoundMap(v: Vector[((Int, Int), Double)]): DistBoundMap =
    v.groupMapReduce(_._1)(_._2)(math.min(_, _))

  /**
    * upper bounds refined by using symmetry
    *
    * @param v vector of upper bounds
    * @return symmetric map of upper bounds
    */
  def symmetrizedBoundMap(v: Vector[((Int, Int), Double)]): DistBoundMap = {
    val symVec = v ++ v.map { case ((i, j), d) => ((j, i), d) }
    bestBoundMap(symVec)
  }

  /**
    * upper bounds refined using the triangle
    *
    * @param m given upper bounds
    * @return map of deduced upper bounds
    */
  def triangleInequalityBounds(
      m: DistBoundMap
  ): DistBoundMap = {
    val v = m.toVector
    val transVec = for {
      ((i, j), d1) <- v
      ((l, k), d2) <- v
      if j == l
    } yield ((i, k) -> (d1 + d2))
    bestBoundMap(v ++ transVec)
  }

  /**
    * checks whether a new distance is significantly better than an old one
    *
    * @param newDist a possibly improved distance bound
    * @param oldDist the original distance
    * @param resolution additive scale for comparison
    * @return boolean giving whether improvement is significant
    */
  def distancesAreImproved(
      newDist: DistBoundMap,
      oldDist: DistBoundMap,
      resolution: Double
  ): Boolean =
    (newDist.keySet -- oldDist.keySet).nonEmpty ||
      newDist.exists { case ((i, j), d) => d < oldDist((i, j)) - resolution }

  /**
    * refine distance till there is no significant improvement
    *
    * @param baseDistance initial distance
    * @param refine map giving way to refine
    * @param resolution additive scale for comparison
    * @return refined upper bound on distances
    */
  @annotation.tailrec
  final def stableDistance(
      baseDistance: DistBoundMap,
      refine: DistBoundMap => DistBoundMap,
      resolution: Double
  ): DistBoundMap = {
    val newDist = refine(baseDistance)
    if (distancesAreImproved(newDist, baseDistance, resolution))
      stableDistance(newDist, refine, resolution)
    else baseDistance
  }

  /**
    * refine distances using the triangle inequality with pruning
    *
    * @param baseDistance initial distance bounds
    * @param maxDistance maximum distance bound - larger bounds than these are ignored
    * @param resolution additive scale for comparison
    * @return refined upper bound map
    */
  def triangleStableDistance(
      baseDistance: DistBoundMap,
      maxDistance: Double,
      resolution: Double
  ): DistBoundMap =
    stableDistance(
      baseDistance,
      (d: DistBoundMap) =>
        triangleInequalityBounds(d).filter(_._2 < maxDistance),
      resolution
    )

  /**
    * matching of indices for complementary distances,
    * corresponds to `(pairs, umatchedInFirst, unmatchedInSecond)`
    */
  type IndexMatch = (Set[(Int, Int)], Vector[Int], Vector[Int]) //

  /**
    * all matches of indices satisfying a condition, typically having a good enough distance bount
    *
    * @param indices1 first collection of indices
    * @param indices2 second collection of indices
    * @param permitted condition for matching
    * @return vector of index matches
    */
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

/**
  * Computation of probabilities of elements in equations for other elements, and of upper bounds on distances,
  * based on equations in indices.
  *
  * @param rhsExprs right-hand side of equation in terms of indices, with the left hand side being the index.
  * @param probVec vector of probabilies, i.e., probabilites as function of indices.
  */
class IndexedBacktrace(
    rhsExprs: Vector[SumIndexExpression],
    probVec: Vector[Double]
) {
  import IndexedBacktrace._

  /**
    * complement of an index in a product term
    *
    * @param index a given index
    * @param exp a product expression
    * @return Double giving the complementary probability
    */
  def numeratorComplement(index: Int, exp: ProductIndexExpression): Double =
    exp.indices
      .filter(_ != index)
      .map { j =>
        probVec(j)
      }
      .fold(exp.constant)(_ * _)

  /**
    * other indices that have significant weight in the given index on tracing back
    *
    * @param index initial index
    * @param cutoff terms with probability below this are ignored
    * @param headWeight the initial weight, for recursive computation
    * @return vector of indices with weights
    */
  def traceBackWeighted(
      index: Int,
      cutoff: Double,
      headWeight: Double
  ): Vector[(Int, Double)] =
    if (headWeight < cutoff) Vector()
    else {
      val rhs: SumIndexExpression = rhsExprs(index)
      val children: Vector[(Int, Double)] =
        rhs.terms.flatMap(
          prodTerm =>
            prodTerm.indices
              .map(i => i -> (headWeight * numeratorComplement(i, prodTerm)))
              .filter(_._2 > cutoff)
        )
      children
        .flatMap {
          case (i, p) =>
            ((i, p)) +: traceBackWeighted(
              i,
              cutoff,
              p
            )
        }
        .filter(_._2 > cutoff)
    }

  /**
    * distance bounds from diagonals and from comparing lhs and rhs of equations
    *
    * @return map of distance bounds
    */
  lazy val baseDistances: DistBoundMap = symmetrizedBoundMap(
    Vector.tabulate(rhsExprs.size)(i => ((i, i), 0.0)) ++ rhsExprs.zipWithIndex
      .flatMap {
        case (rhs, i) =>
          rhs.terms.flatMap(
            prodTerm =>
              prodTerm.indices.map(
                j => (i, j) -> (-math.log(numeratorComplement(j, prodTerm)))
              )
          )
      }
  )

  /**
    * norms of indices
    *
    * @return vector of norms
    */
  lazy val norms: Vector[Double] = probVec.map(p => -math.log(p))

  /**
    * distance between product expressions, given by considering all matches where distance bounds are known;
    * with distance for each match given by distances between matched indices and norms of unmatched ones.
    *  
    * @param exp1 first product expression
    * @param exp2 second product expression
    * @param baseDistance previous distance bounds
    * @return distance bound derived by matching
    */
  def derivedDistance(
      exp1: ProductIndexExpression,
      exp2: ProductIndexExpression,
      baseDistance: DistBoundMap
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

  /**
    * Best derived distances between all pairs, filtered by a bound
    *
    * @param baseDistance given distance bounds
    * @param maxDistance maximum distance bound, beyond which bounds are not considered
    * @return vector of bounds
    */
  def allDerivedDistances(
      baseDistance: DistBoundMap,
      maxDistance: Double
  ): Vector[((Int, Int), Double)] =
    rhsExprs.zipWithIndex.flatMap {
      case (rhs1, i1) =>
        rhs1.terms.flatMap(
          t1 =>
            (rhsExprs.zipWithIndex
              .flatMap {
                case (rhs2, i2) =>
                  rhs2.terms.map {t2 =>
                    val der = derivedDistance(t1, t2, baseDistance)
                    (i1, i2) -> baseDistance
                      .get((i1, i2))
                      .map(d => math.min(d, der))
                      .getOrElse(der)
                  }
              })
              .filter(_._2 < maxDistance)
        )
    }

  /**
    * single step of deriving distances and using symmetry and triangle inequality
    *
    * @param baseDistance given distance bounds
    * @param maxDistance maximum distance bound, beyond which bounds are not considered
    * @param resolution additive scale for comparison
    * @return refined distance
    */
  def derivedStep(
      baseDistance: DistBoundMap,
      maxDistance: Double,
      resolution: Double
  ) = {
    def refine(d: DistBoundMap) =
      triangleStableDistance(
        symmetrizedBoundMap(d.toVector ++ allDerivedDistances(d, maxDistance)),
        maxDistance,
        resolution
      )
    stableDistance(baseDistance, refine, resolution)
  }

  /**
    * stable distance after iterative refinement by deriving and triangle inequality
    *
    * @param maxDistance maximum distance bound, beyond which bounds are not considered
    * @param resolution additive scale for comparison
    * @return stable refined distance
    */
  def finalDistance(maxDistance: Double, resolution: Double): DistBoundMap =
    stableDistance(
      triangleStableDistance(baseDistances, maxDistance, resolution),
      derivedStep(_, maxDistance, resolution),
      resolution
    )
}
