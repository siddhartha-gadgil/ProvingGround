package provingground.learning

import provingground._, HoTT._
import math.{log, max, min}

object BackTraceFromIndexed {

  def relatedTypes(t: Term): Vector[Typ[Term]] =
    t match {
      case tp: Typ[u] => Vector(tp)
      case fn: Func[u, v] =>
        val l = funcToLambdaFixed(fn)
        relatedTypes(l.value)
          .map(tp => piDefn(l.variable)(tp): Typ[Term]) :+ (fn.typ)
      case fn: FuncLike[u, v] =>
        val l = funcToLambda(fn)
        relatedTypes(l.value)
          .map(tp => piDefn(l.variable)(tp): Typ[Term]) :+ (fn.typ)
      case _ => Vector(t.typ)
    }

  def flattenWeights[A](
      data: Vector[(Vector[(A, Double)], Double)]
  ): Vector[(A, Double)] =
    (for {
      (v, p) <- data
      (a, q) <- v
    } yield (a, p * q)).groupMapReduce(_._1)(_._2)(_ + _).toVector

  /**
    * score either of an ingredient in a statement or of the proof of a statement
    *
    * @param successProb generation probability of a proof or relative generation probability given ingredient
    * @param prob generation probability, of statement in case of score of a proof
    * @return
    */
  def score(successProb: Double, prob: Double): Double =
    if (successProb > 0.0) log(prob) / (log(successProb) + log(prob)) else 0.0

  @annotation.tailrec
  def greedyMinimalCover[A, B](
      space: Set[A],
      subspaces: Set[B],
      contains: B => A => Boolean,
      minSize: Int,
      accum: Set[B]
  ): Option[Set[B]] =
    if (accum.size >= minSize && space.forall(
          a => accum.exists(b => contains(b)(a))
        )) Some(accum)
    else {
      if (space.forall(
            a => (accum union subspaces).exists(b => contains(b)(a))
          )) {
        val uncovered = space.filterNot(a => accum.exists(b => contains(b)(a)))
        val best = subspaces.maxBy(
          b =>
            (
              uncovered.count(a => contains(b)(a)),
              space.count(a => contains(b)(a))
            )
        )
        greedyMinimalCover(
          space,
          subspaces - best,
          contains,
          minSize,
          accum + best
        )
      } else None
    }

  @annotation.tailrec
  def bestMinimalCover[A, B, L](
      space: Set[A],
      subspaces: Set[B],
      contains: L => B => A => Boolean,
      minSize: Int,
      baseLevel: L,
      refine: L => L,
      bestSoFar: Option[Set[B]]
  ): Option[Set[B]] = {
    val atThisLevelOpt =
      greedyMinimalCover(space, subspaces, contains(baseLevel), minSize, Set())
    if (atThisLevelOpt.nonEmpty)
      bestMinimalCover(
        space,
        subspaces,
        contains,
        minSize,
        refine(baseLevel),
        refine,
        atThisLevelOpt
      )
    else bestSoFar
  }
}

/**
  * Computation of probabilities of elements in equations for other elements, and of upper bounds on distances,
  * based on equations in indices.
  *
  * @param rhsExprs right-hand side of equation in terms of indices, with the left hand side being the index.
  * @param probVec vector of probabilies, i.e., probabilites as function of indices.
  */
class BackTraceFromIndexed(
    rhsIndexedExprs: Vector[SumIndexExpression],
    probVec: Vector[Double],
    lhsExpressions: Vector[Expression]
) {
  import BackTraceFromIndexed._

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
      val rhs: SumIndexExpression = rhsIndexedExprs(index)
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

  def traceFromBase(
      index: Int,
      base: Int => Option[Double]
  ): (Vector[Int], Double, Int) =
    base(index).map(p => (Vector(index), p, 0)).getOrElse {
      val rhs: SumIndexExpression = rhsIndexedExprs(index)
      val bestProduct: ProductIndexExpression = rhs.terms.maxBy { prod =>
        val numerator   = prod.indices.map(probVec(_)).fold(prod.constant)(_ * _)
        val denominator = prod.negIndices.map(probVec).fold(1.0)(_ * _)
        numerator / denominator
      }
      val weightedTraces =
        bestProduct.indices.map(j => traceFromBase(j, base))
      val traces =
        weightedTraces.flatMap(_._1).distinct
      val depth       = weightedTraces.map(_._3).max + 1
      val denominator = bestProduct.negIndices.map(probVec).fold(1.0)(_ * _)
      val numerator   = weightedTraces.map(_._2).fold(bestProduct.constant)(_ * _)
      (traces, rhs.constantTerm + (numerator / denominator), depth)
    }

  /**
    * probability relative to a weighted set of the expression at an index; often all weights are 1 and the elements in the support are equivalent
    *
    * @param index index of expression
    * @param base optional functions giving weights of terms relative to which we calculate, typically mapping some terms to 1.0
    * @param cutoff cut-off for tracing
    * @return double giving weighted probability
    */
  def relativeProbability(
      index: Int,
      base: Int => Option[Double],
      cutoff: Double
  ): Double =
    base(index)
      .getOrElse(
        if (cutoff > 1) probVec(index)
        else {
          val rhs: SumIndexExpression = rhsIndexedExprs(index)
          rhs.terms.map { prodTerm =>
            prodTerm.indices
              .map(
                i =>
                  relativeProbability(
                    i,
                    base,
                    cutoff / numeratorComplement(i, prodTerm)
                  )
              )
              .product
          }.sum
        }
      )

  import TermRandomVars._, GeneratorVariables._, Expression._

  val termIndexVec: Vector[(Term, Int)] = lhsExpressions.zipWithIndex.collect {
    case (FinalVal(Elem(t: Term, Terms)), j) => (t, j)
  }

  val typIndexVec: Vector[(Typ[Term], Int)] =
    lhsExpressions.zipWithIndex.collect {
      case (FinalVal(Elem(t: Typ[_], Typs)), j) => (t: Typ[Term], j)
    }

  import TermRandomVars.{variableToTermInContext, Terms, Typs}

  // do not require the variable to be terms
  val termInContextIndexVec: Vector[((Term, Vector[Term]), Int)] =
    lhsExpressions.zipWithIndex.flatMap {
      case (FinalVal(variable), j) =>
        variableToTermInContext(variable, (_) => true).map(_ -> j)
      case _ => None
    }

  val termAsTermInContextIndexVec: Vector[((Term, Vector[Term]), Int)] =
    lhsExpressions.zipWithIndex.flatMap {
      case (FinalVal(variable), j) =>
        variableToTermInContext(variable, _ == Terms).map(_ -> j)
      case _ => None
    }

  val typAsTypInContextIndexVec: Vector[((Typ[Term], Vector[Term]), Int)] =
    lhsExpressions.zipWithIndex.flatMap {
      case (FinalVal(variable), j) =>
        variableToTermInContext(variable, _ == Typs).flatMap {
          case (t, ctx) =>
            typOpt(t).map(tp => (tp, ctx)).map(_ -> j)
        }
      case _ => None
    }

  val typAsTypsInContextIndexMap: Map[(Typ[Term], Vector[Term]), Int] =
    typAsTypInContextIndexVec.toMap

  val typInContextInhabitants: Map[Int, Vector[(Int, Double)]] =
    termAsTermInContextIndexVec
      .groupMap {
        case ((t, ctx), _) => (t.typ, ctx)
      }(_._2)
      .flatMap {
        case (tp, js) => typAsTypsInContextIndexMap.get(tp).map(i => i -> js)
      }
      .map {
        case (i, js) => (i, js, js.map(probVec(_)).sum)
      }
      .filter(_._3 > 0.0)
      .map {
        case (i, js, total) => (i, js.map(j => j -> (probVec(j) / total)))
      }
      .toMap

  val indexToTermInContext: Map[Int, (Term, Vector[Term])] =
    termInContextIndexVec.map(ab => (ab._2, ab._1)).toMap

  /**
    * trace back mapping (as a partial quotient to term in context)
    *
    * @param index index from which we trace
    * @param cutoff cut-off both for determining support and for stopping branching
    * @return vector of weighted terms in context
    */
  def termInContextBackTrace(
      index: Int,
      cutoff: Double
  ): Vector[((Term, Vector[Term]), Double)] = {
    val supportTerms: Vector[(Term, Vector[Term])] =
      traceBackWeighted(index, cutoff, 1.0)
        .map(_._1)
        .flatMap(indexToTermInContext.get(_))
    supportTerms.map { tc =>
      val m: Map[Int, Double] = termInContextIndexVec
        .filter(_._1 == tc)
        .map(_._2)
        .map(j => j -> 1.0)
        .toMap
      tc -> relativeProbability(index, m.get(_), cutoff)
    }
  }

  def termInContextBackTraceIndices(
      index: Int,
      cutoff: Double
  ): Vector[(Int, Double)] = {
    val supportIndices =
      traceBackWeighted(index, cutoff, 1.0)
        .map(_._1)
        .filter(indexToTermInContext.keySet.contains(_))
    supportIndices.map { tc =>
      val m = Map(index -> 1.0)
      tc -> relativeProbability(index, m.get(_), cutoff)
    }
  }

  def proofsTraceBack(
      index: Int,
      cutoff: Double
  ): Vector[((Term, Vector[Term]), Double)] =
    flattenWeights(typInContextInhabitants(index).map {
      case (j, q) => (termInContextBackTrace(j, cutoff), q)
    })

  def proofsTraceBackIndices(
      index: Int,
      cutoff: Double
  ) =
    flattenWeights(typInContextInhabitants(index).map {
      case (j, q) => (termInContextBackTraceIndices(j, cutoff), q)
    })

  def proofsOfRelatedTypsFromTrace(
      trace: Vector[((Term, Vector[Term]), Double)]
  ): Vector[(Int, Double)] =
    flattenWeights(trace.flatMap {
      case ((t, ctx), p) =>
        val relTyps = relatedTypes(t)
        (relTyps.flatMap { tp =>
          val pfsOpt = typAsTypsInContextIndexMap
            .get((tp, ctx))
            .flatMap(j => typInContextInhabitants.get(j))
          pfsOpt.map(pfs => (pfs, p / relTyps.size))
        })
    })

  def proofsOfRelatedTyps(index: Int, cutoff: Double): Vector[(Int, Double)] =
    proofsOfRelatedTypsFromTrace(termInContextBackTrace(index, cutoff))

  // older approach with trace back not using relative weights and/or not mapping to terms in context

  def traceBackTermsInContexts(
      index: Int,
      cutoff: Double
  ): Vector[((Term, Vector[Term]), Double)] =
    traceBackWeighted(index, cutoff, 1.0).flatMap {
      case (j, p) => indexToTermInContext.get(j).map(t => (t, p))
    }

  def termInContextIngredientScores(cutoff: Double): Map[(Int, Int), Double] =
    indexToTermInContext.keys.toVector.flatMap { i =>
      val trace: Vector[(Int, Double)] =
        termInContextBackTraceIndices(i, cutoff)
      trace.map { case (j, q) => ((i, j), score(q, probVec(i))) }
    }.toMap

  def typInContextProofScores(cutoff: Double): Map[(Int, Int), Double] =
    typAsTypInContextIndexVec
      .map(_._2)
      .flatMap { i =>
        val trace: Vector[(Int, Double)] = proofsTraceBackIndices(i, cutoff)
        trace.map { case (j, q) => ((i, j), score(q, probVec(i))) }
      }
      .toMap

  def relatedTypProofScores(cutoff: Double): Map[(Int, Int), Double] =
    typAsTypInContextIndexVec
      .map(_._2)
      .flatMap { i =>
        val trace: Vector[(Int, Double)] = proofsOfRelatedTyps(i, cutoff)
        trace.map { case (j, q) => ((i, j), score(q, probVec(i))) }
      }
      .toMap

  val termToIndex: Map[Term, Int] = termIndexVec.toMap

  val typToIndex: Map[Typ[Term], Int] = typIndexVec.toMap

  val indexToTerm: Map[Int, Term] = termIndexVec.map(ab => (ab._2, ab._1)).toMap

  val indexToTyp: Map[Int, Typ[Term]] =
    typIndexVec.map(ab => (ab._2, ab._1)).toMap

  val termTypIndexVec: Vector[(Int, Int)] = termIndexVec.flatMap {
    case (x, i) => typToIndex.get(x.typ).map(j => i -> j)
  }

  val typNegateIndexVec: Map[Int, Int] = typToIndex.flatMap {
    case (typ, j) => typToIndex.get(negate(typ)).map(ntp => (ntp, j))
  }

  val termTypIndexMap = termTypIndexVec.toMap

  val typInhabitantsIndexMap: Map[Int, Vector[Int]] =
    termTypIndexVec.groupMap(_._2)(_._1)

  val typInhabitantsWeight: Map[Int, Double] =
    typInhabitantsIndexMap.map { case (i, v) => (i, v.map(probVec(_)).sum) }

  def traceBackTerms(index: Int, cutoff: Double): Vector[(Term, Double)] =
    traceBackWeighted(index, cutoff, 1.0).flatMap {
      case (j, p) => indexToTerm.get(j).map(t => (t, p))
    }

  def termsInTraceBack(
      index: Int,
      cutoff: Double
  ): Vector[(Term, Double)] = {
    val base  = traceBackTerms(index, cutoff)
    val total = base.map(_._2).sum
    base.map { case (t, p) => (t, p / total) }
  }

  def traceBackTypInhabitants(index: Int, cutoff: Double): Map[Term, Double] =
    typInhabitantsIndexMap(index)
      .flatMap(t => traceBackTerms(t, cutoff))
      .groupMapReduce(_._1)(_._2)(_ + _)

  def typsInProofsTraceBack(
      typIndex: Int,
      cutoff: Double
  ): Map[Typ[Term], Double] =
    traceBackTypInhabitants(typIndex, cutoff).map {
      case (x, p) => (x.typ: Typ[Term], p)
    }

  def scaleMap[A](m: Map[A, Double]): Map[A, Double] = {
    val total = m.map(_._2).sum
    m.map { case (x, p) => (x, p / total) }
  }

  def indexifyTermWeights(m: Map[Term, Double]): Map[Int, Double] =
    m.flatMap { case (x, p) => termToIndex.get(x).map(_ -> p) }

  def indexifyTypWeights(m: Map[Typ[Term], Double]): Map[Int, Double] =
    m.flatMap { case (x, p) => typToIndex.get(x).map(_ -> p) }

  def nextMap(m: Map[Int, Double]): Map[Int,Double] = {
    val support = m.keySet
    val newSupportExpressions = rhsIndexedExprs.zipWithIndex.filter {
      case (rhs, _) => rhs.indices.exists(support.contains(_))
    }
    newSupportExpressions
      .map { case (rhs, j) => (j, rhs.evaluate(m)) }
      .filter(_._2 > 0)
      .toMap
  }

  import ExpressionEquationIndexifier.vecSum

  def averagedNextMap(m: Map[Int, Double], p: Double): Map[Int,Double] = vecSum(
    Vector(m.toVector.map{case (j, q) => (j, q * p)},
     nextMap(m).toVector.map{case (j, q) => (j, q * (1.0 -p))}
  )).toMap

  @annotation.tailrec
  final def forwardMap(m: Map[Int, Double], p: Double, depth: Int) : Map[Int, Double] = 
    if (depth < 1) m else forwardMap(averagedNextMap(m, p), p, depth - 1)
}
