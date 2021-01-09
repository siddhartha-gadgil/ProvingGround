package provingground.learning
import provingground.{FiniteDistribution => FD, _}, HoTT._
import monix.eval._, monix.tail._

import spire.algebra._
import spire.math._
import spire.implicits._
import ExpressionEval._

import GeneratorVariables._, TermRandomVars._, Expression._,
TermGeneratorNodes.{_}

import annotation.tailrec
import spire.util.Opt
import fastparse.internal.Util
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable._
import scala.math.Ordering.Double.TotalOrdering
import scala.collection.immutable.Stream.cons
import scala.collection.immutable.Nil
import shapeless.ops.product
import scala.collection.mutable
import scala.concurrent._
import monix.eval._
import scala.collection.mutable.ArrayBuffer

/**
  * Working with expressions built from initial and final values of random variables, including in islands,
  * given equations satisfied by these
  */
object ExpressionEval {
  val sd: StateDistribution[TermState, FD] =
    implicitly[StateDistribution[TermState, FD]]

  /**
    * extract the distribution
    *
    * @param rv random variable
    * @param p map of expression values
    * @return finite distribution
    */
  def dist[Y](rv: RandomVar[Y], p: Map[Expression, Double]): FD[Y] = {
    val pmf = p.collect {
      case (FinalVal(Elem(x, randomVar)), prob) if rv == randomVar =>
        Weighted(x.asInstanceOf[Y], prob)
    }
    FD(pmf)
  }

  /**
    * initial values in the map of expression values
    *
    * @param p the map
    * @return distribution of initial values.
    */
  def generators(p: Map[Expression, Double]): FD[Term] =
    FD(
      p.collect { case (InitialVal(Elem(x: Term, Terms)), p) => Weighted(x, p) }
    )

  /**
    * checks whether an element is a variable in an island
    */
  def isIsleVar(elem: Elem[_]): Boolean = elem match {
    case Elem(x: Symbolic, _) =>
      x.name match {
        case Name(name) => name.startsWith("@")
        case _          => false
      }
    case Elem(ExstFunc.Wrap(x: Symbolic), _) =>
      x.name match {
        case Name(name) => name.startsWith("@")
        case _          => false
      }
    case Elem(x: Term, _) =>
      Name.getName(x).map(_.startsWith(("@"))).getOrElse(false)
    case _ => false
  }

  /**
    * returns an element if the variable is one, and the context.
    * It is assumed that boats have all the isle information. If the argument is an `Elem`, perhaps nested in isles, we get boats and the random-variable
    *
    * @param elem candidate element
    * @return optional element and context
    */
  def elemContext(
      elem: GeneratorVariables.Variable[_]
  ): Option[(RandomVar[_], Vector[_])] = elem match {
    case Elem(element, randomVar) => Some(randomVar -> Vector())
    case InIsle(isleVar, boat, isle) =>
      elemContext(isleVar).map { case (rv, vec) => (rv, boat +: vec) }
    case _ => None
  }

  /**
    * Initial value of an atomic expression
    */
  def initVal(
      exp: Expression,
      tg: TermGenParams,
      initialState: TermState
  ): Option[Double] =
    exp match {
      case cf @ Coeff(_) => cf.get(tg.nodeCoeffSeq) // FIXME should have the nodeCoeffSeq as a parameter
      case InitialVal(elem: Elem[y]) =>
        import elem._
        val base = initialState.elemVal(element, randomVar)
        //  sd.value(initialState)(randomVar)(element)
        if (base > 0) Some(base)
        else if (randomVar == Goals) Some(0.5) // just a quick-fix
        else if (isIsleVar(elem))
          Some(tg.varWeight / (1 - tg.varWeight)) // for the case of variables in islands
        else Some(0)                              // else throw new Exception(s"no initial value for $elem")
      case IsleScale(_) => Some((1.0 - tg.varWeight))
      case _            => None
    }

  /**
    * Given a collection of atoms, returns map with values for them.
    */
  def initMap(
      atoms: Set[Expression],
      tg: TermGenParams, // FIXME should have the nodeCoeffSeq as a parameter
      initialState: TermState
  ): Map[Expression, Double] = {
    val atomVec = atoms.toVector.par
    Utils.logger.debug(s"Computing initial map with ${atomVec.size} atoms")
    val valueVec = atomVec.map(exp => initVal(exp, tg, initialState))
    Utils.logger.debug("Computed initial values")
    val fn: PartialFunction[(Option[Double], Int), (Expression, Double)] = {
      case (Some(x), n) if x > 0 => (atomVec(n), x)
    }
    val expMapVec = valueVec.zipWithIndex.collect(fn)
    Utils.logger.debug(s"Computed vector for map, size ${expMapVec.size}")
    Utils.logger.debug(s"Zero initial values are ${valueVec.count(_.isEmpty)}")
    val result = expMapVec.toMap
    Utils.logger.debug("Computed map")
    result.seq
  }

  def initMapTask(
      atoms: Set[Expression],
      tg: TermGenParams,
      initialState: TermState
  ): Task[Map[Expression, Double]] = {
    val atomVec = atoms.toVector
    Utils.logger.debug(s"Computing initial map with ${atomVec.size} atoms")
    Utils.logger.debug(
      s"Computed (sizes of) memoized maps: ${initialState.termDistMap.size}, ${initialState.typDistMap.size}, ${initialState.funcDistMap.size}" +
        s", ${initialState.typFamilyDistMap.size}, ${initialState.termsWithTypsMap.size}, ${initialState.funcsWithDomsMap.size}"
    )
    val valueVecTask =
      Task.parSequence(
        atomVec.map(exp => Task(initVal(exp, tg, initialState)))
      )
    valueVecTask.map { valueVec =>
      Utils.logger.debug("Computed initial values")
      val fn: PartialFunction[(Option[Double], Int), (Expression, Double)] = {
        case (Some(x), n) if x > 0 => (atomVec(n), x)
      }
      val expMapVec = valueVec.zipWithIndex.collect(fn)
      Utils.logger.debug(s"Computed vector for map, size ${expMapVec.size}")
      Utils.logger.debug(
        s"Zero initial values are ${valueVec.count(_.isEmpty)}"
      )
      val result = expMapVec.toMap
      Utils.logger.debug("Computed map")
      result
    }
  }

  /**
    * Recursively calculate or update the value on expression, given initial values.
    */
  def recExp(init: Map[Expression, Double], exp: Expression): Double =
    init.getOrElse(
      exp,
      exp match {
        case Sum(xs)       => xs.map(recExp(init, _)).sum
        case Log(a)        => math.log(recExp(init, a))
        case Exp(a)        => math.exp(recExp(init, a))
        case Product(x, y) => recExp(init, x) * recExp(init, y)
        case Literal(x)    => x
        case Quotient(x, y) =>
          if (recExp(init, y) != 0) recExp(init, x) / recExp(init, y)
          else recExp(init, x)
        case _ => 0
      }
    )

  /**
    * Stabilized recursive expression, taking geometric mean with the previous value if defined.
    * This is to avoid oscillations.
    *
    * @param exponent weight (power) of the new value while taking means
    */
  def stabRecExp(
      init: Map[Expression, Double],
      exp: Expression,
      prev: Option[Double],
      exponent: Double = 0.5
  ): Double = {
    val y = recExp(init, exp)
    prev
      .map(z => math.pow(z, 1 - exponent) * math.pow(y, exponent))
      .getOrElse(y)
  }

  /**
    * Updating a map given equations by replacing terms that are the lhs of an equation with rhs evaluated.
    */
  def nextMap(
      init: Map[Expression, Double],
      equations: Set[Equation],
      exponent: Double = 0.5
  ): Map[Expression, Double] = {
    init ++ equations
      .map(eq => eq.lhs -> stabRecExp(init, eq.rhs, init.get(eq.lhs), exponent))
      .filter(_._2 != 0)
      .toMap
  }

  /**
    * Iteratively update a map given equations until the support is stable (so we can safely calculate ratios).
    */
  @tailrec
  def stableSupportMap(
      init: Map[Expression, Double],
      equations: Set[Equation],
      exponent: Double = 0.5,
      decay: Double = 1
  ): Map[Expression, Double] = {
    val newMap = nextMap(init, equations, exponent)
    if (newMap.keySet == init.keySet) newMap
    else stableSupportMap(newMap, equations, exponent * decay)
  }

  /**
    * iteratively evolve a map for a fixed number of steps
    *
    * @param init initial map
    * @param equations equations determining the iteration
    * @param steps number of steps to iterate
    * @return result of iteration
    */
  @tailrec
  def iterateMap(
      init: Map[Expression, Double],
      equations: Set[Equation],
      steps: Int
  ): Map[Expression, Double] =
    if (steps < 1) init
    else iterateMap(nextMap(init, equations), equations, steps - 1)

  def mapRatio[A](m1: Map[A, Double], m2: Map[A, Double]): Double = {
    require(m1.keySet == m2.keySet, "comparing maps with different supports")
    if (m1.isEmpty) 1
    else m1.map { case (k, v) => math.max(v / m2(k), (m2(k) / v)) }.max
  }

  /**
    * Iteratively update a map by equations till it is stable, i.e. almost satisfies the equations.
    */
  @tailrec
  def stableMap(
      init: Map[Expression, Double],
      equations: Set[Equation],
      maxRatio: Double = 1.01,
      exponent: Double = 0.5,
      decay: Double = 1,
      maxTime: Option[Long]
  ): Map[Expression, Double] =
    if (maxTime.map(limit => limit < 0).getOrElse(false)) init
    else {
      val startTime = System.currentTimeMillis()
      val newMap    = nextMap(init, equations, exponent)
      if ((newMap.keySet == init.keySet) && mapRatio(newMap, init) < maxRatio)
        newMap
      else {
        val usedTime = System.currentTimeMillis() - startTime
        stableMap(
          newMap,
          equations,
          maxRatio,
          exponent * decay,
          decay,
          maxTime.map(t => t - usedTime)
        )
      }
    }

  /**
    * atoms from equations
    *
    * @param equations the equations
    * @return set of expressions
    */
  def eqAtoms(equations: Set[Equation], groupSize: Int = 100): Set[Expression] =
    Expression.allAtomsByGroups(
      equations.map(_.rhs).grouped(groupSize).toList,
      equations.map(_.lhs)
    )
  // equations
  //   .map(_.lhs)
  //   .union(equations.flatMap(eq => Expression.atoms(eq.rhs)))

  /**
    * builds an [[ExpressionEval]] given states, equations and parameters, say as the result of a local prover
    *
    * @param initialState initial state
    * @param finalState final state
    * @param equationsS equations
    * @param tgS term-generator parameters
    * @param maxRatioS maximum ratio for stabilization
    * @param scaleS scale for gradient flow
    * @param smoothS smoothing for gradient flow
    * @param exponentS exponent for iteration
    * @param decayS decay during iteration
    * @param maxTimeS max-time during iteration
    * @return [[ExpressionEval]] built
    */
  def fromStates(
      initialState: TermState,
      finalState: TermState,
      equationsS: Set[Equation],
      tgS: TermGenParams,
      maxRatioS: Double = 1.01,
      scaleS: Double = 1.0,
      smoothS: Option[Double] = None,
      exponentS: Double = 0.5,
      decayS: Double = 1,
      maxTimeS: Option[Long] = None
  ) =
    new ExpressionEval {
      val init                                         = initMap(eqAtoms(equationsS), tgS, initialState)
      val finalTyps                                    = finalState.typs
      val equations                                    = equationsS
      val tg                                           = tgS
      val maxRatio                                     = maxRatioS
      val scale                                        = scaleS
      val coeffsAsVars: Boolean                        = false
      val smoothing: Option[Double]                    = smoothS
      val exponent: Double                             = exponentS
      val decay                                        = decayS
      val maxTime: Option[Long]                        = maxTimeS
      val previousMap: Option[Map[Expression, Double]] = None
    }

  /**
    * builds an [[ExpressionEval]] given initial states, equations and parameters,
    * with the final state deduced using the equations
    *
    * @param initialState initial state
    * @param equationsS equations
    * @param tgS term-generator parameters
    * @param maxRatioS maximum ratio for stabilization
    * @param scaleS scale for gradient flow
    * @param smoothS smoothing for gradient flow
    * @param exponentS exponent for iteration
    * @param decayS decay during iteration
    * @param maxTimeS max-time during iteration
    * @return [[ExpressionEval]] built
    */
  def fromInitEqs(
      initialState: TermState,
      equationsS: Set[Equation],
      tgS: TermGenParams,
      maxRatioS: Double = 1.01,
      scaleS: Double = 1.0,
      smoothS: Option[Double] = None,
      exponentS: Double = 0.5,
      decayS: Double = 1,
      maxTimeS: Option[Long] = None,
      previousMapS: Option[Map[Expression, Double]] = None
  ): ExpressionEval =
    new ExpressionEval with GenerateTyps {
      val init                                         = initMap(eqAtoms(equationsS), tgS, initialState)
      val equations                                    = equationsS
      val tg                                           = tgS
      val maxRatio                                     = maxRatioS
      val scale                                        = scaleS
      val coeffsAsVars: Boolean                        = false
      val smoothing: Option[Double]                    = smoothS
      val exponent: Double                             = exponentS
      val decay                                        = decayS
      val maxTime: Option[Long]                        = maxTimeS
      val previousMap: Option[Map[Expression, Double]] = previousMapS
    }

  /**
    * builds an [[ExpressionEval]] given initial states, equations and parameters,
    * with the final state deduced using the equations
    *
    * @param initialState initial state
    * @param equationsS equations
    * @param tgS term-generator parameters
    * @param maxRatioS maximum ratio for stabilization
    * @param scaleS scale for gradient flow
    * @param smoothS smoothing for gradient flow
    * @param exponentS exponent for iteration
    * @param decayS decay during iteration
    * @param maxTimeS max-time during iteration
    * @return [[ExpressionEval]] built
    */
  def fromInitEqsTask(
      initialState: TermState,
      equationsS: Set[Equation],
      tgS: TermGenParams,
      maxRatioS: Double = 1.01,
      scaleS: Double = 1.0,
      smoothS: Option[Double] = None,
      exponentS: Double = 0.5,
      decayS: Double = 1,
      maxTimeS: Option[Long] = None,
      previousMapS: Option[Map[Expression, Double]] = None
  ): Task[ExpressionEval] =
    initMapTask(eqAtoms(equationsS), tgS, initialState).map(
      initM =>
        new ExpressionEval with GenerateTyps {
          val init                                         = initM
          val equations                                    = equationsS
          val tg                                           = tgS
          val maxRatio                                     = maxRatioS
          val scale                                        = scaleS
          val coeffsAsVars: Boolean                        = false
          val smoothing: Option[Double]                    = smoothS
          val exponent: Double                             = exponentS
          val decay                                        = decayS
          val maxTime: Option[Long]                        = maxTimeS
          val previousMap: Option[Map[Expression, Double]] = previousMapS
        }
    )

  /**
    * [[ExpressionEval]] where the type distribution is generated from the equations
    */
  trait GenerateTyps extends ExpressionEval { self =>
    lazy val finalTyps = {
      val base = FD {
        finalDist.collect {
          case (FinalVal(Elem(typ: Typ[Term], Typs)), w) =>
            Weighted(typ, w)
        }
      }
      if (base.pmf.exists(_.weight.isNaN))
        Utils.logger.error(s"NaN for some types before normalizing")
      if (base.pmf.forall(_.weight.isNaN))
        Utils.logger.error(s"NaN for all types before normalizing")

      base.safeNormalized
    }

    override def generateTyps: ExpressionEval = self

    override def modify(
        initNew: Map[Expression, Double] = self.init,
        finalTypsNew: => FD[Typ[Term]] = self.finalTyps,
        equationsNew: Set[Equation] = self.equations,
        tgNew: TermGenParams = self.tg,
        coeffsAsVarsNew: Boolean = self.coeffsAsVars,
        maxRatioNew: Double = self.maxRatio,
        scaleNew: Double = self.scale,
        smoothNew: Option[Double] = self.smoothing,
        exponentNew: Double = self.exponent,
        decayNew: Double = self.decay,
        maxTimeNew: Option[Long] = self.maxTime
    ): ExpressionEval = new ExpressionEval with GenerateTyps {
      val init                                         = initNew
      val equations                                    = equationsNew
      val tg                                           = tgNew
      val coeffsAsVars                                 = coeffsAsVarsNew
      val maxRatio                                     = maxRatioNew
      val scale                                        = scaleNew
      val smoothing                                    = smoothNew
      val exponent: Double                             = exponentNew
      val decay                                        = decayNew
      val maxTime: Option[Long]                        = maxTimeNew
      val previousMap: Option[Map[Expression, Double]] = self.previousMap
    }

    override lazy val thmsByStatement: Map[HoTT.Typ[HoTT.Term], Expression] =
      if (thmSet.isEmpty) Map()
      else {
        val base = thmSet.map { typ =>
          typ -> FinalVal(Elem(typ, Typs))
        }.toMap
        val total = Sum(base.map(_._2).toVector)
        base.map { case (typ, exp) => (typ, exp / total) }
      }

    def setProofWeights(pm: Map[Typ[Term], Double]): ExpressionEval =
      new FixedProofs {
        val proofWeights: Map[HoTT.Typ[HoTT.Term], Double] = pm
        val init                                           = self.init
        val equations                                      = self.equations
        val tg                                             = self.tg
        val coeffsAsVars                                   = self.coeffsAsVars
        val maxRatio                                       = self.maxRatio
        val scale                                          = self.scale
        val smoothing: Option[Double]                      = self.smoothing
        val exponent: Double                               = self.exponent
        val decay: Double                                  = self.decay
        val maxTime: Option[Long]                          = self.maxTime
        val previousMap: Option[Map[Expression, Double]]   = self.previousMap
      }

  }

  /**
    * fixes the weights of proofs, to try to flow with types making worse matches
    */
  trait FixedProofs extends GenerateTyps {
    val proofWeights: Map[Typ[Term], Double]

    override def proofExpression(typ: HoTT.Typ[HoTT.Term]): Expression =
      Literal(proofWeights(typ))

    def adverseIterant(
        hW: Double = 1,
        klW: Double = 1,
        p: Map[Expression, Double] = finalDist
    ): Iterant[Task, Map[Expression, Double]] =
      Iterant.fromLazyStateAction[Task, Map[Expression, Double], Map[
        Expression,
        Double
      ]] { q =>
        for {
          epg <- FixedExpressionProbs(q).entropyProjectionTask(hW, klW)
          s = stableGradShift(q, -epg)
        } yield (s, s)
      }(Task.now(p))

  }

  /**
    * extract variable values from equations
    *
    * @param eqs the equation
    * @return set of variable values
    */
  def values(eqs: Set[Equation]): Set[Expression] =
    // eqs
    //   .flatMap(eq => Set(eq.lhs, eq.rhs))
    //   .flatMap(exp => Expression.varVals(exp)
    Expression
      .allVarVals(eqs.map(_.rhs), eqs.map(_.lhs))
      .map(t => t: Expression)

  /**
    * extract terms from equations
    *
    * @param eqs the equation
    * @return set of terms
    */
  def terms(eqs: Set[EquationNode]): Set[Term] =
    Expression
      .allVarVals(eqs.map(_.rhs), eqs.map(_.lhs))
      .map(_.variable)
      .collect { case Elem(t: Term, _) => t }

  /**
    * extract types from equations
    *
    * @param eqs the equation
    * @return set of types
    */
  def typs(eqs: Set[EquationNode]): Set[Typ[Term]] =
    Expression
      .allVarVals(eqs.map(_.rhs), eqs.map(_.lhs))
      .map(_.variable)
      .collect { case Elem(t: Typ[u], _) => t }

  /**
    * exporting an [[ExpressionEval]] with respect to variables
    *
    * @param ev the initial expression eval
    * @param vars variables
    * @return exported [[ExpressionEval]]
    */
  def export(ev: ExpressionEval, vars: Vector[Term]): ExpressionEval =
    vars match {
      case Vector() => ev
      case xs :+ y  => export(ev.relVariable(y), xs)
    }

}

case class ProdExpr(
    constant: Double,
    indices: Vector[Int],
    negIndices: Vector[Int]
) {
  val isPositiveConstant = indices.isEmpty && constant > 0

  val isConstant = indices.isEmpty

  val initialValue = if (isConstant) constant else 0.0

  def eval(v: ParVector[Double]): Double = {
    val subTerms = (indices.map(j => v(j)) ++ negIndices.map { j =>
      val y = v(j)
      if (y == 0) 1.0
      else {
        val rec = 1.0 / y
        if (rec.isNaN() && !y.isNaN)
          Utils.logger.error(s"the reciprocal of $y is not a number")
        rec
      }
    })
    val result = subTerms.product * constant
    if (result.isNaN() && !subTerms.exists(_.isNaN()) && !constant.isNaN())
      Utils.logger.error(
        s"the product of $subTerms  and constant $constant is not a number"
      )
    if (result.isNaN()) 0 else result
  }

  def gradient(v: collection.parallel.ParSeq[Double]): Vector[(Int, Double)] = {
    val numeratorTerms = indices.map(v(_))
    val denominatorTerms = negIndices.map { j =>
      val y = v(j)
      if (y > 0) 1.0 / y else 1.0
    }
    val numerator   = numeratorTerms.product
    val denominator = denominatorTerms.product
    val posLiebnitz = Vector.tabulate(numeratorTerms.size) { j =>
      j -> (numeratorTerms.take(j) ++ numeratorTerms.drop(j + 1)).product * constant * denominator
    }
    val negLiebnitz = Vector.tabulate(denominatorTerms.size)
      { j =>
        j -> -(denominatorTerms.take(j) ++ denominatorTerms.drop(j + 1)).product / (denominatorTerms(
          j
        ) * denominatorTerms(j)) * constant * numerator
      }
    ExprCalc.vecSum(Vector(posLiebnitz, negLiebnitz))
  }

  def evaluate(m: Map[Int, Double]): Double = {
    val subTerms = (indices.map(j => m.getOrElse(j, 0.0)) ++ negIndices.map {
      j =>
        val y = m.getOrElse(j, 0.0)
        if (y == 0) 1.0
        else {
          val rec = 1.0 / y
          if (rec.isNaN() && !y.isNaN)
            Utils.logger.error(s"the reciprocal of $y is not a number")
          rec
        }
    })
    val result = subTerms.product * constant
    if (result.isNaN() && !subTerms.exists(_.isNaN()) && !constant.isNaN())
      Utils.logger.error(
        s"the product of $subTerms  and constant $constant is not a number"
      )
    result
  }

  val numSupport = indices
    .map { j =>
      s"X($j)"
    }
    .mkString(" * ")
  val denSupport =
    if (negIndices.isEmpty) ""
    else
      negIndices
        .map { j =>
          s"X($j)"
        }
        .mkString("/(", " * ", ")")

  override def toString() =
    s"($constant * $numSupport $denSupport)"

  def *(that: ProdExpr) =
    ProdExpr(
      constant * that.constant,
      indices ++ that.indices,
      negIndices ++ that.negIndices
    )

  def /(that: ProdExpr) =
    ProdExpr(
      if (that.constant > 0) constant / that.constant else constant,
      indices ++ that.negIndices,
      negIndices ++ that.indices
    )
}

case class SumExpr(terms: Vector[ProdExpr]) {
  val constantTerm: Double = terms.filter(_.isConstant).map(_.constant).sum
  val indices: Vector[Int] = terms.flatMap(_.indices).distinct
  val hasConstant: Boolean = terms.exists(_.isPositiveConstant)
  val isPositiveConstant: Boolean = terms.forall(_.isConstant) && terms.exists(
    _.isPositiveConstant
  )
  val isConstant: Boolean = terms.forall(_.isConstant)

  val initialValue =
    if (isPositiveConstant)(terms.map(_.initialValue)).sum else 0.0

  def eval(v: ParVector[Double]): Double = {
    val subTerms = terms.map(_.eval(v))
    val result   = subTerms.sum
    // if (result < 0)
    //   Utils.logger.error(
    //     s"Negative value for expression with terms $terms, values $subTerms"
    //   )
    if (result.isNaN() && !subTerms.exists(_.isNaN()))
      Utils.logger.error(s"the sum of $subTerms is not a number")
    result
  }

  def gradient(v: collection.parallel.ParSeq[Double]): Vector[(Int, Double)] =
    ExprCalc.vecSum(terms.map(_.gradient(v)))

  def evaluate(m: Map[Int, Double]): Double = {
    val subTerms = terms.map(_.evaluate(m))
    val result   = subTerms.sum
    if (result.isNaN() && !subTerms.exists(_.isNaN()))
      Utils.logger.error(s"the sum of $subTerms is not a number")
    result
  }

  override def toString() = terms.mkString(" + ")
}

object ExprEquations {
  def varGroups(
      vars: Vector[GeneratorVariables.Variable[_]]
  ): Vector[Vector[GeneratorVariables.Variable[_]]] = {
    val elems      = vars collect { case el: Elem[_] => el }
    val elemGroups = elems.groupBy(_.randomVar).map(_._2).to(Vector)
    val isleVars = vars
      .collect { case isl: InIsle[_, _, _, _, _] => isl }
      .groupBy(isl => (isl.isle, isl.boat))
      .map(_._2)
      .toVector
    val isleGroups = isleVars.flatMap(gp => varGroups(gp))
    elemGroups ++ isleGroups
  }

  def indexedVarGroups(
      vars: Vector[(Int, GeneratorVariables.Variable[_])]
  ): ParVector[Vector[Int]] = {
    val elems = vars collect { case (n, el: Elem[_]) => n -> el }
    val elemGroups =
      elems.groupMap(_._2.randomVar)(_._1).map(_._2).to(ParVector)
    val isleVars: Vector[Vector[(Int, GeneratorVariables.Variable[_])]] = vars
      .collect { case (n, isl: InIsle[_, _, _, _, _]) => n -> isl }
      .groupMap { case (_, isl) => (isl.isle, isl.boat) } {
        case (n, isl) => (n, isl.isleVar)
      }
      .map(_._2)
      .toVector
    val isleGroups = isleVars.flatMap(gp => indexedVarGroups(gp))
    elemGroups ++ isleGroups
  }

  def exprGroups(exps: Vector[Expression]): Vector[Vector[Expression]] = {
    val finGps =
      varGroups(exps.collect { case FinalVal(variable) => variable }).map(
        vv => vv.map(exp => FinalVal(exp))
      )
    val initGps = varGroups(exps.collect {
      case InitialVal(variable) => variable
    }).map(
      vv => vv.map(exp => InitialVal(exp))
    )
    finGps ++ initGps
  }

  def indexedExprGroups(
      exps: Vector[(Expression, Int)]
  ): ParVector[Vector[Int]] = {
    val finGps = indexedVarGroups(exps.collect {
      case (FinalVal(variable), n) => n -> variable
    })
    val initGps = indexedVarGroups(exps.collect {
      case (InitialVal(variable), n) => n -> variable
    })
    finGps ++ initGps
  }

  def parVector(v: Vector[(Int, Double)], size: Int): ParVector[Double] = {
    val m = v.toMap
    ParVector.tabulate(size)(n => m.getOrElse(n, 0.0))
  }
}

class ExprEquations(
    initMap: Map[Expression, Double],
    equationSet: Set[Equation],
    params: TermGenParams,
    initVariables: Vector[Expression] = Vector()
) {
  import ExprEquations._, ExprCalc.vecSum
  lazy val equationVec: Vector[Equation] = equationSet.toVector //.par

  lazy val size = equationVec.size

  val numVars = size + initVariables.size

  lazy val varVec = equationVec.map(_.lhs)

  lazy val indexMap: Map[Expression, Int] = equationVec
    .map(_.lhs)
    .zipWithIndex
    .toMap ++ (
    initVariables.zipWithIndex.map {
      case (exp, n) => exp -> (n + size)
    }
  )

  def mapToIndexMap[V](m: Map[Expression, V]): Map[Int, V] =
    m.map { case (exp, v) => indexMap(exp) -> v }

  // Set total probability as 1 for each group
  lazy val randomVarIndices: ParVector[Vector[Int]] = indexedExprGroups(
    equationVec.map(_.lhs).zipWithIndex
  )

  // already orthonormal
  lazy val totalProbEquations: ParVector[ParVector[Double]] =
    randomVarIndices.map { gp =>
      val scaled = 1.0 / sqrt(gp.size.toDouble)
      val s      = gp.toSet
      ParVector.tabulate(size)(n => if (s.contains(n)) scaled else 0.0)
    }

  lazy val initPar = initMap.par

  def getProd(exp: Expression): ProdExpr =
    initPar
      .get(exp)
      .map(c => ProdExpr(c, Vector(), Vector()))
      .orElse(
        indexMap.get(exp).map(j => ProdExpr(1, Vector(j), Vector()))
      )
      .getOrElse(
        exp match {
          case cf @ Coeff(_) =>
            ProdExpr(
              cf.get(params.nodeCoeffSeq).getOrElse(0),
              Vector(),
              Vector()
            )
          case Product(x, y)        => getProd(x) * getProd(y)
          case Quotient(x, y)       => getProd(x) / getProd(y)
          case Literal(value)       => ProdExpr(value, Vector(), Vector())
          case InitialVal(variable) => ProdExpr(0, Vector(), Vector())
          case _ =>
            Utils.logger.debug(
              s"cannot decompose $exp as a product, though it is in the rhs of ${equationVec
                .find(eqq => (Expression.atoms(eqq.rhs).contains(exp)))}"
            )
            ProdExpr(0, Vector(), Vector())
        }
      )

  def simplify(exp: Expression): SumExpr =
    SumExpr(
      Expression.sumTerms(exp).map(getProd(_))
    )

  lazy val rhsExprs: Vector[SumExpr] =
    equationVec.map(eq => simplify(eq.rhs))

  lazy val rhsExprsPar = rhsExprs.par

  def rhsInvolves(js: Set[Int]): Set[Int] =
    (0 until (size))
      .filter(i => js.exists(j => rhsExprs(i).indices.contains(j)))
      .toSet

  lazy val constantEquations: Set[Int] =
    (0 until (size)).filter(i => rhsExprs(i).isConstant).toSet

  // not orthonormal
  def equationGradients(
      v: collection.parallel.ParSeq[Double]
  ): ParVector[ParVector[Double]] = {
    ParVector.tabulate(size) { n =>
      val rhsGrad = rhsExprs(n).gradient(v)
      parVector(
        vecSum(Vector(rhsGrad, Vector(n -> -1.0))),
        size
      )
    }
  }

  def orthonormalGradients(
      v: collection.parallel.ParSeq[Double],
      cutoff: Double = 0.0
  ): ParVector[ParVector[Double]] =
    ParGramSchmidt.orthonormalize(
      equationGradients(v),
      totalProbEquations,
      cutoff
    )

  lazy val termIndices: Vector[Int] = {
    val pfn: PartialFunction[(Equation, Int), Int] = {
      case (Equation(FinalVal(Elem(_, Terms)), _), j) => j
    }
    equationVec.zipWithIndex.collect(pfn)
  }

  lazy val termIndexVec: Vector[(Term, Int)] = {
    val pfn: PartialFunction[(Equation, Int), (Term, Int)] = {
      case (Equation(FinalVal(Elem(x: Term, Terms)), _), j) => x -> j
    }
    equationVec.zipWithIndex.collect(pfn)
  }

  lazy val initTermIndices: Vector[Int] = {
    val pfn: PartialFunction[(Expression, Int), Int] = {
      case (InitialVal(Elem(_, Terms)), j) => j + size
    }
    initVariables.zipWithIndex.collect(pfn)
  }

  lazy val typIndices: Vector[Int] = {
    val pfn: PartialFunction[(Equation, Int), Int] = {
      case (Equation(FinalVal(Elem(_, Typs)), _), j) => j
    }
    equationVec.zipWithIndex.collect(pfn)
  }

  lazy val typIndexVec: Vector[(Typ[Term], Int)] = {
    val pfn: PartialFunction[(Equation, Int), (Typ[Term], Int)] = {
      case (Equation(FinalVal(Elem(x: Typ[u], Typs)), _), j) => x -> j
    }
    equationVec.zipWithIndex.collect(pfn)
  }

  lazy val initTypIndices: Vector[Int] = {
    val pfn: PartialFunction[(Expression, Int), Int] = {
      case (InitialVal(Elem(_, Typs)), j) => j + size
    }
    initVariables.zipWithIndex.collect(pfn)
  }

  lazy val thmPfIndices =
    equationVec.zipWithIndex
      .collect {
        case (Equation(FinalVal(Elem(x: Term, Terms)), _), j) => j -> x.typ
      }
      .flatMap {
        case (j, t) => indexMap.get(FinalVal(Elem(t, Typs))).map(k => j -> k)
      }
      .groupMap(_._2)(_._1)

}

class FatExprEquations(
    initMap: Map[Expression, Double],
    equationSet: Set[Equation],
    params: TermGenParams,
    initVariables: Vector[Expression] = Vector()
) extends ExprEquations(initMap, equationSet, params, initVariables) {

  lazy val (bilinearTerms, bilienarQuotient, linearTerms, complexTerms) = {
    val bilMatrix =
      Array.fill(size)(Array.fill(numVars)(Array.fill(numVars)(0f)))
    val divMatrix =
      Array.fill(size)(Array.fill(numVars)(Array.fill(numVars)(0f)))
    val linMatrix = Array.fill(size)(Array.fill(numVars)(0f))
    val complex   = ArrayBuffer.empty[(Int, ProdExpr)]

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

object ExprCalc {
  def vecSum(vecs: Vector[Vector[(Int, Double)]]): Vector[(Int, Double)] =
    vecs.reduce(_ ++ _).groupMapReduce(_._1)(_._2)(_ + _).toVector

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

class ExprCalc(
    ev: ExpressionEval,
    initMap: Map[Expression, Double],
    equationSet: Set[Equation],
    params: TermGenParams
) extends ExprEquations(initMap, equationSet, params, Vector()) {
  import SumExpr._, ev._, ExprCalc._
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
    support
      .map { j =>
        val exp = rhsExprs(j)
        val y   = exp.evaluate(m) // the new value
        val z   = m.getOrElse(j, 0.0) // the old value, if any
        val newValue = if (z > 0) {
          val gm = math.pow(z, 1 - exponent) * math.pow(y, exponent)
          if (gm.isNaN() && (!y.isNaN() & !z.isNaN()))
            Utils.logger.error(
              s"Geometric mean of $y and $z with exponent $exponent is not a number\nEquation with negative value: ${scala.util
                .Try(equationVec(j))}"
            )
          gm
        } else y
        j -> newValue
      }
      .filter(_._2 > 0)
      .toMap ++ constantMap
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
        //   Utils.logger.error(s"Equation with negative value: ${equationVec(j)}")
        val z = v(j)
        if (z > 0) {
          val gm = math.pow(z, 1 - exponent) * math.pow(y, exponent)
          if (gm.isNaN() && (!y.isNaN() & !z.isNaN()))
            Utils.logger.error(
              s"Geometric mean of $y and $z with exponent $exponent is not a number\nEquation with negative value: ${scala.util
                .Try(equationVec(j))}"
            )
          gm
        } else y
    }
    rhsExprs.zipWithIndex.par.map(fn)
  }

  def simpleNextVec(v: ParVector[Double]): ParVector[Double] = {
    // Utils.logger.debug("Computing new vector")
    val fn: ((SumExpr, Int)) => Double = {
      case (exp, j) =>
        val y = exp.eval(v)
        val z = v(j)
        if (z > 0) z else y
    }
    // Utils.logger.debug("Computing new vector: defined function")
    val z = rhsExprs.zipWithIndex
    // Utils.logger.debug(s"Mapping ${z.size} expressions")
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
        x == 0 || y == 0 || ((x / y) <= bound && y / x <= bound)
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
      Utils.logger.error(s"Timeout for stable vector after $steps steps")
      initVec
    } else {
      if (steps % 100 == 2) Utils.logger.debug(s"completed $steps steps")
      val startTime = System.currentTimeMillis()
      val newVec    = nextVec(initVec, exponent)
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
      Utils.logger.error(
        s"Timeout for stable support vector after $steps steps"
      )
      initVec
    } else {
      if (steps % 100 == 2)
        Utils.logger.debug(
          s"completed $steps steps without stable support, support size : ${initVec
            .count(_ > 0)}"
        )
      val startTime = System.currentTimeMillis()
      val newVec    = simpleNextVec(initVec.par)
      val check = (0 until (initVec.size)).forall(
        n => (initVec(n) != 0) || (newVec(n) == 0)
      )
      if (check) {
        Utils.logger.debug(
          s"stable support with support size ${newVec.count(_ != 0)}"
        )
        newVec
      } else {
        // Utils.logger.debug("recursive call for stable support vector")
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
      Utils.logger.error(
        s"Timeout for stable support vector after $steps steps"
      )
      (initMap, initSupport)
    } else {
      if (steps % 100 == 2)
        Utils.logger.debug(
          s"completed $steps steps without stable support, support size : ${initMap.size}"
        )
      val startTime                   = System.currentTimeMillis()
      val (newMap, newSupport, check) = nextMapSupport(initMap, initSupport)
      if (check) {
        Utils.logger.debug(
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
      Utils.logger.error(s"Timeout for stable map after $steps steps")
      initMap
    } else {
      if (steps % 100 == 2) Utils.logger.debug(s"completed $steps steps")
      val startTime = System.currentTimeMillis()
      val newMap    = nextMap(initMap, support, exponent)
      if (normalizedMapBounded(initMap, newMap)) {
        Utils.logger.debug("Obtained stable map")
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
    Utils.logger.debug(
      s"Computing final vector, with maximum time $maxTime, exponent: $exponent, decay: $decay"
    )
    Utils.logger.debug(s"Number of equations: ${equationVec.size}")
    Utils.logger.debug(s"Computed initial vector with size: ${initVector.size}") // to avoid being part of time limit for stable vector
    val stableSupport =
      stableSupportVec(initVector, maxTime, 0L)
    Utils.logger.debug("Obtained vector with stable support")
    stableVec(
      stableSupport,
      exponent,
      decay,
      maxTime,
      0L
    )
  }

  lazy val finalStableMap: Map[Int, Double] = {
    Utils.logger.debug(
      s"Computing final map, with maximum time $maxTime, exponent: $exponent, decay: $decay"
    )
    Utils.logger.debug(s"Number of equations: ${equationVec.size}")
    val (stableM, support) =
      stableSupportMap(startingMap, startingSupport, maxTime, 0L)
    Utils.logger.debug("Obtained map with stable support")
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
    finalVec.zipWithIndex.map(fn).toMap ++ init
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

trait ExpressionEval { self =>
  val init: Map[Expression, Double]
  val finalTyps: FD[Typ[Term]]
  val equations: Set[Equation]
  val tg: TermGenParams
  val coeffsAsVars: Boolean
  val maxRatio: Double
  val scale: Double
  val smoothing: Option[Double]
  val exponent: Double
  val decay: Double
  val maxTime: Option[Long]
  val previousMap: Option[Map[Expression, Double]]

  /**
    * new expression-eval with initial distribution averaged with the current one
    *
    * @param that the other initial distribution
    * @return averaged expression eval
    */
  def avgInit(that: ExpressionEval) =
    new ExpressionEval {
      val init                                         = (0.5 *: self.init) + (0.5 *: that.init)
      lazy val finalTyps                               = self.finalTyps
      val equations                                    = Equation.merge(self.equations, that.equations)
      val tg                                           = self.tg
      val coeffsAsVars                                 = self.coeffsAsVars
      val maxRatio                                     = self.maxRatio
      val scale                                        = self.scale
      val smoothing: Option[Double]                    = self.smoothing
      val exponent: Double                             = self.exponent
      val decay: Double                                = self.decay
      val maxTime: Option[Long]                        = self.maxTime
      val previousMap: Option[Map[Expression, Double]] = self.previousMap
    }

  /**
    * modified copy
    */
  def modify(
      initNew: Map[Expression, Double] = self.init,
      finalTypsNew: => FD[Typ[Term]] = self.finalTyps,
      equationsNew: Set[Equation] = self.equations,
      tgNew: TermGenParams = self.tg,
      coeffsAsVarsNew: Boolean = self.coeffsAsVars,
      maxRatioNew: Double = self.maxRatio,
      scaleNew: Double = self.scale,
      smoothNew: Option[Double] = self.smoothing,
      exponentNew: Double = self.exponent,
      decayNew: Double = self.decay,
      maxTimeNew: Option[Long] = self.maxTime
  ): ExpressionEval = new ExpressionEval {
    val init: Map[Expression, Double]                = initNew
    lazy val finalTyps: FD[Typ[Term]]                = finalTypsNew
    val equations: Set[Equation]                     = equationsNew
    val tg: TermGenParams                            = tgNew
    val coeffsAsVars: Boolean                        = coeffsAsVarsNew
    val maxRatio: Double                             = maxRatioNew
    val scale: Double                                = scaleNew
    val smoothing: Option[Double]                    = smoothNew
    val exponent: Double                             = exponentNew
    val decay                                        = decayNew
    val maxTime: Option[Long]                        = maxTimeNew
    val previousMap: Option[Map[Expression, Double]] = self.previousMap
  }

  /**
    * copy with types generated from equations
    *
    * @return
    */
  def generateTyps: ExpressionEval = new ExpressionEval with GenerateTyps {
    val init                                         = self.init
    val equations                                    = self.equations
    val tg                                           = self.tg
    val coeffsAsVars                                 = self.coeffsAsVars
    val maxRatio                                     = self.maxRatio
    val scale                                        = self.scale
    val smoothing: Option[Double]                    = self.smoothing
    val exponent: Double                             = self.exponent
    val decay                                        = self.decay
    val maxTime: Option[Long]                        = self.maxTime
    val previousMap: Option[Map[Expression, Double]] = self.previousMap
  }

  /**
    * undoing generation of types by freezing them
    */
  def fixTypes: ExpressionEval = new ExpressionEval {
    val init                                         = self.init
    val finalTyps: FD[HoTT.Typ[HoTT.Term]]           = self.finalTyps
    val equations                                    = self.equations
    val tg                                           = self.tg
    val coeffsAsVars                                 = self.coeffsAsVars
    val maxRatio                                     = self.maxRatio
    val scale                                        = self.scale
    val smoothing: Option[Double]                    = self.smoothing
    val exponent: Double                             = self.exponent
    val decay                                        = self.decay
    val maxTime: Option[Long]                        = self.maxTime
    val previousMap: Option[Map[Expression, Double]] = self.previousMap
  }

  /**
    * the atomic expressions in the equations
    */
  lazy val atoms: Set[Expression] =
    Expression.allAtoms(equations.map(_.rhs), equations.map(_.lhs))
  // equations
  //   .map(_.lhs)
  //   .union(equations.flatMap(eq => Expression.atoms(eq.rhs)))
  // val init: Map[Expression, Double] = initMap(eqAtoms(equations), tg, initialState)

  lazy val exprCalc = new ExprCalc(this, init, equations, tg)

  /**
    * The final distributions, obtained from the initial one by finding an almost solution.
    */
  lazy val finalDist: Map[Expression, Double] =
    // exprCalc.finalDistMap
    exprCalc.finalMap //.seq
  // stableMap(init, equations, maxRatio, exponent, decay, maxTime)

  lazy val keys: Vector[Expression] = finalDist.keys.toVector

  /**
    * identifying an isle variable by having an initial value, but one that is not part of the initial distribution
    *
    * @param el the element to decide
    * @return whether the element is an isle-var
    */
  def isleVar(el: Elem[_]): Boolean =
    valueVars.contains(InitialVal(el)) && (el.randomVar == Terms) && !init.keySet
      .contains(InitialVal(el))

  /**
    * Terms in the initial distributions, used to calculate total weights of functions etc
    */
  lazy val initTerms: Vector[Term] =
    (Expression.allAtoms(equations.map(_.rhs), equations.map(_.lhs))
    // equations
    // .map(_.rhs)
    // .flatMap(Expression.atoms(_)
      union init.keySet).collect {
      case InitialVal(el @ Elem(t: Term, Terms)) if !isleVar(el) => t
    }.toVector

  /**
    * Terms in the final (i.e. evolved) distribution
    * * May have extra terms that evaluate to zero
    */
  lazy val finalTermSet: Set[Term] = equations
    .map(_.lhs)
    .collect {
      case FinalVal(el @ Elem(t: Term, Terms)) if !isleVar(el) => t
    }
    .toSet

  /**
    * Typs in the final (i.e. evolved) distribution
    * May have extra types that evaluate to zero
    */
  lazy val finalTypSet: Set[Typ[Term]] = equations
    .map(_.lhs)
    .collect {
      case FinalVal(el @ Elem(t: Typ[Term], Typs)) if !isleVar(el) => t
    }
    .toSet

  /**
    * final distribution on terms
    *
    */
  lazy val finalTerms: FD[HoTT.Term] =
    FD {
      finalDist.collect {
        case (FinalVal(Elem(t: Term, Terms)), w) if w > 0 => Weighted(t, w)
      }
    }.safeNormalized

  /**
    * final term state
    */
  def finalTermState(
      vars: Vector[Term] = Vector(),
      inds: FD[induction.ExstInducDefn] = FD.empty[induction.ExstInducDefn],
      goals: FD[Typ[Term]] = FD.empty,
      context: Context = Context.Empty
  ): TermState =
    TermState(finalTerms, finalTyps, vars, inds, goals, context)

  /**
    * equations not depending on a variable, to be used with boats
    */
  def indepEquations(variable: Term) =
    equations.filterNot(eq => TermRandomVars.equationDepends(variable)(eq))

  def lambdaExportEquations(
      variable: Term
  ): Set[Equation] = {
    // val initState = TermState(generators(init), finalTyps)
    import GeneratorNode._, TermGeneratorNodes._
    val isle =
      Island[Term, TermState, Term, Term](
        Terms,
        ConstRandVar(Terms),
        AddVar(variable.typ),
        LamApply,
        EnterIsle
      )
    import isle._
    // val (isleInit, boat) = initMap(initState) // boat is the same as variable
    val boat  = variable
    val coeff = Coeff(Base.lambdaNode)
    val isleEqs: Set[Equation] =
      equations.map(_.mapVars {
        InIsle.variableMap(boat, isle)
      })
    val bridgeEqs: Set[EquationNode] = finalTermSet.map { x =>
      EquationNode(
        FinalVal(Elem(export(boat, x), Terms)),
        coeff * FinalVal(
          InIsle(Elem(x, Terms), boat, isle)
        )
      )
    }
    lazy val initVarElems =
      // equations
      //   .flatMap { (eq) =>
      //     Expression.varVals(eq.rhs)
      //   }
      Expression
        .allVarVals(equations.map(_.rhs), Set())
        .collect {
          case InitialVal(Elem(el, rv)) => Elem(el, rv)
        }
    val isleIn: Set[EquationNode] =
      initVarElems.map { el =>
        val rhs =
          if (boat == el.element)
            (IsleScale(boat) * -1) + Literal(1)
          else IsleScale(boat) * InitialVal(el)
        EquationNode(
          InitialVal(InIsle(el, boat, isle)),
          rhs
        )
      }
    isleEqs union (Equation.group(isleIn union bridgeEqs))
  }

  def piExportEquations(
      variable: Term
  ): Set[Equation] = {
    // val initState = TermState(generators(init), finalTyps)
    import GeneratorNode._, TermGeneratorNodes._
    val isle =
      Island[Typ[Term], TermState, Typ[Term], Term](
        Typs,
        ConstRandVar(Typs),
        AddVar(variable.typ),
        PiApply,
        EnterIsle
      )
    import isle._
    val boat  = variable
    val coeff = Coeff(Base.piNode)
    val isleEqs: Set[Equation] =
      equations.map(_.mapVars {
        InIsle.variableMap(boat, isle)
      })
    val bridgeEqs: Set[EquationNode] = finalTypSet.map { x =>
      EquationNode(
        FinalVal(Elem(export(boat, x), Typs)),
        coeff * FinalVal(
          InIsle(Elem(x, Typs), boat, isle)
        )
      )
    }
    val initVarElems =
      // equations
      //   .flatMap { (eq) =>
      //     Expression.varVals(eq.rhs)
      //   }
      Expression
        .allVarVals(equations.map(_.rhs), Set())
        .collect {
          case InitialVal(Elem(el, rv)) => Elem(el, rv)
        }
    val isleIn: Set[EquationNode] =
      initVarElems.map { el =>
        val rhs =
          if (boat == el.element)
            (IsleScale(boat) * -1) + Literal(1)
          else IsleScale(boat) * InitialVal(el)
        EquationNode(
          InitialVal(InIsle(el, boat, isle)),
          rhs
        )
      }
    isleEqs union (Equation.group(isleIn union bridgeEqs))
  }

  def piTermExportEquations(
      variable: Term
  ): Set[Equation] = {
    // val initState = TermState(generators(init), finalTyps)
    import GeneratorNode._, TermGeneratorNodes._
    val isle =
      Island[Typ[Term], TermState, Typ[Term], Term](
        Typs,
        ConstRandVar(Typs),
        AddVar(variable.typ),
        PiApply,
        EnterIsle
      )
    import isle._
    val boat  = variable
    val coeff = Coeff(Base.piNode.|(typAsTermSort, Terms))
    val isleEqs: Set[Equation] =
      equations.map(_.mapVars {
        InIsle.variableMap(boat, isle)
      })
    val bridgeEqs: Set[EquationNode] = finalTypSet.map { x =>
      EquationNode(
        FinalVal(Elem(export(boat, x), Terms)),
        coeff * FinalVal(
          InIsle(Elem(x, Typs), boat, isle)
        )
      )
    }
    val initVarElems =
      // equations
      //   .flatMap { (eq) =>
      //     Expression.varVals(eq.rhs)
      //   }
      Expression
        .allVarVals(equations.map(_.rhs), Set())
        .collect {
          case InitialVal(Elem(el, rv)) => Elem(el, rv)
        }
    val isleIn: Set[EquationNode] =
      initVarElems.map { el =>
        val rhs =
          if (boat == el.element)
            (IsleScale(boat) * -1) + Literal(1)
          else IsleScale(boat) * InitialVal(el)
        EquationNode(
          InitialVal(InIsle(el, boat, isle)),
          rhs
        )
      }
    isleEqs union (Equation.group(isleIn union bridgeEqs))
  }

  def relVariable(x: Term): ExpressionEval = {
    val varWeight: Double = math.max(
      init.getOrElse(InitialVal(Elem(x, Terms)), 0.0),
      init.getOrElse(InitialVal(Elem(x, Typs)), 0.0)
    )
    val eqs = piExportEquations(x) union lambdaExportEquations(
      x
    ) union indepEquations(x) union piTermExportEquations(x)
    val newInit = init
      .map {
        case (exp @ InitialVal(Elem(y: Term, Terms)), w) =>
          if (y.dependsOn(x)) None else Some(exp, w / (1.0 - varWeight))
        case (InitialVal(Elem(y: Term, _)), v) if y.dependsOn(x) => None
        case (k, v)                                              => Some(k -> v)
      }
      .flatten
      .toMap
    new ExpressionEval with GenerateTyps {
      val init                                         = newInit
      val equations                                    = eqs
      val tg                                           = self.tg
      val coeffsAsVars                                 = self.coeffsAsVars
      val maxRatio                                     = self.maxRatio
      val scale                                        = self.scale
      val smoothing: Option[Double]                    = self.smoothing
      val exponent: Double                             = self.exponent
      val decay                                        = self.decay
      val maxTime: Option[Long]                        = self.maxTime
      val previousMap: Option[Map[Expression, Double]] = self.previousMap
    }
  }

  lazy val funcTotal: Expression = Sum(
    initTerms
      .filter(isFunc)
      .map { t =>
        InitialVal(Elem(t, Terms))
      }
  )
  // .fold[Expression](Literal(0))(_ + _)

  lazy val typFamilyTotal: Expression = Sum(
    initTerms
      .filter(isTypFamily)
      .map { t =>
        InitialVal(Elem(t, Terms))
      }
  )
  // .fold[Expression](Literal(0))(_ + _)

  lazy val initVarGroups: Map[(RandomVar[_], Vector[_]), Set[Expression]] =
    atoms
      .collect {
        case InitialVal(variable) => variable
      }
      .flatMap(x => elemContext(x).map(y => x -> y))
      .groupMap(_._2) { case (x, _) => InitialVal(x): Expression }

  lazy val finalVarGroups: Map[(RandomVar[_], Vector[_]), Set[Expression]] =
    atoms
      .collect {
        case FinalVal(variable) => variable
      }
      .flatMap(x => elemContext(x).map(y => x -> y))
      .groupMap(_._2) { case (x, _) => FinalVal(x): Expression }

  def expressionGroup(exp: Expression): Option[Set[Expression]] = exp match {
    case InitialVal(variable) =>
      elemContext(variable).flatMap(v => initVarGroups.get(v))
    case FinalVal(variable) =>
      elemContext(variable).flatMap(v => finalVarGroups.get(v))
    case _ => None
  }

  def normalizedMap(p: Map[Expression, Double]) = p map {
    case (exp, value) =>
      expressionGroup(exp)
        .map { s =>
          val total = s.toVector.map(x => p.getOrElse(x, 0.0)).sum
          if (total > 0) exp -> value / total else exp -> value
        }
        .getOrElse(exp -> value)
  }

  /**
    * Vector of all variables. This is frozen so that their indices can be used.
    */
  lazy val valueVars: Vector[Expression] =
    // equations
    //   .flatMap(eq => Set(eq.lhs, eq.rhs))
    //   .flatMap(exp => Expression.varVals(exp).map(t => t: Expression))
    Expression
      .allVarVals(equations.map(_.rhs), equations.map(_.lhs))
      .map(t => t: Expression)
      .toVector

  lazy val coefficients: Vector[Coeff[_]] =
    equations
      .flatMap(eq => Set(eq.lhs, eq.rhs))
      .flatMap(exp => Expression.coefficients(exp))
      .toVector

  lazy val coeffVariance: Expression =
    Sum(
      Utils
        .partition[Coeff[_]](coefficients, {
          case (c1, c2) => c1.sameFamily(c2, tg.nodeCoeffSeq)
        })
        .map(v => Expression.variance(v))
    )
  // .fold[Expression](Literal(0))(Sum(_, _))

  lazy val vars = if (coeffsAsVars) valueVars ++ coefficients else valueVars

  lazy val variableIndex: Map[Expression, Int] =
    vars.zipWithIndex.toMap

  implicit lazy val dim: JetDim = JetDim(vars.size)

  implicit lazy val jetField: Field[Jet[Double]] =
    implicitly[Field[Jet[Double]]]

  case class FixedExpressionProbs(p: Map[Expression, Double]) {
    lazy val spireVarProbs: Map[Expression, Jet[Double]] =
      vars.zipWithIndex.collect {
        case (v, n) if p.getOrElse(v, 0.0) > 0 && p(v) < 1 =>
          val t: Jet[Double] = Jet.h[Double](n)
          val r: Jet[Double] = p(v)
          val d: Jet[Double] = (r + (-1)) * (-1)
          val lx             = log(r / d) // value after inverse sigmoid
          val x              = lx + t
          val y              = exp(x) / (exp(x) + 1.0) // tangent before sigmoid
          v -> y
      }.toMap

    /**
      * Recursive jet for an expression, with variables transformed by sigmoid.
      */
    def jet(expr: Expression): Jet[Double] =
      spireVarProbs.getOrElse(
        expr,
        expr match {
          case Log(exp)       => log(jet(exp))
          case Exp(x)         => exp(jet(x))
          case Sum(xs)        => xs.map(jet(_)).reduce(_ + _)
          case Product(x, y)  => jet(x) * jet(y)
          case Literal(value) => value
          case Quotient(x, y) => jet(x) / jet(y)
          case iv @ InitialVal(el @ Elem(_, _)) =>
            el match {
              case Elem(fn: ExstFunc, Funcs) =>
                jet(InitialVal(Elem(fn.func, Terms)) / funcTotal)
              case Elem(t: Term, TypFamilies) =>
                jet(InitialVal(Elem(t, Terms)) / typFamilyTotal)
              case _ => p(iv)
            }
          case otherCase => p(otherCase)

        }
      )

    def jetTask(expr: Expression): Task[Jet[Double]] =
      spireVarProbs
        .get(expr)
        .map(Task.now(_))
        .getOrElse(
          expr match {
            case Log(exp) => jetTask(exp).map(j => log(j))
            case Exp(x)   => jetTask(x).map(j => exp(j))
            case Sum(xs) =>
              Task.parSequence(xs.map(jetTask(_))).map(_.reduce(_ + _))
            // for {
            //   a <- jetTask(x)
            //   b <- jetTask(y)
            // } yield a + b
            case Product(x, y) =>
              for {
                a <- jetTask(x)
                b <- jetTask(y)
              } yield a * b
            case Literal(value) => Task.now(value)
            case Quotient(x, y) =>
              for {
                a <- jetTask(x)
                b <- jetTask(y)
              } yield a / b
            case iv @ InitialVal(el @ Elem(_, _)) =>
              el match {
                case Elem(fn: ExstFunc, Funcs) =>
                  jetTask(InitialVal(Elem(fn.func, Terms)) / funcTotal)
                case Elem(t: Term, TypFamilies) =>
                  jetTask(InitialVal(Elem(t, Terms)) / typFamilyTotal)
                case _ => Task(p(iv))
              }
            case otherCase => Task(p(otherCase))

          }
        )

    /**
      * Gradients of equations for expression
      */
    lazy val eqnGradients: Vector[Vector[Double]] =
      eqnExpressions.map { exp =>
        jet(exp).infinitesimal.toVector
      }

    lazy val eqnGradientsTask: Task[Vector[Vector[Double]]] =
      Task.parSequence(eqnExpressions.map { exp =>
        jetTask(exp).map(_.infinitesimal.toVector)
      })

    lazy val onEqnGradientsTask: Task[Vector[Vector[Double]]] =
      for {
        eqns <- eqnGradientsTask
        ons  <- MonixGramSchmidt.onVec(eqns)
      } yield ons

    /**
      * Composite entropy projected perpendicular to the equations.
      */
    def entropyProjection(hW: Double = 1, klW: Double = 1): Vector[Double] = {
      val gradient = jet(entropy(hW, klW)).infinitesimal.toVector
      GramSchmidt.perpVec(eqnGradients, gradient)
    }

    def entropyProjectionTask(
        hW: Double = 1,
        klW: Double = 1
    ): Task[Vector[Double]] =
      for {
        der <- jetTask(entropy(hW, klW))
        gradient = der.infinitesimal.toVector
        eqg <- onEqnGradientsTask
        res <- MonixGramSchmidt.makePerpFromON(eqg, gradient)
      } yield res

    def flatEntropyProjectionTask(
        pow: Double,
        hW: Double = 1,
        klW: Double = 1
    ): Task[Vector[Double]] =
      for {
        der <- jetTask(flattenedEntropy(pow, hW, klW))
        gradient = der.infinitesimal.toVector
        eqg <- onEqnGradientsTask
        res <- MonixGramSchmidt.makePerpFromON(eqg, gradient)
      } yield res

    def expressionProjectionTask(
        exp: Expression
    ): Task[Vector[Double]] =
      for {
        der <- jetTask(exp)
        gradient = der.infinitesimal.toVector
        eqg <- onEqnGradientsTask
        res <- MonixGramSchmidt.makePerpFromON(eqg, gradient)
      } yield res

    def expressionGeneratorsTask(
        exp: Expression
    ): Task[Vector[(HoTT.Term, Double)]] = {
      val vT = expressionProjectionTask(exp)
      vT.map(
        v =>
          initTerms.map { t =>
            t -> v(variableIndex(InitialVal(Elem(t, Terms))))
          }
      )
    }

    lazy val typGradGeneratorsTask
        : Map[Typ[Term], Task[Vector[(Term, Double)]]] =
      finalTypSet.map { typ =>
        typ -> expressionGeneratorsTask(FinalVal(Elem(typ, Typs))).memoize
      }.toMap

    // Jets rewritten as maps of expression
    def jetCoordinates(
        jt: Jet[Double]
    ): Map[Expression, Double] =
      (for {
        (x, j) <- vars.zipWithIndex
        v = jt.infinitesimal(j)
        if v != 0
        scale = FixedExpressionProbs(p).jet(x).infinitesimal(j)
      } yield x -> v / scale).toMap

    def backMap(
        exp: Expression
    ): Map[Expression, Double] =
      jetCoordinates(jet(rhs(exp)))

    @annotation.tailrec
    final def recFullBackMap(
        base: Map[Expression, Double],
        cutoff: Double,
        accum: Map[Expression, Double]
    ): Map[Expression, Double] =
      if (base.isEmpty) accum
      else {
        val nextBase = MapVS.compose(base, backMap(_)).filter {
          case (exp, v) => math.abs(v) > cutoff
        }
        val nextAccum = accum + base
        recFullBackMap(nextBase, cutoff, nextAccum)
      }

    def fullBackMap(
        base: Map[Expression, Double],
        cutoff: Double
    ): Map[Expression, Double] =
      recFullBackMap(base, cutoff, Map())

    def fullBackMapExp(
        exp: Expression,
        cutoff: Double
    ): Map[Expression, Double] =
      recFullBackMap(Map(exp -> 1), cutoff, Map())

  }

  lazy val Final = FixedExpressionProbs(finalDist)

  /**
    * Terms of the generating distribution
    */
  lazy val genTerms: Map[Term, Expression] =
    initTerms.map(t => t -> InitialVal(Elem(t, Terms))).toMap

  lazy val thmSet: Set[Typ[Term]] =
    finalTyps.support.intersect(finalTermSet.map(_.typ)).filter(!isUniv(_))

  lazy val thmProbsByStatement: Map[Typ[Term], Double] = finalTyps
    .filter(typ => thmSet.contains(typ))
    .safeNormalized
    .toMap

  lazy val thmsByStatement: Map[Typ[Term], Expression] =
    thmProbsByStatement.view
      .mapValues(Literal)
      .toMap

  def proofExpression(typ: Typ[Term]): Expression =
    Sum(
      finalTermSet
        .filter(_.typ == typ)
        .map(t => FinalVal(Elem(t, Terms)))
        .toVector
    )
  // .reduce[Expression](_ + _)

  lazy val thmsByProof: Map[Typ[Term], Expression] =
    thmSet.map(typ => typ -> proofExpression(typ)).toMap

  /**
    * Expression for entropy of the generating distribution
    */
  lazy val hExp: Expression = Expression.h(genTerms)

  lazy val unknownsExp: Option[Expression] =
    Expression.unknownsCost(thmsByStatement, smoothing)

  lazy val unknownsValue: Option[Double] =
    Expression.simpleUnknownsCost(thmProbsByStatement, smoothing)

  /**
    * Expression for Kullback-Liebler divergence of proofs from statements of theorems.
    */
  lazy val klExp: Expression = {
    val base = Expression.kl(thmsByStatement, thmsByProof, smoothing)
    unknownsExp.map(exp => base + exp).getOrElse(base)
  }

  def flattenedKLExp(pow: Double) = {
    val base =
      Expression.klPower(thmProbsByStatement, thmsByProof, pow, smoothing)
    unknownsValue.map(exp => base + Literal(exp)).getOrElse(base)
  }

  lazy val finalTermMap: Map[Term, Expression] = finalTermSet.map { t =>
    t -> FinalVal(Elem(t, Terms))
  }.toMap

  lazy val finalTermEntropy: Expression = Expression.h(finalTermMap)

  lazy val finalTypMap: Map[Term, Expression] = finalTypSet.map { t =>
    t -> FinalVal(Elem(t, Terms))
  }.toMap

  lazy val finalTypEntropy: Expression = Expression.h(finalTypMap)

  lazy val initTermsSum = Sum(
    initTerms
      .map {
        case t => InitialVal(Elem(t, Terms))
      }
  )
  // .fold(Expression.Literal(0)) { (t1, t2) =>
  //   Sum(t1, t2)
  // }

  lazy val finalTermSetSum = Sum(finalTermSet.map {
    case t => FinalVal(Elem(t, Terms))
  }.toVector)
  // .fold(Expression.Literal(0)) { (t1, t2) =>
  //   Sum(t1, t2)
  // }

  /**
    * Expressions for equations.
    */
  lazy val eqnExpressions: Vector[Expression] =
    equations.toVector.map { eq =>
      eq.lhs - eq.rhs
    } ++ Vector(initTermsSum, finalTermSetSum)

  /**
    * Shift downwards by the gradient, mapped by sigmoids.
    */
  def gradShift(
      p: Map[Expression, Double],
      t: Vector[Double],
      eps: Double = scale
  ): Map[Expression, Double] = {
    p.map {
      case (expr, y) =>
        if (y == 0 || y == 1) expr -> y
        else
          variableIndex
            .get(expr)
            .map { n =>
              val x = log(y / (1 - y)) - (t(n) * eps)
              expr -> exp(x) / (1 + exp(x))
            }
            .getOrElse(expr -> y)
    }
  }

  def stableGradShift(
      p: Map[Expression, Double],
      t: Vector[Double],
      eps: Double = scale
  ): Map[Expression, Double] = {
    val newMap = normalizedMap(
      stableMap(
        gradShift(p, t, eps),
        equations,
        maxRatio,
        exponent,
        decay,
        maxTime
      )
    )
    newMap
  }

  /**
    * Expression for composite entropy.
    */
  def entropy(hW: Double = 1, klW: Double = 1): Expression =
    (hExp * hW) + (klExp * klW)

  def flattenedEntropy(
      pow: Double,
      hW: Double = 1,
      klW: Double = 1
  ): Expression =
    (hExp * hW) + (flattenedKLExp(pow) * klW)

  def iterator(
      hW: Double = 1,
      klW: Double = 1,
      p: Map[Expression, Double] = finalDist
  ): Iterator[Map[Expression, Double]] =
    Iterator.iterate(p)(
      q =>
        stableGradShift(q, FixedExpressionProbs(q).entropyProjection(hW, klW))
    )

  def iterant(
      hW: Double = 1,
      klW: Double = 1,
      p: Map[Expression, Double] = finalDist
  ): Iterant[Task, Map[Expression, Double]] =
    Iterant.fromLazyStateAction[Task, Map[Expression, Double], Map[
      Expression,
      Double
    ]] { q =>
      for {
        epg <- FixedExpressionProbs(q).entropyProjectionTask(hW, klW)
        s = stableGradShift(q, epg)
      } yield (s, s)
    }(Task.now(p))

  def generatorIterant(
      hW: Double = 1,
      klW: Double = 1,
      cutoff: Double,
      p: Map[Expression, Double] = finalDist
  ): Iterant[Task, FD[Term]] =
    Iterant.fromLazyStateAction[Task, Map[Expression, Double], FD[Term]] { q =>
      for {
        epg <- FixedExpressionProbs(q).entropyProjectionTask(hW, klW)
        s = stableGradShift(q, epg)
      } yield (generators(s).purge(cutoff).safeNormalized, s)
    }(Task.now(p))

  /**
    * Optimal value, more precisely stable under gradient flow.
    *
    * @param hW entropy weight
    * @param klW Kullback-Liebler weight
    * @param p Initial distribution
    * @return
    */
  def optimum(
      hW: Double = 1,
      klW: Double = 1,
      cutoff: Double,
      p: Map[Expression, Double] = finalDist,
      maxRatio: Double = 1.01
  ): Map[Expression, Double] = {
    val newMap =
      stableGradShift(p, FixedExpressionProbs(p).entropyProjection(hW, klW))
    if ((newMap.keySet == p.keySet) && (mapRatio(p, newMap) < maxRatio)) newMap
    else optimum(hW, klW, cutoff, newMap, maxRatio)
  }

  def optimumTask(
      hW: Double = 1,
      klW: Double = 1,
      cutoff: Double,
      p: Map[Expression, Double] = finalDist,
      maxRatio: Double = 1.01
  ): Task[Map[Expression, Double]] =
    (for {
      epg <- FixedExpressionProbs(p).entropyProjectionTask(hW, klW)
      newMap = stableGradShift(p, epg).filter(t => t._2 > cutoff)
      stable = ((newMap.keySet == p.keySet) && (mapRatio(p, newMap) < maxRatio))
    } yield stable -> newMap).flatMap {
      case (true, m)  => Task.now(m)
      case (false, m) => optimumTask(hW, klW, cutoff, m)
    }

  def flattenedOptimumTask(
      pow: Double,
      hW: Double = 1,
      klW: Double = 1,
      cutoff: Double,
      p: Map[Expression, Double] = finalDist,
      maxRatio: Double = 1.01
  ): Task[Map[Expression, Double]] =
    (for {
      epg <- FixedExpressionProbs(p).flatEntropyProjectionTask(pow, hW, klW)
      newMap = stableGradShift(p, epg).filter(t => t._2 > cutoff)
      stable = ((newMap.keySet == p.keySet) && (mapRatio(p, newMap) < maxRatio))
    } yield stable -> newMap).flatMap {
      case (true, m)  => Task.now(m)
      case (false, m) => flattenedOptimumTask(pow, hW, klW, cutoff, m)
    }

  // Backward step to see what terms were used in a given term.

  def resolveOpt(exp: Expression): Option[Expression] =
    equations.find(_.lhs == exp).map(_.rhs)

  def rhs(exp: Expression): Expression = resolveOpt(exp).getOrElse(Literal(0))

  def unitJet(p: Map[Expression, Double], exp: Expression): Jet[Double] =
    FixedExpressionProbs(p).spireVarProbs(exp)

  val mvs: VectorSpace[Map[Expression, Double], Double] =
    implicitly[VectorSpace[Map[Expression, Double], Double]]

}

trait EvolvedEquations[State] {
  val initState: State
  val finalState: State
  val equations: Set[Equation]

  def totalSquare(epsilon: Double): Expression =
    Sum(equations.map(_.squareError(epsilon)).toVector) //.reduce(_ + _)

  def mse(epsilon: Double): Expression = totalSquare(epsilon) / (equations.size)

}
