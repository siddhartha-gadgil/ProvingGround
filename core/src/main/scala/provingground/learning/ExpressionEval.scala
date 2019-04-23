package provingground.learning
import provingground.{FiniteDistribution => FD, _}, HoTT._


import GeneratorVariables._,  TermRandomVars._

import annotation.tailrec

object ExpressionEval {
  val sd: StateDistribution[TermState, FD] = implicitly[StateDistribution[TermState, FD]]

  def dist[Y](rv: RandomVar[Y], p: Map[Expression, Double]): FD[Y] = {
    val pmf = p.collect{case (FinalVal(Elem(x, randomVar)), prob) if rv == randomVar => Weighted(x.asInstanceOf[Y], prob)}
    FD(pmf)
  }

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
    case _ => false
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
      case cf @ Coeff(_, _) => cf.get(tg.nodeCoeffSeq)
      case InitialVal(elem @ Elem(el, rv)) =>
        val base = sd.value(initialState)(rv)(el)
        if (base > 0) Some(base)
        else if (isIsleVar(elem))
          Some(tg.varWeight / (1 - tg.varWeight)) // for the case of variables in islands
        else throw new Exception(s"no initial value for $elem")
      case IsleScale(_, _) => Some((1.0 - tg.varWeight))
      case _               => None
    }

  /**
    * Given a collection of atoms, returns map with values for them.
    */
  def initMap(
      atoms: Set[Expression],
      tg: TermGenParams,
      initialState: TermState
  ): Map[Expression, Double] =
    (for {
      exp   <- atoms
      value <- initVal(exp, tg, initialState)
    } yield exp -> value).toMap

  /**
    * Recursively calculate or update the value on expression, given initial values.
    */
  def recExp(init: Map[Expression, Double], exp: Expression): Double =
    init.getOrElse(
      exp,
      exp match {
        case Sum(a, b)     => recExp(init, a) + recExp(init, b)
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
    */
  def stabRecExp(
      init: Map[Expression, Double],
      exp: Expression,
      prev: Option[Double]
  ): Double = {
    val y = recExp(init, exp)
    math.sqrt(prev.getOrElse(y) * y)
  }

  /**
    * Updating a map given equations by replacing terms that are the lhs of an equation with rhs evaluated.
    */
  def nextMap(
      init: Map[Expression, Double],
      equations: Set[Equation]
  ): Map[Expression, Double] = {
    init ++ equations
      .map(eq => eq.lhs -> stabRecExp(init, eq.rhs, init.get(eq.lhs)))
      .filter(_._2 != 0)
  }

  /**
    * Iteratively update a map given equations until the support is stable (so we can safely calculate ratios).
    */
  @tailrec
  def stableSupportMap(
      init: Map[Expression, Double],
      equations: Set[Equation]
  ): Map[Expression, Double] = {
    val newMap = nextMap(init, equations)
    if (newMap.keySet == init.keySet) newMap
    else stableSupportMap(newMap, equations)
  }

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
    m1.map { case (k, v) => math.max(v / m2(k), (m2(k) / v)) }.max
  }

  /**
    * Iteratively update a map by equations till it is stable, i.e. almost satisfies the equations.
    */
  @tailrec
  def stableMap(
      init: Map[Expression, Double],
      equations: Set[Equation],
      maxRatio: Double = 1.01
  ): Map[Expression, Double] = {
    val newMap = nextMap(init, equations)
    if ((newMap.keySet == init.keySet) && mapRatio(newMap, init) < maxRatio)
      newMap
    else stableMap(newMap, equations, maxRatio)
  }
}

import spire.algebra._
import spire.math._
import spire.implicits._
import ExpressionEval._

case class ExpressionEval(
    initialState: TermState,
    finalState: TermState,
    equations: Set[Equation],
    tg: TermGenParams,
    maxRatio: Double = 1.01,
    epsilon: Double = 1.0
) {

  /**
    * the atomic expressions in the equations
    */
  val atoms: Set[Expression] = equations
    .map(_.lhs)
    .union(equations.flatMap(eq => Expression.atoms(eq.rhs)))
  val init: Map[Expression, Double] = initMap(atoms, tg, initialState)

  /**
    * The final distributions, obtained from the initial one by finding an almost solution.
    */
  val finalDist: Map[Expression, Double] = stableMap(init, equations, maxRatio)

  val keys: Vector[Expression] = finalDist.keys.toVector

  /**
    * Terms in the initial distributions, used to calculate total weights of functions etc
    */
  val initTerms: Vector[Term] = keys.collect {
    case InitialVal(el @ Elem(t: Term, Terms)) if !isIsleVar(el) => t
  }

  /**
    * Terms in the final (i.e. evolved) distribution
    */
  val finalTerms: Set[Term] = keys.collect {
    case FinalVal(el @ Elem(t: Term, Terms)) if !isIsleVar(el) => t
  }.toSet

  // TODO check if we need this or should be using the equations instead
  val finalTyps: FD[Typ[Term]] = sd.value(finalState)(Typs)

  val funcTotal: Expression = initTerms
    .filter(isFunc)
    .map { t =>
      InitialVal(Elem(t, Terms))
    }
    .fold[Expression](Literal(0))(_ + _)

  val typFamilyTotal: Expression = initTerms
    .filter(isTypFamily)
    .map { t =>
      InitialVal(Elem(t, Terms))
    }
    .fold[Expression](Literal(0))(_ + _)

  /**
    * Vector of all variables. This is frozen so that their indices can be used.
    */
  val vars: Vector[Expression] =
    equations
      .flatMap(eq => Set(eq.lhs, eq.rhs))
      .flatMap(exp => Expression.varVals(exp).map(t => t: Expression))
      .toVector

  lazy val variableIndex: Map[Expression, Int] =
    vars.zipWithIndex.toMap

  implicit val dim: JetDim = JetDim(vars.size)

  implicit val jetField: Field[Jet[Double]] = implicitly[Field[Jet[Double]]]

  /**
    * Jets for variables. These are transformed using the sigmoid so they are in (0, 1)
    */
  def spireVarProbs(p: Map[Expression, Double]): Map[Expression, Jet[Double]] =
    vars.zipWithIndex.map {
      case (v, n) if p.getOrElse(v, 0.0) > 0 =>
        val t: Jet[Double] = Jet.h[Double](n)
        val r: Jet[Double] = p(v)
        val d: Jet[Double] = r + (-1)
        val lx             = log(r / d) // value after inverse sigmoid
        val x              = lx + t
        val y              = exp(x) / (exp(x) + 1.0) // tangent before sigmoid
        v -> y
    }.toMap

  /**
    * Recursive jet for an expression, with variables transformed by sigmoid.
    */
  def jet(p: Map[Expression, Double])(expr: Expression): Jet[Double] =
    spireVarProbs(p).getOrElse(
      expr,
      expr match {
        case Log(exp)       => log(jet(p)(exp))
        case Exp(x)         => exp(jet(p)(x))
        case Sum(x, y)      => jet(p)(x) + jet(p)(y)
        case Product(x, y)  => jet(p)(x) * jet(p)(y)
        case Literal(value) => value
        case Quotient(x, y) => jet(p)(x) / jet(p)(y)
        case iv @ InitialVal(el @ Elem(_, _)) =>
          el match {
            case Elem(fn: ExstFunc, Funcs) =>
              jet(p)(InitialVal(Elem(fn.func, Terms)) / funcTotal)
            case Elem(t: Term, TypFamilies) =>
              jet(p)(InitialVal(Elem(t, Terms)) / typFamilyTotal)
            case _ => p(iv)
          }
        case otherCase => p(otherCase)

      }
    )

  /**
    * Terms of the generating distribution
    */
  val genTerms: Map[Term, Expression] =
    initTerms.map(t => t -> InitialVal(Elem(t, Terms))).toMap

  val thmSet: Set[Typ[Term]] =
    finalTyps.support.intersect(finalTerms.map(_.typ)).filter(!isUniv(_))

  val thmsByStatement: Map[Typ[Term], Expression] = finalTyps
    .filter(typ => thmSet.contains(typ))
    .safeNormalized
    .toMap
    .mapValues(Literal)

  def proofExpression(typ: Typ[Term]): Expression =
    finalTerms
      .filter(_.typ == typ)
      .map(t => FinalVal(Elem(t, Terms)))
      .reduce[Expression](_ + _)

  val thmsByProof: Map[Typ[Term], Expression] =
    thmSet.map(typ => typ -> proofExpression(typ)).toMap

  /**
    * Expression for entropy of the generating distribution
    */
  val hExp: Expression = Expression.h(genTerms)

  /**
    * Expression for Kullback-Liebler divergence of proofs from statements of theorems.
    */
  val klExp: Expression = Expression.kl(thmsByStatement, thmsByProof)

  /**
    * Expressions for equations.
    */
  val eqnExpressions: Vector[Expression] =
    equations.toVector.map { eq =>
      eq.lhs - eq.rhs
    }

  /**
    * Gradients of equations for expression
    */
  def eqnGradients(p: Map[Expression, Double]): Vector[Vector[Double]] =
    eqnExpressions.map { exp =>
      jet(p)(exp).infinitesimal.toVector
    }

  /**
    * Shift downwards by the gradient, mapped by sigmoids.
    */
  def gradShift(
      p: Map[Expression, Double],
      t: Vector[Double],
      eps: Double = epsilon
  ): Map[Expression, Double] = {
    p.map {
      case (expr, y) =>
        variableIndex
          .get(expr)
          .map { n =>
            val x = log(y / (1 - y)) - (t(n) * eps)
            expr -> (exp(x) / 1 + exp(x))
          }
          .getOrElse(expr -> y)
    }
  }

  def stableGradShift(
      p: Map[Expression, Double],
      t: Vector[Double],
      eps: Double = epsilon
  ): Map[Expression, Double] =
    stableMap(gradShift(p, t, eps), equations)

  /**
    * Expression for composite entropy.
    */
  def entropy(hW: Double = 1, klW: Double = 1): Expression =
    (hExp * hW) + (klExp * klW)

  /**
    * Composite entropy projected perpendicular to the equations.
    */
  def entropyProjection(hW: Double = 1, klW: Double = 1)(
      p: Map[Expression, Double]
  ): Vector[Double] = {
    val gradient = jet(p)(entropy(hW, klW)).infinitesimal.toVector
    GramSchmidt.perpVec(eqnGradients(p), gradient)
  }

  def iterator(
      hW: Double = 1,
      klW: Double = 1,
      p: Map[Expression, Double] = finalDist
  ): Iterator[Map[Expression, Double]] =
    Iterator.iterate(p)(q => stableGradShift(q, entropyProjection(hW, klW)(q)))

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
      p: Map[Expression, Double] = finalDist
  ): Map[Expression, Double] = {
    val newMap = stableGradShift(p, entropyProjection(hW, klW)(p))
    if (newMap.keySet == init.keySet) newMap
    else optimum(hW, klW, newMap)
  }

  /**
    * Jet converted to map, scaled for probabilities
    */
  def jetMap(jet: Jet[Double],
             p: Map[Expression, Double] = finalDist): Map[Expression, Double] =
    (for {
      (x, j) <- vars.zipWithIndex
      v = jet.infinitesimal(j)
      if v > 0
      y = p(x)
      w = v * (exp(y) + 1) * (exp(y) + 1) / exp(y)
    } yield x -> w).toMap

  def resolveOpt(exp: Expression): Option[Expression] =
    equations.find(_.lhs == exp).map(_.rhs)

  // Should correct for sigmoid transformation
  def backStep(
      exp: Expression,
      p: Map[Expression, Double] = finalDist): Map[Expression, Double] =
    jetMap(jet(p)(resolveOpt(exp).getOrElse(Literal(0))))


}

trait EvolvedEquations[State, Boat] {
  val initState: State
  val finalState: State
  val equations: Set[Equation]

  def totalSquare(epsilon: Double): Expression =
    equations.map(_.squareError(epsilon)).reduce(_ + _)

  def mse(epsilon: Double): Expression = totalSquare(epsilon) / (equations.size)

}
