package provingground.learning
import provingground.{FiniteDistribution => FD, _}, HoTT._
import shapeless.HList._
import shapeless._

import monix.eval._

import GeneratorVariables._, Expression._, TermRandomVars._

import annotation.tailrec

import MonixFiniteDistributionEq._

import scala.util.Try

object ExpressionEval {
  val sd = implicitly[StateDistribution[TermState, FD]]

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

  def initMap(
      atoms: Set[Expression],
      tg: TermGenParams,
      initialState: TermState
  ): Map[Expression, Double] =
    (for {
      exp   <- atoms
      value <- initVal(exp, tg, initialState)
    } yield exp -> value).toMap

  def recExp(init: Map[Expression, Double], exp: Expression): Double =
    init.getOrElse(
      exp,
      exp match {
        case Sum(a, b)     => recExp(init, a) + recExp(init, b)
        case Log(a)        => math.log(recExp(init, a))
        case Product(x, y) => recExp(init, x) * recExp(init, y)
        case Literal(x)    => x
        case Quotient(x, y) =>
          if (recExp(init, y) != 0) recExp(init, x) / recExp(init, y)
          else recExp(init, x)
        case _ => 0
      }
    )

  def stabRecExp(
      init: Map[Expression, Double],
      exp: Expression,
      prev: Option[Double]
  ): Double = {
    val y = recExp(init, exp)
    math.sqrt(prev.getOrElse(y) * y)
  }

  def nextMap(
      init: Map[Expression, Double],
      equations: Set[Equation]
  ): Map[Expression, Double] = {
    init ++ equations
      .map(eq => eq.lhs -> stabRecExp(init, eq.rhs, init.get(eq.lhs)))
      .filter(_._2 != 0)
  }.toMap

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
    maxRatio: Double = 1.01
) {
  val atoms = equations
    .map(_.lhs)
    .union(equations.flatMap(eq => Expression.atoms(eq.rhs)))
  val init: Map[Expression, Double] = initMap(atoms, tg, initialState)

  val finalDist: Map[Expression, Double] = stableMap(init, equations, maxRatio)

  val keys      = finalDist.keys.toVector
  // val finalVars = keys.filter(_.isInstanceOf[FinalVal[_]])
  val initTerms = keys.collect {
    case InitialVal(el @ Elem(t: Term, Terms)) if !isIsleVar(el) => t
  }

  val finalTerms: Set[Term] = keys.collect {
    case FinalVal(el @ Elem(t: Term, Terms)) if !isIsleVar(el) => t
  }.toSet

  // val isleVars: Vector[Expression] = keys.collect {
  //   case InitialVal(el @ Elem(t: Term, Terms)) if isIsleVar(el) => InitialVal(el)
  //   case FinalVal(el @ Elem(t: Term, Terms)) if isIsleVar(el) => FinalVal(el)
  // }.toVector

  val finalTyps = sd.value(finalState)(Typs)

  // val initVars = initTerms.map { t =>
  //   InitialVal(Elem(t, Terms))
  // }
  val funcTotal: Expression = initTerms
    .filter(isFunc)
    .map { t =>
      InitialVal(Elem(t, Terms))
    }
    .fold[Expression](Literal(0))(_ + _)
  val typFamilyTotal = initTerms
    .filter(isTypFamily)
    .map { t =>
      InitialVal(Elem(t, Terms))
    }
    .fold[Expression](Literal(0))(_ + _)

  val vars = 
    equations.flatMap(eq => Set(eq.lhs, eq.rhs)).flatMap(exp => Expression.varVals(exp).map(t => t: Expression)).toVector
  //  initVars ++ finalVars

  lazy val variableIndex: Map[Expression, Int] =
    vars.zipWithIndex.toMap

  implicit val dim: JetDim = JetDim(vars.size)

  implicit val jetField: Field[Jet[Double]] = implicitly[Field[Jet[Double]]]

  def spireVarProbs(p: Map[Expression, Double]): Map[Expression, Jet[Double]] =
    vars.zipWithIndex.map {
      case (v, n) =>
        val t: Jet[Double] = Jet.h[Double](n)
        val r: Double      = p.getOrElse(v, 0.0)
        v -> (t + r)
    }.toMap

  def jet(p: Map[Expression, Double])(expr: Expression): Jet[Double] =
    spireVarProbs(p).getOrElse(
      expr,
      expr match {
        case Log(exp)       => log(jet(p)(exp))
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

  val genTerms: Map[Term, Expression] =
    initTerms.map(t => t -> InitialVal(Elem(t, Terms))).toMap

  val thmSet =
    finalTyps.support.toSet.intersect(finalTerms.map(_.typ)).filter(!isUniv(_))

  val thmsByStatement: Map[Typ[Term], Expression] = finalTyps
    .filter(typ => thmSet.contains(typ))
    .safeNormalized
    .toMap
    .mapValues(Literal(_))

  def proofExpression(typ: Typ[Term]) =
    finalTerms
      .filter(_.typ == typ)
      .map(t => FinalVal(Elem(t, Terms)))
      .reduce[Expression](_ + _)

  val thmsByProof: Map[Typ[Term], Expression] =
    thmSet.map(typ => typ -> proofExpression(typ)).toMap

  val hExp: Expression = Expression.h(genTerms)

  val klExp: Expression = Expression.kl(thmsByStatement, thmsByProof)

  val eqnExpressions: Vector[Expression] =
      equations.toVector.map{eq => eq.lhs - eq.rhs}

  def eqnGradients(p: Map[Expression, Double]) : Vector[Vector[Double]] = 
      eqnExpressions.map{
        exp => jet(p)(exp).infinitesimal.toVector
      }

  def entropy(hW: Double = 1, klW: Double = 1): Expression =
      (hExp * hW) + (klExp * klW)

  def entropyProjection(hW: Double = 1, klW: Double = 1)(
    p: Map[Expression, Double]) : Vector[Double] = {
      val gradient = jet(p)(entropy(hW, klW)).infinitesimal.toVector
      GramSchmidt.perpVec(eqnGradients(p), gradient)
    }

  // The below code using matching error. We should use orthogonal projections instead.
  lazy val matchKL: Expression = equations.map(_.klError).reduce(_ + _)

  def cost(hW: Double = 1, klW: Double = 1, matchW: Double = 1): Expression =
    (hExp * hW) + (klExp * klW) + (matchKL * matchW * (1.0 / tg.termInit))

  def shift(hW: Double = 1, klW: Double = 1, matchW: Double = 1)(
      p: Map[Expression, Double]
  ): Map[Expression, Double] = {
    val costJet: Jet[Double] = jet(p)(cost(hW, klW, matchW))
    variableIndex.mapValues(j => costJet.infinitesimal(j))
  }

  def shifted(hW: Double = 1, klW: Double = 1, matchW: Double = 1)(
      p: Map[Expression, Double],
      epsilon: Double = 0.1
  ) = {
    val q = shift(hW, klW, matchW)(p)
    for { (x, w) <- p } yield x -> (w - epsilon * q.getOrElse(x, 0.0))
  }

  def iterator(hW: Double = 1, klW: Double = 1, matchW: Double = 1)(
      p: Map[Expression, Double],
      epsilon: Double = 0.1
  ) =
    Iterator.iterate(p)(q => shifted(hW, klW, matchW)(q, epsilon))

}

trait EvolvedEquations[State, Boat] {
  val initState: State
  val finalState: State
  val equations: Set[Equation]

  def totalSquare(epsilon: Double): Expression =
    equations.map(_.squareError(epsilon)).reduce(_ + _)

  def mse(epsilon: Double): Expression = totalSquare(epsilon) / (equations.size)

}
