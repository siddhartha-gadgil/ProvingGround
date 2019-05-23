package provingground.learning
import provingground.{FiniteDistribution => FD, _}, HoTT._
import monix.eval._, monix.tail._

import GeneratorVariables._, TermRandomVars._

import annotation.tailrec

object ExpressionEval {
  val sd: StateDistribution[TermState, FD] =
    implicitly[StateDistribution[TermState, FD]]

  def dist[Y](rv: RandomVar[Y], p: Map[Expression, Double]): FD[Y] = {
    val pmf = p.collect {
      case (FinalVal(Elem(x, randomVar)), prob) if rv == randomVar =>
        Weighted(x.asInstanceOf[Y], prob)
    }
    FD(pmf)
  }

  def generators(p: Map[Expression, Double]) =
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

  // It is assumed that boats have all the isle information. If the argument is an `Elem`, perhaps nested in isles, we get boats and the random-variable
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

  // should we use the equations instead?
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

  val initVarGroups
      : Map[(RandomVar[_], Vector[_]), Set[GeneratorVariables.Expression]] =
    atoms
      .collect {
        case InitialVal(variable) => variable
      }
      .flatMap(x => elemContext(x).map(y => x -> y))
      .groupBy(_._2)
      .mapValues { s =>
        s.map { case (x, _) => InitialVal(x): Expression }
      }

  val finalVarGroups
      : Map[(RandomVar[_], Vector[_]), Set[GeneratorVariables.Expression]] =
    atoms
      .collect {
        case FinalVal(variable) => variable
      }
      .flatMap(x => elemContext(x).map(y => x -> y))
      .groupBy(_._2)
      .mapValues { s =>
        s.map { case (x, _) => FinalVal(x): Expression }
      }

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
          // pprint.log(s)
          // pprint.log(total)
          // pprint.log(value)
          // pprint.log(exp)
          if (total > 0) exp -> value / total else exp -> value
        }
        .getOrElse(exp -> value)
  }

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

  def jetTask(p: Map[Expression, Double])(expr: Expression): Task[Jet[Double]] =
    spireVarProbs(p)
      .get(expr)
      .map(Task.now(_))
      .getOrElse(
        expr match {
          case Log(exp) => jetTask(p)(exp).map(j => log(j))
          case Exp(x)   => jetTask(p)(x).map(j => exp(j))
          case Sum(x, y) =>
            for {
              a <- jetTask(p)(x)
              b <- jetTask(p)(y)
            } yield a + b
          case Product(x, y) =>
            for {
              a <- jetTask(p)(x)
              b <- jetTask(p)(y)
            } yield a * b
          case Literal(value) => Task.now(value)
          case Quotient(x, y) =>
            for {
              a <- jetTask(p)(x)
              b <- jetTask(p)(y)
            } yield a / b
          case iv @ InitialVal(el @ Elem(_, _)) =>
            el match {
              case Elem(fn: ExstFunc, Funcs) =>
                jetTask(p)(InitialVal(Elem(fn.func, Terms)) / funcTotal)
              case Elem(t: Term, TypFamilies) =>
                jetTask(p)(InitialVal(Elem(t, Terms)) / typFamilyTotal)
              case _ => Task(p(iv))
            }
          case otherCase => Task(p(otherCase))

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

  def eqnGradientsTask(
      p: Map[Expression, Double]
  ): Task[Vector[Vector[Double]]] =
    Task.gather(eqnExpressions.map { exp =>
      jetTask(p)(exp).map(_.infinitesimal.toVector)
    })

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
      eps: Double = epsilon
  ): Map[Expression, Double] =
    normalizedMap(stableMap(gradShift(p, t, eps), equations))

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

  def entropyProjectionTask(hW: Double = 1, klW: Double = 1)(
      p: Map[Expression, Double]
  ): Task[Vector[Double]] =
    for {
      der <- jetTask(p)(entropy(hW, klW))
      gradient = der.infinitesimal.toVector
      eqg <- eqnGradientsTask(p)
      res <- MonixGramSchmidt.perpVec(eqg, gradient)
    } yield res

  def iterator(
      hW: Double = 1,
      klW: Double = 1,
      p: Map[Expression, Double] = finalDist
  ): Iterator[Map[Expression, Double]] =
    Iterator.iterate(p)(q => stableGradShift(q, entropyProjection(hW, klW)(q)))

  def iterant(
      hW: Double = 1,
      klW: Double = 1,
      p: Map[Expression, Double] = finalDist
  ): Iterant[Task, Map[Expression, Double]] =
    Iterant.fromStateActionL[Task, Map[Expression, Double], Map[
      Expression,
      Double
    ]] { q =>
      for {
        epg <- entropyProjectionTask(hW, klW)(q)
        s = stableGradShift(q, epg)
      } yield (s, s)
    }(Task.now(p))

  def generatorIterant(
      hW: Double = 1,
      klW: Double = 1,
      p: Map[Expression, Double] = finalDist
  ): Iterant[Task, FD[Term]] =
    Iterant.fromStateActionL[Task, Map[Expression, Double], FD[Term]] { q =>
      for {
        epg <- entropyProjectionTask(hW, klW)(q)
        s = stableGradShift(q, epg)
      } yield (generators(s), s)
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
      p: Map[Expression, Double] = finalDist,
      maxRatio: Double = 1.01
  ): Map[Expression, Double] = {
    val newMap = stableGradShift(p, entropyProjection(hW, klW)(p))
    if ((newMap.keySet == p.keySet) && (mapRatio(p, newMap) < maxRatio)) newMap
    else optimum(hW, klW, newMap)
  }

  def optimumTask(
      hW: Double = 1,
      klW: Double = 1,
      p: Map[Expression, Double] = finalDist,
      maxRatio: Double = 1.01
  ): Task[Map[Expression, Double]] =
    for {
      epg <- entropyProjectionTask(hW, klW)(p)
      newMap = stableGradShift(p, epg)
      stable = ((newMap.keySet == p.keySet) && (mapRatio(p, newMap) < maxRatio))
      recRes <- Task.defer(optimumTask(hW, klW, newMap))
    } yield if (stable) newMap else recRes

  // Backward step to see what terms were used in a given term.

  def resolveOpt(exp: Expression): Option[Expression] =
    equations.find(_.lhs == exp).map(_.rhs)

  def rhs(exp: Expression): Expression = resolveOpt(exp).getOrElse(Literal(0))

  def unitJet(p: Map[Expression, Double], exp: Expression): Jet[Double] =
    spireVarProbs(p)(exp)

  // Jets rewritten as maps of expression
  def jetCoordinates(
      p: Map[Expression, Double],
      jt: Jet[Double]
  ): Map[Expression, Double] =
    (for {
      (x, j) <- vars.zipWithIndex
      v = jt.infinitesimal(j)
      if v != 0
      scale = jet(p)(x).infinitesimal(j)
    } yield x -> v / scale).toMap

  def backMap(p: Map[Expression, Double], exp: Expression) : Map[Expression, Double] =
    jetCoordinates(p, jet(p)(rhs(exp)))


}

trait EvolvedEquations[State, Boat] {
  val initState: State
  val finalState: State
  val equations: Set[Equation]

  def totalSquare(epsilon: Double): Expression =
    equations.map(_.squareError(epsilon)).reduce(_ + _)

  def mse(epsilon: Double): Expression = totalSquare(epsilon) / (equations.size)

}
