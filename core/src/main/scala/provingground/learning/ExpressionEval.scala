package provingground.learning
import provingground.{FiniteDistribution => FD, _}, HoTT._
import monix.eval._, monix.tail._

import GeneratorVariables._, TermRandomVars._, Expression._,
TermGeneratorNodes.{_}

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
      case cf @ Coeff(_) => cf.get(tg.nodeCoeffSeq)
      case InitialVal(elem @ Elem(el, rv)) =>
        val base = sd.value(initialState)(rv)(el)
        if (base > 0) Some(base)
        else if (isIsleVar(elem))
          Some(tg.varWeight / (1 - tg.varWeight)) // for the case of variables in islands
        else if (rv == Goals) Some(0.5)           // just a quick-fix
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

  def eqAtoms(equations: Set[Equation]) =
    equations
      .map(_.lhs)
      .union(equations.flatMap(eq => Expression.atoms(eq.rhs)))

  def fromStates(
      initialState: TermState,
      finalState: TermState,
      equationsS: Set[Equation],
      tgS: TermGenParams,
      maxRatioS: Double = 1.01,
      scaleS: Double = 1.0
  ) =
    new ExpressionEval {
      val init                  = initMap(eqAtoms(equationsS), tgS, initialState)
      val finalTyps             = finalState.typs
      val equations             = equationsS
      val tg                    = tgS
      val maxRatio              = maxRatioS
      val scale                 = scaleS
      val coeffsAsVars: Boolean = false
    }

  trait GenerateTyps extends ExpressionEval {
    lazy val finalTyps =
      FD {
        finalDist.collect {
          case (FinalVal(Elem(typ: Typ[Term], Typs)), w) => Weighted(typ, w)
        }
      }
  }
}

import spire.algebra._
import spire.math._
import spire.implicits._
import ExpressionEval._

trait ExpressionEval { self =>
  val init: Map[Expression, Double]
  val finalTyps: FD[Typ[Term]]
  val equations: Set[Equation]
  val tg: TermGenParams
  val coeffsAsVars: Boolean
  val maxRatio: Double
  val scale: Double

  def avgInit(that: ExpressionEval) =
    new ExpressionEval {
      val init           = (0.5 *: self.init) + (0.5 *: that.init)
      lazy val finalTyps = self.finalTyps
      val equations      = Equation.merge(self.equations, that.equations)
      val tg             = self.tg
      val coeffsAsVars   = self.coeffsAsVars
      val maxRatio       = self.maxRatio
      val scale          = self.scale
    }

  def modify(
      initNew: Map[Expression, Double] = self.init,
      finalTypsNew: => FD[Typ[Term]] = self.finalTyps,
      equationsNew: Set[Equation] = self.equations,
      tgNew: TermGenParams = self.tg,
      coeffsAsVarsNew: Boolean = self.coeffsAsVars,
      maxRatioNew: Double = self.maxRatio,
      scaleNew: Double = self.scale
  ): ExpressionEval = new ExpressionEval {
    val init: Map[Expression, Double] = initNew
    lazy val finalTyps: FD[Typ[Term]] = finalTypsNew
    val equations: Set[Equation]      = equationsNew
    val tg: TermGenParams             = tgNew
    val coeffsAsVars: Boolean         = coeffsAsVarsNew
    val maxRatio: Double              = maxRatioNew
    val scale: Double                 = scaleNew
  }

  def generateTyps: ExpressionEval = new ExpressionEval with GenerateTyps {
    val init         = self.init
    val equations    = self.equations
    val tg           = self.tg
    val coeffsAsVars = self.coeffsAsVars
    val maxRatio     = self.maxRatio
    val scale        = self.scale
  }

  /**
    * the atomic expressions in the equations
    */
  val atoms: Set[Expression] = equations
    .map(_.lhs)
    .union(equations.flatMap(eq => Expression.atoms(eq.rhs)))
  // val init: Map[Expression, Double] = initMap(eqAtoms(equations), tg, initialState)

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

  def lambdaExportEquations(
      variable: Term
  ): Set[Equation] = {
    val initState = TermState(generators(init), finalTyps)
    import GeneratorNode._, TermGeneratorNodes._
    val isle =
      Island[Term, TermState, Term, Term](
        Terms,
        ConstRandVar(Terms),
        ts => ts.addTerm(variable),
        LamApply,
        EnterIsle
      )
    import isle._
    val (isleInit, boat) = initMap(initState) // boat is the same as variable
    val coeff            = Coeff(Base.lambdaNode)
    val isleEqs: Set[Equation] =
      equations.map(_.mapVars { (x) =>
        InIsle(x, boat, isle)
      })
    val bridgeEqs: Set[EquationNode] = finalTerms.map { x =>
      EquationNode(
        FinalVal(Elem(export(boat, x), Terms)),
        coeff * FinalVal(
          InIsle(Elem(x, Terms), boat, isle)
        )
      )
    }
    val initVarElems = equations
      .flatMap { (eq) =>
        Expression.varVals(eq.rhs)
      }
      .collect {
        case InitialVal(Elem(el, rv)) => Elem(el, rv)
      }
    val isleIn: Set[EquationNode] =
      initVarElems.map { el =>
        val rhs =
          if (boat == el.element)
            (IsleScale(boat, el) * -1) + Literal(1)
          else IsleScale(boat, el) * InitialVal(el)
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
    val initState = TermState(generators(init), finalTyps)
    import GeneratorNode._, TermGeneratorNodes._
    val isle =
      Island[Typ[Term], TermState, Typ[Term], Term](
        Typs,
        ConstRandVar(Typs),
        ts => ts.addTerm(variable),
        PiApply,
        EnterIsle
      )
    import isle._
    val (isleInit, boat) = initMap(initState) // boat is the same as variable
    val coeff            = Coeff(Base.piNode)
    val isleEqs: Set[Equation] =
      equations.map(_.mapVars { (x) =>
        InIsle(x, boat, isle)
      })
    val bridgeEqs: Set[EquationNode] = finalTyps.support.map { x =>
      EquationNode(
        FinalVal(Elem(export(boat, x), Typs)),
        coeff * FinalVal(
          InIsle(Elem(x, Typs), boat, isle)
        )
      )
    }
    val initVarElems = equations
      .flatMap { (eq) =>
        Expression.varVals(eq.rhs)
      }
      .collect {
        case InitialVal(Elem(el, rv)) => Elem(el, rv)
      }
    val isleIn: Set[EquationNode] =
      initVarElems.map { el =>
        val rhs =
          if (boat == el.element)
            (IsleScale(boat, el) * -1) + Literal(1)
          else IsleScale(boat, el) * InitialVal(el)
        EquationNode(
          InitialVal(InIsle(el, boat, isle)),
          rhs
        )
      }
    isleEqs union (Equation.group(isleIn union bridgeEqs))
  }

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

  val initVarGroups: Map[(RandomVar[_], Vector[_]), Set[Expression]] =
    atoms
      .collect {
        case InitialVal(variable) => variable
      }
      .flatMap(x => elemContext(x).map(y => x -> y))
      .groupBy(_._2)
      .mapValues { s =>
        s.map { case (x, _) => InitialVal(x): Expression }
      }

  val finalVarGroups: Map[(RandomVar[_], Vector[_]), Set[Expression]] =
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
          if (total > 0) exp -> value / total else exp -> value
        }
        .getOrElse(exp -> value)
  }

  /**
    * Vector of all variables. This is frozen so that their indices can be used.
    */
  val valueVars: Vector[Expression] =
    equations
      .flatMap(eq => Set(eq.lhs, eq.rhs))
      .flatMap(exp => Expression.varVals(exp).map(t => t: Expression))
      .toVector

  lazy val coefficients: Vector[Coeff[_]] =
    equations
      .flatMap(eq => Set(eq.lhs, eq.rhs))
      .flatMap(exp => Expression.coefficients(exp))
      .toVector

  lazy val coeffVariance: Expression =
    Utils
      .partition[Coeff[_]](coefficients, {
        case (c1, c2) => c1.sameFamily(c2, tg.nodeCoeffSeq)
      })
      .map(v => Expression.variance(v))
      .fold[Expression](Literal(0))(Sum(_, _))

  lazy val vars = if (coeffsAsVars) valueVars ++ coefficients else valueVars

  lazy val variableIndex: Map[Expression, Int] =
    vars.zipWithIndex.toMap

  implicit val dim: JetDim = JetDim(vars.size)

  implicit val jetField: Field[Jet[Double]] = implicitly[Field[Jet[Double]]]

  case class WithP(p: Map[Expression, Double]) {
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
          case Sum(x, y)      => jet(x) + jet(y)
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
            case Sum(x, y) =>
              for {
                a <- jetTask(x)
                b <- jetTask(y)
              } yield a + b
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

    val eqnGradientsTask: Task[Vector[Vector[Double]]] =
      Task.gather(eqnExpressions.map { exp =>
        jetTask(exp).map(_.infinitesimal.toVector)
      })

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
        eqg <- eqnGradientsTask
        res <- MonixGramSchmidt.perpVec(eqg, gradient)
      } yield res

    // Jets rewritten as maps of expression
    def jetCoordinates(
        jt: Jet[Double]
    ): Map[Expression, Double] =
      (for {
        (x, j) <- vars.zipWithIndex
        v = jt.infinitesimal(j)
        if v != 0
        scale = WithP(p).jet(x).infinitesimal(j)
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

  lazy val Final = WithP(finalDist)

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

  lazy val finalTermMap: Map[Term, Expression] = finalTerms.map { t =>
    t -> FinalVal(Elem(t, Terms))
  }.toMap

  lazy val finalTermEntropy: Expression = Expression.h(finalTermMap)

  lazy val finalTypMap: Map[Term, Expression] = finalTyps.support.map { t =>
    t -> FinalVal(Elem(t, Terms))
  }.toMap

  lazy val finalTypEntropy: Expression = Expression.h(finalTypMap)

  lazy val initTermsSum = initTerms
    .map {
      case t => InitialVal(Elem(t, Terms))
    }
    .fold(Expression.Literal(0)) { (t1, t2) =>
      Sum(t1, t2)
    }

  lazy val finalTermsSum = finalTerms
    .map {
      case t => FinalVal(Elem(t, Terms))
    }
    .fold(Expression.Literal(0)) { (t1, t2) =>
      Sum(t1, t2)
    }

  /**
    * Expressions for equations.
    */
  val eqnExpressions: Vector[Expression] =
    equations.toVector.map { eq =>
      eq.lhs - eq.rhs
    } ++ Vector(initTermsSum, finalTermsSum)

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
    val newMap = normalizedMap(stableMap(gradShift(p, t, eps), equations))
    // if (p.keySet == newMap.keySet) pprint.log(mapRatio(p, newMap))
    // else {
    //   pprint.log(p.keySet -- newMap.keySet)
    //   pprint.log(newMap.keySet -- p.keySet)
    // }
    newMap
  }

  /**
    * Expression for composite entropy.
    */
  def entropy(hW: Double = 1, klW: Double = 1): Expression =
    (hExp * hW) + (klExp * klW)

  def iterator(
      hW: Double = 1,
      klW: Double = 1,
      p: Map[Expression, Double] = finalDist
  ): Iterator[Map[Expression, Double]] =
    Iterator.iterate(p)(
      q => stableGradShift(q, WithP(q).entropyProjection(hW, klW))
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
        epg <- WithP(q).entropyProjectionTask(hW, klW)
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
        epg <- WithP(q).entropyProjectionTask(hW, klW)
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
    val newMap = stableGradShift(p, WithP(p).entropyProjection(hW, klW))
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
      epg <- WithP(p).entropyProjectionTask(hW, klW)
      newMap = stableGradShift(p, epg).filter(t => t._2 > cutoff)
      stable = ((newMap.keySet == p.keySet) && (mapRatio(p, newMap) < maxRatio))
      // _ = pprint.log(stable)
    } yield stable -> newMap).flatMap {
      case (true, m)  => Task.now(m)
      case (false, m) => optimumTask(hW, klW, cutoff, m)
    }

  // Backward step to see what terms were used in a given term.

  def resolveOpt(exp: Expression): Option[Expression] =
    equations.find(_.lhs == exp).map(_.rhs)

  def rhs(exp: Expression): Expression = resolveOpt(exp).getOrElse(Literal(0))

  def unitJet(p: Map[Expression, Double], exp: Expression): Jet[Double] =
    WithP(p).spireVarProbs(exp)

  val mvs: VectorSpace[Map[Expression, Double], Double] =
    implicitly[VectorSpace[Map[Expression, Double], Double]]

}

trait EvolvedEquations[State] {
  val initState: State
  val finalState: State
  val equations: Set[Equation]

  def totalSquare(epsilon: Double): Expression =
    equations.map(_.squareError(epsilon)).reduce(_ + _)

  def mse(epsilon: Double): Expression = totalSquare(epsilon) / (equations.size)

}
