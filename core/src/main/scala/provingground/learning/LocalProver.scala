package provingground.learning
import provingground._, interface._
import HoTT._
import monix.eval.Task

import scala.concurrent._
import duration._

import TermRandomVars._

import GeneratorVariables._, Expression._

import EntropyAtomWeight._

import scalahott.NatRing
import monix.tail.Iterant
import provingground.learning.TypSolver.LookupSolver
import upickle.default._, scala.concurrent.duration._

object LocalProver {
  implicit def finDurRW: ReadWriter[FiniteDuration] =
    readwriter[ujson.Value].bimap(
      dur => ujson.Num(dur.toMillis.toDouble),
      js => FiniteDuration(ujson.read(js).num.toLong, MILLISECONDS)
    )

  import TermJson._, TermGenParams._

  implicit val lpRW: ReadWriter[LocalProver] = macroRW
}

/**
  * Collect local/generative/tactical proving;
  * this includes configuration and learning but excludes strategy and attention.
  * This can be called in a loop generating goals based on unproved theorems,
  * using representation/deep learning with attention focussed or interactively.
  */
case class LocalProver(
    initState: TermState =
      TermState(FiniteDistribution.empty, FiniteDistribution.empty),
    tg: TermGenParams = TermGenParams(),
    cutoff: Double = math.pow(10, -4),
    genMaxDepth: Option[Int] = None,
    limit: FiniteDuration = 12.minutes,
    maxRatio: Double = 1.01,
    scale: Double = 1.0,
    steps: Int = 10000,
    maxDepth: Int = 10,
    hW: Double = 1,
    klW: Double = 1,
    val smoothing: Option[Double] = None,
    relativeEval: Boolean = false,
    stateFromEquation: Boolean = false,
    exponent: Double = 0.5,
    decay: Double = 1,
    maxTime: Option[Long] = None
) extends LocalProverStep {
  // Convenience for generation

  def withCutoff(ctf: Double): LocalProver = this.copy(cutoff = ctf)

  override def sharpen(scale: Double = 2.0): LocalProver =
    withCutoff(cutoff / scale)

  def withLimit(l: FiniteDuration): LocalProver = this.copy(limit = l)

  def withParams(p: TermGenParams): LocalProver = this.copy(tg = p)

  def withInit(ts: TermState): LocalProver = this.copy(initState = ts)

  override def addVar(term: Term, weight: Double): LocalProver = {
    val td      = initState.terms .* (1 - weight) .+ (term, weight)
    val allVars = term +: initState.vars
    val ctx     = initState.context.addVariable(term)
    val ts =
      initState.copy(terms = td.safeNormalized, vars = allVars, context = ctx)
    withInit(ts)
  }

  def addTerms(terms: (Term, Double)*): LocalProver = {
    val total = terms.map(_._2).sum
    val td = initState.terms * (1 - total) ++ FiniteDistribution(terms.map {
      case (x, p) => Weighted(x, p)
    })
    val ts = initState.copy(terms = td)
    this.copy(initState = ts)
  }

  lazy val typesByRestrict: LocalProver = {
    val typd = initState.terms.collect { case tp: Typ[Term] => tp }.safeNormalized
    val ts   = initState.copy(typs = typd)
    this.copy(initState = ts)
  }

  lazy val typesByMap: LocalProver = {
    val typd = initState.terms.map(_.typ)
    val ts   = initState.copy(typs = typd)
    this.copy(initState = ts)
  }

  def addVars(terms: (Term, Double)*): LocalProver = {
    val total = terms.map(_._2).sum
    val td = initState.terms * (1 - total) ++ FiniteDistribution(terms.map {
      case (x, p) => Weighted(x, p)
    })
    val allVars = initState.vars ++ terms.map(_._1).toVector
    val ctx = terms.foldLeft(initState.context) {
      case (cx: Context, (y, _)) => cx.addVariable(y)
    }
    val ts =
      initState.copy(terms = td.safeNormalized, vars = allVars, context = ctx)
    this.copy(initState = ts)
  }

  def addTypes(typs: (Typ[Term], Double)*): LocalProver = {
    val total = typs.map(_._2).sum
    val typd = initState.typs * (1 - total) ++ FiniteDistribution(typs.map {
      case (x, p) => Weighted(x, p)
    })
    val ts = initState.copy(typs = typd.safeNormalized)
    this.copy(initState = ts)
  }

  def addAxioms(typs: (Typ[Term], Double)*): LocalProver = {
    val total = typs.map(_._2).sum
    val td = initState.terms * (1 - total) ++ FiniteDistribution(typs.map {
      case (x, p) => Weighted("axiom" :: x, p)
    })
    val ts = initState.copy(terms = td.safeNormalized)
    this.copy(initState = ts)
  }

  def addGoals(typs: (Typ[Term], Double)*): LocalProver = {
    val total = typs.map(_._2).sum
    val typd = initState.goals * (1 - total) ++ FiniteDistribution(typs.map {
      case (x, p) => Weighted(x, p)
    })
    val ts = initState.copy(goals = typd.safeNormalized)
    this.copy(initState = ts)
  }

  def addInd(
      typ: Typ[Term],
      intros: Term*
  )(params: Vector[Term] = Vector(), weight: Double = 1): LocalProver = {
    import induction._
    val str0 = ExstInducStrucs.get(typ, intros.toVector)
    val str = params.foldRight(str0) {
      case (x, s) => ExstInducStrucs.LambdaInduc(x, s)
    }
    val dfn     = ExstInducDefn(typ, intros.toVector, str)
    val indsNew = initState.inds * (1 - weight) ++ FiniteDistribution.unif(dfn)
    val ts      = initState.copy(inds = indsNew)
    this.copy(initState = ts)
  }

  def addIndexedInd(
      typ: Term,
      intros: Term*
  )(params: Vector[Term] = Vector(), weight: Double = 1): LocalProver = {
    import induction._
    val str0 = ExstInducStrucs.getIndexed(typ, intros.toVector)
    val str = params.foldRight(str0) {
      case (x, s) => ExstInducStrucs.LambdaInduc(x, s)
    }
    val dfn     = ExstInducDefn(typ, intros.toVector, str)
    val indsNew = initState.inds * (1 - weight) ++ FiniteDistribution.unif(dfn)
    val ts      = initState.copy(inds = indsNew)
    this.copy(initState = ts)
  }

  lazy val noIsles: LocalProver =
    this.copy(tg = tg.copy(lmW = 0, piW = 0, sigmaW = 0))

  def params(params: TermGenParams) = this.copy(tg = params)

  def isleWeight(w: Double): LocalProver =
    this.copy(tg = tg.copy(lmW = w, piW = w))

  def backwardWeight(w: Double): LocalProver =
    this.copy(tg = tg.copy(typAsCodW = w, targetInducW = w))

  def negateTypes(w: Double): LocalProver =
    this.copy(tg = tg.copy(negTargetW = w))

  def natInduction(w: Double = 1.0): LocalProver = {
    val mixin = initState.inds .+ (NatRing.exstInducDefn, w)
    this.copy(initState.copy(inds = mixin.safeNormalized))
  }

  // Proving etc
  lazy val nextStateDirect: Task[TermState] =
    tg.nextStateTask(initState, cutoff, limit).memoize

  lazy val mfd: MonixFiniteDistributionEq[TermState] =
    MonixFiniteDistributionEq(tg.nodeCoeffSeq, tg.varWeight, limit)

  lazy val mf = MonixFiniteDistribution(tg.nodeCoeffSeq, tg.varWeight)

  lazy val tripleT: Task[
    (FiniteDistribution[Term], Set[EquationNode], EqDistMemo[TermState])
  ] =
    mfd
      .varDist(initState, genMaxDepth, halted(), EqDistMemo.empty[TermState])(
        Terms,
        cutoff
      )
      .map { case (fd, eq, memo) => (fd, eq, memo) }
      .memoize

  lazy val tripleTypT: Task[
    (FiniteDistribution[Typ[Term]], Set[EquationNode], EqDistMemo[TermState])
  ] =
    mfd
      .varDist(initState, genMaxDepth, halted(), EqDistMemo.empty[TermState])(
        Typs,
        cutoff
      )
      .map { case (fd, eq, memo) => (fd, eq, memo) }
      .memoize

  lazy val nextState: Task[TermState] = {
    if (stateFromEquation)
      for {
        ev <- expressionEval
      } yield
        TermState(
          ev.finalTerms,
          ev.finalTyps,
          initState.vars,
          initState.inds,
          initState.goals,
          initState.context
        )
    else
      for {
        terms <- tripleT.map(_._1)
        typs  <- tripleTypT.map(_._1)
      } yield
        TermState(
          terms,
          typs,
          initState.vars,
          initState.inds,
          initState.goals,
          initState.context
        )
  }.memoize

  def varDist[Y](rv: RandomVar[Y]): Task[FiniteDistribution[Y]] =
    mfd.varDist(initState, genMaxDepth, halted())(rv, cutoff).map(_._1)

  def varDistEqs[Y](rv: RandomVar[Y]): Task[(FiniteDistribution[Y], Set[EquationNode] )] =
    mfd.varDist(initState, genMaxDepth, halted())(rv, cutoff).map{case (fd, eqs, _) => (fd, eqs)}
  

  def nodeDist[Y](node: GeneratorNode[Y]): Task[FiniteDistribution[Y]] =
    mfd
      .nodeDist(initState, genMaxDepth, halted())(
        node,
        cutoff,
        Expression.Coeff(node)
      )
      .map(_._1)

  lazy val equationNodes: Task[Set[EquationNode]] =
    for {
      terms <- tripleT.map(_._2)
      typs  <- tripleTypT.map(_._2)
    } yield terms ++ typs

  // Generating provers using results
  lazy val withLemmas: Task[LocalProver] =
    for {
      lws <- lemmas
      lfd = FiniteDistribution(lws.map {
        case (tp, p) => Weighted("lemma" :: tp, p)
      })
      lInit = initState.copy(terms = (initState.terms ++ lfd).safeNormalized)
    } yield this.copy(initState = lInit)

  lazy val optimalInit0: Task[LocalProver] = expressionEval.map { ev =>
    val p                            = ev.optimum(hW, klW, cutoff, ev.finalDist, maxRatio)
    val td: FiniteDistribution[Term] = ExpressionEval.dist(Terms, p)
    val ts                           = initState.copy(terms = td)
    this.copy(initState = ts)
  }

  lazy val optimalInit: Task[LocalProver] =
    for {
      ev <- expressionEval
      p  <- ev.optimumTask(hW, klW, cutoff, ev.finalDist, maxRatio)
      td: FiniteDistribution[Term] = ExpressionEval.dist(Terms, p)
      ts                           = initState.copy(terms = td)
    } yield this.copy(initState = ts)

  lazy val tunedInit: Task[LocalProver] =
    tunedGenerators.map { td =>
      val ts = initState.copy(terms = td)
      this.copy(initState = ts)
    }

}

trait LocalProverStep {
  val initState: TermState
  val nextState: Task[TermState]
  val tg: TermGenParams
  val cutoff: Double
  val genMaxDepth: Option[Int]
  val scale: Double
  val maxRatio: Double
  val steps: Int
  val maxDepth: Int
  val limit: FiniteDuration
  val hW: Double
  val klW: Double
  val smoothing: Option[Double]
  val relativeEval: Boolean
  val exponent: Double
  val decay: Double
  val stateFromEquation: Boolean
  val maxTime: Option[Long]

  var isHalted: Boolean = false

  def halt() = { isHalted = true }

  def reset() = { isHalted = false }

  def halted() = {
    val result = isHalted
    if (result) println("Halted")
    result
  }

  def withCutoff(cutoff: Double): LocalProverStep

  def withLimit(l: FiniteDuration): LocalProverStep

  def withParams(p: TermGenParams): LocalProverStep

  def withInit(ts: TermState): LocalProverStep

  def addVar(term: Term, weight: Double): LocalProverStep = {
    val td      = initState.terms .* (1 - weight) .+ (term, weight)
    val allVars = term +: initState.vars
    val ctx     = initState.context.addVariable(term)
    val ts =
      initState.copy(terms = td.safeNormalized, vars = allVars, context = ctx)
    withInit(ts)
  }

  def addGoal(goal: Typ[Term], weight: Double) = {
    val typd = initState.goals .* (1 - weight) .+ (goal, weight)
    val ts   = initState.copy(goals = typd.safeNormalized)
    withInit(ts)
  }

  def sharpen(scale: Double = 2.0) = withCutoff(cutoff / scale)

  def scaleLimit(scale: Double) =
    withLimit(Duration.fromNanos((limit.toNanos.toDouble * scale).toLong))

  def addSolver(s: TypSolver) = {
    val newSolver = tg.solver || s
    withParams(tg.copy(solver = newSolver))
  }

  def addLookup(ts: Set[Term]) = addSolver(LookupSolver(ts))

  lazy val evolvedState: Task[EvolvedState] = nextState
    .map(
      result => EvolvedState(initState, result, tg, cutoff)
    )
    .memoize

  lazy val theoremsByStatement: Task[FiniteDistribution[Typ[Term]]] =
    nextState.map(_.thmsBySt)

  lazy val theoremsByProof: Task[FiniteDistribution[Typ[Term]]] =
    nextState.map(_.thmsByPf)

  lazy val unknownStatements: Task[FiniteDistribution[Typ[Term]]] =
    nextState.map(_.unknownStatements)

  lazy val orderedUnknowns: Task[Vector[HoTT.Typ[HoTT.Term]]] =
    unknownStatements.map(fd => fd.entropyVec.map(_.elem))

  val equationNodes: Task[Set[EquationNode]]

  lazy val equations: Task[Set[Equation]] = equationNodes.map { eqs =>
    Equation.group(eqs)
  }.memoize

  def bigExpressionEval(additional: Set[Equation]): Task[ExpressionEval] =
    equations.map { eqs =>
      ExpressionEval.fromInitEqs(
        initState,
        Equation.merge(eqs, additional),
        tg,
        maxRatio,
        scale,
        smoothing,
        exponent,
        decay,
        maxTime
      )
    }

  import Utils._, scribe._

  lazy val enhancedExpressionEval: Task[ExpressionEval] =
    for {
      eqs <- equationNodes
      ns  <- nextState
    } yield {
      val eqnTypes = eqs.collect{case eq @ EquationNode(FinalVal(Elem(t: Term, Terms)), _) => t.typ: Typ[Term]}
      val additional = (ns.allTyps union eqnTypes).flatMap { typ =>
        val eqs = DE.formalTypEquations(typ)
        Expression.rhsOrphans(eqs).foreach {
          case (exp, eqq) => logger.debug(s"for type: $typ\n$exp\n orphan in formal $eqq")
        }
        eqs
      }
      // Expression.rhsOrphans(eqs).foreach {
      //   case (exp, eqq) => logger.error(s"$exp orphan in generated $eqq")
      // }
      // Expression.rhsOrphans(additional).foreach {
      //   case (exp, eqq) => logger.error(s"$exp orphan in formal $eqq")
      // }
      ExpressionEval.fromInitEqs(
        initState,
        Equation.group(eqs union additional),
        tg,
        maxRatio,
        scale,
        smoothing,
        exponent,
        decay,
        maxTime
      )
    }

  lazy val enhancedEquationNodes: Task[Set[EquationNode]] =
    for {
      eqs <- equationNodes
      ns  <- nextState
    } yield {
      val eqnTypes = eqs.collect{case eq @ EquationNode(FinalVal(Elem(t: Term, Terms)), _) => t.typ: Typ[Term]}
      def additional = (ns.allTyps union(eqnTypes)).flatMap { typ =>
        val eqs = DE.formalTypEquations(typ)
        eqs
          .collect {
            case eq @ EquationNode(
                  Expression.FinalVal(
                    GeneratorVariables.Elem(x: Term, TermRandomVars.Terms)
                  ),
                  _
                ) if isVar(x) =>
              eq
          }
          .foreach(
            eqq =>
              logger.error(s"formal equations for $typ ; bad equation: $eqq")
          )
        eqs
      }
      additional
        .collect {
          case eq @ EquationNode(
                Expression.FinalVal(
                  GeneratorVariables.Elem(x: Term, TermRandomVars.Terms)
                ),
                _
              ) if isVar(x) =>
            eq
        }
        .foreach(eqq => logger.error(s"Bad equation: $eqq"))
      Expression.rhsOrphans(eqs).foreach {
        case (exp, eqq) => logger.debug(s"$exp orphan in generated $eqq")
      }
      Expression.rhsOrphans(additional).foreach {
        case (exp, eqq) => logger.error(s"$exp orphan in formal $eqq")
      }
      eqs union additional
    }

  lazy val expressionEval: Task[ExpressionEval] =
    if (stateFromEquation)
      equations.map { eqs =>
        ExpressionEval.fromInitEqs(
          initState,
          eqs,
          tg,
          maxRatio,
          scale,
          smoothing,
          exponent,
          decay,
          maxTime
        )
      } else {
      val base = for {
        fs  <- nextState
        eqs <- equations
      } yield
        ExpressionEval.fromStates(
          initState,
          fs,
          eqs,
          tg,
          maxRatio,
          scale,
          smoothing,
          exponent,
          decay
        )
      if (relativeEval)
        if (initState.vars.isEmpty)
          base.map(_.generateTyps)
        else base.map(ev => ExpressionEval.export(ev, initState.vars))
      else base
    }.memoize

  lazy val successes : Task[Vector[(HoTT.Typ[HoTT.Term], Double, Term)]] = nextState.map(_.successes)

  lazy val lemmas: Task[Vector[(Typ[Term], Double)]] =
    (for {
      ev <- evolvedState
    } yield lemmaWeights(ev, steps, hW, klW, scale)).memoize

  lazy val lemmaProofs: Task[FiniteDistribution[HoTT.Term]] =
    for {
      v  <- lemmas
      ns <- nextState
      terms = ns.terms
    } yield
      v.map {
          case (tp, w) => terms.filter(_.typ == tp).safeNormalized * w
        }
        .fold(FiniteDistribution.empty[Term])(_ ++ _)

  lazy val proofTerms = {
    val pfExpsT = lemmaProofs.map(_.pmf.map {
      case Weighted(x, t) => FinalVal(Elem(x, Terms)) -> t
    })
    for {
      pfExps <- pfExpsT
      ev     <- expressionEval
    } yield
      ev.Final.fullBackMap(pfExps.toMap, cutoff).toVector.collect {
        case (FinalVal(Elem(x: Term, Terms)), w) => (x, w)
      }
  }

  // These are candidate generators
  lazy val proofComponents: Task[Vector[(HoTT.Term, Double)]] = for {
    basePfs <- proofTerms
    pfs = basePfs.filter(pfw => initState.terms(pfw._1) == 0).map {
      case (x, _) => x -> 1.0
    }
    ev <- evolvedState
  } yield EntropyAtomWeight.tunedProofs(ev, pfs, cutoff, steps, hW, klW, scale)

  lazy val seek: Task[FiniteDistribution[Term]] =
    for {
      base <- nextState
      goals <- findGoal(
        initState,
        base,
        tg,
        steps,
        maxDepth,
        cutoff,
        hW,
        klW,
        limit,
        scale
      )
    } yield goals

  lazy val functionsForGoals: Task[FiniteDistribution[Term]] =
    (nextState
      .flatMap { fs: TermState =>
        Task.parSequence(fs.remainingGoals.pmf.map {
          case Weighted(goal, p) =>
            val codomainTarget: Task[FiniteDistribution[Term]] = tg.monixFD
              .nodeDist(initState)(
                TermGeneratorNodes.codomainNode(goal),
                cutoff
              )
              .map(_ * p)
            val inductionTarget: Task[FiniteDistribution[Term]] = tg.monixFD
              .nodeDist(initState)(tg.Gen.targetInducBackNode(goal), cutoff)
              .map(_ * p)
            for {
              dcd <- codomainTarget
              did <- inductionTarget
            } yield (dcd ++ did).safeNormalized
        })
      })
      .map(vfd => vfd.foldLeft(FiniteDistribution.empty[Term])(_ ++ _))

  lazy val subGoals: Task[FiniteDistribution[Typ[Term]]] =
    for {
      funcs <- functionsForGoals
      fs    <- nextState
    } yield funcs.flatMap(fs.subGoalsFromFunc _)

  lazy val generatorIterant: Iterant[Task, FiniteDistribution[HoTT.Term]] =
    Iterant
      .liftF(expressionEval)
      .flatMap(ev => ev.generatorIterant(hW, klW, cutoff, ev.finalDist))

  lazy val tunedGenerators: Task[FiniteDistribution[HoTT.Term]] =
    generatorIterant
      .take(steps)
      .lastOptionL
      .map(os => os.getOrElse(initState.terms))

  def tangentProver(xs: Term*): Task[LocalTangentProver] =
    for {
      baseState <- nextState
      tangState: TermState = baseState.tangent(xs: _*)
      eqnds <- equationNodes
    } yield
      LocalTangentProver(
        baseState,
        eqnds,
        tangState,
        tg,
        cutoff,
        genMaxDepth,
        limit,
        maxRatio,
        scale,
        steps,
        maxDepth,
        hW,
        klW,
        smoothing,
        relativeEval,
        stateFromEquation,
        exponent,
        decay,
        maxTime
      )

  def distTangentProver(
      fd: FiniteDistribution[Term],
      tangentCutoff: Double = cutoff
  ): Task[LocalTangentProver] =
    for {
      baseState <- nextState
      tangState: TermState = baseState.distTangent(fd)
      eqnds <- equationNodes
    } yield
      LocalTangentProver(
        baseState,
        eqnds,
        tangState,
        tg,
        tangentCutoff,
        genMaxDepth,
        limit,
        maxRatio,
        scale,
        steps,
        maxDepth,
        hW,
        klW,
        smoothing,
        relativeEval,
        stateFromEquation,
        exponent,
        decay,
        maxTime
      )

  def tangentExpressionEval(
      x: Term,
      weight: Double = 1.0
  ): Task[ExpressionEval] =
    for {
      baseState <- nextState
      tangState: TermState = baseState.tangent(x)
      eqnds <- equationNodes
      mfdt = MonixTangentFiniteDistributionEq(
        tg.nodeCoeffSeq,
        tg.varWeight,
        baseState,
        eqnds,
        limit
      )
      teqnds <- mfdt
        .varDist(tangState, genMaxDepth, halted(), EqDistMemo.empty[TermState])(
          Terms,
          cutoff * weight
        )
        .map(_._2)
      tExpEval = ExpressionEval.fromStates(
        tangState,
        baseState,
        Equation.group(teqnds),
        tg,
        maxRatio,
        scale,
        smoothing,
        exponent,
        decay
      )
      expEv <- expressionEval
    } yield expEv.avgInit(tExpEval)

  def splitTangentProvers(
      terms: Vector[(Term, Double)]
  ): Task[Vector[LocalTangentProver]] =
    for {
      baseState <- nextState
      eqnds     <- equationNodes
    } yield
      terms.map {
        case (t, w) =>
          LocalTangentProver(
            baseState,
            eqnds,
            baseState.tangent(t),
            tg,
            cutoff / w,
            genMaxDepth,
            limit,
            maxRatio,
            scale,
            steps,
            maxDepth,
            hW,
            klW,
            smoothing,
        relativeEval,
        stateFromEquation,
        exponent,
        decay,
        maxTime
          )
      }

  def splitLemmaProvers(scale: Double = 1): Task[Vector[LocalTangentProver]] =
    lemmas.flatMap { v =>
      val pfs = v.map { case (tp, w) => ("proof" :: tp, w * scale) }
      splitTangentProvers(pfs)
    }

  def scaledSplitLemmaProvers(
      scale: Double = 1.0
  ): Task[Vector[LocalTangentProver]] =
    lemmas.flatMap { v =>
      val sc  = scale / v.map(_._2).sum
      val pfs = v.map { case (tp, w) => ("proof" :: tp, w * sc) }
      splitTangentProvers(pfs)
    }

  def proofTangent(tangentCutoff: Double = cutoff): Task[LocalTangentProver] =
    lemmaProofs.flatMap(
      fd => distTangentProver(fd.safeNormalized, tangentCutoff)
    )
}

object LocalTangentProver {
  import TermJson._, TermGenParams._

  implicit val lpRW: ReadWriter[LocalProver] = macroRW
}
case class LocalTangentProver(
    initState: TermState =
      TermState(FiniteDistribution.empty, FiniteDistribution.empty),
    initEquations: Set[EquationNode],
    tangentState: TermState,
    tg: TermGenParams = TermGenParams(),
    cutoff: Double = math.pow(10, -4),
    genMaxDepth: Option[Int] = None,
    limit: FiniteDuration = 3.minutes,
    maxRatio: Double = 1.01,
    scale: Double = 1.0,
    steps: Int = 10000,
    maxDepth: Int = 10,
    hW: Double = 1,
    klW: Double = 1,
    smoothing: Option[Double] = None,
    relativeEval: Boolean = false,
    stateFromEquation: Boolean = false,
    exponent: Double = 0.5,
    decay: Double = 1,
    maxTime: Option[Long] = None
) extends LocalProverStep {

  def withCutoff(ctf: Double): LocalTangentProver = this.copy(cutoff = ctf)

  override def sharpen(scale: Double = 2.0): LocalTangentProver =
    withCutoff(cutoff / scale)

  def withLimit(l: FiniteDuration): LocalTangentProver = this.copy(limit = l)

  def withParams(p: TermGenParams): LocalTangentProver = this.copy(tg = p)

  def withInit(ts: TermState): LocalProverStep = this.copy(initState = ts)

  def apple(w: Double = 0.1) = this.copy(tg = TermGenParams.apple(w))

  val mfd: MonixTangentFiniteDistributionEq[TermState] =
    MonixTangentFiniteDistributionEq(
      tg.nodeCoeffSeq,
      tg.varWeight,
      initState,
      initEquations,
      limit
    )

  def varDist[Y](rv: RandomVar[Y]): Task[FiniteDistribution[Y]] =
    mfd.varDist(initState, genMaxDepth, halted())(rv, cutoff).map(_._1)

  def nodeDist[Y](node: GeneratorNode[Y]): Task[FiniteDistribution[Y]] =
    mfd
      .nodeDist(initState, genMaxDepth, halted())(
        node,
        cutoff,
        Expression.Coeff(node)
      )
      .map(_._1)

  lazy val tripleT: Task[
    (FiniteDistribution[Term], Set[EquationNode], EqDistMemo[TermState])
  ] =
    mfd
      .varDist(tangentState, genMaxDepth, halted(), EqDistMemo.empty[TermState])(
        Terms,
        cutoff
      )
      .map { case (fd, eq, memo) => (fd, eq, memo) }
      .memoize

  lazy val tripleTypT: Task[
    (FiniteDistribution[Typ[Term]], Set[EquationNode], EqDistMemo[TermState])
  ] =
    mfd
      .varDist(tangentState, genMaxDepth, halted(), EqDistMemo.empty[TermState])(
        Typs,
        cutoff
      )
      .map { case (fd, eq, memo) => (fd, eq, memo) }
      .memoize

  lazy val nextState: Task[TermState] = {
    if (stateFromEquation)
      for {
        ev <- expressionEval
      } yield
        TermState(
          ev.finalTerms,
          ev.finalTyps,
          initState.vars,
          initState.inds,
          initState.goals,
          initState.context
        )
    else
      for {
        terms <- tripleT.map(_._1)
        typs  <- tripleTypT.map(_._1)
      } yield
        TermState(
          (terms ++ initState.terms).safeNormalized,
          (typs ++ initState.typs).safeNormalized,
          initState.vars,
          initState.inds,
          initState.goals,
          initState.context
        )
  }.memoize

  lazy val equationNodes: Task[Set[EquationNode]] =
    for {
      terms <- tripleT.map(_._2)
      typs  <- tripleTypT.map(_._2)
    } yield terms union typs union initEquations

}
