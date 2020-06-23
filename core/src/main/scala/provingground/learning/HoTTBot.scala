package provingground.learning

import provingground._, HoTT._
import TypedPostResponse._
import monix.eval._
import LocalQueryable._
import monix.execution.Scheduler.Implicits.{global => monixglobal}
import scala.concurrent._
import TermData._
import shapeless._
import scala.collection.SeqView
import scala.reflect.runtime.universe._
import HoTTMessages._
import Utils.logger
import scala.concurrent._, duration._

case class QueryProver(lp: LocalProver)

object QueryProver {
  implicit val qc: QueryFromPosts[
    QueryProver,
    Weight :: LocalProver :: HNil
  ] =
    QueryFromPosts
      .empty[QueryProver]
      .addCons((lp: LocalProver) => Some(QueryProver(lp)))
      .addMod((w: Weight) => qp => QueryProver(qp.lp.sharpen(w.scale)))
}

case class QueryInitState(init: TermState)

object QueryInitState {
  implicit val qc: QueryFromPosts[
    QueryInitState,
    InitState :: LocalProver :: HNil
  ] =
    QueryFromPosts
      .empty[QueryInitState]
      .addCons((lp: LocalProver) => Some(QueryInitState(lp.initState)))
      .addCons((s: InitState) => Some(QueryInitState(s.ts)))
}

case class QueryBaseState(init: TermState)

object QueryBaseState {
  implicit val qc: QueryFromPosts[
    QueryBaseState,
    InitState :: LocalTangentProver :: LocalProver :: HNil
  ] =
    QueryFromPosts
      .empty[QueryBaseState]
      .addCons((lp: LocalProver) => Some(QueryBaseState(lp.initState)))
      .addCons((lp: LocalTangentProver) => Some(QueryBaseState(lp.initState)))
      .addCons((s: InitState) => Some(QueryBaseState(s.ts)))
}

object HoTTBot {

  type ID = HoTTPostWeb.ID

  type HoTTBot = PostResponse[HoTTPostWeb, ID]

  type SimpleBot[P, Q] = MicroBot[P, Q, HoTTPostWeb, Unit, ID]

  type MicroHoTTBoTT[P, Q, V] = MicroBot[P, Q, HoTTPostWeb, V, ID]

  lazy val lpToExpEv: SimpleBot[LocalProver, ExpressionEval] = {
    val response: Unit => LocalProver => Future[ExpressionEval] = (_) =>
      lp => lp.expressionEval.runToFuture
    MicroBot(response)
  }

  lazy val lpToBigExpEv
      : MicroHoTTBoTT[LocalProver, ExpressionEval, Set[Equation]] = {
    val response: Set[Equation] => LocalProver => Future[ExpressionEval] =
      (adEqs) => lp => lp.bigExpressionEval(adEqs).runToFuture
    MicroBot(response)
  }

  lazy val lpToEnhancedExpEv: SimpleBot[LocalProver, ExpressionEval] = {
    val response: Unit => LocalProver => Future[ExpressionEval] = (_) =>
      lp => lp.enhancedExpressionEval.runToFuture
    MicroBot(response)
  }

  lazy val lptToEnhancedExpEv: SimpleBot[LocalTangentProver, ExpressionEval] = {
    val response: Unit => LocalTangentProver => Future[ExpressionEval] = (_) =>
      lp => lp.enhancedExpressionEval.runToFuture
    MicroBot(response)
  }

  lazy val lptToExpEv: SimpleBot[LocalTangentProver, ExpressionEval] = {
    val response: Unit => LocalTangentProver => Future[ExpressionEval] = (_) =>
      lp => lp.expressionEval.runToFuture
    MicroBot(response)
  }

  lazy val lpToTermResult: SimpleBot[LocalProver, TermResult] = {
    val response: Unit => LocalProver => Future[TermResult] = (_) =>
      lp => termData(lp).runToFuture
    MicroBot(response)
  }

  val lpToFinalState: SimpleBot[LocalProver, FinalState] = {
    val response: Unit => LocalProver => Future[FinalState] = (_) =>
      lp => lp.nextState.map(FinalState(_)).runToFuture
    MicroBot(response)
  }

  val lptToFinalState: SimpleBot[LocalTangentProver, FinalState] = {
    val response: Unit => LocalTangentProver => Future[FinalState] = (_) =>
      lp => lp.nextState.map(FinalState(_)).runToFuture
    MicroBot(response)
  }

  val finalStateToLemma: SimpleBot[FinalState, Lemmas] =
    MicroBot.simple(
      (fs) => Lemmas(fs.ts.lemmas)
    )

  val finalStateToNewLemmas
      : MicroHoTTBoTT[FinalState, Lemmas, QueryBaseState] = {
    val response: QueryBaseState => FinalState => Future[Lemmas] =
      (qinit) =>
        (fs) =>
          Future {
            val proved = qinit.init.terms.map(_.typ).support
            Lemmas(fs.ts.lemmas.filterNot {
              case (tp, _, _) => proved.contains(tp)
            })
          }
    MicroBot(response)
  }

  def tautologyTerms(
      tautGen: TermState,
      eqs: Set[Equation],
      tg: TermGenParams = TermGenParams.zero.copy(appW = 0.1, unAppW = 0.1),
      maxRatio: Double = 1.5,
      maxTime: Option[Long] = Some(60000L)
  ): Set[Term] = {
    val expEv = ExpressionEval.fromInitEqs(
      tautGen,
      equationsS = eqs,
      tgS = tg,
      maxRatioS = maxRatio,
      maxTimeS = maxTime
    )
    expEv.finalTerms.support
  }

  def finalStateFilteredLemmas(
      tg: TermGenParams = TermGenParams.zero.copy(appW = 0.1, unAppW = 0.1),
      maxRatio: Double = 1.5,
      maxTime: Option[Long] = Some(60000L)
  ): MicroHoTTBoTT[FinalState, Lemmas, PreviousPosts[TautologyInitState] :: Set[
    Equation
  ] :: QueryBaseState :: HNil] = {
    val response
        : PreviousPosts[TautologyInitState] :: Set[Equation] :: QueryBaseState :: HNil => FinalState => Future[
          Lemmas
        ] = {
      case (tauts :: eqns :: qinit :: HNil) =>
        (fs) => {
          val tautTerms = Future
            .sequence(tauts.contents.map { t =>
              Future(tautologyTerms(t.tautGen, eqns, tg, maxRatio, maxTime))
            })
            .map(v => v.fold(Set.empty[Term])(_ union _))
          tautTerms.map { tt =>
            val proved =
              (tt union
                qinit.init.terms.support).map(_.typ)
            Lemmas(fs.ts.lemmas.filterNot {
              case (tp, _, _) => proved.contains(tp)
            })
          }
        }
    }
    MicroBot(response)
  }

  lazy val lptToTermResult: SimpleBot[LocalTangentProver, TermResult] = {
    val response: Unit => LocalTangentProver => Future[TermResult] = (_) =>
      lp => termData(lp).runToFuture
    MicroBot(response)
  }

  lazy val termResultToEquations
      : SimpleBot[TermResult, GeneratedEquationNodes] =
    MicroBot.simple(
      (pair: TermResult) => GeneratedEquationNodes(pair._2)
    )

  lazy val termResultToFinalState: SimpleBot[TermResult, FinalState] =
    MicroBot.simple(
      (pair: TermResult) => FinalState(pair._1)
    )

  lazy val postProofsSimple: SimpleBot[FinalState, Option[Set[Proved]]] =
    MicroBot.simple(
      (fs: FinalState) =>
        if (fs.successes.isEmpty) None
        else
          Some(fs.successes.map {
            case (tp, _, pf) =>
              Proved(tp, Some(pf), Context.Empty)
          }.toSet)
    )

  lazy val postProofs: MiniBot[FinalState, Proved, HoTTPostWeb, Unit, ID] = {
    val response: Unit => FinalState => Future[Vector[Proved]] =
      (_) =>
        (fs) =>
          Future {
            (fs.successes.map {
              case (tp, _, pf) =>
                Proved(tp, Some(pf), Context.Empty)
            } ++ fs.ts.successes.map {
              case (tp, _, pf) =>
                Proved(tp, Some(pf), fs.ts.context)
            }).distinct
          }
    MiniBot(response)
  }

  lazy val updateTerms: Callback[FinalState, HoTTPostWeb, Unit, ID] =
    Callback.simple { (web: HoTTPostWeb) => (fs: FinalState) =>
      val allTerms = fs.ts.terms.support union (fs.ts.typs.support
        .map(t => t: Term))
      allTerms
        .filter(isVar(_))
        .foreach(t => logger.error(s"variable $t considered term"))
      web.addTerms(allTerms)
      web.updateDerived()
    }

  lazy val reportSuccesses: TypedPostResponse[FinalState, HoTTPostWeb, ID] =
    Callback.simple { (web: HoTTPostWeb) => (fs: FinalState) =>
      if (fs.successes.size > 0) {
        logger.info("Success: " + fs.successes.toString())
        Utils.report("Success: " + fs.successes.toString())
      }
    }

  def reportProofs(
      results: Vector[Typ[Term]],
      text: String = "Results"
  ): Callback[FinalState, HoTTPostWeb, Unit, ID] =
    Callback.simple { (web: HoTTPostWeb) => (fs: FinalState) =>
      val termsSet = fs.ts.terms.support
      val pfs = results
        .map(typ => typ -> termsSet.filter(_.typ == typ))
        .filter(_._2.nonEmpty)
      val view = s"$text: ${pfs.size}\n${pfs
        .map {
          case (tp, ps) =>
            val best = ps.maxBy(t => fs.ts.terms(t))
            s"Lemma: $tp; best proof: ${best} with weight ${fs.ts.terms(best)}; statement weight ${fs.ts.typs(tp)}"
        }
        .mkString("\n")}"
      logger.info(view)
      Utils.report(view)
    }

  def reportTangentLemmas(
      results: Vector[Typ[Term]]
  ): TypedPostResponse[TangentLemmas, HoTTPostWeb, ID] =
    Callback.simple { (web: HoTTPostWeb) => (ls: TangentLemmas) =>
      val tls = results
        .flatMap(typ => ls.lemmas.find(_._1 == typ).map(typ -> _._3))
      val view = s"Tangent lemmas: ${tls.size}\n${tls
        .mkString("\n")}"
      logger.info(view)
      Utils.report(view)
    }

  def reportMixinLemmas(
      results: Vector[Typ[Term]]
  ): TypedPostResponse[BaseMixinLemmas, HoTTPostWeb, ID] =
    Callback.simple { (web: HoTTPostWeb) => (ls: BaseMixinLemmas) =>
      val tls = results
        .flatMap(typ => ls.lemmas.find(_._1 == typ).map(typ -> _._3))
      val view = s"Base mixin lemmas: ${tls.size}\n${tls
        .mkString("\n")}"
      logger.info(view)
      Utils.report(view)
    }

  def reportTangentBaseTerms(
      steps: Vector[Typ[Term]]
  ): TypedPostResponse[TangentBaseState, HoTTPostWeb, ID] =
    Callback.simple { (web: HoTTPostWeb) => (fs: TangentBaseState) =>
      val termsSet = fs.ts.terms.support
      val pfs = steps
        .map(typ => typ -> termsSet.filter(_.typ == typ))
        .filter(_._2.nonEmpty)
      val view = s"Terms in base: ${pfs.size}\n${pfs
        .map {
          case (tp, ps) =>
            val best = ps.maxBy(t => fs.ts.terms(t))
            s"Type: $tp; best term: ${best} with weight ${fs.ts.terms(best)}"
        }
        .mkString("\n")}"
      logger.info(view)
      Utils.report(view)
    }

  def reportBaseTangentPairs(
      results: Vector[Typ[Term]],
      steps: Vector[Typ[Term]]
  ): Callback[
    TangentBaseState,
    HoTTPostWeb,
    TangentLemmas,
    ID
  ] = {
    val update = (_: HoTTPostWeb) =>
      (ls: TangentLemmas) =>
        (fs: TangentBaseState) =>
          Future {
            val tls = results
              .flatMap(typ => ls.lemmas.find(_._1 == typ).map(typ -> _._3))
            val view1 =
              s"Tangent lemmas (used with base below): ${tls.size}\n${tls
                .mkString("\n")}"
            val termsSet = fs.ts.terms.support
            val pfs = steps
              .map(typ => typ -> termsSet.filter(_.typ == typ))
              .filter(_._2.nonEmpty)
            val view2 =
              s"Terms in base (used with tangents above): ${pfs.size}\n${pfs
                .map {
                  case (tp, ps) =>
                    val best = ps.maxBy(t => fs.ts.terms(t))
                    s"Type: $tp; best term: ${best} with weight ${fs.ts.terms(best)}"
                }
                .mkString("\n")}"
            logger.info(view1 + "\n" + view2 + "\n\n")
            Utils.report(view1 + "\n" + view2 + "\n\n")
          }
    Callback(update)
  }

  lazy val expEvToEqns: SimpleBot[ExpressionEval, GeneratedEquationNodes] =
    MicroBot.simple(
      (ev: ExpressionEval) =>
        GeneratedEquationNodes(ev.equations.flatMap(Equation.split))
    )

  lazy val expEvToFinalState: SimpleBot[ExpressionEval, FinalState] =
    MicroBot.simple(
      (ev: ExpressionEval) => FinalState(ev.finalTermState())
    )

  lazy val instanceToGoal: MicroBot[Instance, Option[
    Consequence :: SeekGoal :: HNil
  ], HoTTPostWeb, SeekInstances, ID] = {
    val response: SeekInstances => Instance => Future[
      Option[Consequence :: SeekGoal :: HNil]
    ] = {
      case seek: SeekInstances => {
        case instance: Instance =>
          Future(
            if (instance.typ == seek.typ) {
              val newGoal = (seek: SeekInstances)
                .goalCast(instance.term)
              val x = instance.typ.Var
              val y = newGoal.Var
              // val z = (seek: SeekInstances)
              //   .goalCast(y)
              val deduction =
                y :~> fold(seek.sigma.paircons)(instance.term, y: Term)
              //  mkPair(instance.term.asInstanceOf[Term], y: Term)
              // translation.FansiShow.fansiPrint.log(newGoal)
              // translation.FansiShow.fansiPrint.log(seek.sigma)
              // translation.FansiShow.fansiPrint.log(deduction)
              // translation.FansiShow.fansiPrint.log(deduction.typ)
              val cons = Consequence(
                newGoal,
                seek.sigma,
                Option(ExstFunc(deduction)),
                seek.context
              )
              Some(
                cons ::
                  SeekGoal(
                    newGoal,
                    seek.context,
                    seek.forConsequences + seek.sigma
                  ) :: HNil
              )
            } else None
          )
      }
    }
    MicroBot(response)
  }

  lazy val skolemBot: MicroBot[SeekGoal, Option[
    Consequence :: SeekGoal :: HNil
  ], HoTTPostWeb, Unit, ID] = {
    val response
        : Unit => SeekGoal => Future[Option[Consequence :: SeekGoal :: HNil]] =
      (_) =>
        (goal) =>
          Future {
            val sk = skolemize(goal.goal)
            if (sk == goal.goal) None
            else
              Some {
                val y         = sk.Var
                val transform = y :~> fromSkolemized(sk)(y)
                val cons =
                  Consequence(
                    sk,
                    goal.goal,
                    Option(ExstFunc(transform)),
                    goal.context
                  )
                cons :: SeekGoal(
                  sk,
                  goal.context,
                  goal.forConsequences + goal.goal
                ) :: HNil
              }
          }

    MicroBot(response)
  }

  lazy val eqnSimpleUpdate
      : Callback[GeneratedEquationNodes, HoTTPostWeb, Unit, ID] =
    Callback.simple(
      (web: HoTTPostWeb) =>
        (eqs: GeneratedEquationNodes) => web.addEqns(eqs.eqn)
    )

  lazy val expnEqnUpdate: TypedPostResponse[ExpressionEval, HoTTPostWeb, ID] =
    Callback.simple(
      (web: HoTTPostWeb) =>
        (ev: ExpressionEval) => {
          val neqs = ev.equations.flatMap(
            eqq => Equation.split(eqq).map(TermData.isleNormalize(_))
          )
          web.addEqns(neqs)
        }
    )

  lazy val eqnUpdate: Callback[GeneratedEquationNodes, HoTTPostWeb, Unit, ID] =
    Callback.simple(
      (web: HoTTPostWeb) =>
        (eqs: GeneratedEquationNodes) => {
          val neqs = eqs.eqn.map(eq => TermData.isleNormalize(eq))
          eqs.eqn
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
          (neqs -- eqs.eqn)
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
          web.addEqns(neqs)
        }
    )

  def eqnsToExpEv(tgOpt: Option[TermGenParams] = None): MicroHoTTBoTT[
    GeneratedEquationNodes,
    ExpressionEval,
    Set[EquationNode] :: QueryProver :: HNil
  ] = {
    val response
        : Set[EquationNode] :: QueryProver :: HNil => GeneratedEquationNodes => Future[
          ExpressionEval
        ] = {
      case eqns :: qlp :: HNil =>
        eqs =>
          val neqs = eqs.eqn.map(eq => TermData.isleNormalize(eq))
          import qlp.lp
          Future(
            ExpressionEval.fromInitEqs(
              lp.initState,
              Equation.group(eqns union (neqs)),
              tgOpt.getOrElse(lp.tg),
              lp.maxRatio,
              lp.scale,
              lp.smoothing,
              lp.exponent,
              lp.decay
            )
          )
    }
    MicroBot(response)
  }

  lazy val termResultToChomp: MicroBot[
    TermData.TermResult,
    ChompResult,
    HoTTPostWeb,
    Set[HoTT.Term] :: LocalProver :: HNil,
    ID
  ] = {
    val response: (Set[Term] :: LocalProver :: HNil) => TermResult => Future[
      ChompResult
    ] = {
      case (terms :: lp :: HNil) => {
        case (ts, _) =>
          StrategicProvers
            .liberalChomper(lp, ts.orderedUnknowns, accumTerms = terms)
            .map {
              case (s, fl, eqs, _) => ChompResult(s, fl, eqs)
            }
            .runToFuture
      }
    }
    MicroBot(response)
  }

  val goalsAfterChomp
      : MiniBot[ChompResult, WithWeight[SeekGoal], HoTTPostWeb, LocalProver :: TermResult :: HNil, ID] = {
    val response: LocalProver :: TermResult :: HNil => ChompResult => Future[
      Vector[WithWeight[SeekGoal]]
    ] = {
      case lp :: (ts, _) :: HNil =>
        (cr) =>
          Future {
            val typDist = FiniteDistribution(cr.failures.map { typ =>
              Weighted(typ, ts.typs(typ))
            }).safeNormalized
            typDist.pmf.map {
              case Weighted(x, p) =>
                withWeight(SeekGoal(x, lp.initState.context), p)
            }
          }
    }

    MiniBot[ChompResult, WithWeight[SeekGoal], HoTTPostWeb, LocalProver :: TermResult :: HNil, ID](
      response
    )
  }

  lazy val deducedEquations
      : MiniBot[Proved, Either[Contradicted, Proved], HoTTPostWeb, GatherMapPost[
        PropagateProof
      ] :: GatherMapPost[
        Decided
      ] :: Set[HoTT.Term] :: HNil, ID] = {
    val response
        : GatherMapPost[PropagateProof] :: GatherMapPost[Decided] :: Set[
          Term
        ] :: HNil => Proved => Future[
          Vector[Either[Contradicted, Proved]]
        ] = {
      case gprop :: gdec :: terms :: HNil =>
        proved =>
          Future {
            derivedProofs(gprop.contents, gdec.contents union terms.map { t =>
              Proved(t.typ, Some(t), Context.Empty)
            }).map(Decided.asEither(_)).toVector
          }
    }

    MiniBot(response)
  }

  lazy val termsFromProofs: Callback[Proved, HoTTPostWeb, Unit, ID] =
    Callback.simple(
      (web: HoTTPostWeb) =>
        (pf: Proved) => pf.proofOpt.foreach(t => web.addTerms(Set(t)))
    )

  lazy val lpLemmas: MicroBot[LocalProver, Lemmas, HoTTPostWeb, Unit, ID] = {
    val response: Unit => LocalProver => Future[Lemmas] =
      (_) =>
        (lp) =>
          lp.lemmas.runToFuture
            .map(v => Lemmas(v.map(xy => (xy._1, None, xy._2))))
    MicroBot(response)
  }

  def lpOptimalInit(decay: Double): MicroBot[
    LocalProver,
    OptimalInitial,
    HoTTPostWeb,
    Unit,
    ID
  ] = {
    val response: Unit => LocalProver => Future[OptimalInitial] =
      (_) =>
        (lp) =>
          lp.optimalInit.map { lpOpt =>
            OptimalInitial(
              lpOpt,
              lp.hW,
              lp.klW,
              lp.smoothing.getOrElse(0.0),
              decay
            )
          }.runToFuture
    MicroBot(response)
  }

  lazy val narrowOptimalInit: MicroBot[
    NarrowOptimizeGenerators,
    OptimalInitial,
    HoTTPostWeb,
    QueryProver,
    ID
  ] = {
    val response
        : QueryProver => NarrowOptimizeGenerators => Future[OptimalInitial] =
      (qp) =>
        (narrow) => {
          val lp = qp.lp
          for {
            ev0 <- lp.expressionEval
            ev = ev0.modify(
              smoothNew = Some(narrow.smoothing),
              decayNew = narrow.decay
            )
            p <- ev.optimumTask(
              narrow.hW,
              narrow.klW,
              lp.cutoff,
              ev.finalDist,
              lp.maxRatio
            )
            td: FiniteDistribution[Term] = ExpressionEval.dist(
              TermRandomVars.Terms,
              p
            )
            ts = lp.initState.copy(terms = td)
          } yield lp.copy(initState = ts)
        }.map { lpOpt =>
          OptimalInitial(
            lpOpt,
            narrow.hW,
            narrow.klW,
            narrow.smoothing,
            narrow.decay
          )
        }.runToFuture

    MicroBot(response)
  }

  lazy val lptLemmas: MicroBot[
    LocalTangentProver,
    Lemmas,
    HoTTPostWeb,
    Unit,
    ID
  ] = {
    val response: Unit => LocalTangentProver => Future[Lemmas] =
      (_) =>
        (lp) =>
          lp.lemmas.runToFuture
            .map(v => Lemmas(v.map(xy => (xy._1, None, xy._2))))
    MicroBot(response)
  }

  lazy val splitLemmas
      : MiniBot[Lemmas, WithWeight[UseLemma], HoTTPostWeb, Unit, ID] = {
    val response: Unit => Lemmas => Future[Vector[WithWeight[UseLemma]]] =
      (_) =>
        lemmas =>
          Future(
            lemmas.lemmas.map {
              case (tp, pfOpt, w) => withWeight(UseLemma(tp, pfOpt), w)
            }
          )
    MiniBot[Lemmas, WithWeight[UseLemma], HoTTPostWeb, Unit, ID](response)
  }

  def scaleSplitLemmas(
      scale: Double = 1.0
  ): MiniBot[Lemmas, WithWeight[UseLemma], HoTTPostWeb, Unit, ID] = {
    val response: Unit => Lemmas => Future[Vector[WithWeight[UseLemma]]] =
      (_) =>
        lemmas =>
          Future {
            val l  = lemmas.lemmas
            val sc = scale / l.map(_._3).sum
            l.map {
              case (tp, pfOpt, w) => withWeight(UseLemma(tp, pfOpt), w * sc)
            }
          }
    MiniBot[Lemmas, WithWeight[UseLemma], HoTTPostWeb, Unit, ID](response)
  }

  def lemmasTangentEquations(
      scale: Double = 1.0,
      power: Double = 1.0
  ): MicroHoTTBoTT[Lemmas, GeneratedEquationNodes, QueryProver] = {
    val response: QueryProver => Lemmas => Future[GeneratedEquationNodes] =
      qp =>
        lemmas => {
          val l = lemmas.lemmas.map {
            case (tp, pfOpt, p) => (tp, pfOpt, math.pow(p, power))
          }
          val sc = scale / l.map(_._3).sum
          val useLemmas = l.map {
            case (tp, pfOpt, w) => (UseLemma(tp, pfOpt), w * sc)
          }
          val tangProvers = useLemmas.map {
            case (lem, w) => qp.lp.sharpen(w).tangentProver(lem.proof)
          }
          val eqGps =
            tangProvers.map(
              tlp =>
                tlp.flatMap(
                  lp =>
                    lp.enhancedEquationNodes.onErrorRecover {
                      case te: TimeoutException =>
                        logger.error(te)
                        Set.empty[EquationNode]
                    }
                )
            )
          val allEqs = Task
            .gather(eqGps)
            .map(_.fold(Set.empty[EquationNode])(_ union _))
            .map(GeneratedEquationNodes(_))
          allEqs.runToFuture
        }
    MicroBot(response)
  }

  def avoidLemmas(decay: Double = 0.5, cutoff: Double = 0.04)(
      v: Vector[UsedLemmas]
  ): Set[HoTT.Typ[HoTT.Term]] = {
    val allLemmas = v.flatMap(_.support).toSet
    val l         = v.length
    def hasLargeWeight(typ: Typ[Term]) =
      (0 until (l)).exists(j => v(j).weight(typ) * math.pow(decay, j) > cutoff)
    allLemmas.filter(hasLargeWeight(_))
  }

  def baseMixinLemmas(power: Double = 1.0): SimpleBot[Lemmas, BaseMixinLemmas] =
    MicroBot.simple(
      lem => {
        val powerSum =
          lem.lemmas.map { case (_, _, p) => math.pow(p, power) }.sum
        val flattened = lem.lemmas.map {
          case (tp, pfOpt, p) => (tp, pfOpt, math.pow(p, power) / powerSum)
        }
        BaseMixinLemmas(flattened)
      }
    )

  def tangentLemmas(
      scale: Double = 1.0,
      decay: Double = 0.5,
      cutoff: Double = 0.04,
      power: Double = 1
  ): MicroHoTTBoTT[Lemmas, UsedLemmas :: TangentLemmas :: HNil, PreviousPosts[
    UsedLemmas
  ]] = {
    val response: PreviousPosts[UsedLemmas] => Lemmas => Future[
      UsedLemmas :: TangentLemmas :: HNil
    ] =
      (ul) =>
        (lem) =>
          Future {
            val exclude = avoidLemmas(decay, cutoff)(ul.contents)
            logger.info(s"excluded lemmas:\n${exclude.mkString("\n")}")
            val l =
              for {
                (tp, pfOpt, p) <- lem.lemmas

              } yield (tp, pfOpt, math.pow(p, power))
            val ltot = l.map(_._3).sum
            val sc   = scale / ltot
            val tangLemmas = l.map {
              case (tp, pfOpt, w) => (tp, pfOpt, w * sc)
            }
            val used = tangLemmas.map { case (tp, _, p) => tp -> p }
            UsedLemmas(used) :: TangentLemmas(tangLemmas) :: HNil
          }
    MicroBot(response)
  }

  def baseState(
      initialState: TermState,
      equations: Set[Equation],
      tg: TermGenParams,
      maxRatio: Double = 1.01,
      scale: Double = 1.0,
      smooth: Option[Double] = None,
      exponent: Double = 0.5,
      decay: Double = 1,
      maxTime: Option[Long] = None
  ) = {
    val expEv = ExpressionEval.fromInitEqs(
      initialState,
      equations union(Equation.group(DE.termStateInit(initialState))),
      tg,
      maxRatio,
      scale,
      smooth,
      exponent,
      decay,
      maxTime
    )

    expEv.finalTermState()
  }

  def baseStateFromLp(
      lemmaMix: Double,
      cutoffScale: Double = 1.0,
      tgOpt: Option[TermGenParams] = None,
      depthOpt: Option[Int] = None
  ): MicroHoTTBoTT[
    BaseMixinLemmas,
    TangentBaseState,
    QueryProver :: Set[Equation] :: HNil
  ] = {
    val response
        : QueryProver :: Set[Equation] :: HNil => BaseMixinLemmas => Future[
          TangentBaseState
        ] = {
      case qp :: eqns :: HNil =>
        lems =>
          Future {
            import qp.lp
            val lemPfDist = FiniteDistribution(
              lems.lemmas.map {
                case (typ, pfOpt, p) =>
                  Weighted(pfOpt.getOrElse("pf" :: typ), p)
              }
            )
            val baseDist = lp.initState.terms
            val tdist    = (baseDist * (1.0 - lemmaMix) ++ (lemPfDist * lemmaMix))
            val bs       = lp.initState.copy(terms = tdist)
            val fs = baseState(
              bs,
              eqns,
              lp.tg,
              lp.maxRatio,
              lp.scale,
              lp.smoothing,
              lp.exponent,
              lp.decay,
              lp.maxTime
            )
            TangentBaseState(
              fs,
              cutoffScale,
              tgOpt,
              depthOpt
            )
          }
    }
    MicroBot(response)
  }

  lazy val baseStateFromSpecialInit: DualMiniBot[
    BaseMixinLemmas,
    TangentBaseState,
    HoTTPostWeb,
    PreviousPosts[SpecialInitState] :: QueryProver :: Set[Equation] :: HNil,
    ID
  ] = {
    val response: PreviousPosts[SpecialInitState] :: QueryProver :: Set[
      Equation
    ] :: HNil => BaseMixinLemmas => Vector[
      Future[
        TangentBaseState
      ]
    ] = {
      case psps :: qp :: eqns :: HNil =>
        lems => {
          logger.info(s"previous special init states are ${psps.contents.size}")
          logger.debug(psps.contents.mkString("\n"))
          psps.contents.map(
            ps =>
              Future {
                import qp.lp
                val lemPfDist = FiniteDistribution(
                  lems.lemmas.map {
                    case (typ, pfOpt, p) =>
                      Weighted(pfOpt.getOrElse("pf" :: typ), p)
                  }
                )
                val baseDist = ps.ts.terms
                val tdist    = (baseDist * (1.0 - ps.lemmaMix) ++ (lemPfDist * ps.lemmaMix))
                val bs       = ps.ts.copy(terms = tdist)
                val fs = baseState(
                  bs,
                  eqns,
                  ps.tgOpt.getOrElse(lp.tg),
                  lp.maxRatio,
                  lp.scale,
                  lp.smoothing,
                  lp.exponent,
                  lp.decay,
                  lp.maxTime
                )
                TangentBaseState(
                  fs.purge(ps.baseCutoff),
                  ps.cutoffScale,
                  ps.tgOpt,
                  ps.depthOpt
                )
              }
          )
        }
    }
    DualMiniBot(response)
  }

  // temporary, for testing
  def appEquations(
      cutoff: Double
  ): MicroHoTTBoTT[TangentBaseState, GeneratedEquationNodes, TangentLemmas] = {
    val response
        : TangentLemmas => TangentBaseState => Future[GeneratedEquationNodes] =
      (tl) =>
        (tb) =>
          Future {
            val lemPfDist = FiniteDistribution(tl.lemmas.map {
              case (t, pfOpt, p) => Weighted(pfOpt.getOrElse("lemma" :: t), p)
            })
            val funcs = tb.ts.terms.condMap(ExstFunc.opt).safeNormalized
            logger.info(s"function domains: ${funcs.support.map(_.dom)}")
            logger.info(s"lemma-proof types: ${lemPfDist.support.map(_.typ)}")
            val eqs = SimpleEquations.allAppEquations(funcs, lemPfDist, cutoff)
            GeneratedEquationNodes(eqs.toSet)
          }

    MicroBot(response)
  }

  lazy val cappedSpecialBaseState
      : TypedPostResponse[BaseMixinLemmas, HoTTPostWeb, ID] =
    baseStateFromSpecialInit
      .reduce((v: Vector[TangentBaseState]) => TangentBaseCompleted)

  def unAppEquations(
      cutoff: Double
  ): MicroHoTTBoTT[TangentBaseCompleted.type, GeneratedEquationNodes, Collated[
    TangentBaseState
  ] :: TangentLemmas :: HNil] = {
    val response
        : Collated[TangentBaseState] :: TangentLemmas :: HNil => TangentBaseCompleted.type => Future[
          GeneratedEquationNodes
        ] = {
      case tbss :: tl :: HNil =>
        (_) => {
          val eqsFut =
            Future
              .sequence(tbss.contents.toSet.map { (tb: TangentBaseState) =>
                {
                  val lemPfDist = FiniteDistribution(tl.lemmas.map {
                    case (t, pfOpt, p) =>
                      Weighted(pfOpt.getOrElse("lemma" :: t), p)
                  })
                  val funcs = tb.ts.terms.condMap(ExstFunc.opt).safeNormalized
                  SimpleEquations.unAppEquations(funcs, lemPfDist, cutoff)

                }
              })
              .map(_.flatten)
          eqsFut.map(eqs => GeneratedEquationNodes(eqs))
        }
    }
    MicroBot(response)
  }

  def timedUnAppEquations(
      cutoff: Double,
      minTime: FiniteDuration,
      cutoffScale: Double = 2
  ): MicroHoTTBoTT[TangentBaseCompleted.type, GeneratedEquationNodes, Collated[
    TangentBaseState
  ] :: TangentLemmas :: HNil] = {
    val response
        : Collated[TangentBaseState] :: TangentLemmas :: HNil => TangentBaseCompleted.type => Future[
          GeneratedEquationNodes
        ] = {
      case tbss :: tl :: HNil =>
        (_) => {
          val eqsFut =
            Task
              .gatherUnordered(tbss.contents.toSet.map {
                (tb: TangentBaseState) =>
                  {
                    val lemPfDist = FiniteDistribution(tl.lemmas.map {
                      case (t, pfOpt, p) =>
                        Weighted(pfOpt.getOrElse("lemma" :: t), p)
                    })
                    val funcs = tb.ts.terms.condMap(ExstFunc.opt).safeNormalized
                    SimpleEquations.timedUnAppEquations(
                      funcs,
                      lemPfDist,
                      cutoff,
                      minTime,
                      cutoffScale
                    )

                  }
              })
              .map(_.flatten)
          eqsFut.map(eqs => GeneratedEquationNodes(eqs.toSet))
        }.runToFuture
    }
    MicroBot(response)
  }

  def cappedBaseState(
      lemmaMix: Double,
      cutoffScale: Double = 1.0,
      tgOpt: Option[TermGenParams] = None,
      depthOpt: Option[Int] = None
  ): TypedPostResponse[BaseMixinLemmas, HoTTPostWeb, ID] =
    (baseStateFromLp(lemmaMix, cutoffScale, tgOpt, depthOpt) && baseStateFromSpecialInit)
      .reduce((v: Vector[TangentBaseState]) => TangentBaseCompleted)

  def reportBaseTangents(
      results: Vector[Typ[Term]],
      steps: Vector[Typ[Term]],
      inferTriples: Vector[(Typ[Term], Typ[Term], Typ[Term])]
  ): Callback[TangentBaseCompleted.type, HoTTPostWeb, Collated[
    TangentBaseState
  ] :: TangentLemmas :: HNil, ID] = {
    val response: HoTTPostWeb => Collated[
      TangentBaseState
    ] :: TangentLemmas :: HNil => TangentBaseCompleted.type => Future[Unit] =
      (_) => {
        case ctbs :: tl :: HNil =>
          (_) =>
            Future {
              logger.info(
                s"generating equations with ${ctbs.contents.size} base states and ${tl.lemmas.size} lemmas (before pruning)"
              )
              val tls = results
                .flatMap(typ => tl.lemmas.find(_._1 == typ).map(typ -> _._3))
              val view1 =
                s"Tangent lemmas (used with bases below): ${tls.size}\n${tls
                  .mkString("\n")}"
              logger.info(view1)
              ctbs.contents.foreach { fs =>
                val termsSet = fs.ts.terms.support
                val basePfs = steps
                  .map(typ => typ -> termsSet.filter(_.typ == typ))
                  .filter(_._2.nonEmpty)
                val view2 =
                  s"Terms in base (used with tangents above): ${basePfs.size}\n${basePfs
                    .map {
                      case (tp, ps) =>
                        val best = ps.maxBy(t => fs.ts.terms(t))
                        s"Type: $tp; best term: ${best} with weight ${fs.ts.terms(best)}"
                    }
                    .mkString("\n")}"
                logger.info(view2)
                val view3 = inferTriples.map {
                  case (f, x, fx) =>
                    val p = fs.ts.terms.map(_.typ)(f)
                    val q = tls.find(_._1 == x).map(_._2).getOrElse(0.0)
                    s"($p, $q, ${p * q}) for ($f, $x, $fx)"
                }
                logger.info(view3.mkString("Inference by unified applications, triples and weights (for this base state)\n","\n", "\n"))
              }
            }
      }
    Callback(response)
  }

  def tangentEquations(
      results: Vector[Typ[Term]],
      steps: Vector[Typ[Term]]
  ): MicroHoTTBoTT[
    TangentBaseCompleted.type,
    GeneratedEquationNodes,
    Collated[TangentBaseState] :: TangentLemmas :: QueryProver :: Set[
      EquationNode
    ] :: HNil
  ] = {
    val response
        : Collated[TangentBaseState] :: TangentLemmas :: QueryProver :: Set[
          EquationNode
        ] :: HNil => TangentBaseCompleted.type => Future[
          GeneratedEquationNodes
        ] = {
      case ctbs :: tl :: qp :: eqns :: HNil =>
        (_) => {
          import qp.lp

          logger.info(
            s"generating equations with ${ctbs.contents.size} base states and ${tl.lemmas.size} lemmas (before pruning)"
          )
          val tls = results
            .flatMap(typ => tl.lemmas.find(_._1 == typ).map(typ -> _._3))
          val view1 =
            s"Tangent lemmas (used with bases below): ${tls.size}\n${tls
              .mkString("\n")}"
          logger.info(view1)
          ctbs.contents.foreach { fs =>
            val termsSet = fs.ts.terms.support
            val pfs = steps
              .map(typ => typ -> termsSet.filter(_.typ == typ))
              .filter(_._2.nonEmpty)
            val view2 =
              s"Terms in base (used with tangents above): ${pfs.size}\n${pfs
                .map {
                  case (tp, ps) =>
                    val best = ps.maxBy(t => fs.ts.terms(t))
                    s"Type: $tp; best term: ${best} with weight ${fs.ts.terms(best)}"
                }
                .mkString("\n")}"
            logger.info(view2)
          }

          val provers =
            ctbs.contents
              .flatMap { tbs =>
                tl.lemmas.map {
                  case (tp, pfOpt, w) =>
                    val pf        = pfOpt.getOrElse("lemma" :: tp)
                    val tangState = tbs.ts.tangent(pf)
                    val lpt =
                      LocalTangentProver(
                        tbs.ts,
                        eqns,
                        tangState,
                        tbs.tgOpt.getOrElse(lp.tg),
                        (lp.cutoff / tbs.cutoffScale) * w,
                        tbs.depthOpt.orElse(lp.genMaxDepth),
                        lp.limit,
                        lp.maxRatio,
                        lp.scale,
                        lp.steps,
                        lp.maxDepth,
                        lp.hW,
                        lp.klW,
                        lp.smoothing,
                        lp.relativeEval,
                        lp.stateFromEquation,
                        lp.exponent,
                        lp.decay,
                        lp.maxTime
                      )
                    lpt
                }

              }
              .filter(_.cutoff < 1)

          var remaining = provers.size
          logger.info(s"${provers.size} tangent provers after filtering")
          val equationsTasks =
            provers.map { lpt =>
              lpt.enhancedEquationNodes
                .map { eqns =>
                  logger.info(s"obtained ${eqns.size} equation nodes")
                  remaining = remaining - 1
                  logger.info(s"provers still running: $remaining")
                  eqns
                }
                .onErrorRecover {
                  case te: TimeoutException =>
                    logger.error(te.getMessage())
                    logger.debug(te)
                    remaining = remaining - 1
                    logger.info(s"provers still running: $remaining")
                    Set.empty[EquationNode]
                  case te =>
                    logger.error(s"Serious error")
                    logger.error(te)
                    remaining = remaining - 1
                    logger.info(s"provers still running: $remaining")
                    Set.empty[EquationNode]
                }
            }
          val allTask = Task
            .gatherUnordered(equationsTasks)
            .map(_.fold(Set.empty)(_ union _))
            .map(GeneratedEquationNodes(_))
          allTask.runToFuture
        }
    }
    MicroBot(response)
  }

  lazy val forkedTangentEquations: DualMiniBot[
    TangentBaseState,
    GeneratedEquationNodes,
    HoTTPostWeb,
    TangentLemmas :: LocalProver :: Set[EquationNode] :: HNil,
    ID
  ] = {
    val responses: TangentLemmas :: LocalProver :: Set[
      EquationNode
    ] :: HNil => TangentBaseState => Vector[
      Future[GeneratedEquationNodes]
    ] = {
      case tl :: lp :: eqns :: HNil =>
        tbs =>
          (
            tl.lemmas.map {
              case (tp, pfOpt, w) =>
                val pf        = pfOpt.getOrElse("lemma" :: tp)
                val tangState = tbs.ts.tangent(pf)
                val lpt =
                  LocalTangentProver(
                    tbs.ts,
                    eqns,
                    tangState,
                    tbs.tgOpt.getOrElse(lp.tg),
                    (lp.cutoff / tbs.cutoffScale) * w,
                    tbs.depthOpt.orElse(lp.genMaxDepth),
                    lp.limit,
                    lp.maxRatio,
                    lp.scale,
                    lp.steps,
                    lp.maxDepth,
                    lp.hW,
                    lp.klW,
                    lp.smoothing,
                    lp.relativeEval,
                    lp.stateFromEquation,
                    lp.exponent,
                    lp.decay,
                    lp.maxTime
                  )
                lpt.enhancedEquationNodes
                  .onErrorRecover {
                    case te: TimeoutException =>
                      logger.error(te.getMessage())
                      logger.debug(te)
                      Set.empty[EquationNode]
                    case te =>
                      logger.error(s"Serious error")
                      logger.error(te)
                      Set.empty[EquationNode]
                  }
                  .map(GeneratedEquationNodes(_))
                  .runToFuture
            }
          )

    }

    DualMiniBot(responses)
  }

  lazy val cappedForkedTangentEquations =
    forkedTangentEquations
      .triggerWith[TangentBaseCompleted.type]
      .reduce(
        (v: Vector[GeneratedEquationNodes]) => {
          val all = v.map(_.eqn).fold(Set.empty[EquationNode])(_ union _)
          GeneratedEquationNodes(all) :: EquationsCompleted :: HNil
        }
      )

  def lemmasBigTangentEquations(
      scale: Double = 1.0,
      power: Double = 1.0,
      lemmaMix: Double = 0,
      decay: Double = 0.5,
      cutoff: Double = 0.04
  ): MicroHoTTBoTT[
    Lemmas,
    UsedLemmas :: GeneratedEquationNodes :: HNil,
    QueryProver :: Set[
      EquationNode
    ] :: FinalState :: PreviousPosts[UsedLemmas] :: HNil
  ] = {
    val response
        : QueryProver :: Set[EquationNode] :: FinalState :: PreviousPosts[
          UsedLemmas
        ] :: HNil => Lemmas => Future[
          UsedLemmas :: GeneratedEquationNodes :: HNil
        ] = {
      case qp :: baseEqs :: fs :: ul :: HNil =>
        lemmas => {
          val exclude = avoidLemmas(decay, cutoff)(ul.contents)
          val l0 =
            for {
              (tp, pfOpt, p) <- lemmas.lemmas
            } yield (tp, pfOpt, math.pow(p, power))
          val l     = l0.filterNot { case (tp, _, _) => exclude.contains(tp) }
          val ltot  = l.map(_._3).sum
          val l0tot = l0.map(_._3).sum
          val sc    = scale / ltot
          val useLemmas = l.map {
            case (tp, pfOpt, w) => (UseLemma(tp, pfOpt), w * sc)
          }
          val initRestrictEquations = DE.allInitEquations(fs.ts.terms.support)
          val evolvedState =
            if (lemmaMix == 0) fs.ts
            else {
              import qp.lp
              val baseDist = lp.initState.terms
              val lemPfDist = FiniteDistribution(
                l0.map {
                  case (typ, pfOpt, p) =>
                    Weighted(pfOpt.getOrElse("pf" :: typ), p / l0tot)
                }
              )
              val tdist     = (baseDist * (1.0 - lemmaMix) ++ (lemPfDist * lemmaMix))
              val baseState = lp.initState
              val expEv = ExpressionEval.fromInitEqs(
                baseState.copy(terms = tdist),
                Equation.group(baseEqs),
                lp.tg,
                lp.maxRatio,
                lp.scale,
                lp.smoothing,
                lp.exponent,
                lp.decay
              )
              expEv.finalTermState()
            }
          val usedLemmas = UsedLemmas(useLemmas.map {
            case (lem, w) => (lem.lemma, w)
          })
          val tangProvers = useLemmas.map {
            case (lem, w) =>
              qp.lp
                .sharpen(w)
                .tangentProver(lem.proof)
                .map(
                  _.copy(
                    initEquations = baseEqs union initRestrictEquations,
                    initState = evolvedState
                  )
                )
          }
          val eqGps =
            tangProvers.map(
              tlp =>
                tlp.flatMap(
                  lp =>
                    lp.enhancedEquationNodes.onErrorRecover {
                      case te: TimeoutException =>
                        logger.error(te)
                        Set.empty[EquationNode]
                      case te =>
                        logger.error(s"Serious error")
                        logger.error(te)
                        Set.empty[EquationNode]
                    }
                )
            )
          val allEqs = Task
            .gather(eqGps)
            .map(_.fold(Set.empty[EquationNode])(_ union _))
            .map(GeneratedEquationNodes(_))
          allEqs.runToFuture.map(aEq => usedLemmas :: aEq :: HNil)
        }
    }
    MicroBot(response)
  }

  def lemmaDistributions(
      sizes: Map[Int, Double]
  ): MiniBot[Lemmas, UseLemmaDistribution, HoTTPostWeb, Unit, ID] = {
    val response: Unit => Lemmas => Future[Vector[UseLemmaDistribution]] =
      (_) =>
        lemmas =>
          Future {
            val s = lemmas.lemmas.map(_._1).toSet
            (sizes.flatMap {
              case (j, w) =>
                val ss = s.subsets(j).toSet
                val fdsUnscaled = ss.map { supp =>
                  FiniteDistribution {
                    val weights = supp.flatMap(
                      typ => lemmas.lemmas.find(_._1 == typ).map(_._3)
                    )
                    val prodWeight = weights.fold(1.0)(_ * _)
                    supp.map { typ =>
                      Weighted(typ, prodWeight)
                    }
                  }
                }
                val totalWeight = fdsUnscaled.map(_.total).sum
                val fds         = fdsUnscaled.map(_ * (w / totalWeight))
                fds.map(fd => UseLemmaDistribution(fd, None))
            }).toVector
          }

    MiniBot[Lemmas, UseLemmaDistribution, HoTTPostWeb, Unit, ID](response)
  }

  lazy val backwardFunction
      : MiniBot[SeekGoal, WithWeight[FunctionForGoal], HoTTPostWeb, QueryProver, ID] = {
    val response: QueryProver => SeekGoal => Future[
      Vector[WithWeight[FunctionForGoal]]
    ] =
      qp =>
        goal => {
          import TermGeneratorNodes._, TermRandomVars._
          qp.lp
            .nodeDist(codomainNode(goal.goal))
            .map { fd: FiniteDistribution[Term] =>
              fd.pmf.map {
                case Weighted(x, p) =>
                  withWeight(
                    FunctionForGoal(
                      x,
                      goal.goal,
                      goal.context,
                      goal.forConsequences + goal.goal
                    ),
                    p
                  )
              }
            }
            .runToFuture
        }

    MiniBot[SeekGoal, WithWeight[FunctionForGoal], HoTTPostWeb, QueryProver, ID](
      response
    )
  }

  lazy val instanceFromLp
      : MiniBot[SeekInstances, WithWeight[Instance], HoTTPostWeb, QueryProver, ID] = {
    val response: QueryProver => SeekInstances => Future[
      Vector[WithWeight[Instance]]
    ] =
      qp =>
        seekInst => {
          import TermGeneratorNodes._, TermRandomVars._
          qp.lp
            .varDist(termsWithTyp(seekInst.typ))
            .map { fd: FiniteDistribution[Term] =>
              fd.pmf.map {
                case Weighted(x, p) =>
                  withWeight(
                    Instance(
                      x,
                      seekInst.typ,
                      seekInst.context
                    ),
                    p
                  )
              }
            }
            .runToFuture
        }

    MiniBot[SeekInstances, WithWeight[Instance], HoTTPostWeb, QueryProver, ID](
      response
    )
  }

  lazy val inductionBackward
      : MiniBot[SeekGoal, WithWeight[FunctionForGoal], HoTTPostWeb, QueryProver, ID] = {
    val response: QueryProver => SeekGoal => Future[
      Vector[WithWeight[FunctionForGoal]]
    ] =
      qp =>
        goal => {
          import TermGeneratorNodes._
          qp.lp
            .nodeDist(qp.lp.tg.Gen.targetInducBackNode(goal.goal))
            .map { fd: FiniteDistribution[Term] =>
              fd.pmf.map {
                case Weighted(x, p) =>
                  withWeight(
                    FunctionForGoal(
                      x,
                      goal.goal,
                      goal.context,
                      goal.forConsequences
                    ),
                    p
                  )
              }
            }
            .runToFuture
        }

    MiniBot[SeekGoal, WithWeight[FunctionForGoal], HoTTPostWeb, QueryProver, ID](
      response
    )
  }

  lazy val addInductive: MicroBot[
    ConsiderInductiveTypes,
    LocalProver,
    HoTTPostWeb,
    QueryProver,
    ID
  ] = {
    val response: QueryProver => ConsiderInductiveTypes => Future[LocalProver] =
      (qp) =>
        (consInds) =>
          Future {
            qp.lp.copy(
              initState = qp.lp.initState.copy(
                inds = (qp.lp.initState.inds ++ consInds.inducs).safeNormalized
              )
            )
          }

    MicroBot(response)
  }

  lazy val funcToFromAll: SimpleBot[FunctionForGoal, Option[FromAll]] = {
    MicroBot.simple { (fng: FunctionForGoal) =>
      FromAll.get(fng.fn, fng.goal, fng.forConsequences, fng.context)
    }
  }

  lazy val resolveFromAll: MiniBot[FromAll, SeekGoal, HoTTPostWeb, Unit, ID] = {
    val response: Unit => FromAll => Future[Vector[SeekGoal]] =
      (_) =>
        (fromAll) =>
          Future {
            fromAll.typs.map(
              typ =>
                SeekGoal(
                  typ,
                  fromAll.context,
                  fromAll.forConsequences + fromAll.conclusion
                )
            )
          }

    MiniBot[FromAll, SeekGoal, HoTTPostWeb, Unit, ID](response)
  }

  lazy val resolveFromAny: MiniBot[FromAny, SeekGoal, HoTTPostWeb, Unit, ID] = {
    val response: Unit => FromAny => Future[Vector[SeekGoal]] =
      (_) =>
        (fromAny) =>
          Future {
            fromAny.typs.map(
              typ =>
                SeekGoal(
                  typ,
                  fromAny.context,
                  fromAny.forConsequences + fromAny.conclusion
                )
            )
          }

    MiniBot[FromAny, SeekGoal, HoTTPostWeb, Unit, ID](response)
  }

  lazy val productBackward: SimpleBot[SeekGoal, Option[FromAll]] = {
    MicroBot.simple(
      (sk: SeekGoal) =>
        sk.goal match {
          case pd: ProdTyp[u, v] =>
            Some(
              FromAll(
                Vector(pd.first, pd.second),
                sk.goal, {
                  Some(pd.paircons)
                },
                sk.context,
                sk.forConsequences
              )
            )
          case _ => None
        }
    )
  }

  lazy val coproductBackward: SimpleBot[SeekGoal, Option[FromAny]] = {
    MicroBot.simple(
      (sk: SeekGoal) =>
        sk.goal match {
          case pt: PlusTyp[u, v] =>
            Some(
              FromAny(
                Vector(pt.first, pt.second),
                sk.goal,
                true,
                Vector(
                  Some(ExstFunc(pt.incl1)),
                  Some(ExstFunc(pt.incl2))
                ),
                sk.context,
                sk.forConsequences
              )
            )
          case _ => None
        }
    )
  }

  lazy val sigmaBackward: SimpleBot[SeekGoal, Option[SeekInstances]] = {
    MicroBot.simple[SeekGoal, Option[SeekInstances], HoTTPostWeb, ID](
      (sk: SeekGoal) =>
        sk.goal match {
          case pt: SigmaTyp[u, v] =>
            Some(
              SeekInstances(
                pt.fibers.dom,
                pt.fibers,
                sk.context,
                sk.forConsequences
              )
            )
          case _ => None
        }
    )
  }

  def lemmaTangents(tangentScale: Double = 1.0): MicroBot[
    UseLemma,
    LocalTangentProver,
    HoTTPostWeb,
    QueryProver,
    ID
  ] = {
    val response: QueryProver => UseLemma => Future[LocalTangentProver] =
      qp =>
        lem => qp.lp.sharpen(tangentScale).tangentProver(lem.proof).runToFuture

    MicroBot(response)
  }

  def lemmaMixin(weight: Double = 0.3): MicroBot[
    UseLemma,
    LocalProver,
    HoTTPostWeb,
    QueryProver,
    ID
  ] = {
    val response: QueryProver => UseLemma => Future[LocalProver] =
      qp =>
        lem =>
          Future {
            val ts = qp.lp.initState
            qp.lp.copy(
              initState =
                ts.copy(terms = (ts.terms + (lem.proof, weight)).safeNormalized)
            )
          }
    //  qp.lp.sharpen(tangentScale).tangentProver(lem.proof).runToFuture

    MicroBot(response)
  }

  val lemmaGoal: MicroBot[
    UseLemma,
    SeekGoal,
    HoTTPostWeb,
    SeekGoal,
    ID
  ] = {
    val response: SeekGoal => UseLemma => Future[SeekGoal] =
      sg =>
        lem =>
          Future(
            SeekGoal(lem.lemma ->: sg.goal, sg.context, sg.forConsequences)
          )
    MicroBot(response)
  }

  val goalInContext: SimpleBot[SeekGoal, Option[SeekGoal]] = MicroBot.simple(
    (sk: SeekGoal) =>
      sk.goal match {
        case PiDefn(variable: Term, value: Typ[u]) =>
          Some(SeekGoal(value, sk.context.addVariable(variable)))
        case FuncTyp(dom: Typ[v], codom: Typ[u]) =>
          val x = dom.Var
          Some(SeekGoal(codom, sk.context.addVariable(x), sk.forConsequences))
        case _ => None
      }
  )

  val fullGoalInContext: SimpleBot[SeekGoal, Option[SeekGoal]] =
    MicroBot.simple(_.inContext)

  def goalToProver(
      varWeight: Double,
      goalWeight: Double
  ): MicroBot[SeekGoal, Option[LocalProver], HoTTPostWeb, QueryProver :: Set[
    HoTT.Term
  ] :: HNil, ID] = {
    val subContext: SeekGoal => QueryProver :: Set[Term] :: HNil => Boolean =
      (goal) => {
        case (qp: QueryProver) :: terms :: HNil => {
          val lpVars   = qp.lp.initState.context.variables
          val goalVars = goal.context.variables
          // pprint.log(lpVars)
          // pprint.log(goalVars)
          lpVars == goalVars.take(lpVars.size)
        }
      }

    val response: QueryProver :: Set[Term] :: HNil => SeekGoal => Future[
      Option[LocalProver]
    ] = {
      case (qp: QueryProver) :: terms :: HNil =>
        // println(qp)
        // pprint.log(qp)
        goal =>
          Future {
            if (goal.relevantGiven(terms)) {
              val lpVars   = qp.lp.initState.context.variables
              val goalVars = goal.context.variables
              val newVars  = goalVars.drop(lpVars.size)
              // pprint.log(newVars)
              val withVars = newVars.foldLeft(qp.lp) {
                case (lp: LocalProver, x: Term) =>
                  // pprint.log(x)
                  lp.addVar(x, varWeight)
              }
              Some(withVars.addGoals(goal.goal -> goalWeight))
            } else None
          }
    }

    MicroBot(response, subContext)
  }

  // def fansiLog(post: PostData[_, HoTTPostWeb, ID]): Future[Unit] =
  //   Future {
  //     translation.FansiShow.fansiPrint.log(post.pw.tag)
  //     translation.FansiShow.fansiPrint.log(post.content, height = 20)
  //     pprint.log(post.id)
  //   }

  // def tagLog(post: PostData[_, HoTTPostWeb, ID]): Future[Unit] =
  //   Future {
  //     translation.FansiShow.fansiPrint.log(post.pw.tag)
  //     pprint.log(post.id)
  //   }

  def scribeLog(post: PostData[_, HoTTPostWeb, ID]): Future[Unit] = Future {
    logger.info(s"Post; tag: ${post.pw.tag}, id: ${post.id}")
    logger.debug(
      s"Full post; tag: ${post.pw.tag}, id: ${post.id}, content:\n${post.content}"
    )
  }
}

import HoTTBot._
// May want to avoid inheritance
class HoTTWebSession(
    initialWeb: HoTTPostWeb = new HoTTPostWeb(),
    bots: Vector[HoTTBot] = HoTTWebSession.initBots
) extends SimpleSession[HoTTPostWeb, (Int, Int)](
      initialWeb,
      bots,
      Vector(scribeLog(_))
    ) {

  // just an illustration, should just use rhs
  def postLocalProverFuture(
      lp: LocalProver,
      pred: Set[ID] = Set()
  ): Future[PostData[_, HoTTPostWeb, ID]] =
    post(lp, pred)

  def postLP(
      lp: LocalProver,
      pred: Set[ID] = Set()
  ): Future[PostData[_, HoTTPostWeb, ID]] =
    postLocalProverFuture(lp, pred)
}

object HoTTWebSession {
  val initBots = Vector(lpToExpEv, expEvToEqns, eqnUpdate)

  def launch(
      state: WebState[HoTTPostWeb, (Int, Int)],
      bots: Vector[HoTTBot] = initBots
  ) = {
    val session = new HoTTWebSession(state.web, bots)
    state.apexPosts.foreach {
      case pd: PostData[x, HoTTPostWeb, (Int, Int)] =>
        session.respond(pd.content, pd.id)(pd.pw)
    }
    session
  }
}
