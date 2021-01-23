package provingground.learning

import provingground._, HoTT._, induction._
import TypedPostResponse._
import monix.eval._
import LocalQueryable._
import monix.execution.Scheduler.Implicits.{global => monixglobal}
import scala.concurrent._
import TermData._
import shapeless._
import scala.collection.View
import scala.reflect.runtime.universe._
import HoTTMessages._
import Utils.logger
import scala.concurrent._, duration._
import provingground.learning.Expression.FinalVal
import scala.math.Ordering.Double.TotalOrdering
import fastparse.internal.Util
import provingground.induction.ExstInducDefn
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable._

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

case class QueryEquations(equations: Set[Equation]) {
  lazy val nodes: Set[EquationNode] =
    equations.flatMap(Equation.split(_)).map(TermData.isleNormalize(_))

  lazy val isleNormalized: Set[Equation] = Equation.group(nodes)
}

object QueryEquations {
  implicit val lqe: Queryable[QueryEquations, HoTTPostWeb] =
    new Queryable[QueryEquations, HoTTPostWeb] {
      def get(
          web: HoTTPostWeb,
          predicate: QueryEquations => Boolean
      ): Future[QueryEquations] = {
        val lookup = web.equationNodes
        val gatheredGen =
          LocalQueryable
            .query[GatherPost[GeneratedEquationNodes], HoTTPostWeb](
              web,
              (_) => true
            )
            .map(
              gp =>
                gp.contents
                  .flatMap(_.eqn)
                  .map(TermData.isleNormalize(_))
            )
        val gatheredExpEv =
          LocalQueryable
            .query[GatherPost[ExpressionEval], HoTTPostWeb](
              web,
              (_) => true
            )
            .map(
              gp =>
                gp.contents
                  .flatMap(_.equations)
                  .flatMap(Equation.split(_))
                  .map(TermData.isleNormalize(_))
            )
        for {
          genEqs <- gatheredGen
          expEqs <- gatheredExpEv
        } yield
          QueryEquations(
            Equation.group(lookup union genEqs.toSet union expEqs.toSet)
          )
      }
    }
}

object HoTTBot {

  type ID = HoTTPostWeb.ID

  type HoTTBot = PostResponse[HoTTPostWeb, ID]

  type SimpleBot[P, Q] = MicroBot[P, Q, HoTTPostWeb, Unit, ID]

  type MicroHoTTBoTT[P, Q, V] = MicroBot[P, Q, HoTTPostWeb, V, ID]

  type HoTTCallBack[P, Q] = Callback[P, HoTTPostWeb, Q, ID]

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
      (fs) => Lemmas(fs.ts.lemmas.par)
    )

  val finalStateToNewLemmas
      : MicroHoTTBoTT[FinalState, Lemmas, QueryBaseState] = {
    val response: QueryBaseState => FinalState => Future[Lemmas] =
      (qinit) =>
        (fs) =>
          Future {
            val proved = qinit.init.terms.map(_.typ).support
            Lemmas(fs.ts.lemmas.par.filterNot {
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
      coeffvalS = tg.coeffVal(_),
      varWeightS = tg.varWeight,
      maxRatioS = maxRatio,
      maxTimeS = maxTime
    )
    expEv.finalTerms.support
  }

  def newLemmas(
      previous: Vector[FinalState],
      fs: FinalState
  ): Vector[(Typ[Term], Option[Term], Double)] = {
    if (previous.size > 1) {
      val earliestTyps = previous.last.ts.terms.support.map(_.typ)
      val pfMap: scala.collection.immutable.Map[Typ[Term], Set[Term]] =
        fs.ts.terms.support
          .filterNot(t => earliestTyps.contains(t.typ))
          .groupBy(_.typ)
      logger.info(s"New lemmas: ${pfMap.size}")
      logger.info(
        s"Theorem numbers: ${previous.map(fs => fs.ts.terms.map(_.typ).support.size)}"
      )
      val thmsByPf: FiniteDistribution[Typ[Term]] = fs.ts.terms
        .map(_.typ)
        .filter(tp => !earliestTyps.contains(tp))
        .safeNormalized
      pfMap.toVector.map {
        case (tp, terms) =>
          (tp, Some(terms.maxBy(fs.ts.terms(_))), thmsByPf(tp))
      }
    } else {
      logger.info("New lemmas : none as there were no previous states")
      Vector()
    }
  }

  def finalStateFilteredLemmas(
      tg: TermGenParams = TermGenParams.zero.copy(appW = 0.1, unAppW = 0.1),
      maxRatio: Double = 1.5,
      maxTime: Option[Long] = Some(60000L)
  ): MicroHoTTBoTT[FinalState, Lemmas, PreviousPosts[FinalState] :: PreviousPosts[
    TautologyInitState
  ] :: Set[
    Equation
  ] :: QueryBaseState :: HNil] = {
    val response
        : PreviousPosts[FinalState] :: PreviousPosts[TautologyInitState] :: Set[
          Equation
        ] :: QueryBaseState :: HNil => FinalState => Future[
          Lemmas
        ] = {
      case (prevFS :: tauts :: eqns :: qinit :: HNil) =>
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
            Utils.logger.info(
              s"avoiding ${proved.size} types as tautologies and generators."
            )
            Lemmas(
              (fs.ts.lemmas.par).filterNot {
                case (tp, _, _) => proved.contains(tp)
              },
              Some(
                fs.ts.thmsByStMap
              )
            )
          }
        }
    }
    MicroBot(response, name = Some("final state to filtered lemmas"))
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

  lazy val reportProved: Callback[Proved, HoTTPostWeb, Unit, ID] =
    Callback.simple(
      { (_) => (proved) =>
        Future {
          Utils.logger.info(
            s"Proved ${proved.statement} by ${proved.proofOpt} in context ${proved.context}"
          )
        }
      },
      name = Some("report proved")
    )

  lazy val reportContradicted: Callback[Contradicted, HoTTPostWeb, Unit, ID] =
    Callback.simple({ (_) => (proved) =>
      Future {
        Utils.logger.info(
          s"Contradicted ${proved.statement} in context ${proved.context}"
        )
      }
    }, name = Some("report contradicted"))

  lazy val updateTerms: Callback[FinalState, HoTTPostWeb, Unit, ID] =
    Callback.simple(
      { (web: HoTTPostWeb) => (fs: FinalState) =>
        val allTerms = fs.ts.terms.support union (fs.ts.typs.support.par
          .map(t => t: Term)
          .seq)
        val newTerms = (allTerms -- web.terms).par
        newTerms
          .filter(isVar(_))
          .foreach(t => logger.error(s"variable $t considered term"))
        web.addTerms(newTerms.seq)
        val typs =
          newTerms.collect { case t: Typ[u] => t: Typ[Term] } union allTerms
            .map(
              _.typ
            )
        val allEquations = newTerms.flatMap(DE.formalEquations(_)) union (typs
          .flatMap(
            DE.formalTypEquations(_)
          ))
        Utils.logger.info(s"Obtained ${allEquations.size} formal equations")
        val normalEquations = allEquations.map(TermData.isleNormalize(_))
        web.addEqns(normalEquations.seq)
      },
      Some("update terms")
    )

  lazy val reportSuccesses: TypedPostResponse[FinalState, HoTTPostWeb, ID] =
    Callback.simple { (web: HoTTPostWeb) => (fs: FinalState) =>
      if (fs.successes.size > 0) {
        logger.info("Success: " + fs.successes.toString())
        Utils.report("Success: " + fs.successes.toString())
      }
    }

  def reportProofs(
      results: Vector[Typ[Term]],
      text: String = "Results",
      goalOpt: Option[Typ[Term]] = None // we halt if we get this, should be subset of results
  ): Callback[
    FinalState,
    HoTTPostWeb,
    QueryEquations,
    ID
  ] = {
    val response: HoTTPostWeb => QueryEquations => FinalState => Future[
      Unit
    ] = { (web: HoTTPostWeb) =>
      {
        case qe =>
          (fs: FinalState) =>
            Future {
              val termsSet = fs.ts.terms.support
              val pfs = results
                .map(typ => typ -> termsSet.filter(_.typ == typ))
                .filter(_._2.nonEmpty)
              val view = s"$text: ${pfs.size}\n${pfs
                .map {
                  case (tp, ps) =>
                    val best = ps.maxBy(t => fs.ts.terms(t))
                    val output = s"Lemma: $tp; best proof: ${best} with weight ${fs.ts
                      .terms(best)}; statement weight ${fs.ts.typs(tp)}"
                    if (fs.ts.typs(tp) == 0) {
                      import GeneratorVariables._, Expression._
                      val (eqns, terms) = EquationNode.traceBack(qe.nodes, FinalVal(Elem(tp, TermRandomVars.Typs)), 4)
                      val eqV           = eqns.mkString("Traced back equations:\n", "\n", "\n")
                      val termsWeights  = elemVals(terms, fs.ts)
                      val tV =
                        termsWeights
                          .map { case (exp, p) => s"$exp -> $p" }
                          .mkString("Weights of terms and types:\n", "\n", "\n")
                      s"$output\nVanising Lemma: $tp\n$eqV$tV"
                    } else output
                }
                .mkString("\n")}"
              logger.info(view)
              Utils.report(view)
              goalOpt.foreach { g =>
                if (pfs.map(_._1).contains(g)) {
                  web.halt()
                  Utils.running = false
                }
              }
            }
      }
    }
    Callback(response, name = Some("report proofs"))
  }

  def reportProofsSimple(
      results: Vector[Typ[Term]],
      text: String = "Results",
      goalOpt: Option[Typ[Term]] = None // we halt if we get this, should be subset of results
  ): Callback[
    FinalState,
    HoTTPostWeb,
    Unit,
    ID
  ] = {
    val response: HoTTPostWeb => Unit => FinalState => Future[
      Unit
    ] = { (web: HoTTPostWeb) =>
      {
        case _ =>
          (fs: FinalState) =>
            Future {
              val termsSet = fs.ts.terms.support.par
              val pfs = results
                .map(typ => typ -> termsSet.filter(_.typ == typ))
                .filter(_._2.nonEmpty)
              val view = s"$text: ${pfs.size}\n${pfs
                .map {
                  case (tp, ps) =>
                    val best = ps.maxBy(t => fs.ts.terms(t))
                    s"Lemma: $tp; best proof: ${best} with weight ${fs.ts
                      .terms(best)}; statement weight ${fs.ts.typs(tp)}"
                }
                .mkString("\n")}"
              logger.info(view)
              // Utils.report(view)
              goalOpt.foreach { g =>
                if (pfs.map(_._1).contains(g)) {
                  web.halt()
                  Utils.running = false
                }
              }
            }
      }
    }
    Callback(response, name = Some("report proofs (simple)"))
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
    Callback(update, name = Some("report base and tangent pairs"))
  }

  lazy val expEvToEqns: SimpleBot[ExpressionEval, GeneratedEquationNodes] =
    MicroBot.simple(
      (ev: ExpressionEval) =>
        GeneratedEquationNodes(ev.equations.flatMap(Equation.split))
    )

  lazy val expEvToFinalState: SimpleBot[ExpressionEval, FinalState] =
    MicroBot.simple(
      (ev: ExpressionEval) => {
        Utils.logger.info("computing final state")
        FinalState(ev.finalTermState())
      }
    )

  /**
    * given an instance, which is just a term of specified types, looks for instances sought and
    * in case of match returns the corresponding new goal and how the old is a consequence of the new.
    *
    * @return
    */
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
              val consequence = Consequence(
                newGoal,
                seek.sigma,
                Option(ExstFunc(deduction)),
                seek.context
              )
              Some(
                consequence ::
                  SeekGoal(
                    newGoal,
                    seek.context,
                    seek.sigma +: seek.forConsequences
                  ) :: HNil
              )
            } else None
          )
      }
    }
    MicroBot(response, name = Some("goals from instances"))
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
                  goal.goal +: goal.forConsequences
                ) :: HNil
              }
          }

    MicroBot(response, name = Some("skolemizing goal"))
  }

  lazy val viaZeroBot: MicroBot[SeekGoal, Option[
    Consequence :: SeekGoal :: HNil
  ], HoTTPostWeb, Unit, ID] = {
    val response
        : Unit => SeekGoal => Future[Option[Consequence :: SeekGoal :: HNil]] =
      (_) =>
        (goal) =>
          Future {
            goal.goal match {
              case ft: FuncTyp[u, v] =>
                val newGoal = negate(ft.dom)
                if (newGoal == goal.goal) {
                  Utils.logger.info(
                    s"nothing gained by contradicting hypothesis for ${goal.goal}"
                  )
                  None
                } else {
                  Utils.logger.info(
                    s"try to prove $newGoal as contradicting hypothesis  for ${goal.goal}"
                  )
                  val x = ft.dom.Var
                  val y = newGoal.Var
                  val transform = {
                    import Fold._
                    y :-> (x :-> vacuous(ft.codom)(
                      negateContra(ft.dom)(x)(y)
                    ))
                  }
                  val cons =
                    Consequence(
                      newGoal,
                      goal.goal,
                      Option(ExstFunc(transform)),
                      goal.context
                    )
                  Utils.logger.info(s"Consequence: $cons")
                  Some(
                    cons :: SeekGoal(
                      newGoal,
                      goal.context,
                      goal.goal +: goal.forConsequences
                    ) :: HNil
                  )
                }
              case pd: PiDefn[u, v] =>
                val newGoal = negate(pd.domain)
                if (newGoal == goal.goal) {
                  Utils.logger.info(
                    s"nothing gained by contradicting hypothesis for ${goal.goal}"
                  )
                  None
                } else {
                  Utils.logger.info(
                    s"try to prove $newGoal as contradicting hypothesis for ${goal.goal}"
                  )
                  val x = pd.domain.Var
                  val y = newGoal.Var
                  val transform = {
                    import Fold._
                    y :-> (x :-> vacuous(pd.fibers(x))(
                      negateContra(pd.domain)(x)(y)
                    ))
                  }
                  val cons =
                    Consequence(
                      newGoal,
                      goal.goal,
                      Option(ExstFunc(transform)),
                      goal.context
                    )
                  Utils.logger.info(s"Consequence: $cons")
                  Some(
                    cons :: SeekGoal(
                      newGoal,
                      goal.context,
                      goal.goal +: goal.forConsequences
                    ) :: HNil
                  )
                }
              case _ =>
                Utils.logger.info(
                  s"cannot prove ${goal.goal} by contradicting hypothesis"
                )
                None
            }
          }

    MicroBot(response, name = Some("prove by contradicting hypothesis"))
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

  lazy val chompEqnUpdate: TypedPostResponse[ChompResult, HoTTPostWeb, ID] =
    Callback.simple(
      (web: HoTTPostWeb) =>
        (cr: ChompResult) => {
          val neqs = cr.eqns.map(TermData.isleNormalize(_))
          web.addEqns(neqs)
        }
    )

  lazy val eqnUpdate: Callback[GeneratedEquationNodes, HoTTPostWeb, Unit, ID] =
    Callback.simple(
      (web: HoTTPostWeb) =>
        (eqs: GeneratedEquationNodes) => {
          val neqs = eqs.normalized
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
        },
      name = Some("update equations")
    )

  def eqnsToExpEv(
      cvOpt: Option[Expression.Coeff[_] => Option[Double]] = None
  ): MicroHoTTBoTT[
    GeneratedEquationNodes,
    ExpressionEval,
    Set[EquationNode] :: QueryProver :: ExpressionEval :: HNil
  ] = {
    val response
        : Set[EquationNode] :: QueryProver :: ExpressionEval :: HNil => GeneratedEquationNodes => Future[
          ExpressionEval
        ] = {
      case eqns :: qlp :: expEv :: HNil =>
        eqs =>
          val neqs  = eqs.normalized
          val nodes = eqns union (neqs)
          Utils.logger.debug("Obtained normalized equations")
          val groupedItEqns = Equation.groupIt(nodes)
          Utils.logger.debug("Obtained grouped equations as iterator")
          val groupedVecEqns = groupedItEqns.toVector
          Utils.logger.debug("Obtained grouped equations as vector")
          val groupedEqns = Utils.makeSet(groupedVecEqns)
          Utils.logger.debug("Obtained set of grouped equations")
          import qlp.lp
          Future(
            ExpressionEval.fromInitEqs(
              lp.initState,
              groupedEqns,
              cvOpt.getOrElse(lp.tg.coeffVal(_)),
              lp.tg.varWeight,
              lp.maxRatio,
              lp.scale,
              lp.smoothing,
              lp.exponent,
              lp.decay,
              lp.maxTime,
              Some(expEv.finalDist)
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

  def finalStateToChomp(solverWeight: Double = 0.05): MicroBot[
    FinalState,
    ChompResult,
    HoTTPostWeb,
    Set[HoTT.Term] :: QueryProver :: HNil,
    ID
  ] = {
    val response: (Set[Term] :: QueryProver :: HNil) => FinalState => Future[
      ChompResult
    ] = {
      case (terms :: qp :: HNil) => {
        case fs =>
          val unknowns = fs.ts.orderedUnknowns
          Utils.logger.info(s"seeking to chomp ${unknowns.size} unknowns")
          StrategicProvers
            .targetChomper(
              qp.lp.withParams(qp.lp.tg.copy(solverW = solverWeight)),
              unknowns,
              accumTerms = terms
            )
            .map {
              case (s, fl, eqs, _) => ChompResult(s, fl, eqs)
            }
            .runToFuture
      }
    }
    MicroBot(response, name = Some("chomp"))
  }

  def finalStateToConcurrentChomp(
      solverWeight: Double = 0.05,
      concurrency: Int = Utils.threadNum
  ): MicroBot[
    FinalState,
    ChompResult,
    HoTTPostWeb,
    Set[HoTT.Term] :: QueryProver :: HNil,
    ID
  ] = {
    val response: (Set[Term] :: QueryProver :: HNil) => FinalState => Future[
      ChompResult
    ] = {
      case (terms :: qp :: HNil) => {
        case fs =>
          val unknowns = fs.ts.orderedUnknowns.grouped(concurrency).toVector
          Utils.logger.info(s"seeking to chomp ${unknowns.size} unknown groups")
          StrategicProvers
            .concurrentTargetChomper(
              qp.lp.withParams(qp.lp.tg.copy(solverW = solverWeight)),
              unknowns,
              concurrency,
              accumTerms = terms
            )
            .map {
              case (s, fl, eqs, _) => ChompResult(s, fl, eqs)
            }
            .runToFuture
      }
    }
    MicroBot(response, name = Some("concurrent chomp"))
  }

  def finalStateToParallelChomp(
      solverWeight: Double = 0.05,
      cutoffScales: List[Double] = List(1)
  ): MicroBot[
    FinalState,
    ChompResult,
    HoTTPostWeb,
    Set[HoTT.Term] :: QueryProver :: HNil,
    ID
  ] = {
    val response: (Set[Term] :: QueryProver :: HNil) => FinalState => Future[
      ChompResult
    ] = {
      case (terms :: qp :: HNil) => {
        case fs =>
          val unknowns = fs.ts.orderedUnknowns
          val lp       = qp.lp.withParams(qp.lp.tg.copy(solverW = solverWeight))
          Utils.logger.info(s"seeking to chomp ${unknowns.size} unknown groups")
          Future {
            val (s, fl, eqs, _) = StrategicProvers
              .parChomper(
                unknowns,
                ParMapState.fromTermState(lp.initState),
                lp.tg,
                cutoffScales.map(_ * lp.cutoff),
                accumTerms = terms
              )
            ChompResult(s, fl, eqs)
          }
      }
    }
    MicroBot(response, name = Some("concurrent chomp"))
  }

  def finalStateToLiberalChomp(solverWeight: Double = 0.05): MicroBot[
    FinalState,
    ChompResult,
    HoTTPostWeb,
    Set[HoTT.Term] :: QueryProver :: HNil,
    ID
  ] = {
    val response: (Set[Term] :: QueryProver :: HNil) => FinalState => Future[
      ChompResult
    ] = {
      case (terms :: qp :: HNil) => {
        case fs =>
          val unknowns = fs.ts.orderedUnknowns
          Utils.logger.info(s"seeking to chomp ${unknowns.size} unknowns")
          StrategicProvers
            .liberalChomper(
              qp.lp.withParams(qp.lp.tg.copy(solverW = solverWeight)),
              unknowns,
              accumTerms = terms
            )
            .map {
              case (s, fl, eqs, _) => ChompResult(s, fl, eqs)
            }
            .runToFuture
      }
    }
    MicroBot(response)
  }

  def chompReport(
      numFailures: Int = 10
  ): HoTTCallBack[ChompResult, FinalState] = {
    val response: HoTTPostWeb => FinalState => ChompResult => Future[Unit] =
      (_) =>
        (fs) =>
          (cr) =>
            Future {
              Utils.logger.info(s"number of successes: ${cr.successes.size}")
              val topFails = cr.failures
                .map(tp => tp -> fs.ts.typs(tp))
                .sortBy { case (_, p) => -p }
                .take(numFailures)
              Utils.logger.info(s"number of failures: ${cr.failures.size}")
              Utils.logger.info(s"""top ${numFailures} failures: ${cr.failures
                .mkString(", ")}""")
            }
    Callback(response, name = Some("chomp report"))
  }

  val goalsAfterChomp
      : MiniBot[ChompResult, WithWeight[SeekGoal], HoTTPostWeb, QueryInitState :: FinalState :: HNil, ID] = {
    val response: QueryInitState :: FinalState :: HNil => ChompResult => Future[
      Vector[WithWeight[SeekGoal]]
    ] = {
      case qis :: fs :: HNil =>
        (cr) =>
          Future {
            val typDist = FiniteDistribution(cr.failures.flatMap { typ =>
              val p = fs.ts.typs(typ)
              Vector(Weighted(typ, p / 2), Weighted(negate(typ), p / 2))
            }).safeNormalized
            typDist.pmf.map {
              case Weighted(x, p) =>
                withWeight(SeekGoal(x, qis.init.context), p)
            }
          }
    }

    MiniBot[ChompResult, WithWeight[SeekGoal], HoTTPostWeb, QueryInitState :: FinalState :: HNil, ID](
      response
    )
  }

  def failedAfterChomp(power: Double = 1.0): MiniBot[ChompResult, WithWeight[
    FailedToProve
  ], HoTTPostWeb, QueryInitState :: FinalState :: HNil, ID] = {
    val response: QueryInitState :: FinalState :: HNil => ChompResult => Future[
      Vector[WithWeight[FailedToProve]]
    ] = {
      case qis :: fs :: HNil =>
        (cr) =>
          Future {
            val typDist = FiniteDistribution(cr.failures.flatMap { typ =>
              val p = fs.ts.typs(typ)
              Vector(Weighted(typ, p / 2), Weighted(negate(typ), p / 2))
            }).safeNormalized
            typDist.pmf.map {
              case Weighted(x, p) =>
                withWeight(
                  FailedToProve(x, qis.init.context),
                  math.pow(p, power)
                )
            }
          }
    }

    MiniBot[ChompResult, WithWeight[FailedToProve], HoTTPostWeb, QueryInitState :: FinalState :: HNil, ID](
      response
    )
  }

  val goalsAfterTRChomp
      : MiniBot[ChompResult, WithWeight[SeekGoal], HoTTPostWeb, QueryInitState :: TermResult :: HNil, ID] = {
    val response: QueryInitState :: TermResult :: HNil => ChompResult => Future[
      Vector[WithWeight[SeekGoal]]
    ] = {
      case qis :: (ts, _) :: HNil =>
        (cr) =>
          Future {
            val typDist = FiniteDistribution(cr.failures.map { typ =>
              Weighted(typ, ts.typs(typ))
            }).safeNormalized
            typDist.pmf.map {
              case Weighted(x, p) =>
                withWeight(SeekGoal(x, qis.init.context), p)
            }
          }
    }

    MiniBot[ChompResult, WithWeight[SeekGoal], HoTTPostWeb, QueryInitState :: TermResult :: HNil, ID](
      response
    )
  }

  lazy val deducedResults
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
            }).toVector
              .distinctBy(pf => (pf.statement, pf.context))
              .map(Decided.asEither(_))
          }
    }

    MiniBot(response, name = Some("deduced results"))
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
            .map(v => Lemmas(v.par.map(xy => (xy._1, None, xy._2))))
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
            p <- ev.flattenedOptimumTask(
              narrow.pow,
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
            .map(v => Lemmas(v.par.map(xy => (xy._1, None, xy._2))))
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
            }.seq
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
            }.seq
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
            .parSequence(eqGps.seq)
            .map(_.fold(Set.empty[EquationNode])(_ union _))
            .map(GeneratedEquationNodes(_))
          allEqs.runToFuture
        }
    MicroBot(response)
  }

  def avoidLemmas(decay: Double = 0.5, cutoff: Double = 0.04)(
      v: Vector[UsedLemmas]
  ): ParSet[HoTT.Typ[HoTT.Term]] = {
    val allLemmas = v.par.flatMap(_.support).to(ParSet)
    val l         = v.length
    def hasLargeWeight(typ: Typ[Term]) =
      (0 until (l)).exists(j => v(j).weight(typ) * math.pow(decay, j) > cutoff)
    allLemmas.filter(hasLargeWeight(_))
  }

  def baseMixinLemmas(
      power: Double = 1.0,
      pfWeight: Double = 0.0
  ): SimpleBot[Lemmas, BaseMixinLemmas] =
    MicroBot.simple(
      lem => {
        val powerSum =
          lem.lemmas.map { case (_, _, p) => math.pow(p, power) }.sum
        val proofPowerSum =
          lem.lemmas.map { case (tp, _, _) => math.pow(lem.weight(tp), power) }.sum
        val flattened = lem.lemmas.map {
          case (tp, pfOpt, p) =>
            (
              tp,
              pfOpt,
              (math.pow(p, power) * (1.0 - pfWeight) / powerSum) + (math
                .pow(lem.weight(tp), power) * pfWeight / proofPowerSum)
            )
        }
        BaseMixinLemmas(flattened)
      }
    )

  def tangentLemmas(
      scale: Double = 1.0,
      decay: Double = 0.5,
      cutoff: Double = 0.04,
      power: Double = 1,
      pfScale: Double = 0.0
  ): MicroHoTTBoTT[Lemmas, UsedLemmas :: TangentLemmas :: HNil, PreviousPosts[
    UsedLemmas
  ]] = {
    val response: PreviousPosts[UsedLemmas] => Lemmas => Future[
      UsedLemmas :: TangentLemmas :: HNil
    ] =
      (ul) =>
        (lem) =>
          Future {
            // val exclude = avoidLemmas(decay, cutoff)(ul.contents)
            // logger.info(s"excluded lemmas:\n${exclude.mkString("\n")}")
            val l =
              for {
                (tp, pfOpt, p) <- lem.lemmas
                q = lem.weight(tp) * pfScale
              } yield (tp, pfOpt, math.pow(p, power), math.pow(q, power))
            val ltot  = l.map(_._3).sum
            val sc    = scale / ltot
            val pfTot = l.map(_._4).sum
            val pfSc  = if (pfTot == 0.0) 0.0 else pfScale / pfTot
            Utils.logger.debug(s"proof powers scaled by $pfSc")
            val tangLemmas = l.map {
              case (tp, pfOpt, w, u) => (tp, pfOpt, (w * sc) + (u * pfSc))
            }
            val used = tangLemmas.map { case (tp, _, p) => tp -> p }
            UsedLemmas(used) :: TangentLemmas(tangLemmas) :: HNil
          }
    MicroBot(response)
  }

  def baseState(
      initialState: TermState,
      equationNodes: Set[EquationNode],
      tg: TermGenParams,
      maxRatio: Double = 1.01,
      scale: Double = 1.0,
      smooth: Option[Double] = None,
      exponent: Double = 0.5,
      decay: Double = 1,
      maxTime: Option[Long] = None
  ) = {
    logger.info("Computing base state")
    val groupedVec = Equation
      .groupMap(equationNodes, DE.termStateInitMap(initialState))
      .values
      .toVector
    Utils.logger.info("Created vector of equations")
    val groupedSet = Utils.makeSet(
      groupedVec
      // .map(TermData.isleNormalize(_))
    )
    Utils.logger.debug("Created set of equations")
    val expEv = ExpressionEval.fromInitEqs(
      initialState,
      groupedSet,
      tg.coeffVal(_),
      tg.varWeight,
      maxRatio,
      scale,
      smooth,
      exponent,
      decay,
      maxTime
    )
    logger.debug("Computed expression evaluator")
    (expEv.finalTermState(), expEv)
  }

  def baseStateTask(
      initialState: TermState,
      equationNodes: Set[EquationNode],
      tg: TermGenParams,
      maxRatio: Double = 1.01,
      scale: Double = 1.0,
      smooth: Option[Double] = None,
      exponent: Double = 0.5,
      decay: Double = 1,
      maxTime: Option[Long] = None
  ) = {
    logger.info("Computing base state")
    val groupSetTask = Task {
      val groupedVec =
        Equation
          .groupMap(equationNodes, DE.termStateInitMap(initialState))
          .values
          .toVector
      Utils.logger.info("Created vector of equations")
      Utils.makeSet(
        groupedVec
        // .map(TermData.isleNormalize(_))
      )
    }
    for {
      groupedSet <- groupSetTask
      _ = Utils.logger.debug("Created set of equations")
      expEv <- ExpressionEval.fromInitEqsTask(
        initialState,
        groupedSet,
        tg.coeffVal(_),
        tg.varWeight,
        maxRatio,
        scale,
        smooth,
        exponent,
        decay,
        maxTime
      )
      _ = logger.debug("Computed expression evaluator")
    } yield (expEv.finalTermState(), expEv)
  }

  def baseStateFromLp(
      lemmaMix: Double,
      cutoffScale: Double = 1.0,
      tgOpt: Option[TermGenParams] = None,
      depthOpt: Option[Int] = None
  ): MicroHoTTBoTT[
    BaseMixinLemmas,
    TangentBaseState,
    QueryProver :: QueryEquations :: HNil
  ] = {
    val response
        : QueryProver :: QueryEquations :: HNil => BaseMixinLemmas => Future[
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
              }.seq
            )
            val baseDist = lp.initState.terms
            val tdist    = (baseDist * (1.0 - lemmaMix) ++ (lemPfDist * lemmaMix))
            val bs       = lp.initState.copy(terms = tdist)
            val fs = baseState(
              bs,
              eqns.nodes,
              lp.tg,
              lp.maxRatio,
              lp.scale,
              lp.smoothing,
              lp.exponent,
              lp.decay,
              lp.maxTime
            )._1
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

  def baseStateFromSpecialInit(verbose: Boolean = true): DualMiniBot[
    BaseMixinLemmas,
    TangentBaseState,
    HoTTPostWeb,
    PreviousPosts[SpecialInitState] :: QueryProver :: Set[EquationNode] :: HNil,
    ID
  ] = {
    val response: PreviousPosts[SpecialInitState] :: QueryProver :: Set[
      EquationNode
    ] :: HNil => BaseMixinLemmas => Vector[
      Future[
        TangentBaseState
      ]
    ] = {
      case psps :: qp :: neqs :: HNil =>
        lems => {
          logger.info(s"previous special init states are ${psps.contents.size}")
          logger.debug(psps.contents.mkString("\n"))
          // val neqs = eqns.nodes
          logger.debug(s"Using ${neqs.size} equation nodes for base states")
          psps.contents.map(
            ps =>
              Future {
                import qp.lp
                val lemPfDist = FiniteDistribution(
                  lems.lemmas.map {
                    case (typ, pfOpt, p) =>
                      Weighted(pfOpt.getOrElse("pf" :: typ), p)
                  }.seq
                )
                val baseDist = ps.ts.terms
                val tdist    = (baseDist * (1.0 - ps.lemmaMix) ++ (lemPfDist * ps.lemmaMix))
                val bs       = ps.ts.copy(terms = tdist)
                val (fs, expEv) = baseState(
                  bs,
                  neqs,
                  ps.tgOpt.getOrElse(lp.tg),
                  lp.maxRatio,
                  lp.scale,
                  lp.smoothing,
                  lp.exponent,
                  lp.decay,
                  lp.maxTime
                )
                TangentBaseState(
                  fs
                  // .purge(ps.baseCutoff)
                  ,
                  ps.cutoffScale,
                  ps.tgOpt,
                  ps.depthOpt,
                  if (verbose) Some(expEv) else None,
                  Some(ps)
                )
              }
          )
        }
    }
    DualMiniBot(response)
  }

  def baseStateFromSpecialInitTask(verbose: Boolean = true): DualMiniBotTask[
    BaseMixinLemmas,
    TangentBaseState,
    HoTTPostWeb,
    PreviousPosts[SpecialInitState] :: QueryProver :: Set[EquationNode] :: HNil,
    ID
  ] = {
    val response: PreviousPosts[SpecialInitState] :: QueryProver :: Set[
      EquationNode
    ] :: HNil => BaseMixinLemmas => Vector[
      Task[
        TangentBaseState
      ]
    ] = {
      case psps :: qp :: neqs :: HNil =>
        lems => {
          logger.info(s"previous special init states are ${psps.contents.size}")
          logger.debug(psps.contents.mkString("\n"))
          // val neqs = eqns.nodes
          logger.debug(s"Using ${neqs.size} equation nodes for base states")
          // val eqVec = Equation.groupIt(neqs).toVector
          // Utils.logger.info("Grouped into equations from lookup")
          psps.contents.map(
            ps => {
              import qp.lp
              val lemPfDist = FiniteDistribution(
                lems.lemmas.map {
                  case (typ, pfOpt, p) =>
                    Weighted(pfOpt.getOrElse("pf" :: typ), p)
                }.seq
              )
              val baseDist = ps.ts.terms
              val tdist    = (baseDist * (1.0 - ps.lemmaMix) ++ (lemPfDist * ps.lemmaMix))

              val bs = ps.ts.copy(terms = tdist)
              baseStateTask(
                bs,
                neqs,
                ps.tgOpt.getOrElse(lp.tg),
                lp.maxRatio,
                lp.scale,
                lp.smoothing,
                lp.exponent,
                lp.decay,
                lp.maxTime
              ).map {
                case (fs, expEv) =>
                  TangentBaseState(
                    fs
                    // .purge(ps.baseCutoff)
                    ,
                    ps.cutoffScale,
                    ps.tgOpt,
                    ps.depthOpt,
                    if (verbose) Some(expEv) else None,
                    Some(ps)
                  )
              }
            }
          )
        }
    }
    DualMiniBotTask(
      response,
      name = Some("base state from special initial state")
    )
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
            }.seq)
            val funcs = tb.ts.terms.condMap(ExstFunc.opt).safeNormalized
            logger.debug(s"function domains: ${funcs.support.map(_.dom)}")
            logger.debug(s"lemma-proof types: ${lemPfDist.support.map(_.typ)}")
            val eqs = SimpleEquations.allAppEquations(funcs, lemPfDist, cutoff)
            GeneratedEquationNodes(eqs.toSet)
          }

    MicroBot(response)
  }

  def cappedSpecialBaseState(
      verbose: Boolean = true
  ): TypedPostResponse[BaseMixinLemmas, HoTTPostWeb, ID] =
    baseStateFromSpecialInitTask(verbose)
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
                  }.seq)
                  val funcs = tb.ts.terms.condMap(ExstFunc.opt).safeNormalized
                  SimpleEquations.unAppEquations(funcs, lemPfDist, cutoff)

                }
              })
              .map(_.flatten)
          eqsFut.map(eqs => GeneratedEquationNodes(eqs))
        }
    }
    MicroBot(response, name = Some("unified application equations"))
  }

  def timedUnAppEquations(
      cutoff: Double,
      maxTime: FiniteDuration,
      cutoffScale: Double = 2,
      minCutoff: Option[Double] = None
  ): MicroHoTTBoTT[TangentBaseCompleted.type, GeneratedEquationNodes, Collated[
    TangentBaseState
  ] :: TangentLemmas :: Set[Term] :: HNil] = {
    val response
        : Collated[TangentBaseState] :: TangentLemmas :: Set[Term] :: HNil => TangentBaseCompleted.type => Future[
          GeneratedEquationNodes
        ] = {
      case tbss :: tl :: terms :: HNil =>
        (_) => {
          val eqsFut =
            Task
              .parSequenceUnordered(tbss.contents.toSet.map {
                (tb: TangentBaseState) =>
                  {
                    val lemPfDist = FiniteDistribution(tl.lemmas.map {
                      case (t, pfOpt, p) =>
                        Weighted(pfOpt.getOrElse("lemma" :: t), p)
                    }.seq)
                    val funcs = tb.ts.terms.condMap(ExstFunc.opt).safeNormalized
                    SimpleEquations.timedUnAppEquations(
                      funcs,
                      lemPfDist,
                      cutoff,
                      maxTime,
                      minCutoff,
                      cutoffScale,
                      terms,
                      terms
                        .collect { case t: Typ[u] => t: Typ[Term] } union (terms
                        .map(_.typ))
                    )

                  }
              })
              .map(_.flatten)
          eqsFut.map(eqs => GeneratedEquationNodes(eqs.toSet, true))
        }.runToFuture
    }
    MicroBot(response, name = Some("unified equations"))
  }

  def lpUnAppEquations(
      cutoff: Double,
      maxTime: FiniteDuration
  ): MicroHoTTBoTT[TangentBaseCompleted.type, GeneratedEquationNodes, Collated[
    TangentBaseState
  ] :: TangentLemmas :: Set[Term] :: HNil] = {
    val response
        : Collated[TangentBaseState] :: TangentLemmas :: Set[Term] :: HNil => TangentBaseCompleted.type => Future[
          GeneratedEquationNodes
        ] = {
      case tbss :: tl :: terms :: HNil =>
        (_) => {
          val eqsTask = Task
            .parSequenceUnordered(tbss.contents.toSet.map {
              (tb: TangentBaseState) =>
                {
                  val lpt = LocalTangentProver(
                    tb.ts,
                    Set(),
                    TermState(tl.fd, FiniteDistribution.empty),
                    TermGenParams.zero.copy(unAppW = 0.4),
                    cutoff,
                    limit = maxTime,
                    genMaxDepth = Some(1)
                  )
                  lpt.enhancedEquationNodes
                }
            })
            .map(_.flatten)
          eqsTask.map(eqs => GeneratedEquationNodes(eqs.toSet, true))
        }.runToFuture
    }
    MicroBot(response, name = Some("local prover equations"))
  }

  def parGenUnAppEquations(
      cutoff: Double,
      maxTime: FiniteDuration
  ): MicroHoTTBoTT[TangentBaseCompleted.type, GeneratedEquationNodes, Collated[
    TangentBaseState
  ] :: TangentLemmas :: Set[Term] :: HNil] = {
    val response
        : Collated[TangentBaseState] :: TangentLemmas :: Set[Term] :: HNil => TangentBaseCompleted.type => Future[
          GeneratedEquationNodes
        ] = {
      case tbss :: tl :: terms :: HNil =>
        (_) => {
          val eqsFut = Future
            .sequence(tbss.contents.toSet.map { (tb: TangentBaseState) =>
              Future {
                val limit        = System.currentTimeMillis() + maxTime.toMillis
                def halted()     = System.currentTimeMillis() > limit
                val baseState    = ParMapState.fromTermState(tb.ts)
                val tangentState = ParMapState(tl.fd.toParMap, ParMap())
                import ParMapState.{normalize, add}
                val state = ParMapState(
                  normalize(add(baseState.termDist, tangentState.termDist)),
                  baseState.typDist,
                  baseState.vars,
                  baseState.inds,
                  baseState.goalDist,
                  baseState.context
                )
                val tg  = TermGenParams.zero.copy(unAppW = 0.3)
                val ns  = ParMapState.parNodeSeq(tg)
                val pde = new ParDistEq(ns.nodeCoeffSeq)
                val (terms, eqs) = pde.varDist(
                  state,
                  Some(1),
                  halted()
                )(TermRandomVars.Terms, cutoff)
                val formalTypEqs =
                  terms.keySet.map(_.typ).flatMap(DE.formalTypEquations(_))
                eqs union (formalTypEqs)
              }
            })
            .map(_.flatten)
          eqsFut.map(eqs => GeneratedEquationNodes(eqs.toSet, false))
        }
    }
    MicroBot(response, name = Some("parallel generation equations"))
  }

  def cappedBaseState(
      lemmaMix: Double,
      cutoffScale: Double = 1.0,
      tgOpt: Option[TermGenParams] = None,
      depthOpt: Option[Int] = None,
      verbose: Boolean = true
  ): TypedPostResponse[BaseMixinLemmas, HoTTPostWeb, ID] =
    (baseStateFromLp(lemmaMix, cutoffScale, tgOpt, depthOpt) && baseStateFromSpecialInit(
      verbose
    )).reduce((v: Vector[TangentBaseState]) => TangentBaseCompleted)

  def proofEquationLHS(
      eqns: Set[EquationNode],
      tp: Typ[Term]
  ): Set[Expression] = {
    import GeneratorVariables._, Expression._
    eqns.collect {
      case eq @ EquationNode(FinalVal(Elem(x: Term, TermRandomVars.Terms)), _)
          if x.typ == tp =>
        eq.lhs
    }
  }

  def proofTrace(
      eqns: Set[EquationNode],
      tp: Typ[Term],
      depth: Int
  ): (Vector[EquationNode], Vector[Expression]) = {
    val pair = proofEquationLHS(eqns, tp).toVector.map(
      exp => EquationNode.traceBack(eqns, exp, depth)
    )
    (pair.flatMap(_._1), pair.flatMap(_._2))
  }

  def elemVals(elems: Vector[Expression], ts: TermState) = {
    import GeneratorVariables._, TermRandomVars.{Terms, Typs}
    elems.collect {
      case exp @ FinalVal(Elem(x: Term, Terms))     => exp -> ts.terms(x)
      case exp @ FinalVal(Elem(x: Typ[Term], Typs)) => exp -> ts.typs(x)
    }
  }

  def reportBaseTangentsCalc(
      results: Vector[Typ[Term]],
      steps: Vector[Typ[Term]],
      inferTriples: Vector[(Typ[Term], Typ[Term], Typ[Term])],
      depth: Int = 4,
      verbose: Boolean = true
  ): Callback[
    TangentBaseCompleted.type,
    HoTTPostWeb,
    Collated[
      TangentBaseState
    ] :: TangentLemmas :: HNil,
    ID
  ] = {
    val response: HoTTPostWeb => Collated[
      TangentBaseState
    ] :: TangentLemmas :: HNil => TangentBaseCompleted.type => Future[Unit] = {
      (_) =>
        {
          case ctbs :: tl :: HNil =>
            (_) =>
              Future {
                logger.debug(
                  s"generating equations with ${ctbs.contents.size} base states and ${tl.lemmas.size} lemmas (before pruning)"
                )
                val tls = results
                  .flatMap(typ => tl.lemmas.find(_._1 == typ).map(typ -> _._3))
                val view1 =
                  s"Tangent lemmas (used with bases below): ${tls.size}\n${tls
                    .mkString("\n")}"
                logger.debug(view1)
                ctbs.contents.foreach { tbs =>
                  val initString: String = tbs.initOpt
                    .map(_.ts.terms.supp)
                    .map(terms => s"initial terms ${terms.mkString(", ")}")
                    .getOrElse(s"no initial state recorded")
                  logger.debug(
                    s"Details for base state: $initString\nInitial state (optional): ${tbs.initOpt}"
                  )
                  val termsSet = tbs.ts.terms.support
                  val basePfs = steps
                    .map(typ => typ -> termsSet.filter(_.typ == typ))
                    .filter(_._2.nonEmpty)
                  val view2 =
                    s"Terms in base (used with tangents above) with $initString: ${basePfs.size}\n${basePfs
                      .map {
                        case (tp, ps) =>
                          val best = ps.maxBy(t => tbs.ts.terms(t))
                          s"Type: $tp; best term: ${best} with weight ${tbs.ts.terms(best)}"
                      }
                      .mkString("\n")}"
                  logger.debug(view2)
                  val baseLemmas = results
                    .map(typ => typ -> termsSet.filter(_.typ == typ))
                    .filter(_._2.nonEmpty)
                  val view3 =
                    s"Lemmas in base state with $initString for mixin (used with tangents above): ${baseLemmas.size}\n${baseLemmas
                      .map {
                        case (tp, ps) =>
                          val best = ps.maxBy(t => tbs.ts.terms(t))
                          s"Type: $tp; best term: ${best} with weight ${tbs.ts.terms(best)}"
                      }
                      .mkString("\n")}"
                  logger.debug(view3)
                  val view4 = inferTriples
                    .map {
                      case (f, x, fx) =>
                        val p = tbs.ts.terms.map(_.typ)(f)
                        val q = tls.find(_._1 == x).map(_._2).getOrElse(0.0)
                        s"($p, $q, ${p * q}) for ($f, $x, $fx)"
                    }
                    .mkString(
                      s"Inference by unified applications, triples and weight for base state with $initString\n",
                      "\n",
                      "\n"
                    )
                  logger.debug(view4)
                  if (verbose) Future {
                    tbs.evOpt.map { ev =>
                      logger.debug(
                        s"Extracting data from expression-evaluator for $initString"
                      )
                      val calc = ev.exprCalc
                      logger.debug(
                        s"Using expression-calculator for $initString"
                      )
                      steps.zipWithIndex.foreach {
                        case (typ, j) =>
                          Future {
                            logger.debug(
                              s"Tracing back $typ, step ${1 + j}, for base state with $initString"
                            )
                            val pfData = calc.proofData(typ)
                            Utils.logger.debug(
                              s"""|
                                |Located proof data for $typ, step ${1 + j}, for base state $initString
                                |Equations: 
                                |  ${pfData.map(_._2).mkString("\n")}
                                |Indices:${pfData.map(_._1).mkString(", ")}
                                |""".stripMargin
                            )
                            Future {
                              val backIndices = pfData
                                .map(_._1)
                                .flatMap(j => calc.traceIndices(j, depth))
                              Utils.logger.debug(
                                s"The traced back indices for $typ, step ${1 + j}, for base with $initString with depth $depth are ${backIndices.size}, namely ${backIndices
                                  .mkString(", ")}"
                              )

                              backIndices.foreach { i =>
                                Utils.logger.debug(
                                  s"""|Details for index $i, traced back for $typ, step ${1 + j}, for base with $initString with depth $depth
                                                    |Equation: ${ev.exprCalc
                                       .equationVec(
                                         i
                                       )}
                                                    |Rhs-expression: ${calc
                                       .rhsExprs(i)}
                                                    |Value: ${calc.finalVec(i)}
                                                    |--------------""".stripMargin
                                )
                              }
                            }
                          }
                      }
                    }
                  }
                }
              }
        }
    }
    Callback(response, name = Some("report base and tangent"))
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

          logger.debug(
            s"generating equations with ${ctbs.contents.size} base states and ${tl.lemmas.size} lemmas (before pruning)"
          )
          val tls = results
            .flatMap(typ => tl.lemmas.find(_._1 == typ).map(typ -> _._3))
          val view1 =
            s"Tangent lemmas (used with bases below): ${tls.size}\n${tls
              .mkString("\n")}"
          logger.debug(view1)
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
            logger.debug(view2)
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
          logger.debug(s"${provers.size} tangent provers after filtering")
          val equationsTasks =
            provers.map { lpt =>
              lpt.enhancedEquationNodes
                .map { eqns =>
                  logger.debug(s"obtained ${eqns.size} equation nodes")
                  remaining = remaining - 1
                  logger.debug(s"provers still running: $remaining")
                  eqns
                }
                .onErrorRecover {
                  case te: TimeoutException =>
                    logger.error(te.getMessage())
                    logger.debug(te)
                    remaining = remaining - 1
                    logger.debug(s"provers still running: $remaining")
                    Set.empty[EquationNode]
                  case te =>
                    logger.error(s"Serious error")
                    logger.error(te)
                    remaining = remaining - 1
                    logger.debug(s"provers still running: $remaining")
                    Set.empty[EquationNode]
                }
            }
          val allTask = Task
            .parSequenceUnordered(equationsTasks)
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
            }.seq
          )

    }

    DualMiniBot(responses)
  }

  lazy val cappedForkedTangentEquations =
    forkedTangentEquations
      .triggerWith[TangentBaseCompleted.type]
      .reduce(
        (v: Vector[GeneratedEquationNodes]) => {
          val all = Utils.gatherSet(v.map(_.eqn.toVector), Set())
          GeneratedEquationNodes(all)
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
                }.seq
              )
              val tdist     = (baseDist * (1.0 - lemmaMix) ++ (lemPfDist * lemmaMix))
              val baseState = lp.initState
              val expEv = ExpressionEval.fromInitEqs(
                baseState.copy(terms = tdist),
                Equation.group(baseEqs),
                lp.tg.coeffVal(_),
                lp.tg.varWeight,
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
            .parSequence(eqGps.seq)
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
                val ss = s.seq.subsets(j).toSet
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
                      goal.goal +: goal.forConsequences
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

  lazy val inductionBackward
      : MicroHoTTBoTT[SeekGoal, Option[FromAll], GatherPost[ExstInducDefn]] = {
    val response
        : GatherPost[ExstInducDefn] => SeekGoal => Future[Option[FromAll]] =
      gpInd =>
        sg =>
          Future {
            import TermGeneratorNodes.targetInducFuncs
            val funcs =
              gpInd.contents.flatMap(ind => targetInducFuncs(ind, sg.goal))
            val fromAlls = funcs.flatMap(
              fn => FromAll.get(fn, sg.goal, sg.forConsequences, sg.context)
            )
            fromAlls.headOption
          }
    MicroBot(response, name = Some("induction resolved"))
  }

  /**
    * generate instances with weights from a local prover
    * alternatively we can just use a final state and given goal,
    * with the final state generated from the local prover if desired,
    * but this way we target the type
    *
    * @return
    */
  lazy val instancesFromLP
      : MiniBot[SeekInstances, WithWeight[Instance], HoTTPostWeb, QueryProver, ID] = {
    val response: QueryProver => SeekInstances => Future[
      Vector[WithWeight[Instance]]
    ] =
      qp =>
        seekInst => {
          Utils.logger.info(
            s"seeking instances for ${seekInst.typ} with cutoff ${qp.lp.cutoff}"
          )
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
      response,
      name = Some("instances from local prover")
    )
  }

  lazy val inductionWeightedBackward: MiniBot[SeekGoal, WithWeight[
    FunctionForGoal
  ], HoTTPostWeb, QueryProver, ID] = {
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
                  fromAll.conclusion +: fromAll.forConsequences
                )
            )
          }

    MiniBot[FromAll, SeekGoal, HoTTPostWeb, Unit, ID](
      response,
      name = Some("resolve from-all into separate goals")
    )
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
                  fromAny.conclusion +: fromAny.forConsequences
                )
            )
          }

    MiniBot[FromAny, SeekGoal, HoTTPostWeb, Unit, ID](
      response,
      name = Some("resolve from-any into separate goals")
    )
  }

  lazy val negateGoal: MicroBot[SeekGoal, Option[
    Contradicts :: SeekGoal :: HNil
  ], HoTTPostWeb, GatherPost[SeekGoal], ID] = {
    val response: GatherPost[SeekGoal] => SeekGoal => Future[
      Option[Contradicts :: SeekGoal :: HNil]
    ] =
      (previous) =>
        sg =>
          Future {
            val newGoal =
              SeekGoal(negate(sg.goal), sg.context, sg.forConsequences)
            if (previous.contents.contains(newGoal)) None
            else
              Some(Contradicts.fromTyp(sg.goal, sg.context) :: newGoal :: HNil)
          }
    MicroBot(response, name = Some("negating goal"))
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
        },
      Some("product resolved")
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
        },
      Some("coproduct resolved")
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
          case _ =>
            Utils.logger.info(s"no instances sought for ${sk.goal}")
            None
        },
      Some("sigma-type resolved")
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
                ts.copy(terms = (ts.terms.+(lem.proof, weight)).safeNormalized)
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

  val goalInContext
      : SimpleBot[SeekGoal, Option[ProofLambda :: SeekGoal :: HNil]] =
    MicroBot.simple(
      (sk: SeekGoal) =>
        sk.goal match {
          case PiDefn(variable: Term, value: Typ[u]) =>
            val newSeek = SeekGoal(
              value,
              sk.context.addVariable(variable),
              sk.goal +: sk.forConsequences
            )
            val pfLambda = ProofLambda(
              sk.goal,
              newSeek.goal,
              sk.context,
              newSeek.context,
              variable,
              true
            )
            Some(pfLambda :: newSeek :: HNil)
          case FuncTyp(dom: Typ[v], codom: Typ[u]) =>
            val x = dom.Var
            val newSeek = SeekGoal(
              codom,
              sk.context.addVariable(x),
              sk.goal +: sk.forConsequences
            )
            val pfLambda = ProofLambda(
              sk.goal,
              newSeek.goal,
              sk.context,
              newSeek.context,
              x,
              false
            )
            Some(pfLambda :: newSeek :: HNil)
          case _ => None
        }
    )

  val exportProof
      : MicroHoTTBoTT[Proved, Option[Proved], GatherPost[ProofLambda]] = {
    val response: GatherPost[ProofLambda] => Proved => Future[Option[Proved]] =
      (gpPfLam) =>
        (proved) =>
          Future {
            gpPfLam.contents.flatMap(_.export(proved)).headOption
          }
    MicroBot(response, name = Some("export proof from context"))
  }

  val fullGoalInContext: SimpleBot[SeekGoal, Option[SeekGoal]] =
    MicroBot.simple(_.inContext)

  /**
    * prover with variables for the context of the goal (if compatible) and with goal mixed in.
    *
    * @param varWeight weight of variables added based on context
    * @param goalWeight weight of the goal
    * @return optionally a local prover
    */
  def goalToProver(
      varWeight: Double,
      goalWeight: Double
  ): MicroBot[SeekGoal, Option[LocalProver], HoTTPostWeb, QueryProver :: Set[
    HoTT.Term
  ] :: GatherMapPost[Decided] :: HNil, ID] = {
    // check whether the provers context is an initial segment of the context of the goal
    val subContext: SeekGoal => QueryProver :: Set[Term] :: GatherMapPost[
      Decided
    ] :: HNil => Boolean =
      (goal) => {
        case (qp: QueryProver) :: terms :: _ :: HNil => {
          val lpVars   = qp.lp.initState.context.variables
          val goalVars = goal.context.variables
          // pprint.log(lpVars)
          // pprint.log(goalVars)
          lpVars == goalVars.take(lpVars.size)
        }
      }

    val response
        : QueryProver :: Set[Term] :: GatherMapPost[Decided] :: HNil => SeekGoal => Future[
          Option[LocalProver]
        ] = {
      case (qp: QueryProver) :: terms :: gp :: HNil =>
        // println(qp)
        // pprint.log(qp)
        goal =>
          Future {
            if (goal.relevantGiven(terms, gp.contents)) {
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

  def goalAttempt(
      varWeight: Double
  ): MicroBot[SeekGoal, Option[Either[FailedToProve, Proved]], HoTTPostWeb, QueryProver :: Set[
    HoTT.Term
  ] :: GatherMapPost[Decided] :: HNil, ID] = {
    // check whether the provers context is an initial segment of the context of the goal
    val subContext: SeekGoal => QueryProver :: Set[Term] :: GatherMapPost[
      Decided
    ] :: HNil => Boolean =
      (goal) => {
        case (qp: QueryProver) :: terms :: _ :: HNil => {
          val lpVars   = qp.lp.initState.context.variables
          val goalVars = goal.context.variables
          lpVars == goalVars.take(lpVars.size)
        }
      }

    val response
        : QueryProver :: Set[Term] :: GatherMapPost[Decided] :: HNil => SeekGoal => Future[
          Option[Either[FailedToProve, Proved]]
        ] = {
      case (qp: QueryProver) :: terms :: gp :: HNil =>
        // pprint.log(qp)
        goal =>
          Future(terms.find(_.typ == goal.goal)).flatMap(
            tOpt =>
              if (tOpt.nonEmpty)
                Future.successful(
                  Some(Right(Proved(goal.goal, tOpt, goal.context)))
                )
              else if (goal.relevantGiven(terms, gp.contents)) {
                val lpVars   = qp.lp.initState.context.variables
                val goalVars = goal.context.variables
                val newVars  = goalVars.drop(lpVars.size)
                val withVars = newVars.foldLeft(qp.lp) {
                  case (lp: LocalProver, x: Term) =>
                    lp.addVar(x, varWeight)
                }
                Utils.logger.info(s"initial state ${withVars.initState}")
                withVars
                  .varDist(TermRandomVars.termsWithTyp(goal.goal))
                  .runToFuture
                  .map { fd =>
                    if (fd.pmf.isEmpty)
                      Some(
                        Left(
                          FailedToProve(
                            goal.goal,
                            goal.context,
                            goal.forConsequences
                          )
                        )
                      )
                    else {
                      val best = fd.pmf.maxBy(_.weight)
                      Some(
                        Right(Proved(goal.goal, Some(best.elem), goal.context))
                      )
                    }
                  }
              } else Future.successful(None)
          )
    }

    MicroBot(response, subContext, name = Some("attempting goal"))
  }

  lazy val goalLookup
      : MicroHoTTBoTT[SeekGoal, Either[FailedToProve, Proved], Set[Term]] =
    MicroBot(
      terms =>
        sg =>
          Future(
            terms
              .find(_.typ == sg.goal)
              .map(
                pf => Right(Proved(sg.goal, Some(pf), sg.context))
              )
              .getOrElse(
                Left(FailedToProve(sg.goal, sg.context, sg.forConsequences))
              )
          )
    )

  def parGoalAttemptEqs(
      varWeight: Double
  ): MicroBot[SeekGoal, Option[
    GeneratedEquationNodes :: Either[FailedToProve, Proved] :: HNil
  ], HoTTPostWeb, QueryProver :: Set[
    HoTT.Term
  ] :: GatherMapPost[Decided] :: HNil, ID] = {
    // check whether the provers context is an initial segment of the context of the goal
    val subContext: SeekGoal => QueryProver :: Set[Term] :: GatherMapPost[
      Decided
    ] :: HNil => Boolean =
      (goal) => {
        case (qp: QueryProver) :: terms :: _ :: HNil => {
          val lpVars   = qp.lp.initState.context.variables
          val goalVars = goal.context.variables
          lpVars == goalVars.take(lpVars.size)
        }
      }

    val response
        : QueryProver :: Set[Term] :: GatherMapPost[Decided] :: HNil => SeekGoal => Future[
          Option[
            GeneratedEquationNodes :: Either[FailedToProve, Proved] :: HNil
          ]
        ] = {
      case (qp: QueryProver) :: terms :: gp :: HNil =>
        // pprint.log(qp)
        goal =>
          Future(terms.find(_.typ == goal.goal)).flatMap(
            tOpt =>
              if (tOpt.nonEmpty)
                Future.successful(
                  Some(
                    GeneratedEquationNodes(Set()) :: Right(
                      Proved(goal.goal, tOpt, goal.context)
                    ) :: HNil
                  )
                )
              else if (goal.relevantGiven(terms, gp.contents)) {
                val lpVars   = qp.lp.initState.context.variables
                val goalVars = goal.context.variables
                val newVars  = goalVars.drop(lpVars.size)
                val withVars = newVars.foldLeft(qp.lp) {
                  case (lp: LocalProver, x: Term) =>
                    lp.addVar(x, varWeight)
                }
                Utils.logger.info(s"initial state ${withVars.initState}")
                val pde = ParDistEq.fromParams(qp.lp.tg, qp.lp.tg.varWeight)
                Future {
                  val (fd, eqs) = pde.varDist(
                    ParMapState.fromTermState(withVars.initState),
                    withVars.genMaxDepth,
                    withVars.halted()
                  )(TermRandomVars.termsWithTyp(goal.goal), withVars.cutoff)
                  val initTerms = withVars.initState.terms.support union (withVars.initState.typs.support
                    .map(x => x: Term))
                  val expEqs =
                    EquationExporter.export(eqs.to(Set), initTerms, newVars)
                  val geqs = GeneratedEquationNodes(expEqs)
                  Some(geqs :: {
                    Utils.logger.info(s"""|Failed for ${TermRandomVars
                                           .termsWithTyp(goal.goal)}
                          |initial state: ${withVars.initState}
                          |cutoff: ${withVars.cutoff}
                          |parameters : ${withVars.tg}""".stripMargin)
                    if (fd.isEmpty)
                      Left(
                        FailedToProve(
                          goal.goal,
                          goal.context,
                          goal.forConsequences
                        )
                      )
                    else {
                      val best = fd.maxBy(_._2)._1
                      Right(Proved(goal.goal, Some(best), goal.context))
                    }
                  } :: HNil)
                }
              } else Future.successful(None)
          )
    }

    MicroBot(
      response,
      subContext,
      name = Some("attempting goal with equations")
    )
  }

  def goalAttemptEqs(
      varWeight: Double
  ): MicroBot[SeekGoal, Option[
    GeneratedEquationNodes :: Either[FailedToProve, Proved] :: HNil
  ], HoTTPostWeb, QueryProver :: Set[
    HoTT.Term
  ] :: GatherMapPost[Decided] :: HNil, ID] = {
    // check whether the provers context is an initial segment of the context of the goal
    val subContext: SeekGoal => QueryProver :: Set[Term] :: GatherMapPost[
      Decided
    ] :: HNil => Boolean =
      (goal) => {
        case (qp: QueryProver) :: terms :: _ :: HNil => {
          val lpVars   = qp.lp.initState.context.variables
          val goalVars = goal.context.variables
          lpVars == goalVars.take(lpVars.size)
        }
      }

    val response
        : QueryProver :: Set[Term] :: GatherMapPost[Decided] :: HNil => SeekGoal => Future[
          Option[
            GeneratedEquationNodes :: Either[FailedToProve, Proved] :: HNil
          ]
        ] = {
      case (qp: QueryProver) :: terms :: gp :: HNil =>
        // pprint.log(qp)
        goal =>
          Future(terms.find(_.typ == goal.goal)).flatMap(
            tOpt =>
              if (tOpt.nonEmpty)
                Future.successful(
                  Some(
                    GeneratedEquationNodes(Set()) :: Right(
                      Proved(goal.goal, tOpt, goal.context)
                    ) :: HNil
                  )
                )
              else if (goal.relevantGiven(terms, gp.contents)) {
                val lpVars   = qp.lp.initState.context.variables
                val goalVars = goal.context.variables
                val newVars  = goalVars.drop(lpVars.size)
                val withVars = newVars.foldLeft(qp.lp) {
                  case (lp: LocalProver, x: Term) =>
                    lp.addVar(x, varWeight)
                }
                Utils.logger.info(s"initial state ${withVars.initState}")
                withVars
                  .varDistEqs(TermRandomVars.termsWithTyp(goal.goal))
                  .runToFuture
                  .map {
                    case (fd, eqs) =>
                      val initTerms = withVars.initState.terms.support union (withVars.initState.typs.support
                        .map(x => x: Term))
                      val expEqs =
                        EquationExporter.export(eqs, initTerms, newVars)
                      val geqs = GeneratedEquationNodes(expEqs)
                      Some(geqs :: {
                        if (fd.pmf.isEmpty) {
                          Utils.logger.info(
                            s"""|Failed for 
                                            |${TermRandomVars
                                 .termsWithTyp(goal.goal)}
                                             |initial state: ${withVars.initState}
                                             |cutoff: ${withVars.cutoff}
                                             |parameters : ${withVars.tg}""".stripMargin
                          )
                          Left(
                            FailedToProve(
                              goal.goal,
                              goal.context,
                              goal.forConsequences
                            )
                          )
                        } else {
                          val best = fd.pmf.maxBy(_.weight)
                          Right(
                            Proved(goal.goal, Some(best.elem), goal.context)
                          )
                        }
                      } :: HNil)
                  }
              } else Future.successful(None)
          )
    }

    MicroBot(
      response,
      subContext,
      name = Some("attempting goal with equations")
    )
  }

  def decided(web: HoTTPostWeb): Future[Set[HoTTMessages.Decided]] =
    implicitly[Queryable[GatherMapPost[Decided], HoTTPostWeb]]
      .get(web, (_) => true)
      .map { gp =>
        gp.contents
      }

  def topLevelGoals(web: HoTTPostWeb, context: Context): Future[Set[SeekGoal]] =
    implicitly[Queryable[GatherPost[SeekGoal], HoTTPostWeb]]
      .get(web, (_) => true)
      .map { gp =>
        gp.contents.filter(
          goal => goal.forConsequences.isEmpty && goal.context == context
        )
      }

  def topLevelRelevantGoals(
      web: HoTTPostWeb,
      context: Context
  ): Future[Set[SeekGoal]] =
    for {
      prior <- topLevelGoals(web, context)
      dec   <- decided(web)
    } yield {
      val terms = web.terms
      prior.filter(x => x.relevantGiven(terms, dec))
    }

  def topLevelRelevantGoalsBot[P](
      haltIfEmpty: Boolean = false,
      context: Context = Context.Empty
  )(
      implicit pp: Postable[P, HoTTPostWeb, ID]
  ): Callback[P, HoTTPostWeb, Unit, ID] = {
    val response: HoTTPostWeb => Unit => P => Future[Unit] =
      (web) =>
        (_) =>
          (_) =>
            topLevelRelevantGoals(web, context).map { goals =>
              Utils.logger.info(s"remaining top level goals: ${goals.size}")
              Utils.logger.info(goals.mkString("\n"))
              if (haltIfEmpty && goals.isEmpty) {
                web.halt()
                Utils.running = false
              }
            }
    Callback(response, name = Some("remaining top level goals"))
  }

  def repost[P: TypeTag](
      implicit pw: Postable[P, HoTTPostWeb, ID]
  ): MicroHoTTBoTT[Cap.type, P, P] = {
    MicroBot((p: P) => (_) => Future(p))
  }

  def repostGoals(lp: LocalProver, context: Context = Context.Empty): MicroBot[
    Cap.type,
    Option[LocalProver :: FinalState :: ChompResult :: HNil],
    HoTTPostWeb,
    ChompResult :: GatherPost[
      Proved
    ] :: GatherPost[FinalState] :: HNil,
    ID
  ] = {
    val response: ChompResult :: GatherPost[
      Proved
    ] :: GatherPost[FinalState] :: HNil => Cap.type => Future[
      Option[LocalProver :: FinalState :: ChompResult :: HNil]
    ] = {
      case cr :: gprvd :: cfs :: _ =>
        (_) =>
          val newProofs = gprvd.contents.filter { pr =>
            pr.context == context && cr.failures.contains(pr.statement)
          }.toVector
          Utils.logger.info(
            s"New top-level proofs obtained: ${newProofs.mkString("\n", "\n", "\n")}"
          )
          val failures =
            cr.failures.filterNot(newProofs.map(_.statement).contains(_))
          if (failures.isEmpty) Future(None)
          else
            Future(Some {
              val fs = cfs.contents.head
              Utils.logger.info(s"reposting because of failures: ${failures.mkString("; ")}")
              lp :: fs :: ChompResult(
                cr.successes ++ newProofs.map(
                  pr =>
                    (
                      pr.statement,
                      0.0,
                      pr.proofOpt.getOrElse("proved" :: pr.statement)
                    )
                ),
                failures,
                Set()
              ) :: HNil
            })
    }

    MicroBot(response, name = Some("reposting goals"))
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
    bots: Vector[HoTTBot] = HoTTWebSession.initBots,
    completionResponse: Option[
      HoTTPostWeb => Future[PostData[_, HoTTPostWeb, ID]]
    ]
) extends SimpleSession[HoTTPostWeb, (Int, Int)](
      initialWeb,
      bots,
      Vector(
        // scribeLog(_)
      ),
      completionResponse
    ) {
  override def running = initialWeb.running

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
      bots: Vector[HoTTBot] = initBots,
      completion: Option[
        HoTTPostWeb => Future[PostData[_, HoTTPostWeb, (Int, Int)]]
      ] = None
  ) = {
    val session = new HoTTWebSession(state.web, bots, completion)
    state.apexPosts.foreach {
      case pd: PostData[x, HoTTPostWeb, (Int, Int)] =>
        session.respond(pd.content, pd.id)(pd.pw)
    }
    session
  }
}
