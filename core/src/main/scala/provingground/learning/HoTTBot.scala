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

case class QueryProver(lp: LocalProver)

object QueryProver {
  implicit val qc =
    QueryFromPosts
      .empty[QueryProver]
      .addCons((lp: LocalProver) => Some(QueryProver(lp)))
      .addMod((w: Weight) => qp => QueryProver(qp.lp.sharpen(w.scale)))
}

object HoTTBot {

  type ID = HoTTPostWeb.ID

  type HoTTBot = PostResponse[HoTTPostWeb, ID]

  lazy val lpToExpEv: HoTTBot = {
    val response: Unit => LocalProver => Future[ExpressionEval] = (_) =>
      lp => lp.expressionEval.runToFuture
    MicroBot(response)
  }

  lazy val lptToExpEv: HoTTBot = {
    val response: Unit => LocalTangentProver => Future[ExpressionEval] = (_) =>
      lp => lp.expressionEval.runToFuture
    MicroBot(response)
  }

  lazy val lpToTermResult: HoTTBot = {
    val response: Unit => LocalProver => Future[TermResult] = (_) =>
      lp => termData(lp).runToFuture
    MicroBot(response)
  }

  lazy val lptToTermResult: HoTTBot = {
    val response: Unit => LocalTangentProver => Future[TermResult] = (_) =>
      lp => termData(lp).runToFuture
    MicroBot(response)
  }

  lazy val termResultToEquations: HoTTBot =
    MicroBot.simple(
      (pair: TermResult) => GeneratedEquationNodes(pair._2)
    )

  lazy val termResultToFinalState: HoTTBot =
    MicroBot.simple(
      (pair: TermResult) => FinalState(pair._1)
    )

  lazy val expEvToEqns: HoTTBot =
    MicroBot.simple(
      (ev: ExpressionEval) =>
        GeneratedEquationNodes(ev.equations.flatMap(Equation.split))
    )

  lazy val instanceToGoal: HoTTBot = {
    val response: SeekInstances => Instance => Future[
      Option[Consequence :: SeekGoal :: HNil]
    ] = {
      case seek: SeekInstances => {
        case instance: Instance =>
          Future(
            if (instance.typ == seek.typ) {
              val newGoal = (seek: SeekInstances)
                .goalCast(instance.term)
              val deduction: Term => Term =
                (x) => mkPair(instance.term.asInstanceOf[Term], x: Term)
              val cons = Consequence(
                newGoal,
                seek.sigma,
                Option(deduction),
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

  lazy val skolemBot: HoTTBot = {
    val response
        : Unit => SeekGoal => Future[Option[Consequence :: SeekGoal :: HNil]] =
      (_) =>
        (goal) =>
          Future {
            val sk = skolemize(goal.goal)
            if (sk == goal.goal) None
            else
              Some {
                val transform = fromSkolemized(sk) _
                val cons =
                  Consequence(sk, goal.goal, Option(transform), goal.context)
                cons :: SeekGoal(
                  sk,
                  goal.context,
                  goal.forConsequences + goal.goal
                ) :: HNil
              }
          }

    MicroBot(response)
  }

  lazy val eqnUpdate: HoTTBot =
    Callback.simple(
      (web: HoTTPostWeb) =>
        (eqs: GeneratedEquationNodes) => web.addEqns(eqs.eqn)
    )

  lazy val termResultToChomp: HoTTBot = {
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

  val goalsAfterChomp: HoTTBot = {
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

  lazy val deducedEquations: HoTTBot = {
    val response
        : GatherMapPost[PropagateProof] :: GatherMapPost[Decided] :: Set[
          Term
        ] :: HNil => Proved => Future[
          Set[Either[Contradicted, Proved]]
        ] = {
      case gprop :: gdec :: terms :: HNil =>
        proved =>
          Future {
            derivedProofs(gprop.contents, gdec.contents union terms.map { t =>
              Proved(t.typ, Some(t), Context.Empty)
            }).map(Decided.asEither(_))
          }
    }

    MicroBot(response)
  }

  lazy val termsFromProofs: HoTTBot =
    Callback.simple(
      (web: HoTTPostWeb) =>
        (pf: Proved) => pf.proofOpt.foreach(t => web.addTerms(Set(t)))
    )

  lazy val lpLemmas: HoTTBot = {
    val response: Unit => LocalProver => Future[Lemmas] =
      (_) =>
        (lp) =>
          lp.lemmas.runToFuture
            .map(v => Lemmas(v.map(xy => (xy._1, None, xy._2))))
    MicroBot(response)
  }

  def lpOptimalInit(decay: Double): HoTTBot = {
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

  lazy val narrowOptimalInit: HoTTBot = {
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

  lazy val lptLemmas: HoTTBot = {
    val response: Unit => LocalTangentProver => Future[Lemmas] =
      (_) =>
        (lp) =>
          lp.lemmas.runToFuture
            .map(v => Lemmas(v.map(xy => (xy._1, None, xy._2))))
    MicroBot(response)
  }

  lazy val splitLemmas: HoTTBot = {
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

  def lemmaDistributions(sizes: Map[Int, Double]) = {
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

  lazy val backwardFunction: HoTTBot = {
    val response: QueryProver => SeekGoal => Future[
      Vector[WithWeight[FunctionForGoal]]
    ] =
      qp =>
        goal => {
          import TermGeneratorNodes._
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

  lazy val instanceFromLp: HoTTBot = {
    val response: QueryProver => SeekInstances => Future[
      Vector[WithWeight[Instance]]
    ] =
      qp =>
        seekInst => {
          import TermGeneratorNodes._
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

  lazy val inductionBackward: HoTTBot = {
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

  lazy val addInductive: HoTTBot = {
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

  lazy val funcToFromAll: HoTTBot = {
    MicroBot.simple { (fng: FunctionForGoal) =>
      FromAll.get(fng.fn, fng.goal, fng.forConsequences, fng.context)
    }
  }

  lazy val resolveFromAll: HoTTBot = {
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

  lazy val resolveFromAny: HoTTBot = {
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

  lazy val productBackward: HoTTBot = {
    MicroBot.simple(
      (sk: SeekGoal) =>
        sk.goal match {
          case pd: ProdTyp[u, v] =>
            Some(
              FromAll(
                Vector(pd.first, pd.second),
                sk.goal, {
                  case Vector(x, y) =>
                    Some(pd.paircons(x.asInstanceOf[u])(y.asInstanceOf[v]))
                  case _ => None
                },
                sk.context,
                sk.forConsequences
              )
            )
          case _ => None
        }
    )
  }

  lazy val coproductBackward: HoTTBot = {
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
                  (x: Term) => Some(pt.incl1(x.asInstanceOf[u])),
                  (x: Term) => Some(pt.incl2(x.asInstanceOf[v]))
                ),
                sk.context,
                sk.forConsequences
              )
            )
          case _ => None
        }
    )
  }

  lazy val sigmaBackward: HoTTBot = {
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

  def lemmaTangents(tangentScale: Double = 1.0): HoTTBot = {
    val response: QueryProver => UseLemma => Future[LocalTangentProver] =
      qp =>
        lem => qp.lp.sharpen(tangentScale).tangentProver(lem.proof).runToFuture

    MicroBot(response)
  }

  def lemmaMixin(weight: Double = 0.3): HoTTBot = {
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

  def goalToProver(varWeight: Double, goalWeight: Double): HoTTBot = {
    val subContext: SeekGoal => QueryProver :: Set[Term] :: HNil => Boolean =
      (goal) => {
        case (qp: QueryProver) :: terms :: HNil => {
          val lpVars   = qp.lp.initState.context.variables
          val goalVars = goal.context.variables
          lpVars == goalVars.take(lpVars.size)
        }
      }

    val response: QueryProver :: Set[Term] :: HNil => SeekGoal => Future[
      Option[LocalProver]
    ] = {
      case (qp: QueryProver) :: terms :: HNil =>
        goal =>
          Future {
            if (goal.relevantGiven(terms)) {
              val lpVars   = qp.lp.initState.context.variables
              val goalVars = goal.context.variables
              val newVars  = goalVars.drop(lpVars.size)
              val withVars = newVars.foldLeft(qp.lp) {
                case (lp: LocalProver, x: Term) => lp.addVar(x, varWeight)
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

    import Utils.logger

  def scribeLog(post: PostData[_, HoTTPostWeb, ID]): Future[Unit] = Future {
    logger.info(s"posted ${post.pw.tag.tpe}")
    logger.info(post.id.toString)
    logger.debug(post.content.toString)
  }
}

import HoTTBot._
class HoTTWebSession
    extends SimpleSession[HoTTPostWeb, (Int, Int)](
      new HoTTPostWeb(),
      Vector(lpToExpEv, expEvToEqns, eqnUpdate),
      Vector(scribeLog(_))
    ) {

  // just an illustration, should just use rhs
  def postLocalProverFuture(
      lp: LocalProver,
      pred: Set[ID] = Set()
  ): Future[PostData[LocalProver, HoTTPostWeb, HoTTPostWeb.ID]] =
    postFuture(lp, pred)

  def postLP(
      lp: LocalProver,
      pred: Set[ID] = Set()
  ): Future[PostData[LocalProver, HoTTPostWeb, HoTTPostWeb.ID]] =
    postLocalProverFuture(lp, pred)
}
