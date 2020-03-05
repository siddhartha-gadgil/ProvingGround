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

  lazy val expEvToEqns: HoTTBot =
    MicroBot.simple(
      (ev: ExpressionEval) =>
        GeneratedEquationNodes(ev.equations.flatMap(Equation.split))
    )

  lazy val instanceToGoal: HoTTBot = {
    val response: SeekInstances[_, _] => Instance[_] => Future[
      Option[Consequence :: SeekGoal :: HNil]
    ] = {
      case seek: SeekInstances[a, b] => {
        case instance: Instance[c] =>
          Future(
            if (instance.typ == seek.typ) {
              val newGoal = (seek: SeekInstances[a, b])
                .goal(instance.term.asInstanceOf[a])
              val deduction: Term => Term =
                (x) => mkPair(instance.term.asInstanceOf[Term], x: Term)
              val cons = Consequence(newGoal, seek.sigma, Option(deduction))
              Some(
                cons ::
                  SeekGoal(
                    newGoal,
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
                val cons      = Consequence(sk, goal.goal, Option(transform))
                cons :: SeekGoal(
                  sk,
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

  lazy val deducedEquations: HoTTBot = {
    val response
        : GatherMapPost[PropagateProof] :: GatherMapPost[Decided] :: HNil => Proved => Future[
          Vector[Either[Contradicted, Proved]]
        ] = {
      case gprop :: gdec :: HNil =>
        proved =>
          Future {
            derivedProofs(gprop.contents, gdec.contents)
              .map(Decided.asEither(_))
              .toVector
          }
    }

    new MiniBot[
      Proved,
      Either[Contradicted, Proved],
      HoTTPostWeb,
      GatherMapPost[PropagateProof] :: GatherMapPost[Decided] :: HNil,
      ID
    ](
      response,
      (_) => true
    )

  }

  lazy val lpLemmas: HoTTBot = {
    val response: Unit => LocalProver => Future[Lemmas] =
      (_) =>
        (lp) =>
          lp.lemmas.runToFuture
            .map(v => Lemmas(v.map(xy => (xy._1, None, xy._2))))
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
                    val weights = supp.flatMap(typ => lemmas.lemmas.find(_._1 == typ).map(_._3))
                    val prodWeight = weights.fold(1.0)(_ * _)
                    supp.map { typ =>
                      Weighted(typ, prodWeight)
                    }
                  }
                }
                val totalWeight = fdsUnscaled.map(_.total).sum
                val fds = fdsUnscaled.map(_ * (w/totalWeight))
                fds.map(fd => UseLemmaDistribution(fd, None))
            }).toVector
          }

    MiniBot[Lemmas, UseLemmaDistribution, HoTTPostWeb, Unit, ID](response)
  }

  lazy val backwardFunction: HoTTBot = {
    val response: QueryProver => SeekGoal => Future[Vector[WithWeight[FunctionForGoal]]] = 
      qp =>
        goal =>
          {
            import TermGeneratorNodes._
            qp.lp.nodeDist(codomainNode(goal.goal)).map{
              fd : FiniteDistribution[Term] => 
                fd.pmf.map{case Weighted(x, p) => withWeight(FunctionForGoal(x, goal.goal, goal.forConsequences), p)}
            }.runToFuture
          }

    MiniBot[SeekGoal, WithWeight[FunctionForGoal], HoTTPostWeb, QueryProver, ID](response)
  }

  lazy val inductionBackward: HoTTBot = {
    val response: QueryProver => SeekGoal => Future[Vector[WithWeight[FunctionForGoal]]] = 
      qp =>
        goal =>
          {
            import TermGeneratorNodes._
            qp.lp.nodeDist(qp.lp.tg.Gen.targetInducBackNode(goal.goal)).map{
              fd : FiniteDistribution[Term] => 
                fd.pmf.map{case Weighted(x, p) => withWeight(FunctionForGoal(x, goal.goal, goal.forConsequences), p)}
            }.runToFuture
          }

    MiniBot[SeekGoal, WithWeight[FunctionForGoal], HoTTPostWeb, QueryProver, ID](response)
  }

  lazy val funcToFromAll: HoTTBot = {
    MicroBot.simple{
      (fng: FunctionForGoal) => FromAll.get(fng.fn, fng.goal, fng.forConsequences)
    }
  }

  lazy val resolveFromAll: HoTTBot = {
    val response: Unit => FromAll => Future[Vector[SeekGoal]] = 
      (_) =>
        (fromAll) =>
          Future{
            fromAll.typs.map(typ => SeekGoal(typ, fromAll.forConsequences))
          }

    MiniBot[FromAll, SeekGoal, HoTTPostWeb, Unit, ID](response)
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

  def fansiLog(post: PostData[_, HoTTPostWeb, ID]): Future[Unit] =
    Future {
      translation.FansiShow.fansiPrint.log(post.pw.tag)
      translation.FansiShow.fansiPrint.log(post.content, height = 20)
      pprint.log(post.id)
    }

  val wrapTest = implicitly[LocalQueryable[QueryProver, HoTTPostWeb, ID]] // a test

}

import HoTTBot._
class HoTTWebSession
    extends SimpleSession[HoTTPostWeb, (Int, Int)](
      new HoTTPostWeb(),
      Vector(lpToExpEv, expEvToEqns, eqnUpdate),
      Vector(fansiLog(_))
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
