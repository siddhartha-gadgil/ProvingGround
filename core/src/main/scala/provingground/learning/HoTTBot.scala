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
    val response
        : SeekInstances[_, _] => Instance[_] => Future[Option[Consequence :: SeekGoal :: HNil]] = {
      case seek: SeekInstances[a, b] => {
        case instance: Instance[c] =>
          Future(
            if (instance.typ == seek.typ)
              { val newGoal = (seek: SeekInstances[a, b])
                    .goal(instance.term.asInstanceOf[a])
                val deduction : Term => Term = (x) => mkPair(instance.term.asInstanceOf[Term], x : Term)
                val cons = Consequence(newGoal, seek.sigma, Option(deduction))
                Some(
                  cons:: 
                SeekGoal(
                  newGoal,
                  seek.forConsequences + seek.sigma
                ) :: HNil
              )
              }
            else None
          )
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

  def fansiLog(post: PostData[_, HoTTPostWeb, ID]): Future[Unit] =
    Future {
      translation.FansiShow.fansiPrint.log(post.pw.tag)
      translation.FansiShow.fansiPrint.log(post.content, height = 20)
      pprint.log(post.id)
    }

  case class QueryProver(lp: LocalProver)

  object QueryProver {
    implicit val qc =
      QueryFromPosts
        .empty[QueryProver]
        .addCons((lp: LocalProver) => Some(QueryProver(lp)))
        .addMod((w: Weight) => qp => QueryProver(qp.lp.sharpen(w.scale)))
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
