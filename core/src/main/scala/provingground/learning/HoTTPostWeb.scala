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

/**
  * Better building of post webs, without depending on separate lists for implicits and history (history to be done).
  */
class HoTTPostWeb {
  import HoTTPostWeb._

  implicit val global: GlobalID[ID] = new CounterGlobalID()

  import global.postGlobal

  var equationNodes: Set[EquationNode] = Set()

  def equations = Equation.group(equationNodes)

  def terms = ExpressionEval.terms(equationNodes)

  def addEqns(eqs: Set[EquationNode]): Unit = {
    equationNodes ++= eqs
  }

  val polyBuffer =
    PostBuffer.build[Lemmas, ID] :: PostBuffer
      .build[InitState, ID] ::
      ErasablePostBuffer.build[FinalState, ID] ::
      ErasablePostBuffer.build[TermResult, ID] ::
      ErasablePostBuffer.build[GeneratedEquationNodes, ID] ::
      PostBuffer.build[LocalTangentProver, ID] ::
      PostBuffer.build[ExpressionEval, ID] ::
      PostBuffer.build[ChompResult, ID] ::
      PostBuffer.build[LocalProver, ID] :: HNil
}

object HoTTPostWeb {
  type ID = (Int, Int)

  val polyImpl                                                        = BuildPostable.get((w: HoTTPostWeb) => w.polyBuffer)
  implicit val (b9 :: b8 :: b7 :: b6 :: b5 :: b4 :: b3 :: b2 :: b1 :: HNil) = polyImpl

  implicit val history: PostHistory[HoTTPostWeb, ID] =
    HistoryGetter.get((w: HoTTPostWeb) => w.polyBuffer)

  implicit def equationNodeQuery: Queryable[Set[EquationNode], HoTTPostWeb] =
    Queryable.simple(_.equationNodes)

  implicit def equationQuery: Queryable[Set[Equation], HoTTPostWeb] =
    Queryable.simple(_.equations)

  implicit def termSetQuery: Queryable[Set[Term], HoTTPostWeb] =
    Queryable.simple(_.terms)

  lazy val lpToExpEv: PostResponse[HoTTPostWeb, ID] = {
    val response: Unit => LocalProver => Future[ExpressionEval] = (_) =>
      lp => lp.expressionEval.runToFuture
    MicroBot(response)
  }

  lazy val lptToExpEv: PostResponse[HoTTPostWeb, ID] = {
    val response: Unit => LocalTangentProver => Future[ExpressionEval] = (_) =>
      lp => lp.expressionEval.runToFuture
    MicroBot(response)
  }

  lazy val lpToTermResult: PostResponse[HoTTPostWeb, ID] = {
    val response: Unit => LocalProver => Future[TermResult] = (_) =>
      lp => termData(lp).runToFuture
    MicroBot(response)
  }

  lazy val lptToTermResult: PostResponse[HoTTPostWeb, ID] = {
    val response: Unit => LocalTangentProver => Future[TermResult] = (_) =>
      lp => termData(lp).runToFuture
    MicroBot(response)
  }

  lazy val termResultToEquations: PostResponse[HoTTPostWeb, ID] =
    MicroBot.simple(
      (pair: TermResult) => GeneratedEquationNodes(pair._2)
    )

  lazy val expEvToEqns: PostResponse[HoTTPostWeb, ID] =
    MicroBot.simple(
      (ev: ExpressionEval) => GeneratedEquationNodes(ev.equations.flatMap(Equation.split))
    )

  lazy val eqnUpdate: PostResponse[HoTTPostWeb, ID] =
    Callback.simple(
      (web: HoTTPostWeb) => (eqs: GeneratedEquationNodes) => web.addEqns(eqs.eqn)
    )

  lazy val termResultToChomp: PostResponse[HoTTPostWeb, ID] = {
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

  def fansiLog(post: PostData[_, HoTTPostWeb, ID]): Future[Unit] =
    Future {
      translation.FansiShow.fansiPrint.log(post.pw.tag)
      translation.FansiShow.fansiPrint.log(post.content, height = 20)
      pprint.log(post.id)
    }

}

import HoTTPostWeb._
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
