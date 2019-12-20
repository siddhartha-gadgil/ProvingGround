package provingground.learning

import provingground._, HoTT._
import TypedPostResponse._
import monix.eval._
import HoTTPost._
import monix.execution.Scheduler.Implicits.{global => monixglobal}

class HoTTPost { web =>
  val global = new CounterGlobalID()

  var equationNodes: Set[EquationNode] = Set()

  def equations = Equation.group(equationNodes)

  def addEqns(eqs: Set[EquationNode]): Unit = {
    equationNodes ++= eqs
  }

  val lpBuff = PostBuffer[LocalProver, ID](global.postGlobal)

  val expEvalBuff = PostBuffer[ExpressionEval, ID](global.postGlobal)

  val eqnNodeBuff = PostBuffer[Set[EquationNode], ID](global.postGlobal)

  def postLocalProverTask(
      lp: LocalProver,
      pred: Set[ID] = Set()
  ): Task[PostData[LocalProver, HoTTPost, HoTTPost.ID]] =
    Postable.postTask(lp, web, pred)

  def postLP(
      lp: LocalProver,
      pred: Set[ID] = Set()
  ): PostData[LocalProver, HoTTPost, HoTTPost.ID] =
    postLocalProverTask(lp, pred).runSyncUnsafe()
}

object HoTTPost {
  type ID = (Int, Int)

  implicit val postLP: Postable[LocalProver, HoTTPost, ID] = {
    def postFunc(lp: LocalProver, web: HoTTPost, ids: Set[ID]): Task[ID] =
      web.lpBuff.post(lp, ids)
    Postable(postFunc, true)
  }

  implicit val postExpEv: Postable[ExpressionEval, HoTTPost, ID] = {
    def postFunc(ev: ExpressionEval, web: HoTTPost, ids: Set[ID]): Task[ID] =
      web.expEvalBuff.post(ev, ids)
    Postable(postFunc, false)
  }

  implicit val postEqnNodes: Postable[Set[EquationNode], HoTTPost, ID] = {
    def postFunc(
        eqns: Set[EquationNode],
        web: HoTTPost,
        ids: Set[ID]
    ): Task[ID] = web.eqnNodeBuff.post(eqns, ids)
    Postable(postFunc, false)
  }

  lazy val lpToExpEv: PostResponse[HoTTPost, ID] = {
    val response: Unit => LocalProver => Task[ExpressionEval] = (_) =>
      lp => lp.expressionEval
    Poster(response)
  }

  lazy val expEvToEqns: PostResponse[HoTTPost, ID] = {
    val response: Unit => ExpressionEval => Task[Set[EquationNode]] = (_) =>
      (expEv) => Task(expEv.equations.flatMap(Equation.split))
    Poster(response)
  }

  lazy val eqnUpdate: PostResponse[HoTTPost, ID] = {
    val update: HoTTPost => Unit => Set[EquationNode] => Task[Unit] = web =>
      (_) => eqns => Task(web.addEqns(eqns))
    Callback(update)
  }

}

class HoTTSession
    extends SimpleSession(
      new HoTTPost(),
      Vector(lpToExpEv, expEvToEqns, eqnUpdate)
    )
