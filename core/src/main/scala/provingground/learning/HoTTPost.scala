package provingground.learning

import provingground._, HoTT._
import TypedPostResponse._
import monix.eval._
import HoTTPost._, LocalQueryable._
import monix.execution.Scheduler.Implicits.{global => monixglobal}
import scala.concurrent._

class HoTTPost { web =>
  val global = new CounterGlobalID()

  import global.postGlobal

  var equationNodes: Set[EquationNode] = Set()

  def equations = Equation.group(equationNodes)

  def addEqns(eqs: Set[EquationNode]): Unit = {
    equationNodes ++= eqs
  }

  val tgBuff = PostBuffer[TermGenParams, ID](postGlobal)

  val initStateBuff = PostBuffer[InitState, ID](postGlobal)

  val finalStateBuff = PostBuffer[FinalState, ID](postGlobal)

  val lpBuff = PostBuffer[LocalProver, ID](postGlobal)

  val lptBuff = PostBuffer[LocalTangentProver, ID](postGlobal)

  val expEvalBuff = PostBuffer[ExpressionEval, ID](postGlobal)

  val eqnNodeBuff = PostBuffer[Set[EquationNode], ID](postGlobal)

  val tunedLpBuff = PostBuffer[TunedLocalProver, ID](postGlobal)

  val genEqnBuff = PostBuffer[GeneratedEquationNodes, ID](postGlobal)

  val isleNormEqnBuff = PostBuffer[IsleNormalizedEquationNodes, ID](postGlobal)

  val lemmaBuffer = PostBuffer[Lemmas, ID](postGlobal)

  val buffers: Vector[PostBuffer[_, ID]] =
    Vector(
      tgBuff,
      lpBuff,
      eqnNodeBuff,
      expEvalBuff,
      lptBuff,
      initStateBuff,
      finalStateBuff,
      tunedLpBuff,
      genEqnBuff,
      isleNormEqnBuff,
      lemmaBuffer
    )

}

object HoTTPost {
  type ID = (Int, Int)

  case class InitState(ts: TermState, weight: Double)

  case class FinalState(ts: TermState, weight: Double) // should also record source, whether by evolution or from equations etc

  case class TunedLocalProver(lp: LocalProver)

  case class GeneratedEquationNodes(eqn: Set[EquationNode])

  case class IsleNormalizedEquationNodes(eqn: Set[EquationNode])

  case class Lemmas(lemmas: Vector[(Typ[Term], Double)])

  import PostBuffer.bufferPost

  implicit val postLP: Postable[LocalProver, HoTTPost, ID] =
    bufferPost(_.lpBuff, true)

  implicit val postLPT: Postable[LocalTangentProver, HoTTPost, ID] =
    bufferPost(_.lptBuff, true)

  implicit val postExpEv: Postable[ExpressionEval, HoTTPost, ID] = bufferPost(
    _.expEvalBuff
  )

  implicit val postEqnNodes: Postable[Set[EquationNode], HoTTPost, ID] =
    bufferPost(_.eqnNodeBuff)

  implicit val postTg: Postable[TermGenParams, HoTTPost, ID] = bufferPost(
    _.tgBuff
  )

  implicit val postInit: Postable[InitState, HoTTPost, ID] = bufferPost(
    _.initStateBuff
  )

  implicit val postFinal: Postable[FinalState, HoTTPost, ID] = bufferPost(
    _.finalStateBuff
  )

  implicit val postTunedLP: Postable[TunedLocalProver, HoTTPost, ID] =
    bufferPost(_.tunedLpBuff)

  implicit val postGenEqns: Postable[GeneratedEquationNodes, HoTTPost, ID] =
    bufferPost(_.genEqnBuff)

  implicit val postIslNrmEqns
      : Postable[IsleNormalizedEquationNodes, HoTTPost, ID] =
    bufferPost(_.isleNormEqnBuff)

  implicit val postLemmas: Postable[Lemmas, HoTTPost, ID] =
    bufferPost(_.lemmaBuffer)

  case class WebBuffer[P](buffer: PostBuffer[P, ID])(
      implicit pw: Postable[P, HoTTPost, ID]
  ) {
    def getPost(id: ID): Option[(PostData[_, HoTTPost, ID], Set[ID])] =
      buffer.find(id)

    def data: Vector[PostData[_, HoTTPost, ID]] = buffer.bufferData

    def fullData: Vector[(PostData[_, HoTTPost, ID], ID, Set[ID])] =
      buffer.bufferFullData
  }

  def webBuffers(web: HoTTPost): Vector[WebBuffer[_]] =
    Vector() :+ WebBuffer(web.lpBuff) :+ WebBuffer(web.expEvalBuff) :+ WebBuffer(
      web.eqnNodeBuff
    )

  def findInWeb(
      web: HoTTPost,
      index: ID
  ): Option[(PostData[_, HoTTPost, ID], Set[ID])] =
    webBuffers(web)
      .map(_.getPost(index))
      .fold[Option[(PostData[_, HoTTPost, ID], Set[ID])]](None)(_ orElse _)

  implicit def postHistory: PostHistory[HoTTPost, ID] =
    new PostHistory[HoTTPost, ID] {
      def findPost(
          web: HoTTPost,
          index: ID
      ): Option[(PostData[_, HoTTPost, ID], Set[ID])] = findInWeb(web, index)
    }

  def allPosts(web: HoTTPost): Vector[PostData[_, HoTTPost, ID]] =
    webBuffers(web).flatMap(_.data)

  def allPostFullData(
      web: HoTTPost
  ): Vector[(PostData[_, HoTTPost, ID], ID, Set[ID])] =
    webBuffers(web).flatMap(_.fullData)

  case class HoTTPostData(
      number: Int,
      posts: Vector[(PostData[_, HoTTPost, ID], ID, Set[ID])]
  ) {
    def successors(id: ID) = posts.filter(_._3.contains(id))

    val allIndices: Vector[ID] = posts.map(_._2)

    lazy val leafIndices: Vector[ID] =
      allIndices.filter(id => successors(id).isEmpty)

    def filterMap[P, U](
        func: P => U
    )(implicit pw: Postable[P, HoTTPost, ID]): Vector[U] =
      posts.map {
        case (pd: PostData[q, HoTTPost, ID], _, _) =>
          AnswerFromPost[P, U, HoTTPost, ID](func).fromPost(pd)
      }.flatten
  }

  implicit def hottPostDataQuery: Queryable[HoTTPostData, HoTTPost] =
    new Queryable[HoTTPostData, HoTTPost] {
      def get(
          web: HoTTPost,
          predicate: HoTTPostData => Boolean
      ): Future[HoTTPostData] = Future {
        HoTTPostData(
          web.global.counter,
          allPostFullData(web)
        )
      }
    }

  implicit def equationNodeQuery: Queryable[Set[EquationNode], HoTTPost] =
    Queryable.simple(_.equationNodes)

  implicit def equationQuery: Queryable[Set[Equation], HoTTPost] =
    Queryable.simple(_.equations)

  case class Apex[P](base: P)

  implicit def postToLeaves[P](
      implicit bp: Postable[P, HoTTPost, ID]
  ): Postable[Apex[P], HoTTPost, ID] =
    new Postable[Apex[P], HoTTPost, ID] {

      def post(content: Apex[P], web: HoTTPost, pred: Set[ID]): Future[ID] = {
        val dataFuture = query[HoTTPostData, HoTTPost](web, (_) => true)
        for {
          data <- dataFuture
          leaves = data.leafIndices.toSet
          postData <- Postable.postFuture(content.base, web, pred union leaves)
        } yield postData.id
      }

      val contextChange: Boolean = bp.contextChange
    }

  lazy val lpToExpEv: PostResponse[HoTTPost, ID] = {
    val response: Unit => LocalProver => Future[ExpressionEval] = (_) =>
      lp => lp.expressionEval.runToFuture
    Poster(response)
  }

  lazy val lptToExpEv: PostResponse[HoTTPost, ID] = {
    val response: Unit => LocalTangentProver => Future[ExpressionEval] = (_) =>
      lp => lp.expressionEval.runToFuture
    Poster(response)
  }

  lazy val expEvToEqns: PostResponse[HoTTPost, ID] =
    Poster.simple(
      (ev: ExpressionEval) => ev.equations.flatMap(Equation.split)
    )

  lazy val eqnUpdate: PostResponse[HoTTPost, ID] =
    Callback.simple(
      (web: HoTTPost) => (eqs: Set[EquationNode]) => web.addEqns(eqs)
    )

  lazy val lpFromInit: PostResponse[HoTTPost, ID] = {
    val response: TermGenParams => InitState => Future[LocalProver] =
      (tg) => (init) => Future(LocalProver(init.ts, tg).sharpen(init.weight))
    Poster(response)
  }

  lazy val lpFromTG: PostResponse[HoTTPost, ID] = {
    val response: InitState => TermGenParams => Future[LocalProver] =
      (init) => (tg) => Future(LocalProver(init.ts, tg).sharpen(init.weight))
    Poster(response)
  }

  lazy val tuneLP: PostResponse[HoTTPost, ID] = {
    val response: Unit => LocalProver => Future[TunedLocalProver] =
      (_) => (lp) => lp.tunedInit.runToFuture.map(TunedLocalProver(_))
    Poster(response)
  }

  lazy val isleNormalizeEqns: PostResponse[HoTTPost, ID] =
    Poster.simple(
      (ge: GeneratedEquationNodes) =>
        IsleNormalizedEquationNodes(
          ge.eqn.map(eq => TermData.isleNormalize(eq))
        )
    )

  lazy val lpLemmas: PostResponse[HoTTPost, ID] = {
    val response: Unit => LocalProver => Future[Lemmas] =
      (_) => (lp) => lp.lemmas.runToFuture.map(Lemmas(_))
    Poster(response)
  }

  lazy val lptLemmas: PostResponse[HoTTPost, ID] = {
    val response: Unit => LocalTangentProver => Future[Lemmas] =
      (_) => (lp) => lp.lemmas.runToFuture.map(Lemmas(_))
    Poster(response)
  }

  lazy val lemmaToTangentProver: PostResponse[HoTTPost, ID] = {
    val response: LocalProver => Lemmas => Future[Vector[LocalTangentProver]] =
      (lp) =>
        (lm) =>
          Future.sequence(lm.lemmas.map {
            case (tp, w) =>
              lp.tangentProver("lemma" :: tp).map(_.sharpen(w)).runToFuture : Future[LocalTangentProver]
          })
    new VectorPoster[Lemmas, LocalTangentProver, HoTTPost, LocalProver, ID](
      response,
      (_) => true
    )
  }

}

class HoTTSession
    extends SimpleSession(
      new HoTTPost(),
      Vector(lpToExpEv, expEvToEqns, eqnUpdate)
    ) {
  // just an illustration, should just use rhs
  def postLocalProverFuture(
      lp: LocalProver,
      pred: Set[ID] = Set()
  ): Future[PostData[LocalProver, HoTTPost, HoTTPost.ID]] =
    postFuture(lp, pred)

  def postLP(
      lp: LocalProver,
      pred: Set[ID] = Set()
  ): Future[PostData[LocalProver, HoTTPost, HoTTPost.ID]] =
    postLocalProverFuture(lp, pred)
}
