package provingground.learning

import provingground._, HoTT._
import TypedPostResponse._
import monix.eval._
import HoTTPost._ , LocalQueryable._
import monix.execution.Scheduler.Implicits.{global => monixglobal}
import scala.concurrent._

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

  val buffers: Vector[PostBuffer[_, ID]] =
    Vector(lpBuff, eqnNodeBuff, expEvalBuff)

}

object HoTTPost {
  type ID = (Int, Int)

  implicit val postLP: Postable[LocalProver, HoTTPost, ID] = {
    def postFunc(lp: LocalProver, web: HoTTPost, ids: Set[ID]): Future[ID] =
      web.lpBuff.post(lp, ids)
    Postable(postFunc, true)
  }

  implicit val postExpEv: Postable[ExpressionEval, HoTTPost, ID] = {
    def postFunc(ev: ExpressionEval, web: HoTTPost, ids: Set[ID]): Future[ID] =
      web.expEvalBuff.post(ev, ids)
    Postable(postFunc, false)
  }

  implicit val postEqnNodes: Postable[Set[EquationNode], HoTTPost, ID] = {
    def postFunc(
        eqns: Set[EquationNode],
        web: HoTTPost,
        ids: Set[ID]
    ): Future[ID] = web.eqnNodeBuff.post(eqns, ids)
    Postable(postFunc, false)
  }

  case class WebBuffer[P](buffer: PostBuffer[P, ID])(
      implicit pw: Postable[P, HoTTPost, ID]
  ) {
    def getPost(id: ID): Option[(PostData[_, HoTTPost, ID], Set[ID])] =
      buffer.find(id) 

    def data : Vector[PostData[_, HoTTPost, ID]] = buffer.bufferData

    def fullData : Vector[(PostData[_,HoTTPost,ID], ID, Set[ID])] = buffer.bufferFullData
  }

  def webBuffers(web: HoTTPost): Vector[WebBuffer[_]] =
    Vector() :+ WebBuffer(web.lpBuff) :+ WebBuffer(web.expEvalBuff) :+ WebBuffer(
      web.eqnNodeBuff
    )

  def findInWeb(web: HoTTPost, index: ID) : Option[(PostData[_, HoTTPost, ID], Set[ID])] =
    webBuffers(web)
      .map(_.getPost(index))
      .fold[Option[(PostData[_, HoTTPost, ID], Set[ID])]](None)(_ orElse _)

  implicit def postHistory: PostHistory[HoTTPost, ID] = new PostHistory[HoTTPost, ID] {
    def findPost(web: HoTTPost, index: ID) :  Option[(PostData[_, HoTTPost, ID], Set[ID])] = findInWeb(web, index)
  }

  def allPosts(web: HoTTPost): Vector[PostData[_, HoTTPost, ID]] = webBuffers(web).flatMap(_.data)

  def allPostFullData(web: HoTTPost) : Vector[(PostData[_, HoTTPost, ID], ID, Set[ID])]= webBuffers(web).flatMap(_.fullData)

  case class HoTTPostData(number: Int, posts: Vector[(PostData[_, HoTTPost, ID], ID, Set[ID])]){
    def successors(id: ID) = posts.filter(_._3.contains(id))

    val allIndices : Vector[ID ]= posts.map(_._2)

    lazy val leafIndices : Vector[ID] = allIndices.filter(id => successors(id).isEmpty)
  }

  implicit def hottPostDataQuery : Queryable[HoTTPostData, HoTTPost] = new Queryable[HoTTPostData, HoTTPost]{
    def get(web: HoTTPost): Future[HoTTPostData] = Future{
      HoTTPostData(
        web.global.counter,
        allPostFullData(web)
      )
    }
  }

  case class Apex[P](base: P)

  implicit def postToLeaves[P](implicit bp:  Postable[P, HoTTPost, ID]) : Postable[Apex[P], HoTTPost, ID] =
    new Postable[Apex[P], HoTTPost, ID] {

      def post(content: Apex[P], web: HoTTPost, pred: Set[ID]): Future[ID] = 
       {
         val dataFuture = query[HoTTPostData, HoTTPost](web)
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

  lazy val expEvToEqns: PostResponse[HoTTPost, ID] = {
    val response: Unit => ExpressionEval => Future[Set[EquationNode]] = (_) =>
      (expEv) => Future(expEv.equations.flatMap(Equation.split))
    Poster(response)
  }

  lazy val eqnUpdate: PostResponse[HoTTPost, ID] = {
    val update: HoTTPost => Unit => Set[EquationNode] => Future[Unit] = web =>
      (_) => eqns => Future(web.addEqns(eqns))
    Callback(update)
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
