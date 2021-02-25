package provingground.learning

import provingground._, HoTT._
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
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable._

@deprecated("migrating to HoTTPostWeb", "soon")
class HoTTPost { web =>
  import HoTTPost._

  val global = new CounterGlobalID()

  import global.postGlobal

  var equationNodes: Set[EquationNode] = Set()

  def equations = Equation.group(equationNodes)

  def terms = ExpressionEval.terms(equationNodes)

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

  val chompBuffer = PostBuffer[ChompResult, ID](postGlobal)

  val termResultBuffer = PostBuffer[TermResult, ID](postGlobal)

  val errorBuffer = PostBuffer[Throwable, ID](postGlobal)

  val hnilBuffer = PostBuffer[HNil, ID](postGlobal)

  val representationBuffer =
    PostBuffer[Map[GeneratorVariables.Variable[_], Vector[Double]], ID](
      postGlobal
    )

  lazy val webBuffers: Vector[WebBuffer[_, ID]] =
    Vector() :+ WebBuffer(web.lpBuff) :+ WebBuffer(web.expEvalBuff) :+ WebBuffer(
      web.eqnNodeBuff
    ) :+ WebBuffer(web.chompBuffer) :+ WebBuffer(web.errorBuffer) :+
      WebBuffer(web.finalStateBuff) :+ WebBuffer(web.genEqnBuff) :+ WebBuffer(
      web.hnilBuffer
    ) :+
      WebBuffer(web.initStateBuff) :+ WebBuffer(web.isleNormEqnBuff) :+ WebBuffer(
      web.lemmaBuffer
    ) :+
      WebBuffer(web.lptBuff) :+ WebBuffer(web.representationBuffer) :+ WebBuffer(
      web.termResultBuffer
    ) :+
      WebBuffer(web.tgBuff) :+ WebBuffer(web.tunedLpBuff)

}

object HoTTPost {
  type ID = (Int, Int)

  def fansiLog(post: PostData[_, HoTTPost, ID]): Future[Unit] =
    Future {
      translation.FansiShow.fansiPrint.log(post.pw.tag)
      translation.FansiShow.fansiPrint.log(post.content, height = 20)
      pprint.log(post.id)
    }

  def testGet: PostHistory[HoTTPost, ID] =
    PostHistory.get((w: HoTTPost) => w.initStateBuff :: HNil)

  // a test - pick these for implicits
  lazy val b2 :: b1 :: HNil = BuildPostable.get(
    (web: HoTTPost) => (web.chompBuffer) :: (web.lemmaBuffer) :: (HNil: HNil)
  )

  case class TunedLocalProver(lp: LocalProver)


  case class IsleNormalizedEquationNodes(eqn: Set[EquationNode])


  import PostBuffer.bufferPost

  implicit val postUnit: Postable[Unit, HoTTPost, ID] =
    new Postable[Unit, HoTTPost, ID] {
      val tag: TypeTag[Unit] = implicitly
      def post(content: Unit, web: HoTTPost, pred: Set[ID]): Future[ID] =
        web.global.postGlobal(content)
    }

  implicit val postError: Postable[Throwable, HoTTPost, ID] =
    bufferPost(_.errorBuffer)

  implicit val postHNil: Postable[HNil, HoTTPost, ID] =
    bufferPost(_.hnilBuffer)

  implicit val postLP: Postable[LocalProver, HoTTPost, ID] =
    bufferPost(_.lpBuff)

  implicit val postLPT: Postable[LocalTangentProver, HoTTPost, ID] =
    bufferPost(_.lptBuff)

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

  implicit val postChomp: Postable[ChompResult, HoTTPost, ID] =
    bufferPost(_.chompBuffer)

  implicit val postResult: Postable[TermResult, HoTTPost, ID] =
    bufferPost(_.termResultBuffer)

  implicit val repPost: Postable[Map[GeneratorVariables.Variable[_], Vector[
    Double
  ]], HoTTPost, ID] =
    bufferPost(_.representationBuffer)

  implicit val someTGQuery =
    LocalQueryable.answerAsSome[TermGenParams, HoTTPost, ID] ||
      ((lp: LocalProver) => Some(lp.tg)) ||
      ((lp: LocalTangentProver) => Some(lp.tg))

  implicit val someInitQuery =
    LocalQueryable.answerAsSome[InitState, HoTTPost, ID] ||
      ((lp: LocalProver) => Some(InitState(lp.initState, 1))) ||
      ((lp: LocalTangentProver) => Some(InitState(lp.initState, 1)))

  def mutWebBuffers(web: HoTTPost): Vector[ErasableWebBuffer[_, ID]] = Vector()

  def findInWeb(
      web: HoTTPost,
      index: ID
  ): Option[(PostData[_, HoTTPost, ID], Set[ID])] =
    (web.webBuffers
      .map(_.getPost(index)) ++ mutWebBuffers(web).map(_.getPost(index)))
      .fold[Option[(PostData[_, HoTTPost, ID], Set[ID])]](None)(_ orElse _)
      .map {
        case (pd, ids) => (pd, ids.flatMap(skipDeleted(web, _)))
      }

  def skipDeleted(web: HoTTPost, id: ID): Set[ID] =
    skipDeletedStep(web, id)
      .map(ids => ids.flatMap(skipDeleted(web, _)))
      .getOrElse(Set(id))

  def skipDeletedStep(web: HoTTPost, id: ID): Option[Set[ID]] =
    mutWebBuffers(web)
      .map(_.buffer.skipDeletedStep(index = id))
      .fold(None)(_ orElse _)

  implicit def postHistory: PostHistory[HoTTPost, ID] =
    new PostHistory[HoTTPost, ID] {
      def findPost(
          web: HoTTPost,
          index: ID
      ): Option[(PostData[_, HoTTPost, ID], Set[ID])] = findInWeb(web, index)

      def allPosts(web: HoTTPost): View[PostData[_, HoTTPost, ID]] =
        web.webBuffers.view.flatMap(_.data)

      def redirects(web: HoTTPost): Map[ID, Set[ID]] = Map()

      override def postTags(web: HoTTPost): Vector[(reflect.runtime.universe.TypeTag[_], ID, Option[Set[ID]])] = ???

    }

  def allPostFullData(
      web: HoTTPost
  ): Vector[(PostData[_, HoTTPost, ID], ID, Set[ID])] =
    web.webBuffers.flatMap(_.fullData) ++ mutWebBuffers(web).flatMap(_.fullData)

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
      ): Future[HoTTPostData] = web.global.counterVar.read().map {counter => 
        HoTTPostData(
          counter,
          allPostFullData(web).map {
            case (pd, id, ids) => (pd, id, ids.flatMap(skipDeleted(web, _)))
          }
        )
      }
    }

  implicit def equationNodeQuery: Queryable[Set[EquationNode], HoTTPost] =
    Queryable.simple(_.equationNodes)

  implicit def equationQuery: Queryable[Set[Equation], HoTTPost] =
    Queryable.simple(_.equations)

  implicit def termSetQuery: Queryable[Set[Term], HoTTPost] =
    Queryable.simple(_.terms)

  implicit def lpStepQuery: LocalQueryable[LocalProverStep, HoTTPost, ID] =
    LatestAnswer(
      Vector(
        AnswerFromPost((lp: LocalProver) => lp: LocalProverStep),
        AnswerFromPost((lp: LocalTangentProver) => lp: LocalProverStep)
      )
    )

  case class Apex[P](base: P)

  implicit def postToLeaves[P: TypeTag](
      implicit bp: Postable[P, HoTTPost, ID], dg: DataGetter[P, HoTTPost, ID]
  ): Postable[Apex[P], HoTTPost, ID] =
    new Postable[Apex[P], HoTTPost, ID] {
      val tag: TypeTag[Apex[P]] = implicitly

      def post(content: Apex[P], web: HoTTPost, pred: Set[ID]): Future[ID] = {
        val dataFuture = query[HoTTPostData, HoTTPost](web, (_) => true)
        for {
          data <- dataFuture
          leaves = data.leafIndices.toSet
          postData <- Postable.post(content.base, web, pred union leaves)
        } yield postData.id
      }
    }

  lazy val lpToExpEv: PostResponse[HoTTPost, ID] = {
    val response: Unit => LocalProver => Future[ExpressionEval] = (_) =>
      lp => lp.expressionEval.runToFuture
    MicroBot(response)
  }

  lazy val lptToExpEv: PostResponse[HoTTPost, ID] = {
    val response: Unit => LocalTangentProver => Future[ExpressionEval] = (_) =>
      lp => lp.expressionEval.runToFuture
    MicroBot(response)
  }

  lazy val lpToTermResult: PostResponse[HoTTPost, ID] = {
    val response: Unit => LocalProver => Future[TermResult] = (_) =>
      lp => termData(lp).runToFuture
    MicroBot(response)
  }

  lazy val lptToTermResult: PostResponse[HoTTPost, ID] = {
    val response: Unit => LocalTangentProver => Future[TermResult] = (_) =>
      lp => termData(lp).runToFuture
    MicroBot(response)
  }

  lazy val termResultToEquations: PostResponse[HoTTPost, ID] =
    MicroBot.simple(
      (pair: TermResult) => GeneratedEquationNodes(pair._2)
    )

  lazy val expEvToEqns: PostResponse[HoTTPost, ID] =
    MicroBot.simple(
      (ev: ExpressionEval) => ev.equations.flatMap(Equation.split)
    )

  lazy val eqnUpdate: PostResponse[HoTTPost, ID] =
    Callback.simple(
      (web: HoTTPost) => (eqs: Set[EquationNode]) => web.addEqns(eqs)
    )

  lazy val normEqnUpdate: PostResponse[HoTTPost, ID] =
    Callback.simple(
      (web: HoTTPost) =>
        (eqs: IsleNormalizedEquationNodes) => web.addEqns(eqs.eqn)
    )

  lazy val lpFromInit: PostResponse[HoTTPost, ID] = {
    val response: TermGenParams => InitState => Future[LocalProver] =
      (tg) => (init) => Future(LocalProver(init.ts, tg).sharpen(init.weight))
    MicroBot(response)
  }

  lazy val lpFromTG: PostResponse[HoTTPost, ID] = {
    val response: InitState => TermGenParams => Future[LocalProver] =
      (init) => (tg) => Future(LocalProver(init.ts, tg).sharpen(init.weight))
    MicroBot(response)
  }

  lazy val tuneLP: PostResponse[HoTTPost, ID] = {
    val response: Unit => LocalProver => Future[TunedLocalProver] =
      (_) => (lp) => lp.tunedInit.runToFuture.map(TunedLocalProver(_))
    MicroBot(response)
  }

  lazy val isleNormalizeEqns: PostResponse[HoTTPost, ID] =
    MicroBot.simple(
      (ge: GeneratedEquationNodes) =>
        IsleNormalizedEquationNodes(
          ge.eqn.map(eq => TermData.isleNormalize(eq))
        )
    )

  lazy val lpLemmas: PostResponse[HoTTPost, ID] = {
    val response: Unit => LocalProver => Future[Lemmas] =
      (_) => (lp) => lp.lemmas.runToFuture.map(v => Lemmas(v.map(xy => (xy._1, None, xy._2)).par))
    MicroBot(response)
  }

  lazy val lptLemmas: PostResponse[HoTTPost, ID] = {
    val response: Unit => LocalTangentProver => Future[Lemmas] =
      (_) => (lp) => lp.lemmas.runToFuture.map(v => Lemmas(v.map(xy => (xy._1, None, xy._2)).par))
    MicroBot(response)
  }

  lazy val lemmaToTangentProver: PostResponse[HoTTPost, ID] = {
    val response: LocalProver => Lemmas => Future[Vector[LocalTangentProver]] =
      (lp) =>
        (lm) =>
          Future.sequence(lm.lemmas.map {
            case (tp, _, w) =>
              lp.tangentProver("lemma" :: tp).map(_.sharpen(w)).runToFuture : Future[LocalTangentProver]
          }.seq)
    new MiniBot[Lemmas, LocalTangentProver, HoTTPost, LocalProver, ID](
      response,
      (_) => true
    )
  }

  lazy val lpToChomp: PostResponse[HoTTPost, ID] = {
    val response: Set[Term] => LocalProver => Future[ChompResult] =
      terms =>
        lp =>
          lp.orderedUnknowns
            .flatMap(
              typs =>
                StrategicProvers.liberalChomper(lp, typs, accumTerms = terms)
            )
            .map {
              case (s, fl, eqs, _) => ChompResult(s, fl, eqs)
            }
            .runToFuture
    MicroBot(response)
  }

  lazy val termResultToChomp: PostResponse[HoTTPost, ID] = {
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

  lazy val recomputeFromInit
      : PostResponse[HoTTPost, ID] = { // when equations are posted, ask for initial state and recompute final state
    val response: (
        Set[EquationNode] :: InitState :: TermGenParams :: HNil
    ) => Set[EquationNode] => Future[FinalState] = {
      case (alleqs :: init :: tg :: HNil) =>
        eqs =>
          Future {
            val expEv = ExpressionEval.fromInitEqs(
              init.ts,
              Equation.group(eqs union alleqs),
              tg.coeffVal(_),
              tg.varWeight
            )
            FinalState(expEv.finalTermState())
          }
    }
    MicroBot(response)
  }

}

