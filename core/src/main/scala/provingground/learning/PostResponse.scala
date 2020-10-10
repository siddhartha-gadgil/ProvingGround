package provingground.learning

import provingground._, HoTT._
import monix.eval._, monix.tail._
// import monix.execution.Scheduler.Implicits.global
import shapeless._
import scala.concurrent.Future
import scala.collection.mutable.ArrayBuffer
import scala.collection.View
import scala.util._
import scala.reflect.runtime.universe._
import Utils.logger
import Utils.ec



/**
 * Response to a post, generating one or more posts or just a callback;
 * this exists mainly for nicer type collections.
 */ 
sealed trait PostResponse[W, ID]{
  type PostType

  def post(web: W, content: PostType, id: ID): Future[Vector[PostData[_, W, ID]]]

  def respond(web: W)(pds: Vector[PostData[PostType, W, ID]]) : Future[Vector[PostData[_, W, ID]]] =
    Future.sequence(pds.map{pd => post(web, pd.content, pd.id)}).map(_.flatten)
}

object PostResponse {
  implicit val ec: scala.concurrent.ExecutionContext =
    Utils.ec

  /**
   * Casting to a typed post response if the Postables match
   */ 
  def typedResponseOpt[Q, W, ID](response: PostResponse[W, ID])(
      implicit qp: Postable[Q, W, ID]
  ): Option[TypedPostResponse[Q, W, ID]] = response match {
    case r: TypedPostResponse[p, W, ID] =>
      if (qp.tag.tpe =:= r.pw.tag.tpe) {
        // logger.info(s"triggered response with type ${r.pw.tag}")
        Some(r.asInstanceOf[TypedPostResponse[Q, W, ID]])}
      else None
  }

  /**
   * Given a post response and a post, 
   * if types match (as evidenced by implicits) then the the response is run and the 
   * new posts wrapped as PostData are returned (the wrapping allows implicits to be passed on).
   */ 
  def postResponse[Q, W, ID](
      web: W,
      post: Q,
      id: ID,
      response: PostResponse[W, ID]
  )(
      implicit qp: Postable[Q, W, ID]
  ): Future[Vector[PostData[_, W, ID]]] = {
    val chainOpt = typedResponseOpt(response)(qp)
      .map(tr => tr.post(web, post, id))
      .toVector
    val flip = Future.sequence(chainOpt)
    flip.map(_.flatten)
  }

}

/**
  * A simple session to post stuff, call responses and if these responses generate posts, recursively call itself for them.
  *
  * @param web the web where the posts are stored
  * @param responses the bots responding to posts
  * @param logs side-effect responses
  */ 
class SimpleSession[W, ID](
    val web: W,
    var responses: Vector[PostResponse[W, ID]],
    logs: Vector[PostData[_, W, ID] => Future[Unit]],
) {
      def running: Boolean = true


  /**
    * recursively posting and running (as side-effects) offspring tasks, this posts the head but 
    * bots will be called with a different method that does not post the head, to avoid duplication
    *
    * @param content the post content
    * @param preds the predecessor posts
    * @param pw postability
    * @return the data of the post as a future
    */ 
  def post[P](content: P, preds: Set[ID], withResponse: Boolean = true)(implicit pw: Postable[P, W, ID]): Future[PostData[_, W, ID]] = {
    val postIdFuture = pw.post(content, web, preds)
    if (withResponse) postIdFuture.foreach { postID => // posting done, the id is now the predecessor for further posts
      respond(content, postID)
    }
    postIdFuture.map{id => 
      val data = PostData(content, id)
      logs.foreach{fn => fn(data).foreach(_ => ())}
      data}
  }

  /**
    * the recursive step for posting, the given content is not posted, only the responses are.
    *
    * @param content the head content, not post
    * @param postID the ID of the head, to be used as predecessor for the other posts
    * @param pw postability
    */
  def respond[P](content: P, postID: ID)(implicit pw: Postable[P, W, ID]) : Unit = if (running) {
    responses.foreach(
      response =>
        PostResponse.postResponse(web, content, postID, response).map {
          v =>
            v.map {
              case pd: PostData[q, W, ID] => 
               logs.foreach{fn => fn(pd).foreach(_ => ())}
                respond(pd.content, pd.id)(pd.pw)
            }
        }
    ) 
  }

  /**
    * a query from the web
    *
    * @param predicate condition to be satisfied
    * @param q queribility
    * @return response to query as a future
    */
  def query[Q](predicate: Q => Boolean = (_: Q) => true)(implicit q: Queryable[Q, W]) : Future[Q] = q.get(web, predicate)

  /**
    * a query from the web at a position
    *
    * @param id the postion
    * @param predicate condition to be satisfied
    * @param q queribility
    * @return response to query as a future
    */
  def queryAt[Q](id: ID, predicate: Q => Boolean= (_: Q) => true)(implicit q: LocalQueryable[Q, W, ID]) : Future[Vector[Q]] =
    q.getAt(web, id, predicate)
}

/**
  * Post response with type `P` of post as a type parameter
  *
  * @param pw postability of `P`
  */
sealed abstract class TypedPostResponse[P, W, ID](
    implicit val pw: Postable[P, W, ID]
) extends PostResponse[W, ID] {
  type PostType = P

  // the posts in response to a given one, may be none
  def post(web: W, content: P, id: ID): Future[Vector[PostData[_, W, ID]]]

  def >>[Q](that: TypedPostResponse[Q, W, ID])(implicit qw: Postable[Q, W, ID], dg: DataGetter[Q, W, ID]) =
    TypedPostResponse.ComposedResponse(this, that)

  def &&(that: TypedPostResponse[P, W, ID]) = TypedPostResponse.ConcurrentResponse(this, that)

  def reduce[Q, R](reduction: Vector[Q] => R)(
    implicit qw: Postable[Q, W, ID],
      rw : Postable[R, W, ID],
      dgr: DataGetter[R, W, ID]) = TypedPostResponse.ReducedResponse(this, reduction)

  def andThen[Q, V](second: TypedPostResponse.Callback[Q, W, V, ID])(implicit pw: Postable[P, W, ID], qw: Postable[Q, W, ID]) =
    TypedPostResponse.AndThen(this, second)
}

object TypedPostResponse {
  /**
    * Callback executed on a post of type P;
    * the returned task gives an empty vector, but running it executes the callback,
    * in fact, a callback is executed for each value of the auxiliary queryable
    *
    * @param update the callback, may also update the web as a side-effect
    * @param predicate condition on post to trigger callbask
    * @param pw postability 
    * @param lv queryability of parameters on which the callback depends
    */ 
  case class Callback[P, W, V, ID](update: W => V => P => Future[Unit], predicate: V => Boolean = (_ : V) => true,
      name: Option[String] = None)(
      implicit pw: Postable[P, W, ID],
      lv: LocalQueryable[V, W, ID]
  ) extends TypedPostResponse[P, W, ID] {

    def post(
        web: W,
        content: P,
        id: ID
    ): Future[Vector[PostData[_, W, ID]]] = {
      val message = name.getOrElse(this.hashCode().toHexString)
      logger.info(s"triggered callback ${message} for type ${pw.tag} with input hash ${content.hashCode()}")
      val auxFuture = lv.getAt(web, id, predicate)
      val task = auxFuture.flatMap { auxs =>
        Future.sequence(auxs.map(aux => update(web)(aux)(content))).map(_ => Vector.empty[PostData[_, W, ID]])
      }.andThen(_ => logger.info(s"completed callback ${message} for type ${pw.tag} with input hash ${content.hashCode()}"))
      task
    }
  }

  object Callback{
    def simple[P, W, ID](func: W => P => Unit,
      name: Option[String] = None)(
      implicit pw: Postable[P, W, ID]) = 
      Callback[P, W, Unit, ID]({
        (web: W) => (_: Unit) => (p: P) => Future(func(web)(p))
    }, name = name)
  }

  /**
    * Bot responding to a post returning a vector of posts, 
    * one for each value of the auxiliary queryable (in simple cases a singleton is returned)
    *
    * @param response the action of the bot
    * @param predicate the condition the post must satisfy to trigger the bot
    * @param pw postability of the post type
    * @param qw postability of the response post type
    * @param lv queryability of the other arguments
    */ 
  case class MicroBot[P, Q, W, V, ID](response: V => P => Future[Q], predicate: P => V => Boolean = (_: P) => (_ : V) => true)(
      implicit val ppw: Postable[P, W, ID],
      val qw: Postable[Q, W, ID],
      val lv: LocalQueryable[V, W, ID],
      val dg: DataGetter[Q, W, ID]
  ) extends TypedPostResponse[P, W, ID] {

    def post(
        web: W,
        content: P,
        id: ID
    ): Future[Vector[PostData[_, W, ID]]] = {
      logger.info(s"triggered response ${this.hashCode().toHexString} of type ${qw.tag} to posts of type ${pw.tag}")
      val auxFuture = lv.getAt(web, id, predicate(content)) // auxiliary data from queries
      val taskNest =
        auxFuture.map{
          (auxs => 
            auxs.map{
              aux => 
                val newPostFuture = response(aux)(content)
                newPostFuture.flatMap{newPost => 
                  val idNewFuture = qw.post(newPost, web, Set(id))
                  idNewFuture.map(idNew => PostData.get(newPost, idNew)(dg))}
            })
        }
      val task = taskNest.flatMap(st => Future.sequence(st)).andThen(_ =>
              logger.info(s"completed response ${this.hashCode().toHexString} of type ${qw.tag} to posts of type ${pw.tag}")
)
      task
    }

    def triggerWith[R](implicit rw: Postable[R, W, ID], pq: LocalQueryable[P, W, ID]) = 
      {implicit val lpv : LocalQueryable[P:: V :: HNil, W, ID] = LocalQueryable.hconsQueryable(implicitly, implicitly)
        MicroBot[R, Q, W, P :: V :: HNil, ID](
        {
          case p :: v :: HNil =>
            _ : R =>
              response(v)(p)
        }
      )
      }
    
    def :+[QQ : TypeTag, VV: TypeTag](that : MicroBot[P, QQ, W, VV, ID])(implicit qqw: Postable[QQ, W, ID], dgqq : DataGetter[QQ, W, ID],
      nilGetter: Postable[HNil, W, ID]) : MicroBot[P,Q :: QQ :: HNil,W,V :: VV :: HNil,ID] = 
      {
        import that.{dg, lv => tlv, ppw}
        import qw.tag

        MicroBot[P, Q :: QQ :: HNil, W, V :: VV:: HNil, ID]{
        case v :: vv :: HNil => p =>
            response(v)(p).flatMap(r1 =>
             that.response(vv)(p).map(r2 => r1 :: r2 :: HNil) )             
          }
        }
  }

  /**
    * Probabaly not needed, need to just post pairs
    *
    * @param response
    * @param predicate
    * @param pw
    * @param q1w
    * @param q2w
    * @param lv
    */
  case class PairBot[P, Q1, Q2<: HList, W, V, ID](response: V => P => Future[Q1 :: Q2], predicate: V => Boolean = (_ : V) => true)(
      implicit pw: Postable[P, W, ID],
      q1w: Postable[Q1, W, ID],
      q2w: Postable[Q2, W, ID],
      lv: LocalQueryable[V, W, ID]
  ) extends TypedPostResponse[P, W, ID] {

    def post(
        web: W,
        content: P,
        id: ID
    ): Future[Vector[PostData[_, W, ID]]] = {
      val auxFuture = lv.getAt(web, id, predicate) // auxiliary data from queries
      val taskNest =
        auxFuture.map{
          (auxs => 
            auxs.map{
              aux => 
                val newPostFuture = response(aux)(content)
                newPostFuture.flatMap{case newPost :: np2  => 
                  val idNewFuture = q1w.post(newPost, web, Set(id))
                  idNewFuture.flatMap{idNew => 
                    val id2Fut = q2w.post(np2, web, Set(idNew))
                    id2Fut.map(id2 => PostData.get(np2, id2))}
                  }
            })
        }
      val task = taskNest.flatMap(st => Future.sequence(st))
      task
    }
  }

  object MicroBot{
    def simple[P, Q, W, ID](func: P => Q)(implicit pw: Postable[P, W, ID],
    qw: Postable[Q, W, ID], dg: DataGetter[Q, W, ID]) = MicroBot[P, Q, W, Unit, ID](
     (_ : Unit) => (p: P) => Future(func(p))
    )
  }


  /**
    * Bot responding to a post returning a vector of posts for  
    * each value of the auxiliary queryable - so even if the auxiliary has a single response, many posts are made 
    *
    * @param responses the responses of the bot
    * @param predicate the condition the post must satisfy to trigger the bot
    * @param pw postability of the post type
    * @param qw postability of the response post type
    * @param lv queryability of the other arguments
    */
  case class MiniBot[P, Q, W, V, ID](responses: V => P => Future[Vector[Q]], predicate: V => Boolean = (_: V) => true)(
        implicit pw: Postable[P, W, ID],
        qw: Postable[Q, W, ID],
        lv: LocalQueryable[V, W, ID],
        dg: DataGetter[Q, W, ID]
    ) extends TypedPostResponse[P, W, ID] {

      def post(
          web: W,
          content: P,
          id: ID
      ): Future[Vector[PostData[_, W, ID]]] = {
        logger.info(s"triggered (multiple) responses ${this.hashCode().toHexString} of type ${qw.tag} to posts of type ${pw.tag}")
        val auxFuture = lv.getAt(web, id, predicate) // auxiliary data from queries
        val taskNest =
          auxFuture.map{
            (auxs => 
              auxs.map{
                aux => 
                  val newPostsFuture = responses(aux)(content)
                  newPostsFuture.flatMap{newPosts => // extra nesting for multiple posts
                    Future.sequence(newPosts.map{newPost =>
                      val idNewFuture = qw.post(newPost, web, Set(id))
                      idNewFuture.map(idNew => PostData.get(newPost, idNew))}
                    )}
              })
          }
        val task = taskNest.flatMap(st => Future.sequence(st).map(_.flatten)).andThen(_ =>
                logger.info(s"completed all responses ${this.hashCode().toHexString} of type ${qw.tag} to posts of type ${pw.tag}")
)
        task
      }
  }

  /**
    * Bot responding to a post returning a vector of posts for  
    * each value of the auxiliary queryable - so even if the auxiliary has a single response, many posts are made 
    * but posts are in the future : this works if branches are known in advance but each branch calculation is expensive.
    *
    * @param responses the responses of the bot
    * @param predicate the condition the post must satisfy to trigger the bot
    * @param pw postability of the post type
    * @param qw postability of the response post type
    * @param lv queryability of the other arguments
    */
  case class DualMiniBot[P, Q, W, V, ID](responses: V => P => Vector[Future[Q]], predicate: V => Boolean = (_: V) => true)(
        implicit pw: Postable[P, W, ID],
        qw: Postable[Q, W, ID],
        lv: LocalQueryable[V, W, ID],
        dg: DataGetter[Q, W, ID]
    ) extends TypedPostResponse[P, W, ID] {

      def post(
          web: W,
          content: P,
          id: ID
      ): Future[Vector[PostData[_, W, ID]]] = {
        logger.info(s"triggered (multiple) responses ${this.hashCode().toHexString} of type ${qw.tag} to posts of type ${pw.tag}")
        val auxFuture = lv.getAt(web, id, predicate) // auxiliary data from queries
        val taskNest =
          auxFuture.flatMap{
            {auxs =>              
              Utils.logger.info(s"launching ${auxs.size} branches")
              Future.sequence(auxs.flatMap{
                aux => 
                  val newPostsFuture = responses(aux)(content)
                   var remaining = newPostsFuture.size
                  Utils.logger.info(s"launching ${remaining} responses  of type ${qw.tag} in branch")
                  val newPostsData = newPostsFuture.map(cF => cF.flatMap{c => 
                      val idNewF = qw.post(c, web, Set(id))
                      idNewF.map(idNew => PostData.get(c, idNew)).andThen{
                        case Success(_) => remaining = remaining - 1
                          Utils.logger.info(s"remaining $remaining responses of type ${qw.tag}") 
                        case Failure(exception) => 
                          Utils.logger.error(s"response failed with error")
                          Utils.logger.error(exception)
                      }
                    }
                  )
                  newPostsData})
                  }
          }.andThen(_ => 
          logger.info(s"completed (multiple) responses ${this.hashCode().toHexString} of type ${qw.tag} to posts of type ${pw.tag}")
          )
        taskNest
      }

    def triggerWith[R](implicit rw: Postable[R, W, ID], pq: LocalQueryable[P, W, ID]) = 
      {implicit val lpv : LocalQueryable[P:: V :: HNil, W, ID] = LocalQueryable.hconsQueryable(implicitly, implicitly)
        DualMiniBot[R, Q, W, P :: V :: HNil, ID](
        {
          case p :: v :: HNil =>
            _ : R =>
              responses(v)(p)
        }
      )
      }
  }

    /**
    * Bot responding to a post returning a vector of posts for  
    * each value of the auxiliary queryable - so even if the auxiliary has a single response, many posts are made 
    * but posts are in the future : this works if branches are known in advance but each branch calculation is expensive.
    *
    * @param responses the responses of the bot
    * @param predicate the condition the post must satisfy to trigger the bot
    * @param pw postability of the post type
    * @param qw postability of the response post type
    * @param lv queryability of the other arguments
    */
  case class DualMiniBotTask[P, Q, W, V, ID](responses: V => P => Vector[Task[Q]], predicate: V => Boolean = (_: V) => true)(
        implicit pw: Postable[P, W, ID],
        qw: Postable[Q, W, ID],
        lv: LocalQueryable[V, W, ID],
        dg: DataGetter[Q, W, ID]
    ) extends TypedPostResponse[P, W, ID] {

      import monix.execution.Scheduler.Implicits.global
      def post(
          web: W,
          content: P,
          id: ID
      ): Future[Vector[PostData[_, W, ID]]] = {
        logger.info(s"triggered (multiple) responses ${this.hashCode().toHexString} of type ${qw.tag} to posts of type ${pw.tag}")
        val auxFuture = lv.getAt(web, id, predicate) // auxiliary data from queries
        val auxTask = Task.fromFuture(auxFuture)
        val taskNest =
          auxTask.flatMap{
            {auxs =>              
              Utils.logger.info(s"launching ${auxs.size} branches")
              Task.parSequence(auxs.flatMap{
                aux => 
                  val newPostsTask = responses(aux)(content)
                   var remaining = newPostsTask.size
                  Utils.logger.info(s"launching ${remaining} responses  of type ${qw.tag} in branch")
                  val newPostsData = newPostsTask.map(cF => cF.flatMap{c => 
                      val idNewT = Task.fromFuture(qw.post(c, web, Set(id)))
                      idNewT.map(idNew => PostData.get(c, idNew)).materialize.map{
                        case Success(result) => remaining = remaining - 1
                          Utils.logger.info(s"remaining $remaining responses of type ${qw.tag}") 
                          result
                        case Failure(exception) => 
                          Utils.logger.error(s"response failed with error")
                          Utils.logger.error(exception)
                         throw exception
                      }
                    }
                  )
                  newPostsData})
                  }
          }
          .map{result => 
          logger.info(s"completed (multiple) responses ${this.hashCode().toHexString} of type ${qw.tag} to posts of type ${pw.tag}")
          result
          }
        taskNest.runToFuture
      }

    def triggerWith[R](implicit rw: Postable[R, W, ID], pq: LocalQueryable[P, W, ID]) = 
      {implicit val lpv : LocalQueryable[P:: V :: HNil, W, ID] = LocalQueryable.hconsQueryable(implicitly, implicitly)
        DualMiniBotTask[R, Q, W, P :: V :: HNil, ID](
        {
          case p :: v :: HNil =>
            _ : R =>
              responses(v)(p)
        }
      )
      }
  }


  case class ComposedResponse[P, Q, W, ID](first: TypedPostResponse[P, W, ID], 
    second: TypedPostResponse[Q, W, ID])(implicit pw: Postable[P, W, ID], qw: Postable[Q, W, ID]) extends TypedPostResponse[P, W, ID]{
    def post(web: W, content: P, id: ID): Future[Vector[PostData[_, W, ID]]] = {
      val firstPosts = first.post(web, content, id)
      val secondPosts = firstPosts.flatMap(pds => 
        Future.sequence(pds.flatMap(pd => pd.getOpt[Q].map(q => second.post(web, q, pd.id)))))
      secondPosts.map(_.flatten)}
  }

  case class AndThen[P, Q, V, W, ID](first: TypedPostResponse[P, W, ID], 
    second: Callback[Q, W, V, ID])(implicit pw: Postable[P, W, ID], qw: Postable[Q, W, ID]) extends TypedPostResponse[P, W, ID]{
    def post(web: W, content: P, id: ID): Future[Vector[PostData[_, W, ID]]] = {
      val firstPosts = first.post(web, content, id)
      val secondPosts = firstPosts.flatMap(pds => 
        Future.sequence(pds.flatMap(pd => pd.getOpt[Q].map(q => second.post(web, q, pd.id)))))
      firstPosts}
  }


  case class ConcurrentResponse[P, W, ID](first: TypedPostResponse[P, W, ID], second: TypedPostResponse[P, W, ID])(implicit pw: Postable[P, W, ID]) extends TypedPostResponse[P, W, ID]{
    def post(web: W, content: P, id: ID): Future[Vector[PostData[_, W, ID]]] =
      for {
        r1 <- first.post(web, content, id)
        r2 <- second.post(web, content, id)
      }  yield r1 ++ r2
  }

  case class ReducedResponse[P, Q, R, W, ID](first: TypedPostResponse[P, W, ID], reduce: Vector[Q] => R)(
    implicit pw: Postable[P, W, ID], qw: Postable[Q, W, ID], rw: Postable[R, W, ID], dgr: DataGetter[R, W, ID]) extends TypedPostResponse[P, W, ID]{
      def post(web: W, content: P, id: ID): Future[Vector[PostData[_, W, ID]]] = {
              val firstPosts = first.post(web, content, id)
              val qPairsF = firstPosts.map(pds => pds.flatMap(pd => pd.getOpt[Q].map(q => q -> pd.id)))
              val finalData = qPairsF.flatMap{
                case qPair =>
                      val preds = qPair.map(_._2)
                      val qs = qPair.map(_._1)
                      val result = reduce(qs)
                      rw.post(result, web, preds.toSet).map(
                        id => Vector(PostData.get(result, id))
                      )
              }
        finalData}
    }
}

import Postable.ec, TypedPostResponse.MicroBot

case class WebState[W, ID](web: W, apexPosts: Vector[PostData[_, W, ID]] = Vector()){
  implicit val ec: scala.concurrent.ExecutionContext =
    Utils.ec

  def post[P](content: P, predecessors: Set[ID])(implicit pw: Postable[P, W, ID], dg: DataGetter[P, W, ID]) : Future[WebState[W, ID]] = 
    pw.post(content, web ,predecessors).map{id => WebState(web, PostData.get(content, id) +: apexPosts )} 

  def act[P](bot: TypedPostResponse[P, W, ID])(implicit pw: Postable[P, W, ID]) = 
    Future.sequence(apexPosts.flatMap{pd =>
      pd.getOpt[P].map{content => (pd, content, pd.id)}
    }.map{
      case (pd, content, id) => bot.post(web, content, id).map{w => if (w.isEmpty) Vector(pd) else w}
    }).map{vv => WebState(web, vv.flatten ++ apexPosts.filter(_.getOpt[P].isEmpty))} 

  def postApex[P](content: P)(implicit pw: Postable[P, W, ID]) : Future[WebState[W,ID]] = 
    Future.sequence(apexPosts.map(pd => post(content, Set(pd.id)))).map{
      v => WebState(v.head.web, v.map(_.apexPosts).reduce(_ ++ _))
    }

  def postLast[P](content: P)(implicit pw: Postable[P, W, ID], dg: DataGetter[P, W, ID]) : Future[WebState[W,ID]] = 
   pw.post(content, web , apexPosts.map(_.id).toSet).map{id => WebState(web, Vector(PostData.get(content, id)))} 


  def queryApex[Q](predicate: Q => Boolean = (_: Q) => true)(implicit lp: LocalQueryable[Q, W, ID]) =
    Future.sequence(apexPosts.map{
      pd => lp.getAt(web, pd.id, predicate)
    }).map(_.flatten)
}
