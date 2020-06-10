package provingground.learning

import provingground._, HoTT._
import monix.eval._, monix.tail._
import monix.execution.Scheduler.Implicits.global
import shapeless._
import scala.concurrent.Future
import scala.collection.mutable.ArrayBuffer
import scala.collection.SeqView
import scala.util._
import scala.reflect.runtime.universe._
import Utils.logger



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
    var running: Boolean = true
) {
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
  case class Callback[P, W, V, ID](update: W => V => P => Future[Unit], predicate: V => Boolean = (_ : V) => true)(
      implicit pw: Postable[P, W, ID],
      lv: LocalQueryable[V, W, ID]
  ) extends TypedPostResponse[P, W, ID] {

    def post(
        web: W,
        content: P,
        id: ID
    ): Future[Vector[PostData[_, W, ID]]] = {
      logger.info(s"triggered callback for type ${pw.tag}")
      val auxFuture = lv.getAt(web, id, predicate)
      val task = auxFuture.flatMap { auxs =>
        Future.sequence(auxs.map(aux => update(web)(aux)(content))).map(_ => Vector.empty[PostData[_, W, ID]])
      }
      task
    }
  }

  object Callback{
    def simple[P, W, ID](func: W => P => Unit)(
      implicit pw: Postable[P, W, ID]) = 
      Callback[P, W, Unit, ID]{
        (web: W) => (_: Unit) => (p: P) => Future(func(web)(p))
    }
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
      implicit pw: Postable[P, W, ID],
      qw: Postable[Q, W, ID],
      lv: LocalQueryable[V, W, ID],
      val dg: DataGetter[Q, W, ID]
  ) extends TypedPostResponse[P, W, ID] {

    def post(
        web: W,
        content: P,
        id: ID
    ): Future[Vector[PostData[_, W, ID]]] = {
      logger.info(s"triggered response of type ${qw.tag} to posts of type ${pw.tag}")
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
      val task = taskNest.flatMap(st => Future.sequence(st))
      task
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
      logger.info(s"triggered (multiple) responses of type ${qw.tag} to posts of type ${pw.tag}")
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
      val task = taskNest.flatMap(st => Future.sequence(st).map(_.flatten))
      task
    }
}

import Postable.ec, TypedPostResponse.MicroBot

case class WebState[W, ID](web: W, apexPosts: Vector[PostData[_, W, ID]] = Vector()){
  def post[P](content: P, predecessors: Set[ID])(implicit pw: Postable[P, W, ID]) : Future[WebState[W, ID]] = 
    pw.post(content, web ,predecessors).map{id => WebState(web, PostData.get(content, id) +: apexPosts )} 

  def act[P](bot: TypedPostResponse[P, W, ID])(implicit pw: Postable[P, W, ID]) = 
    Future.sequence(apexPosts.flatMap{pd =>
      // pprint.log(pd) 
      // pprint.log(pd.getOpt[P])
      // pprint.log(pd.pw.tag)
      // pprint.log(pw.tag)
      pd.getOpt[P].map{content => (pd, content, pd.id)}
    }.map{
      case (pd, content, id) => bot.post(web, content, id).map{w => if (w.isEmpty) Vector(pd) else w}
    }).map{vv => WebState(web, vv.flatten ++ apexPosts.filter(_.getOpt[P].isEmpty))} 

  def postApex[P](content: P)(implicit pw: Postable[P, W, ID]) : Future[WebState[W,ID]] = 
    Future.sequence(apexPosts.map(pd => post(content, Set(pd.id)))).map{
      v => WebState(v.head.web, v.map(_.apexPosts).reduce(_ ++ _))
    }

  def queryApex[Q](predicate: Q => Boolean = (_: Q) => true)(implicit lp: LocalQueryable[Q, W, ID]) =
    Future.sequence(apexPosts.map{
      pd => lp.getAt(web, pd.id, predicate)
    }).map(_.flatten)
}
