package provingground.learning

import provingground._, HoTT._
import monix.eval._, monix.tail._
import monix.execution.Scheduler.Implicits.global
import shapeless._
import scala.concurrent.Future
import scala.collection.mutable.ArrayBuffer
import scala.collection.SeqView

/**
 * Typeclass for being able to post with content type P in W
 */
trait Postable[P, W, ID] {
  def post(content: P, web: W, pred: Set[ID]): Future[ID]

  val contextChange: Boolean
}
/**
 *  Typeclass for being able to query W for type Q
 */
trait Queryable[U, W] {
  def get(web: W, predicate: U => Boolean): Future[U]
}

object Queryable{
  def simple[U, W](func: W => U) = new Queryable[U, W] {
    def get(web: W, predicate: U => Boolean): Future[U] = 
      Future(func(web))
  }

  implicit def allView[P, W, ID](implicit pw: Postable[P, W, ID], ph: PostHistory[W, ID]) : Queryable[SeqView[P, Seq[_]], W] = 
    new Queryable[SeqView[P, Seq[_]], W] {
      def get(web: W, predicate: SeqView[P, Seq[_]] => Boolean): Future[SeqView[P, Seq[_]]] = 
       Future{ ph.allPosts(web).filter(_.pw == pw).map{_.asInstanceOf[P]}}
    }
    

}
/**
 * Typeclass for being able to query W for a vector of elements of type Q at an index
 */ 
trait LocalQueryable[U, W, ID] {
  def getAt(web: W, id: ID, predicate: U => Boolean): Future[Vector[U]]
}

case class AnswerFromPost[P, U, W, ID](func: P => U)(
  implicit pw: Postable[P, W, ID]
) {
def fromPost[Q](data: PostData[Q, W, ID]) : Option[U] =
  if (pw == data.pw) Some(func(data.content.asInstanceOf[P])) else None
}

case class LatestAnswer[Q, W, ID](
  answers: Seq[AnswerFromPost[_, Q, W, ID]]
)(implicit h: PostHistory[W, ID])
  extends LocalQueryable[Q, W, ID] {
    def getAt(web: W, id: ID, predicate: Q => Boolean): Future[Vector[Q]] = Future{
      def answer: PostData[_, W, ID] => Option[Q] = 
        (pd) => answers.flatMap(ans => ans.fromPost(pd)).filter(predicate).headOption
      h.latestAnswers(web, id, answer).toVector
    }
  }


trait FallBackLookups{
  implicit   def lookupLatest[Q, W, ID](implicit qw: Postable[Q, W, ID], ph: PostHistory[W, ID]) : LocalQueryable[Q, W, ID] = 
  LatestAnswer(Seq(AnswerFromPost[Q, Q, W, ID](identity)))

  implicit def somePostQuery[P, W, ID](implicit pw: Postable[P, W, ID], ph: PostHistory[W, ID]) : LocalQueryable[SomePost[P], W, ID] = new LocalQueryable[SomePost[P], W, ID] {
   def getAt(web: W, id: ID, predicate: SomePost[P] => Boolean): Future[Vector[SomePost[P]]] =
     Future{ph.allPosts(web).filter(_.pw == pw).map{post => SomePost(post.content.asInstanceOf[P])}.filter(predicate).toVector
  } 
}
     
}

object LocalQueryable extends FallBackLookups{
  def query[Q, W](web: W, predicate: Q => Boolean)(implicit q: Queryable[Q, W]) = q.get(web, predicate)

  def queryAt[Q, W, ID](web: W, id: ID, predicate: Q => Boolean)(implicit q: LocalQueryable[Q, W, ID]) =
    q.getAt(web, id, predicate)

  implicit def localize[U, W, ID](
      implicit q: Queryable[U, W]
  ): LocalQueryable[U, W, ID] =
    new LocalQueryable[U, W, ID] {
      def getAt(web: W, id: ID, predicate: U => Boolean): Future[Vector[U]] = q.get(web, predicate).map(Vector(_))
    }

  def lookupAnswer[P, W, ID](
    implicit pw: Postable[P, W, ID]
  ): AnswerFromPost[P, P, W, ID] = AnswerFromPost(identity(_))

  /**
   * Look up an answer in the history of a post, assuming an implicit history provider
   */ 
  case class RecentAnswer[Q, W, ID](
      answers: Seq[AnswerFromPost[_, Q, W, ID]]
  )(implicit h: PostHistory[W, ID])
      extends LocalQueryable[Q, W, ID] {
    def getAt(web: W, id: ID, predicate: Q => Boolean) =
      Future {
        val stream = for {
          pd  <- h.history(web, id)
          ans <- answers
          res <- ans.fromPost(pd)
        } yield res
        stream.toVector.take(1)
      }
  }


  

  implicit def hconsQueryable[U, V <: HList, W, ID](
      implicit qu: LocalQueryable[U, W, ID],
      qv: LocalQueryable[V, W, ID]
  ) : LocalQueryable[U :: V, W, ID] = new LocalQueryable[U:: V, W, ID] {
      def getAt(web: W, id: ID, predicate: U :: V => Boolean): Future[Vector[U :: V]] = 
        (for {
            head <- qu.getAt(web, id, (_) => true)
            tail <- qv.getAt(web, id, (_) => true)
        } yield head.flatMap(x => tail.map(y => x :: y))
        ).map(_.filter{case pair => predicate(pair)})
          
  }


  implicit def hNilQueryable[W, ID] : LocalQueryable[HNil, W, ID] = new LocalQueryable[HNil, W, ID] {
      def getAt(web: W, id: ID, predicate: HNil => Boolean): Future[Vector[HNil]] = Future(Vector(HNil))
  }

  implicit def unitQueryable[W, ID] : LocalQueryable[Unit, W, ID] = new LocalQueryable[Unit, W, ID] {
    def getAt(web: W, id: ID, predicate: Unit => Boolean): Future[Vector[Unit]] = Future(Vector(()))
}
}
/**
 * Data for a post, including the implicit saying it is postable
 * @param content the content of the post
 * @param id the index of the post, returned after posting
 */ 
case class PostData[P, W, ID](content: P, id: ID)(
    implicit val pw: Postable[P, W, ID]
){
  val contextChange = pw.contextChange
} 

trait PostHistory[W, ID] {
  // the post itself and all its predecessors
  def findPost(web: W, index: ID) :  Option[(PostData[_, W, ID], Set[ID])]

  def allPosts(web: W): SeqView[PostData[_, W, ID], Seq[_]]



  def history(web: W, id: ID): Stream[PostData[_, W, ID]] = {
    val next : ((Set[PostData[_, W, ID]], Set[ID])) =>  (Set[PostData[_, W, ID]], Set[ID])   = {case (d, indices) =>
      val pairs = indices.map(findPost(web, _)).flatten
      (pairs.map(_._1), pairs.flatMap(_._2))
    }
    def stream : Stream[(Set[PostData[_, W, ID]], Set[ID])] = 
      ((Set.empty[PostData[_, W, (ID)]], Set(id))) #:: stream.map(next)
    stream.flatMap(_._1)
  }

  def latestAnswers[Q](web: W, id: ID, answer: PostData[_, W, ID] => Option[Q]) : Set[Q] = 
    findPost(web, id).map{
      case (pd, preds) => 
        // pprint.log(id)
        answer(pd).map(Set(_)).getOrElse(
          preds.flatMap(pid => latestAnswers(web, pid, answer))
          )
    }.getOrElse(Set())
}

case class SomePost[P](content: P)

/**
 * Response to a post, generating one or more posts or just a callback;
 * this exists mainly for nicer type collections.
 */ 
sealed trait PostResponse[W, ID]{
  type PostType

  def postFuture(web: W, content: PostType, id: ID): Future[Vector[PostData[_, W, ID]]]
}

object PostResponse {

  /**
   * Casting to a typed post response if the Postables match
   */ 
  def typedResponseOpt[Q, W, ID](post: Q, response: PostResponse[W, ID])(
      implicit qp: Postable[Q, W, ID]
  ): Option[TypedPostResponse[Q, W, ID]] = response match {
    case r: TypedPostResponse[p, W, ID] =>
      if (qp == r.pw) Some(r.asInstanceOf[TypedPostResponse[Q, W, ID]])
      else None
  }

  /**
   * Given a post response and a post, 
   * if types match (as evidenced by implicits) then the task is carried out and the 
   * new post wrapped as PostData are returned (the wrapping allows implicits to be passed on).
   */ 
  def postResponseFuture[Q, W, ID](
      web: W,
      post: Q,
      id: ID,
      response: PostResponse[W, ID]
  )(
      implicit qp: Postable[Q, W, ID]
  ): Future[Vector[PostData[_, W, ID]]] = {
    val chainOpt = typedResponseOpt(post, response)(qp)
      .map(tr => tr.postFuture(web, post, id))
      .toVector
    val flip = Future.sequence(chainOpt)
    flip.map(_.flatten)
  }

}

/**
 * A simple session to post stuff, call responses and if these responses generate posts, recursively call itself for them.
 */ 
class SimpleSession[W, ID](
    val web: W,
    var responses: Vector[PostResponse[W, ID]]
) {
  /**
   * recursively posting and running (as side-effects) offspring tasks
   */ 
  def postFuture[P](content: P, preds: Set[ID])(implicit pw: Postable[P, W, ID]): Future[PostData[P, W, ID]] = {
    val postIdFuture = pw.post(content, web, preds)
    postIdFuture.foreach { postID => // posting done, the id is now the predecessor for further posts
      tailPostFuture(content, postID)
    }
    postIdFuture.map{id => PostData(content, id)}
  }

  def tailPostFuture[P](content: P, postID: ID)(implicit pw: Postable[P, W, ID]) : Unit = {
    responses.foreach(
      response =>
        PostResponse.postResponseFuture(web, content, postID, response).map {
          v =>
            v.map {
              case pd: PostData[q, W, ID] => tailPostFuture(pd.content, pd.id)(pd.pw)
            }
        }
    ) 
  }

  def query[Q](predicate: Q => Boolean = (_: Q) => true)(implicit q: Queryable[Q, W]) = q.get(web, predicate)

  def queryAt[Q](id: ID, predicate: Q => Boolean= (_: Q) => true)(implicit q: LocalQueryable[Q, W, ID]) =
    q.getAt(web, id, predicate)
}

sealed abstract class TypedPostResponse[P, W, ID](
    implicit val pw: Postable[P, W, ID]
) extends PostResponse[W, ID] {
  type PostType = P

  // the posts in response to a given one, may be none
  def postFuture(web: W, content: P, id: ID): Future[Vector[PostData[_, W, ID]]]
}

object TypedPostResponse {
/**
 * Callback executed on a post of type P;
 * the returned task gives an empty vector, but running it executes the callback,
 * in fact, a callback is executed for each value of the auxiliary queryable
 */ 
  case class Callback[P, W, V, ID](update: W => V => P => Future[Unit], predicate: V => Boolean = (_ : V) => true)(
      implicit pw: Postable[P, W, ID],
      lv: LocalQueryable[V, W, ID]
  ) extends TypedPostResponse[P, W, ID] {

    def postFuture(
        web: W,
        content: P,
        id: ID
    ): Future[Vector[PostData[_, W, ID]]] = {
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
   * Response to a post returning a vector of posts, 
   * one for each value of the auxiliary queryable (in simple cases a singleton is returned)
   */ 
  case class Poster[P, Q, W, V, ID](response: V => P => Future[Q], predicate: V => Boolean = (_ : V) => true)(
      implicit pw: Postable[P, W, ID],
      qw: Postable[Q, W, ID],
      lv: LocalQueryable[V, W, ID]
  ) extends TypedPostResponse[P, W, ID] {

    def postFuture(
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
                newPostFuture.flatMap{newPost => 
                  val idNewFuture = qw.post(newPost, web, Set(id))
                  idNewFuture.map(idNew => PostData(newPost, idNew))}
            })
        }
      val task = taskNest.flatMap(st => Future.sequence(st))
      task
    }
  }

  object Poster{
    def simple[P, Q, W, ID](func: P => Q)(implicit pw: Postable[P, W, ID],
    qw: Postable[Q, W, ID]) = Poster[P, Q, W, Unit, ID](
      (_ : Unit) => (p: P) => Future(func(p))
    )
  }
}



case class VectorPoster[P, Q, W, V, ID](responses: V => P => Future[Vector[Q]], predicate: V => Boolean)(
      implicit pw: Postable[P, W, ID],
      qw: Postable[Q, W, ID],
      lv: LocalQueryable[V, W, ID]
  ) extends TypedPostResponse[P, W, ID] {

    def postFuture(
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
                val newPostsFuture = responses(aux)(content)
                newPostsFuture.flatMap{newPosts => // extra nesting for multiple posts
                  Future.sequence(newPosts.map{newPost =>
                    val idNewFuture = qw.post(newPost, web, Set(id))
                    idNewFuture.map(idNew => PostData(newPost, idNew))}
                  )}
            })
        }
      val task = taskNest.flatMap(st => Future.sequence(st).map(_.flatten))
      task
    }
}

trait GlobalPost[P, ID] {
  def postGlobal(content: P): Future[ID]
}

object GlobalPost {
  class IndexGlobalPost[P] extends GlobalPost[P, Int] {
    val globalBuffer: ArrayBuffer[P] = ArrayBuffer()
    def postGlobal(content: P): Future[Int] = {
      Future {
        globalBuffer += content
        val index = globalBuffer.size - 1
        assert(globalBuffer(index) == content)
        index
      }
    }
  }
}

trait PostBuffer[P, ID] extends GlobalPost[P, ID] { self =>
  val buffer: ArrayBuffer[(P, ID, Set[ID])] = ArrayBuffer()

  def post(content: P, prev: Set[ID]): Future[ID] = {
    val idT = postGlobal(content)
    idT.map { id =>
      buffer += ((content, id, prev))
      id
    }
  }

  def find[W](index: ID)(implicit pw:  Postable[P, W, ID]) :  Option[(PostData[P,W,ID], Set[ID])] = buffer.find(_._2 == index).map{
    case (p, _, preds) => (PostData[P, W, ID](p, index), preds)
  }

  def bufferData[W](implicit pw: Postable[P, W, ID]) : Vector[PostData[_, W, ID]] =
    buffer.map{case (p, id, _) => PostData(p, id)}.toVector

  def bufferFullData[W](implicit pw: Postable[P, W, ID]) : Vector[(PostData[P,W,ID], ID, Set[ID])] =
    buffer.map{case (p, id, preds) => (PostData(p, id), id, preds)}.toVector

}

object PostBuffer {
  def apply[P, ID](globalPost: => (P => Future[ID])) : PostBuffer[P, ID] = new PostBuffer[P, ID] {
    def postGlobal(content: P): Future[ID] = globalPost(content)
  }

  def get[P, ID](pb: PostBuffer[P, ID], id: ID): Option[P] =
    pb.buffer.find(_._2 == id).map(_._1)

  def previous[P, ID](pb: PostBuffer[P, ID], id: ID) : Set[ID] = {
    val withId =  pb.buffer.filter(_._2 == id).toSet
    withId.flatMap(_._3)
  }

  def bufferPost[P, W, ID](buffer: W => PostBuffer[P, ID], ctx: Boolean = false) : Postable[P, W, ID] = {
    def postFunc(p: P, web: W, ids: Set[ID]): Future[ID] =
      buffer(web).post(p, ids)
    Postable(postFunc, ctx)
  }
}


class IndexedPostBuffer[P]
    extends GlobalPost.IndexGlobalPost[P]
    with PostBuffer[P, Int]


object Postable {
  def apply[P, W, ID](postFunc: (P, W, Set[ID]) => Future[ID], ctx: Boolean) : Postable[P, W, ID] = 
    new Postable[P, W, ID] {
        def post(content: P, web: W, pred: Set[ID]): Future[ID] = 
            postFunc(content, web, pred)
        val contextChange: Boolean = ctx
    }

  def postFuture[P, W, ID](content: P, web: W, pred: Set[ID])(implicit pw: Postable[P, W, ID]) : Future[PostData[P, W, ID]] = 
    pw.post(content, web, pred).map{id => PostData(content, id)} 

  implicit def bufferPostable[P, ID, W <: PostBuffer[P, ID]]
      : Postable[P, W, ID] =
    new Postable[P, W, ID] {
      def post(content: P, web: W, pred: Set[ID]): Future[ID] =
        web.post(content, pred)
      val contextChange: Boolean = false
    }

  implicit def indexedBufferPostable[P, W <: IndexedPostBuffer[P]]
      : Postable[P, W, Int] =
    new Postable[P, W, Int] {
      def post(content: P, web: W, pred: Set[Int]): Future[Int] =
        web.post(content, pred)
      val contextChange: Boolean = false
    }
}

class CounterGlobalID(log : Any => Unit = (_) => ()){
  var counter: Int = 0

  def postGlobal[P](content: P) : Future[(Int, Int)] = {
    val index = counter
    counter +=1
    log(content)
    Future((counter, content.hashCode()))
  }
}