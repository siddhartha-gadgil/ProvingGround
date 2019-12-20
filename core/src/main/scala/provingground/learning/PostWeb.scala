package provingground.learning

import provingground._, HoTT._
import monix.eval._, monix.tail._
import monix.execution.Scheduler.Implicits.global
import shapeless._
import scala.concurrent.Future
import scala.collection.mutable.ArrayBuffer

trait Postable[P, W, ID] {
  def post(content: P, web: W, pred: Set[ID]): Task[ID]

  val contextChange: Boolean
}

trait Queryable[U, W] {
  def get(web: W): Task[U]
}

trait LocalQueryable[U, W, ID] {
  def getAt(web: W, id: ID): Task[Vector[U]]
}

object LocalQueryable {
  implicit def localize[U, W, ID](
      implicit q: Queryable[U, W]
  ): LocalQueryable[U, W, ID] =
    new LocalQueryable[U, W, ID] {
      def getAt(web: W, id: ID): Task[Vector[U]] = q.get(web).map(Vector(_))
    }

  case class AnswerFromPost[P, U, W, ID](func: P => U)(
      implicit pw: Postable[P, W, ID]
  ) {
    def fromPost[Q](data: PostData[Q, W, ID]) =
      if (pw == data.pw) Some(func(data.content.asInstanceOf[P])) else None
  }

  def lookupAnswer[P, W, ID](
      implicit pw: Postable[P, W, ID]
  ): AnswerFromPost[P, P, W, ID] = AnswerFromPost(identity(_))

  case class RecentAnswer[Q, W, ID](
      answers: Seq[AnswerFromPost[_, Q, W, ID]]
  )(implicit h: PostHistory[W, ID])
      extends LocalQueryable[Q, W, ID] {
    def getAt(web: W, id: ID) =
      Task {
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
      def getAt(web: W, id: ID): Task[Vector[U :: V]] = 
        for {
            head <- qu.getAt(web, id)
            tail <- qv.getAt(web, id)
        } yield head.flatMap(x => tail.map(y => x :: y))
          
  }

  implicit def hNilQueryable[W, ID] : LocalQueryable[HNil, W, ID] = new LocalQueryable[HNil, W, ID] {
      def getAt(web: W, id: ID): Task[Vector[HNil]] = Task.now(Vector(HNil))
  }

  implicit def unitQueryable[W, ID] : LocalQueryable[Unit, W, ID] = new LocalQueryable[Unit, W, ID] {
    def getAt(web: W, id: ID): Task[Vector[Unit]] = Task.now(Vector(()))
}
}

case class PostData[P, W, ID](content: P, id: ID)(
    implicit val pw: Postable[P, W, ID]
){
  val contextChange = pw.contextChange
} 

trait PostHistory[W, ID] {
  def history(web: W, id: ID): Stream[PostData[_, W, ID]]
}

sealed trait PostResponse[W, ID]

object PostResponse {
  def typedResponseOpt[Q, W, ID](post: Q, response: PostResponse[W, ID])(
      implicit qp: Postable[Q, W, ID]
  ): Option[TypedPostResponse[Q, W, ID]] = response match {
    case r: TypedPostResponse[p, W, ID] =>
      if (qp == r.pw) Some(r.asInstanceOf[TypedPostResponse[Q, W, ID]])
      else None
  }

  def postResponseTask[Q, W, ID](
      web: W,
      post: Q,
      id: ID,
      response: PostResponse[W, ID]
  )(
      implicit qp: Postable[Q, W, ID]
  ): Task[Vector[PostData[_, W, ID]]] = {
    val chainOpt = typedResponseOpt(post, response)(qp)
      .map(tr => tr.postTask(web, post, id))
      .toVector
    val flip = Task.sequence(chainOpt)
    flip.map(_.flatten)
  }

  def postChainTask[W, ID](
      web: W,
      posts: Vector[PostData[_, W, ID]],
      responses: Vector[PostResponse[W, ID]]
  ): Task[Vector[PostData[_, W, ID]]] = {
    val groups =
      for {
        response <- responses
        data     <- posts
      } yield {
        data match {
          case pd: PostData[a, b, c] =>
            postResponseTask(web, pd.content, pd.id, response)(pd.pw)
        }

      }

    Task.sequence(groups).map(_.flatten)
  }

  def iterant[W, ID](
      web: W,
      initialPosts: Vector[PostData[_, W, ID]],
      responses: Vector[PostResponse[W, ID]]
  ): Iterant[Task, Vector[PostData[_, W, ID]]] = {
    def step(posts: Vector[PostData[_, W, ID]]) =
      postChainTask(web, posts, responses).map(ps => ps -> ps)
    Iterant.fromLazyStateAction(step)(Task.now(initialPosts))
  }

}

class SimpleSession[W, ID](
    val web: W,
    var responses: Vector[PostResponse[W, ID]]
) {
  def post[P](content: P, preds: Set[ID])(implicit pw: Postable[P, W, ID]): Task[ID] = {
    val postIdTask = pw.post(content, web, preds)
    postIdTask.foreach { postID =>
      val reactions = responses.map(
        response =>
          PostResponse.postResponseTask(web, content, postID, response).map {
            v =>
              v.map {
                case pd: PostData[q, W, ID] => post(pd.content, Set(postID))(pd.pw)
              }
          }
      )
      reactions.foreach(_.runToFuture)
    }
    postIdTask
  }

  def query[Q](implicit q: Queryable[Q, W]) = q.get(web)

  def queryAt[Q](id: ID)(implicit q: LocalQueryable[Q, W, ID]) =
    q.getAt(web, id)
}

sealed abstract class TypedPostResponse[P, W, ID](
    implicit val pw: Postable[P, W, ID]
) extends PostResponse[W, ID] {

  def postTask(web: W, content: P, id: ID): Task[Vector[PostData[_, W, ID]]]
}

object TypedPostResponse {
  case class Callback[P, W, V, ID](update: W => V => P => Task[Unit])(
      implicit pw: Postable[P, W, ID],
      lv: LocalQueryable[V, W, ID]
  ) extends TypedPostResponse[P, W, ID] {

    def postTask(
        web: W,
        content: P,
        id: ID
    ): Task[Vector[PostData[_, W, ID]]] = {
      val auxTask = lv.getAt(web, id)
      val task = auxTask.flatMap { auxs =>
        Task.sequence(auxs.map(aux => update(web)(aux)(content))).map(_ => Vector.empty[PostData[_, W, ID]])
      }
      task
    }
  }

  case class Poster[P, Q, W, V, ID](response: V => P => Task[Q])(
      implicit pw: Postable[P, W, ID],
      qw: Postable[Q, W, ID],
      lv: LocalQueryable[V, W, ID]
  ) extends TypedPostResponse[P, W, ID] {

    def postTask(
        web: W,
        content: P,
        id: ID
    ): Task[Vector[PostData[_, W, ID]]] = {
      val auxTask = lv.getAt(web, id)
      val taskNest =
        auxTask.map{
          (auxs => 
            auxs.map{
              aux => 
                val newPostTask = response(aux)(content)
                newPostTask.flatMap{newPost => 
                  val idNewTask = qw.post(newPost, web, Set(id))
                  idNewTask.map(idNew => PostData(newPost, idNew))}
            })
        }
      val task = taskNest.flatMap(st => Task.sequence(st))
      task
    }
  }
}

trait GlobalPost[P, ID] {
  def postGlobal(content: P): Task[ID]
}

object GlobalPost {
  class IndexGlobalPost[P] extends GlobalPost[P, Int] {
    val globalBuffer: ArrayBuffer[P] = ArrayBuffer()
    def postGlobal(content: P): Task[Int] = {
      Task {
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

  def post(content: P, prev: Set[ID]): Task[ID] = {
    val idT = postGlobal(content)
    idT.map { id =>
      buffer += ((content, id, prev))
      id
    }
  }

  def bufferData[W](implicit pw: Postable[P, W, ID]) : Vector[PostData[_, W, ID]] =
    buffer.map{case (p, id, _) => PostData(p, id)}.toVector

}

object PostBuffer {
  def apply[P, ID](globalPost: => (P => Task[ID])) : PostBuffer[P, ID] = new PostBuffer[P, ID] {
    def postGlobal(content: P): Task[ID] = globalPost(content)
  }

  def get[P, ID](pb: PostBuffer[P, ID], id: ID): Option[P] =
    pb.buffer.find(_._2 == id).map(_._1)

  def previous[P, ID](pb: PostBuffer[P, ID], id: ID) : Set[ID] = {
    val withId =  pb.buffer.filter(_._2 == id).toSet
    withId.flatMap(_._3)
  }
}


class IndexedPostBuffer[P]
    extends GlobalPost.IndexGlobalPost[P]
    with PostBuffer[P, Int]


object Postable {
  def apply[P, W, ID](postFunc: (P, W, Set[ID]) => Task[ID], ctx: Boolean) : Postable[P, W, ID] = 
    new Postable[P, W, ID] {
        def post(content: P, web: W, pred: Set[ID]): Task[ID] = 
            postFunc(content, web, pred)
        val contextChange: Boolean = ctx
    }

  def postTask[P, W, ID](content: P, web: W, pred: Set[ID])(implicit pw: Postable[P, W, ID]) : Task[PostData[P, W, ID]] = 
    pw.post(content, web, pred).map{id => PostData(content, id)} 

  implicit def bufferPostable[P, ID, W <: PostBuffer[P, ID]]
      : Postable[P, W, ID] =
    new Postable[P, W, ID] {
      def post(content: P, web: W, pred: Set[ID]): Task[ID] =
        web.post(content, pred)
      val contextChange: Boolean = false
    }

  implicit def indexedBufferPostable[P, W <: IndexedPostBuffer[P]]
      : Postable[P, W, Int] =
    new Postable[P, W, Int] {
      def post(content: P, web: W, pred: Set[Int]): Task[Int] =
        web.post(content, pred)
      val contextChange: Boolean = false
    }
}

class CounterGlobalID(log : Any => Unit = (_) => ()){
  var counter: Int = 0

  def postGlobal[P](content: P) : Task[(Int, Int)] = {
    val index = counter
    counter +=1
    log(content)
    Task((counter, content.hashCode()))
  }
}