package provingground.learning

import provingground._, HoTT._
import monix.eval._, monix.tail._
import monix.execution.Scheduler.Implicits.global
import shapeless._
import scala.concurrent.Future

trait Postable[P, W, ID] {
  def post(content: P, web: W, pred: Option[ID]): Task[ID]
}

trait Queryable[U, W] {
  def get(web: W): Task[U]
}

trait LocalQueryable[U, W, ID] {
  def getAt(web: W, id: ID): Task[U]
}

object LocalQueryable {
  implicit def localize[U, W, ID](
      implicit q: Queryable[U, W]
  ): LocalQueryable[U, W, ID] =
    new LocalQueryable[U, W, ID] {
      def getAt(web: W, id: ID): Task[U] = q.get(web)
    }
}

case class PostData[P, W, ID](content: P, id: ID)(
    implicit val pw: Postable[P, W, ID]
)

sealed trait PostResponse[W, ID]

object PostResponse {
  def typedResponseOpt[Q, W, ID](post: Q, response: PostResponse[W, ID])(
      implicit qp: Postable[Q, W, ID]
  ): Option[TypedPostResponse[Q, W, ID]] = response match {
    case r: TypedPostResponse[p, W, ID] =>
      if (qp == r.pw) Some(r.asInstanceOf[TypedPostResponse[Q, W, ID]])
      else None
  }

  def postChainResponseTask[Q, W, ID](
      web: W,
      post: Q,
      id: ID,
      response: PostResponse[W, ID]
  )(
      implicit qp: Postable[Q, W, ID]
  ): Task[Set[PostData[_, W, ID]]] = {
    val chainOpt = typedResponseOpt(post, response)(qp)
      .map(tr => tr.postTask(web, post, id))
      .toSet
    val flip = Task.sequence(chainOpt)
    flip.map(_.flatten)
  }

  def postChainTask[W, ID](
      web: W,
      posts: Set[PostData[_, W, ID]],
      responses: Set[PostResponse[W, ID]]
  ): Task[Set[PostData[_, W, ID]]] = {
    val groups =
      for {
        response <- responses
        data     <- posts
      } yield {
        data match {
          case pd: PostData[a, b, c] =>
            postChainResponseTask(web, pd.content, pd.id, response)(pd.pw)
        }

      }

    Task.sequence(groups).map(_.flatten)
  }

  def iterant[W, ID](web: W, initialPosts: Set[PostData[_, W, ID]],
    responses: Set[PostResponse[W, ID]]
    ) : Iterant[Task,Set[PostData[_, W, ID]]] ={
        def step(posts: Set[PostData[_, W, ID]]) = postChainTask(web, posts, responses).map(ps => ps -> ps)
        Iterant.fromLazyStateAction(step)(Task.now(initialPosts))
    }

}

sealed abstract class TypedPostResponse[P, W, ID](
    implicit val pw: Postable[P, W, ID]
) extends PostResponse[W, ID] {
  // Just for checking the structure, runs only one step, not tasks spawned by tasks etc.
  def runStep(web: W, content: P, id: ID): Future[Unit]

  def postTask(web: W, content: P, id: ID): Task[Option[PostData[_, W, ID]]]
}

object TypedPostResponse {
  case class Callback[P, W, V, ID](cb: V => P => Task[Unit])(
      implicit pw: Postable[P, W, ID],
      lv: LocalQueryable[V, W, ID]
  ) extends TypedPostResponse[P, W, ID] {
    def runStep(web: W, content: P, id: ID): Future[Unit] = {
      val auxTask = lv.getAt(web, id)
      val task = auxTask.flatMap { aux =>
        cb(aux)(content)
      }
      task.runToFuture
    }

    def postTask(
        web: W,
        content: P,
        id: ID
    ): Task[Option[PostData[_, W, ID]]] = {
      val auxTask = lv.getAt(web, id)
      val task = auxTask.flatMap { aux =>
        cb(aux)(content).map(_ => None)
      }
      task
    }
  }

  case class Poster[P, Q, W, V, ID](response: V => P => Task[Q])(
      implicit pw: Postable[P, W, ID],
      qw: Postable[Q, W, ID],
      lv: LocalQueryable[V, W, ID]
  ) extends TypedPostResponse[P, W, ID] {
    def runStep(web: W, content: P, id: ID): Future[Unit] = {
      val auxTask = lv.getAt(web, id)
      val task =
        for {
          aux     <- auxTask
          newPost <- response(aux)(content)
          idNew   <- qw.post(newPost, web, Some(id))
        } yield ()
      task.runToFuture
    }

    def postTask(
        web: W,
        content: P,
        id: ID
    ): Task[Option[PostData[_, W, ID]]] = {
      val auxTask = lv.getAt(web, id)
      val task =
        for {
          aux     <- auxTask
          newPost <- response(aux)(content)
          idNew   <- qw.post(newPost, web, Some(id))
        } yield Some(PostData(newPost, idNew))
      task
    }
  }
}
