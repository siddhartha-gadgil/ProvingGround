package provingground.learning

import monix.execution.Scheduler.Implicits.global
import shapeless._
import scala.concurrent.Future
import scala.collection.mutable.ArrayBuffer
import scala.util._
import scala.reflect.runtime.universe._

/**
  * Allows posting any content, typically just returns an ID to be used by something else.
  */
trait GlobalPost[P, ID] {
  def postGlobal(content: P): Future[ID]
}

/**
  * A buffer for storing posts, extending `GlobalPost` which supplies an ID
  */
trait PostBuffer[P, ID] extends GlobalPost[P, ID] { self =>
  val buffer: ArrayBuffer[(P, ID, Set[ID])] = ArrayBuffer()

  def post(content: P, prev: Set[ID]): Future[ID] = {
    val idT = postGlobal(content)
    idT.map { id =>
      buffer += ((content, id, prev))
      id
    }
  }

  def find[W](index: ID)(
      implicit pw: Postable[P, W, ID]
  ): Option[(PostData[P, W, ID], Set[ID])] = buffer.find(_._2 == index).map {
    case (p, _, preds) => (PostData[P, W, ID](p, index), preds)
  }

  def bufferData[W](
      implicit pw: Postable[P, W, ID]
  ): Vector[PostData[_, W, ID]] =
    buffer.map { case (p, id, _) => PostData(p, id) }.toVector

  def bufferFullData[W](
      implicit pw: Postable[P, W, ID]
  ): Vector[(PostData[P, W, ID], ID, Set[ID])] =
    buffer.map { case (p, id, preds) => (PostData(p, id), id, preds) }.toVector

}

@deprecated("using HoTTPostWeb", "soon")
case class WebBuffer[P, ID](buffer: PostBuffer[P, ID])(
    implicit pw: Postable[P, HoTTPost, ID]
) {
  def getPost(id: ID): Option[(PostData[_, HoTTPost, ID], Set[ID])] =
    buffer.find(id)

  def data: Vector[PostData[_, HoTTPost, ID]] = buffer.bufferData

  def fullData: Vector[(PostData[_, HoTTPost, ID], ID, Set[ID])] =
    buffer.bufferFullData
}

object ErasablePostBuffer {
  def bufferPost[P: TypeTag, W, ID](
      buffer: W => ErasablePostBuffer[P, ID]
  ): Postable[P, W, ID] = {
    def postFunc(p: P, web: W, ids: Set[ID]): Future[ID] =
      buffer(web).post(p, ids)
    Postable(postFunc)
  }

  def build[P, ID](implicit gp: GlobalID[ID]): ErasablePostBuffer[P, ID] =
    new ErasablePostBuffer[P, ID] {
      def postGlobal(content: P): Future[ID] = gp.postGlobal(content)
    }

}

trait PostDiscarder[P, ID] extends GlobalPost[P, ID]

object PostDiscarder {
  def discardPost[P: TypeTag, W, ID](
      buffer: W => PostDiscarder[P, ID]
  ): Postable[P, W, ID] = new Postable[P, W, ID] {
    def post(content: P, web: W, pred: Set[ID]): Future[ID] =
      buffer(web).postGlobal(content)
    val tag: reflect.runtime.universe.TypeTag[P] = implicitly
  }

  def build[P, ID](implicit gp: GlobalID[ID]): PostDiscarder[P, ID] =
    new PostDiscarder[P, ID] {
      def postGlobal(content: P): Future[ID] = gp.postGlobal(content)
    }
}

trait ErasablePostBuffer[P, ID] extends GlobalPost[P, ID] { self =>
  val buffer: ArrayBuffer[(Option[P], ID, Set[ID])] = ArrayBuffer()

  def redirects: Map[ID, Set[ID]] =
    buffer.collect { case (None, id, preds) => id -> preds }.toMap

  def post(content: P, prev: Set[ID]): Future[ID] = {
    val idT = postGlobal(content)
    idT.map { id =>
      buffer += ((Some(content), id, prev))
      id
    }
  }

  def find[W](
      index: ID
  )(implicit pw: Postable[P, W, ID]): Option[(PostData[P, W, ID], Set[ID])] =
    buffer.find(_._2 == index).flatMap {
      case (pOpt, _, preds) =>
        pOpt.map(p => (PostData[P, W, ID](p, index), preds))
    }

  def skipDeletedStep(index: ID): Option[Set[ID]] =
    buffer.find(pd => pd._1.isEmpty && pd._2 == index).map(_._3)

  def bufferData[W](
      implicit pw: Postable[P, W, ID]
  ): Vector[PostData[_, W, ID]] =
    buffer.flatMap {
      case (pOpt, id, _) => pOpt.map(p => PostData[P, W, ID](p, id))
    }.toVector

  def bufferFullData[W](
      implicit pw: Postable[P, W, ID]
  ): Vector[(PostData[P, W, ID], ID, Set[ID])] =
    buffer.flatMap {
      case (pOpt, id, preds) =>
        pOpt.map(p => (PostData[P, W, ID](p, id), id, preds))
    }.toVector

}

@deprecated("using HoTTPostWeb", "soon")
case class ErasableWebBuffer[P, ID](buffer: ErasablePostBuffer[P, ID])(
    implicit pw: Postable[P, HoTTPost, ID]
) {
  def getPost(id: ID): Option[(PostData[_, HoTTPost, ID], Set[ID])] =
    buffer.find(id)

  def data: Vector[PostData[_, HoTTPost, ID]] = buffer.bufferData

  def fullData: Vector[(PostData[_, HoTTPost, ID], ID, Set[ID])] =
    buffer.bufferFullData
}

object PostBuffer {

  /**
    * creating a post buffer
    *
    * @param globalPost the supplier of the ID
    * @return buffer storing posts
    */
  def apply[P, ID](globalPost: => (P => Future[ID])): PostBuffer[P, ID] =
    new PostBuffer[P, ID] {
      def postGlobal(content: P): Future[ID] = globalPost(content)
    }

  def build[P, ID](implicit gp: GlobalID[ID]): PostBuffer[P, ID] =
    new PostBuffer[P, ID] {
      def postGlobal(content: P): Future[ID] = gp.postGlobal(content)
    }

  /**
    * content from buffer
    *
    * @param pb the buffer
    * @param id ID
    * @return content optionally
    */
  def get[P, ID](pb: PostBuffer[P, ID], id: ID): Option[P] =
    pb.buffer.find(_._2 == id).map(_._1)

  /**
    * immediate predecessor posts in buffer
    *
    * @param pb the buffer
    * @param id ID
    * @return set of IDs of immediate predecessors
    */
  def previous[P, ID](pb: PostBuffer[P, ID], id: ID): Set[ID] = {
    val withId = pb.buffer.filter(_._2 == id).toSet
    withId.flatMap(_._3)
  }

  /**
    * postability using a buffer, the main way posting is done
    *
    * @param buffer the buffer to which to post as a function of the web
    * @return postability
    */
  def bufferPost[P: TypeTag, W, ID](
      buffer: W => PostBuffer[P, ID]
  ): Postable[P, W, ID] = {
    def postFunc(p: P, web: W, ids: Set[ID]): Future[ID] =
      buffer(web).post(p, ids)
    Postable(postFunc)
  }
}

/**
  * typeclass for building HLists of postables based on HLists of buffers, but formally just returns object of type `P`
  */
trait BuildPostable[W, B, P] {
  def postable(buffer: W => B): P
}

object BuildPostable {
  def get[W, B, P](buffer: W => B)(implicit bp: BuildPostable[W, B, P]): P =
    bp.postable(buffer)

  implicit def hnilTriv[W]: BuildPostable[W, HNil, HNil] =
    new BuildPostable[W, HNil, HNil] {
      def postable(buffer: W => HNil): HNil = HNil
    }

  implicit def bufferCons[W, P: TypeTag, ID, Bt <: HList, Pt <: HList](
      implicit tailBuilder: BuildPostable[W, Bt, Pt]
  ): BuildPostable[W, PostBuffer[P, ID] :: Bt, Postable[P, W, ID] :: Pt] =
    new BuildPostable[W, PostBuffer[P, ID] :: Bt, Postable[P, W, ID] :: Pt] {
      def postable(
          buffer: W => PostBuffer[P, ID] :: Bt
      ): Postable[P, W, ID] :: Pt =
        PostBuffer.bufferPost((web: W) => buffer(web).head) :: tailBuilder
          .postable((web: W) => buffer(web).tail)
    }

  implicit def erasablebufferCons[W, P: TypeTag, ID, Bt <: HList, Pt <: HList](
      implicit tailBuilder: BuildPostable[W, Bt, Pt]
  ): BuildPostable[W, ErasablePostBuffer[P, ID] :: Bt, Postable[P, W, ID] :: Pt] =
    new BuildPostable[
      W,
      ErasablePostBuffer[P, ID] :: Bt,
      Postable[P, W, ID] :: Pt
    ] {
      def postable(
          buffer: W => ErasablePostBuffer[P, ID] :: Bt
      ): Postable[P, W, ID] :: Pt =
        ErasablePostBuffer.bufferPost((web: W) => buffer(web).head) :: tailBuilder
          .postable((web: W) => buffer(web).tail)
    }

  implicit def discardCons[W, P: TypeTag, ID, Bt <: HList, Pt <: HList](
      implicit tailBuilder: BuildPostable[W, Bt, Pt]
  ): BuildPostable[W, PostDiscarder[P, ID] :: Bt, Postable[P, W, ID] :: Pt] =
    new BuildPostable[W, PostDiscarder[P, ID] :: Bt, Postable[P, W, ID] :: Pt] {
      def postable(
          buffer: W => PostDiscarder[P, ID] :: Bt
      ): Postable[P, W, ID] :: Pt =
        PostDiscarder.discardPost((web: W) => buffer(web).head) :: tailBuilder
          .postable((web: W) => buffer(web).tail)
    }
}

/**
  * Wrapper for post content that should be posted, with the previous elements of the same type also posted, in general with transformations (e.g. rescale)
  *
  * @param content the content to be posted
  * @param transformation transformations of other posts, typically rescaling
  * @param pw postability of P
  * @param pq queryability of P
  */
case class SplitPost[P: TypeTag, Q: TypeTag, W: TypeTag, ID: TypeTag](
    content: P,
    transformation: Q => P
)(implicit val pw: Postable[P, W, ID], val qq: LocalQueryable[Q, W, ID])

object SplitPost {
  def simple[P: TypeTag, W: TypeTag, ID: TypeTag](content: P)(
      implicit pw: Postable[P, W, ID],
      qq: LocalQueryable[P, W, ID]
  ): SplitPost[P, P, W, ID] =
    SplitPost[P, P, W, ID](content, identity[P](_))

  def some[P: TypeTag, W: TypeTag, ID: TypeTag](content: P)(
      implicit pw: Postable[P, W, ID],
      qq: LocalQueryable[Some[P], W, ID]
  ): SplitPost[P, Some[P], W, ID] =
    SplitPost[P, Some[P], W, ID](content, _.value)

  implicit def splitPostable[P: TypeTag, Q: TypeTag, W: TypeTag, ID: TypeTag]
      : Postable[SplitPost[P, Q, W, ID], W, ID] = {
    def post(
        content: SplitPost[P, Q, W, ID],
        web: W,
        pred: Set[ID]
    ): Future[ID] = {
      content.pw.post(content.content, web, pred).map { postID =>
        val othersFutVec = content.qq.getAt(web, postID, (_) => true)
        othersFutVec.foreach { v =>
          v.foreach { x =>
            content.pw.post(content.transformation(x), web, pred)
          }
        }
        postID
      }
    }
    Postable.Impl(post)
  }
}

trait GlobalID[ID] {
  def postGlobal[P](content: P): Future[ID]
}

/**
  * allows posting globally and keeps count without stroing anything
  *
  * @param log logging on post
  */
class CounterGlobalID(log: Any => Unit = (_) => ())
    extends GlobalID[(Int, Int)] {
  var counter: Int = 0

  /**
    * post arbitrary content
    *
    * @param content content of some type
    * @return ID, consisting of an index and a hashCode
    */
  def postGlobal[P](content: P): Future[(Int, Int)] = {
    val index = counter
    counter += 1
    log(content)
    Future((counter, content.hashCode()))
  }
}
