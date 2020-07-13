package provingground.learning

import monix.execution.Scheduler.Implicits.global
import shapeless._
import scala.concurrent.Future
import scala.collection.mutable.ArrayBuffer
import scala.util._
import scala.reflect.runtime.universe._
import provingground._, Utils.logger
import ujson.Value

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

  def postAt(content: P, id: ID, prev: Set[ID]): Future[Unit] =
    Future {
      buffer.append((content, id, prev))
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
  var forgetDefault: Boolean = false

  def bufferPost[P: TypeTag, W, ID](
      buffer: W => ErasablePostBuffer[P, ID]
  ): BiPostable[P, W, ID] = {
    new BiPostable[P, W, ID] {
      def allPosts(web: W): Vector[(P, ID, Set[ID])] =
        buffer(web).buffer.toVector.flatMap {
          case (opt, id, preds) =>
            opt.map { content =>
              (content, id, preds)
            }
        }

      def post(content: P, web: W, pred: Set[ID]): Future[ID] = {
        val idF = buffer(web).post(content, pred)
        idF.map { id =>
          logger.info(s"Post; tag: ${implicitly[TypeTag[P]]}, id: ${id}")
          logger.debug(
            s"Full post; tag: ${implicitly[TypeTag[P]]}, id: ${id}, content:\n${content}"
          )
          id
        }
      }

      val tag: reflect.runtime.universe.TypeTag[P] = implicitly

      def postAt(content: P, web: W, id: ID, pred: Set[ID]): Future[Unit] =
        Future {
          buffer(web).buffer.append((Some(content), id, pred))
        }

    }
  }

  def build[P, ID](frgtThisOpt: Option[Boolean] = None)(implicit gp: GlobalID[ID]): ErasablePostBuffer[P, ID] =
    new ErasablePostBuffer[P, ID] {
      var forgetThisOpt: Option[Boolean] = frgtThisOpt

      def postGlobal(content: P): Future[ID] = gp.postGlobal(content)
    }

}

trait PostDiscarder[P, ID] extends GlobalPost[P, ID] {
  val zero: ID
}

object PostDiscarder {
  def discardPost[P: TypeTag, W, ID](
      buffer: W => PostDiscarder[P, ID]
  ): BiPostable[P, W, ID] = new BiPostable[P, W, ID] {
    def post(content: P, web: W, pred: Set[ID]): Future[ID] =
      Future(buffer(web).zero)
    val tag: reflect.runtime.universe.TypeTag[P] = implicitly

    def postAt(content: P, web: W, id: ID, pred: Set[ID]): Future[Unit] =
      Future(())

    def allPosts(web: W): Vector[(P, ID, Set[ID])] = Vector()

  }

  def build[P, ID](nop: ID)(implicit gp: GlobalID[ID]): PostDiscarder[P, ID] =
    new PostDiscarder[P, ID] {
      val zero: ID                           = nop
      def postGlobal(content: P): Future[ID] = gp.postGlobal(content)
    }
}

trait ErasablePostBuffer[P, ID] extends GlobalPost[P, ID] { self =>
  var forgetThisOpt : Option[Boolean]

  def forgetPosts: Boolean = forgetThisOpt.getOrElse(ErasablePostBuffer.forgetDefault)

  val buffer: ArrayBuffer[(Option[P], ID, Set[ID])] = ArrayBuffer()

  def redirects: Map[ID, Set[ID]] =
    buffer.collect { case (None, id, preds) => id -> preds }.toMap

  def post(content: P, prev: Set[ID]): Future[ID] = {
    val idT = postGlobal(content)
    idT.map { id =>
      if (forgetPosts) buffer += ((None, id, prev))
      else buffer += ((Some(content), id, prev))
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

  def build[P, ID]()(implicit gp: GlobalID[ID]): PostBuffer[P, ID] =
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
  ): BiPostable[P, W, ID] = {
    new BiPostable[P, W, ID] {
      def post(content: P, web: W, pred: Set[ID]): Future[ID] = {
        val idF = buffer(web).post(content, pred)
        idF.map { id =>
          logger.info(
            s"Post; tag: ${implicitly[TypeTag[P]]}, id: ${id}, content:\n${content}"
          )
          id
        }
      }

      val tag: reflect.runtime.universe.TypeTag[P] = implicitly

      def postAt(content: P, web: W, id: ID, pred: Set[ID]): Future[Unit] =
        buffer(web).postAt(content, id, pred)

      def allPosts(web: W): Vector[(P, ID, Set[ID])] =
        buffer(web).buffer.toVector

    }
  }
}

/**
  * typeclass for building HLists of postables (and other things) based on HLists of buffers, but formally just returns object of type `P`
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
  ): BuildPostable[W, PostBuffer[P, ID] :: Bt, BiPostable[P, W, ID] :: Pt] =
    new BuildPostable[W, PostBuffer[P, ID] :: Bt, BiPostable[P, W, ID] :: Pt] {
      def postable(
          buffer: W => PostBuffer[P, ID] :: Bt
      ): BiPostable[P, W, ID] :: Pt =
        PostBuffer.bufferPost((web: W) => buffer(web).head) :: tailBuilder
          .postable((web: W) => buffer(web).tail)
    }

  implicit def erasablebufferCons[W, P: TypeTag, ID, Bt <: HList, Pt <: HList](
      implicit tailBuilder: BuildPostable[W, Bt, Pt]
  ): BuildPostable[W, ErasablePostBuffer[P, ID] :: Bt, BiPostable[P, W, ID] :: Pt] =
    new BuildPostable[
      W,
      ErasablePostBuffer[P, ID] :: Bt,
      BiPostable[P, W, ID] :: Pt
    ] {
      def postable(
          buffer: W => ErasablePostBuffer[P, ID] :: Bt
      ): BiPostable[P, W, ID] :: Pt =
        ErasablePostBuffer.bufferPost((web: W) => buffer(web).head) :: tailBuilder
          .postable((web: W) => buffer(web).tail)
    }

  implicit def discardCons[W, P: TypeTag, ID, Bt <: HList, Pt <: HList](
      implicit tailBuilder: BuildPostable[W, Bt, Pt]
  ): BuildPostable[W, PostDiscarder[P, ID] :: Bt, BiPostable[P, W, ID] :: Pt] =
    new BuildPostable[W, PostDiscarder[P, ID] :: Bt, BiPostable[P, W, ID] :: Pt] {
      def postable(
          buffer: W => PostDiscarder[P, ID] :: Bt
      ): BiPostable[P, W, ID] :: Pt =
        PostDiscarder.discardPost((web: W) => buffer(web).head) :: tailBuilder
          .postable((web: W) => buffer(web).tail)
    }
}

trait BuffersJson[W, B] {
  def save(web: W, buffers: W => B): Future[ujson.Value]

  def load(web: W, buffers: W => B, js: ujson.Value): Future[Unit]
}

object BuffersJson {

  implicit def hnilJson[W]: BuffersJson[W, HNil] = new BuffersJson[W, HNil] {
    def save(web: W, buffers: W => HNil): Future[Value] =
      Future(ujson.Obj())

    def load(web: W, buffers: W => HNil, js: Value): Future[Unit] = Future(())

  }

  implicit def pairJson[W, B1, B2](
      implicit bj1: BuffersJson[W, B1],
      bj2: BuffersJson[W, B2]
  ): BuffersJson[W, (B1, B2)] =
    new BuffersJson[W, (B1, B2)] {
      def save(web: W, buffers: W => (B1, B2)): Future[Value] =
        for {
          first  <- bj1.save(web, w => buffers(w)._1)
          second <- (bj2.save(web, w => buffers(w)._2))
        } yield first.obj ++ second.obj

      def load(web: W, buffers: W => (B1, B2), js: Value): Future[Unit] = {
        bj1.load(web, (w) => buffers(w)._1, js)
        bj2.load(web, (w) => buffers(w)._2, js)
      }
    }

  import upickle.default._

  implicit def consJson[P, W, ID, B <: HList](
      implicit bt: BuffersJson[W, B],
      tag: TypeTag[P],
      rwP: ReadWriter[P],
      rwID: ReadWriter[ID]
  ): BuffersJson[W, BiPostable[P, W, ID] :: B] =
    new BuffersJson[W, BiPostable[P, W, ID] :: B] {
      def save(web: W, buffers: W => BiPostable[P, W, ID] :: B): Future[Value] =
        bt.save(web, w => buffers(w).tail).map { js =>
          val posts = buffers(web).head.allPosts(web)
          js(tag.toString()) = write(posts)
          js
        }

      def load(
          web: W,
          buffers: W => BiPostable[P, W, ID] :: B,
          js: Value
      ): Future[Unit] =
        bt.load(web, w => buffers(w).tail, js).map { _ =>
          val v = js.obj(tag.toString())
          buffers(web).head.postAll(web, read[Vector[(P, ID, Set[ID])]](v))
        }

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
