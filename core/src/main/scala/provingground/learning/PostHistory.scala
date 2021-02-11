package provingground.learning
import scala.collection.View
import shapeless._
import scala.reflect.runtime.universe._
trait PostHistory[W, ID] {
  // the post itself and all its predecessors
  def findPost(web: W, index: ID): Option[(PostData[_, W, ID], Set[ID])]

  def predPosts(web: W, index: ID) =
    findPost(web, index).map(_._2).getOrElse(Set())

  // all posts as a view
  def allPosts(web: W): View[PostData[_, W, ID]]

  def apexPosts(web: W): Vector[PostData[_, W, ID]] = { // very inefficient since a lot of stuff is recomputed, should override if efficiency mattersI
    val v = allPosts(web).toVector
    val notApex = v.toSet.flatMap { (pd: PostData[_, W, ID]) =>
      predPosts(web, pd.id)
    }
    v.filterNot(p => notApex.contains(p.id))
  }

  def summary(web: W) = {
    allPosts(web).toVector.flatMap(pd => findPost(web, pd.id)).map {
      case (pd, preds) => (pd.id, pd.pw.tag.toString(), preds)
    }
  }

  def snapShot(web: W): WebState[W, ID] = WebState(web, apexPosts(web))

  def postTags(web: W): Vector[(TypeTag[_], ID, Option[Set[ID]])]

  def allTagged[Q](
      web: W
  )(implicit tag: TypeTag[Q]) =
    postTags(web).toSet.filter(_._1.tpe =:= tag.tpe).flatMap {
      case (_, id, _) => findPost(web, id).map(_._1)
    }

  def history(web: W, id: ID): LazyList[PostData[_, W, ID]] = {
    val next: (
        (Set[PostData[_, W, ID]], Set[ID])
    ) => (Set[PostData[_, W, ID]], Set[ID]) = {
      case (d, indices) =>
        val pairs = indices.map(findPost(web, _)).flatten
        (pairs.map(_._1), pairs.flatMap(_._2))
    }
    def stream: LazyList[(Set[PostData[_, W, ID]], Set[ID])] =
      ((Set.empty[PostData[_, W, (ID)]], Set(id))) #:: stream.map(next)
    stream.flatMap(_._1)
  }

  def redirects(web: W): Map[ID, Set[ID]]

  /**
    * latest answers in the query using history
    *
    * @param web the web with all posts
    * @param id where we query
    * @param answer transformation of posts
    * @return
    */
  def latestAnswers[Q](
      web: W,
      id: ID,
      answer: PostData[_, W, ID] => Option[Q]
  ): Set[Q] =
    findPost(web, id)
      .map {
        case (pd, preds) =>
          answer(pd)
            .map(Set(_))
            .getOrElse(
              preds.flatMap(pid => latestAnswers(web, pid, answer))
            )
      }
      .orElse(
        redirects(web)
          .get(id)
          .map(ids => ids.flatMap(rid => latestAnswers(web, rid, answer)))
      )
      .getOrElse(Set())

  def latestTagged[Q](
      web: W,
      id: ID
  )(implicit tag: TypeTag[Q]): Set[Q] =
    PostHistory
      .latestTaggedID(id, postTags(web), redirects(web))
      .flatMap { id =>
        findPost(web, id)
      }
      .map(_._1.content.asInstanceOf[Q])

  def previousAnswers[Q](
      web: W,
      id: ID
  )(implicit qw: Postable[Q, W, ID]): Vector[Q] =
    findPost(web, id)
      .map {
        case (pd, preds) =>
          val head = pd.getOpt[Q].toVector
          val tail = preds.toVector.flatMap(pid => previousAnswers(web, pid))
          head ++ tail
      }
      .orElse(
        redirects(web)
          .get(id)
          .map(ids => ids.toVector.flatMap(rid => previousAnswers(web, rid)))
      )
      .getOrElse(Vector())
      .distinct
}

object PostHistory {
  def latestTaggedID[Q, ID](
      id: ID,
      data: Vector[(TypeTag[_], ID, Option[Set[ID]])],
      redirects: Map[ID, Set[ID]]
  )(implicit tag: TypeTag[Q]): Set[ID] =
    data
      .find(d => d._1.tpe =:= tag.tpe && d._2 == id)
      .map(x => Set(x._2))
      .orElse(
        redirects
          .get(id)
          .map(ids => ids.flatMap(rid => latestTaggedID(rid, data, redirects)))
      )
      .getOrElse(Set())
  case class Empty[W, ID]() extends PostHistory[W, ID] {
    def findPost(web: W, index: ID): Option[(PostData[_, W, ID], Set[ID])] =
      None
    def allPosts(web: W): View[PostData[_, W, ID]] = Seq().view
    def redirects(web: W): Map[ID, Set[ID]]        = Map()

    def postTags(
        web: W
    ): Vector[(reflect.runtime.universe.TypeTag[_], ID, Option[Set[ID]])] =
      Vector()
  }

  case class Or[W, ID](first: PostHistory[W, ID], second: PostHistory[W, ID])
      extends PostHistory[W, ID] {
    def findPost(web: W, index: ID): Option[(PostData[_, W, ID], Set[ID])] =
      first.findPost(web, index).orElse(second.findPost(web, index))
    def allPosts(web: W): View[PostData[_, W, ID]] =
      first.allPosts(web) ++ second.allPosts(web)
    def redirects(web: W): Map[ID, Set[ID]] =
      first.redirects(web) ++ second.redirects(web)

    def postTags(
        web: W
    ): Vector[(reflect.runtime.universe.TypeTag[_], ID, Option[Set[ID]])] =
      first.postTags(web) ++ second.postTags(web)
  }

  def get[W, B, ID](buffer: W => B)(
      implicit hg: HistoryGetter[W, B, ID]
  ): PostHistory[W, ID] = hg.getHistory(buffer)
}

trait HistoryGetter[W, B, ID] {
  def getHistory(buffer: W => B): PostHistory[W, ID]
}

object HistoryGetter {
  def get[W, B, ID](buffer: W => B)(
      implicit hg: HistoryGetter[W, B, ID]
  ): PostHistory[W, ID] = hg.getHistory(buffer)

  implicit def nilGetter[W, ID]: HistoryGetter[W, HNil, ID] =
    new HistoryGetter[W, HNil, ID] {
      def getHistory(buffer: W => HNil): PostHistory[W, ID] =
        PostHistory.Empty[W, ID]()
    }

  implicit def consGetter[W, B1, B2 <: HList, ID](
      implicit g1: HistoryGetter[W, B1, ID],
      g2: HistoryGetter[W, B2, ID]
  ): HistoryGetter[W, B1 :: B2, ID] =
    new HistoryGetter[W, B1 :: B2, ID] {
      def getHistory(buffer: W => B1 :: B2): PostHistory[W, ID] =
        PostHistory.Or(
          g1.getHistory((w: W) => buffer(w).head),
          g2.getHistory((w: W) => buffer(w).tail)
        )
    }

  implicit def bufferGetter[P, W, ID](
      implicit pw: Postable[P, W, ID]
  ): HistoryGetter[W, PostBuffer[P, ID], ID] =
    new HistoryGetter[W, PostBuffer[P, ID], ID] {
      def getHistory(buffer: W => PostBuffer[P, ID]): PostHistory[W, ID] =
        new PostHistory[W, ID] {
          def findPost(
              web: W,
              index: ID
          ): Option[(PostData[_, W, ID], Set[ID])] = buffer(web).find(index)
          def allPosts(web: W): View[PostData[_, W, ID]] =
            buffer(web).bufferData.view
          def redirects(web: W): Map[ID, Set[ID]] = Map()

          def postTags(web: W): Vector[
            (reflect.runtime.universe.TypeTag[_], ID, Option[Set[ID]])
          ] = buffer(web).tagData
        }
    }

  implicit def erasableBufferGetter[P, W, ID](
      implicit pw: Postable[P, W, ID]
  ): HistoryGetter[W, ErasablePostBuffer[P, ID], ID] =
    new HistoryGetter[W, ErasablePostBuffer[P, ID], ID] {
      def getHistory(
          buffer: W => ErasablePostBuffer[P, ID]
      ): PostHistory[W, ID] =
        new PostHistory[W, ID] {
          def findPost(
              web: W,
              index: ID
          ): Option[(PostData[_, W, ID], Set[ID])] = buffer(web).find(index)
          def allPosts(web: W): View[PostData[_, W, ID]] =
            buffer(web).bufferData.view
          def redirects(web: W): Map[ID, Set[ID]] = buffer(web).redirects

          def postTags(web: W): Vector[
            (reflect.runtime.universe.TypeTag[_], ID, Option[Set[ID]])
          ] = buffer(web).tagData
        }
    }

  implicit def postDiscardGetter[P, W, ID](
      implicit pw: Postable[P, W, ID]
  ): HistoryGetter[W, PostDiscarder[P, ID], ID] =
    new HistoryGetter[W, PostDiscarder[P, ID], ID] {
      def getHistory(buffer: W => PostDiscarder[P, ID]): PostHistory[W, ID] =
        new PostHistory[W, ID] {
          def findPost(
              web: W,
              index: ID
          ): Option[(PostData[_, W, ID], Set[ID])]       = None
          def allPosts(web: W): View[PostData[_, W, ID]] = Seq().view
          def redirects(web: W): Map[ID, Set[ID]]        = Map()

          def postTags(web: W): Vector[
            (reflect.runtime.universe.TypeTag[_], ID, Option[Set[ID]])
          ] = Vector()
        }
    }
}
