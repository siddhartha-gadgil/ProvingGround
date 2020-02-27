package provingground.learning
import scala.collection.SeqView
import shapeless._

trait PostHistory[W, ID] {
  // the post itself and all its predecessors
  def findPost(web: W, index: ID) :  Option[(PostData[_, W, ID], Set[ID])]

  // all posts as a view
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

  def redirects(web: W) : Map[ID, Set[ID]] 

  /**
    * latest answers in the query using history
    *
    * @param web the web with all posts
    * @param id where we query
    * @param answer transformation of posts
    * @return
    */
  def latestAnswers[Q](web: W, id: ID, answer: PostData[_, W, ID] => Option[Q]) : Set[Q] = 
    findPost(web, id).map{
      case (pd, preds) => 
        answer(pd).map(Set(_)).getOrElse(
          preds.flatMap(pid => latestAnswers(web, pid, answer))
          )
    }.orElse(redirects(web).get(id).map(ids => ids.flatMap(rid => latestAnswers(web, rid, answer)))
    ).getOrElse(Set())
}

object PostHistory{
  case class Empty[W, ID]() extends PostHistory[W, ID]{
    def findPost(web: W, index: ID): Option[(PostData[_, W, ID], Set[ID])] = None
    def allPosts(web: W): SeqView[PostData[_, W, ID],Seq[_]] = Seq().view
    def redirects(web: W): Map[ID,Set[ID]] = Map()
  }

  case class Or[W, ID](first: PostHistory[W, ID], second: PostHistory[W, ID]) extends PostHistory[W, ID]{
    def findPost(web: W, index: ID): Option[(PostData[_, W, ID], Set[ID])] = first.findPost(web, index).orElse(second.findPost(web, index))
    def allPosts(web: W): SeqView[PostData[_, W, ID],Seq[_]] = first.allPosts(web) ++ second.allPosts(web)
    def redirects(web: W): Map[ID,Set[ID]] = first.redirects(web) ++ second.redirects(web)
  }

  def get[W, B, ID](buffer: W => B)(implicit hg : HistoryGetter[W, B, ID]) : PostHistory[W, ID] = hg.getHistory(buffer)
}

trait HistoryGetter[W, B, ID]{
  def getHistory(buffer: W => B) : PostHistory[W, ID]
}

object HistoryGetter{
  def get[W, B, ID](buffer: W => B)(implicit hg: HistoryGetter[W, B, ID]) : PostHistory[W,ID] = hg.getHistory(buffer)

  implicit def nilGetter[W, ID] : HistoryGetter[W, HNil, ID] = 
    new HistoryGetter[W, HNil, ID] {
      def getHistory(buffer: W => HNil): PostHistory[W,ID] = PostHistory.Empty[W, ID]
    }
  
  implicit def consGetter[W, B1, B2 <: HList, ID](implicit g1: HistoryGetter[W, B1, ID], g2: HistoryGetter[W, B2, ID]) : HistoryGetter[W, B1 :: B2, ID] =
    new HistoryGetter[W, B1 :: B2, ID] {
      def getHistory(buffer: W => B1 :: B2): PostHistory[W,ID] = 
      PostHistory.Or(
        g1.getHistory((w: W) => buffer(w).head),
        g2.getHistory((w: W) => buffer(w).tail)
      )
    }

  implicit def bufferGetter[P, W, ID](implicit pw: Postable[P, W, ID]) : HistoryGetter[W, PostBuffer[P, ID], ID] = 
    new HistoryGetter[W, PostBuffer[P, ID], ID] {
      def getHistory(buffer: W => PostBuffer[P,ID]): PostHistory[W,ID] = 
        new PostHistory[W, ID] {
          def findPost(web: W, index: ID): Option[(PostData[_, W, ID], Set[ID])] = buffer(web).find(index)
          def allPosts(web: W): SeqView[PostData[_, W, ID],Seq[_]] = buffer(web).bufferData.view
          def redirects(web: W): Map[ID,Set[ID]] = Map()
        }
    }

  implicit def erasableBufferGetter[P, W, ID](implicit pw: Postable[P, W, ID]) : HistoryGetter[W, ErasablePostBuffer[P, ID], ID] = 
    new HistoryGetter[W, ErasablePostBuffer[P, ID], ID] {
      def getHistory(buffer: W => ErasablePostBuffer[P,ID]): PostHistory[W,ID] = 
        new PostHistory[W, ID] {
          def findPost(web: W, index: ID): Option[(PostData[_, W, ID], Set[ID])] = buffer(web).find(index)
          def allPosts(web: W): SeqView[PostData[_, W, ID],Seq[_]] = buffer(web).bufferData.view
          def redirects(web: W): Map[ID,Set[ID]] = buffer(web).redirects
        }
    }
  
  implicit def postDiscardGetter[P, W, ID](implicit pw: Postable[P, W, ID]) : HistoryGetter[W, PostDiscarder[P, ID], ID] =
    new  HistoryGetter[W, PostDiscarder[P, ID], ID]{
      def getHistory(buffer: W => PostDiscarder[P,ID]): PostHistory[W,ID] = 
        new PostHistory[W, ID] {
          def findPost(web: W, index: ID): Option[(PostData[_, W, ID], Set[ID])] = None
          def allPosts(web: W): SeqView[PostData[_, W, ID],Seq[_]] = Seq().view
          def redirects(web: W): Map[ID,Set[ID]] = Map()
        }
    }
}

