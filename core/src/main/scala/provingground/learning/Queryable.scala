package provingground.learning

// import provingground._, HoTT._
// import monix.eval._, monix.tail._
import monix.execution.Scheduler.Implicits.global
import shapeless._
import scala.concurrent.Future
import scala.collection.SeqView


/* Code for mixed autonomous and interactive running.
 * We can interact by posting various objects.
 * There are also bots that post in reaction to other posts, in general using the results of queries.
 * The behaviour of a post is largely determined by its (scala) type.
 * The type `W` (for web) will handle posting and querying based on posts (this is implemented using typeclasses with `W`)
 */



/**
  * Typeclass for being able to query W for type Q
  */
trait Queryable[U, W] {
  def get(web: W, predicate: U => Boolean): Future[U]
}

object Queryable{
  /**
    * a simple query, with result blocking and no predicate
    *
    * @param func the query function
    * @return typeclass implementation based on `func`
    */
  def simple[U, W](func: W => U) = new Queryable[U, W] {
    def get(web: W, predicate: U => Boolean): Future[U] = 
      Future(func(web))

  implicit def gatherQuery[P, W, ID](implicit ph: PostHistory[W, ID], pw: Postable[P, W, ID]) : Queryable[GatherPost[P] , W] = 
    new Queryable[GatherPost[P], W] {
     def get(web: W, predicate: GatherPost[P] => Boolean): Future[GatherPost[P]] = 
      Future(
        GatherPost(ph.allPosts(web).flatMap(_.getOpt[P]).toSet)
      )
    }
  }

  /**
    * querying for objects of a type, as a view
    *
    * @param pw postability of `P`, needed for history
    * @param ph post history of `P` from `W`
    * @return implementation of querying typeclass
    */
  implicit def allView[P, W, ID](implicit pw: Postable[P, W, ID], ph: PostHistory[W, ID]) : Queryable[SeqView[P, Seq[_]], W] = 
    new Queryable[SeqView[P, Seq[_]], W] {
      def get(web: W, predicate: SeqView[P, Seq[_]] => Boolean): Future[SeqView[P, Seq[_]]] = 
       Future{ ph.allPosts(web).filter(_.pw.tag == pw.tag).map{_.asInstanceOf[P]}}
    }
    

}
/**
 * Typeclass for being able to query W for a vector of elements of type Q at an index
 */ 
trait LocalQueryable[U, W, ID] {
  def getAt(web: W, id: ID, predicate: U => Boolean): Future[Vector[U]]
}

/** 
  * Looking up an answer to a query for `U` from posts of type `P`
  * meant for querying for the latest post etc.
  *
  * @param func transformation of the content of the post
  * @param pw postability
  */
case class AnswerFromPost[P, U, W, ID](func: P => U)(
  implicit pw: Postable[P, W, ID]
) {
def fromPost[Q](data: PostData[Q, W, ID]) : Option[U] =
  if (pw.tag == data.pw.tag) Some(func(data.content.asInstanceOf[P])) else None
}

/**
  * Queries answered by taking the latest out of possibly many choices of posts to be looked up and transformed.
  *
  * @param answers various ways in which the query can be answered
  * @param h the history
  */
case class LatestAnswer[Q, W, ID](
  answers: Vector[AnswerFromPost[_, Q, W, ID]]
)(implicit h: PostHistory[W, ID])
  extends LocalQueryable[Q, W, ID] {
    def getAt(web: W, id: ID, predicate: Q => Boolean): Future[Vector[Q]] = Future{
      def answer: PostData[_, W, ID] => Option[Q] = 
        (pd) => answers.flatMap(ans => ans.fromPost(pd)).filter(predicate).headOption
      h.latestAnswers(web, id, answer).toVector


    }

    def ||[P](ans: P => Q)(implicit pw: Postable[P, W, ID]) = 
      LatestAnswer(
        answers :+ AnswerFromPost[P, Q, W, ID](ans)
      )
  }


trait FallBackLookups{
  /**
    * default implementation of lookup for stuff that can be posted given a post-history implementation,
    * where we look fot the latest post.
    *
    * @param qw postability of `Q`
    * @param ph post history
    * @return implementation of query lookup
    */
  implicit   def lookupLatest[Q, W, ID](implicit qw: Postable[Q, W, ID], ph: PostHistory[W, ID]) : LocalQueryable[Q, W, ID] = 
  LatestAnswer(Vector(AnswerFromPost[Q, Q, W, ID](identity)))

  /**
    * Lookup for the wrapped type `SomePost`, which returns all posts independent of relative position in history.
    *
    * @param pw postability of `P`
    * @param ph post history
    * @return implementation of query lookup
    */
  implicit def somePostQuery[P, W, ID](implicit pw: Postable[P, W, ID], ph: PostHistory[W, ID]) : LocalQueryable[SomePost[P], W, ID] = new LocalQueryable[SomePost[P], W, ID] {
   def getAt(web: W, id: ID, predicate: SomePost[P] => Boolean): Future[Vector[SomePost[P]]] =
     Future{ph.allPosts(web).filter(_.pw.tag == pw.tag).map{post => SomePost(post.content.asInstanceOf[P])}.filter(predicate).toVector
  } 
}
     
}

object LocalQueryable extends FallBackLookups{
  /**
    * Query for an object
    *
    * @param web the web which we query
    * @param predicate property the object must satisfy
    * @param q queryability
    * @return future result of type`Q`
    */
  def query[Q, W](web: W, predicate: Q => Boolean)(implicit q: Queryable[Q, W]):  Future[Q] = q.get(web, predicate)

  /**
    * Query for an object at a position, for example the latest before that position
    *
    * @param web the web which we query
    * @param id the position where we query
    * @param predicate property the object must satisfy
    * @param q queryability
    * @return future result of type`Q`
    */
  def queryAt[Q, W, ID](web: W, id: ID, predicate: Q => Boolean)(implicit q: LocalQueryable[Q, W, ID]) =
    q.getAt(web, id, predicate)

  /**
    * use a global query for local querying, ignoring position
    * 
    * @param q global queryability
    * @return local query implementation
    */
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
    * query for `Some[A]` using query for `A` by looking up history. Meant to be starting point for several options.
    *
    * @param qw query for `A`
    * @param ph post history
    * @return queryable for `Some[A]`
    */
  def answerAsSome[Q, W, ID](implicit qw: Postable[Q, W, ID], ph: PostHistory[W, ID]) : LatestAnswer[Some[Q],W,ID] = 
    LatestAnswer(Vector(AnswerFromPost[Q, Some[Q], W, ID](Some(_))))


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


  
  /**
    * query a conjunction of two queryable types
    * 
    * @param qu queryability of the first
    * @param qv queryability of the second
    * @return implemented queryability putting these together
    */
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

  /**
  * Querying for HNil
  *
  * @return trivial query response
  */
  implicit def hNilQueryable[W, ID] : LocalQueryable[HNil, W, ID] = new LocalQueryable[HNil, W, ID] {
      def getAt(web: W, id: ID, predicate: HNil => Boolean): Future[Vector[HNil]] = Future(Vector(HNil))
  }

  /**
  * Querying for Unit
  *
  * @return trivial query response
  */
  implicit def unitQueryable[W, ID] : LocalQueryable[Unit, W, ID] = new LocalQueryable[Unit, W, ID] {
    def getAt(web: W, id: ID, predicate: Unit => Boolean): Future[Vector[Unit]] = Future(Vector(()))
}
}



/**
  * Wrapper to query for all posts, even after the query position or in a different thread
  *
  * @param content the wrapped content
  */
case class SomePost[P](content: P)

case class GatherPost[P](contents: Set[P])

