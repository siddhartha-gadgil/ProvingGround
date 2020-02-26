package provingground.learning

// import provingground._, HoTT._
// import monix.eval._, monix.tail._
import monix.execution.Scheduler.Implicits.global
import shapeless._
import scala.concurrent.Future
import scala.collection.SeqView
import scala.reflect.runtime.universe._
import provingground.learning.QueryFromPosts.CaseCons
import provingground.learning.QueryFromPosts.ModCons
import provingground.FiniteDistribution

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

object Queryable {

  /**
    * a simple query, with result blocking and no predicate
    *
    * @param func the query function
    * @return typeclass implementation based on `func`
    */
  def simple[U, W](func: W => U) = new Queryable[U, W] {
    def get(web: W, predicate: U => Boolean): Future[U] =
      Future(func(web))

  }

  implicit def gatherMapQuery[P, W, ID](
      implicit ph: PostHistory[W, ID],
      mpw: PostMaps[P]
  ): Queryable[GatherMapPost[P], W] =
    new Queryable[GatherMapPost[P], W] {
      def get(
          web: W,
          predicate: GatherMapPost[P] => Boolean
      ): Future[GatherMapPost[P]] = Future{
        val posts = ph.allPosts(web).toSet
        mpw.gather(posts)
      }
    }

  implicit def gatherQuery[P, W, ID](
      implicit ph: PostHistory[W, ID],
      pw: Postable[P, W, ID]
  ): Queryable[GatherPost[P], W] =
    new Queryable[GatherPost[P], W] {
      def get(
          web: W,
          predicate: GatherPost[P] => Boolean
      ): Future[GatherPost[P]] =
        Future(
          GatherPost(ph.allPosts(web).flatMap(_.getOpt[P]).toSet)
        )
    }

  /**
    * querying for objects of a type, as a view
    *
    * @param pw postability of `P`, needed for history
    * @param ph post history of `P` from `W`
    * @return implementation of querying typeclass
    */
  implicit def allView[P, W, ID](
      implicit pw: Postable[P, W, ID],
      ph: PostHistory[W, ID]
  ): Queryable[SeqView[P, Seq[_]], W] =
    new Queryable[SeqView[P, Seq[_]], W] {
      def get(
          web: W,
          predicate: SeqView[P, Seq[_]] => Boolean
      ): Future[SeqView[P, Seq[_]]] =
        Future {
          ph.allPosts(web).filter(_.pw.tag == pw.tag).map { _.asInstanceOf[P] }
        }
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
  def fromPost[Q](data: PostData[Q, W, ID]): Option[U] =
    if (pw.tag == data.pw.tag) Some(func(data.content.asInstanceOf[P]))
    else None
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
  def getAt(web: W, id: ID, predicate: Q => Boolean): Future[Vector[Q]] =
    Future {
      def answer: PostData[_, W, ID] => Option[Q] =
        (pd) =>
          answers.flatMap(ans => ans.fromPost(pd)).filter(predicate).headOption
      h.latestAnswers(web, id, answer).toVector

    }

  def ||[P](ans: P => Q)(implicit pw: Postable[P, W, ID]) =
    LatestAnswer(
      answers :+ AnswerFromPost[P, Q, W, ID](ans)
    )
}

trait FallBackLookups {

  /**
    * default implementation of lookup for stuff that can be posted given a post-history implementation,
    * where we look fot the latest post.
    *
    * @param qw postability of `Q`
    * @param ph post history
    * @return implementation of query lookup
    */
  implicit def lookupLatest[Q, W, ID](
      implicit qw: Postable[Q, W, ID],
      ph: PostHistory[W, ID]
  ): LocalQueryable[Q, W, ID] =
    LatestAnswer(Vector(AnswerFromPost[Q, Q, W, ID](identity)))

  /**
    * Lookup for the wrapped type `SomePost`, which returns all posts independent of relative position in history.
    *
    * @param pw postability of `P`
    * @param ph post history
    * @return implementation of query lookup
    */
  implicit def somePostQuery[P, W, ID](
      implicit pw: Postable[P, W, ID],
      ph: PostHistory[W, ID]
  ): LocalQueryable[SomePost[P], W, ID] =
    new LocalQueryable[SomePost[P], W, ID] {
      def getAt(
          web: W,
          id: ID,
          predicate: SomePost[P] => Boolean
      ): Future[Vector[SomePost[P]]] =
        Future {
          ph.allPosts(web)
            .filter(_.pw.tag == pw.tag)
            .map { post =>
              SomePost(post.content.asInstanceOf[P])
            }
            .filter(predicate)
            .toVector
        }
    }

}

object LocalQueryable extends FallBackLookups {

  /**
    * Query for an object
    *
    * @param web the web which we query
    * @param predicate property the object must satisfy
    * @param q queryability
    * @return future result of type`Q`
    */
  def query[Q, W](web: W, predicate: Q => Boolean)(
      implicit q: Queryable[Q, W]
  ): Future[Q] = q.get(web, predicate)

  /**
    * Query for an object at a position, for example the latest before that position
    *
    * @param web the web which we query
    * @param id the position where we query
    * @param predicate property the object must satisfy
    * @param q queryability
    * @return future result of type`Q`
    */
  def queryAt[Q, W, ID](web: W, id: ID, predicate: Q => Boolean)(
      implicit q: LocalQueryable[Q, W, ID]
  ) =
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
      def getAt(web: W, id: ID, predicate: U => Boolean): Future[Vector[U]] =
        q.get(web, predicate).map(Vector(_))
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
  def answerAsSome[Q, W, ID](
      implicit qw: Postable[Q, W, ID],
      ph: PostHistory[W, ID]
  ): LatestAnswer[Some[Q], W, ID] =
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
  ): LocalQueryable[U :: V, W, ID] = new LocalQueryable[U :: V, W, ID] {
    def getAt(
        web: W,
        id: ID,
        predicate: U :: V => Boolean
    ): Future[Vector[U :: V]] =
      (for {
        head <- qu.getAt(web, id, (_) => true)
        tail <- qv.getAt(web, id, (_) => true)
      } yield head.flatMap(x => tail.map(y => x :: y))).map(_.filter {
        case pair => predicate(pair)
      })

  }

  /**
    * Querying for HNil
    *
    * @return trivial query response
    */
  implicit def hNilQueryable[W, ID]: LocalQueryable[HNil, W, ID] =
    new LocalQueryable[HNil, W, ID] {
      def getAt(
          web: W,
          id: ID,
          predicate: HNil => Boolean
      ): Future[Vector[HNil]] = Future(Vector(HNil))
    }

  /**
    * Querying for Unit
    *
    * @return trivial query response
    */
  implicit def unitQueryable[W, ID]: LocalQueryable[Unit, W, ID] =
    new LocalQueryable[Unit, W, ID] {
      def getAt(
          web: W,
          id: ID,
          predicate: Unit => Boolean
      ): Future[Vector[Unit]] = Future(Vector(()))
    }
}

/**
  * Wrapper to query for all posts, even after the query position or in a different thread
  *
  * @param content the wrapped content
  */
case class SomePost[P](content: P)

case class GatherPost[P](contents: Set[P])

case class GatherMapPost[P](contents: Set[P])

sealed trait PostMaps[P] {
  def getOpt[W, ID](pd: PostData[_, W, ID]): Option[P]

  def gather[W, ID](pds: Set[PostData[_, W, ID]]): GatherMapPost[P] =
    GatherMapPost(pds.flatMap(getOpt(_)))

  def ||[Q: TypeTag](f: Q => P) = PostMaps.Cons(f, this)
}
object PostMaps {
  def empty[P] = Empty[P]
  case class Empty[P]() extends PostMaps[P] {
    def getOpt[W, ID](pd: PostData[_, W, ID]): Option[P] = None
  }

  case class Cons[Q, P](transform: Q => P, tail: PostMaps[P])(
      implicit val qt: TypeTag[Q]
  ) extends PostMaps[P] {
    def getOpt[W, ID](pd: PostData[_, W, ID]): Option[P] =
      pd.getTagOpt[Q].map(transform).orElse(tail.getOpt(pd))
  }
}

case class QueryOptions[Q, W, ID](
    answers: PostData[_, W, ID] => Option[Q],
    modifiers: PostData[_, W, ID] => Q => Q
) {
  def addAnswer(fn: PostData[_, W, ID] => Option[Q]) =
    QueryOptions(
      (pd: PostData[_, W, ID]) => fn(pd).orElse(answers(pd)),
      modifiers
    )

  def addMod(fn: PostData[_, W, ID] => Q => Q) =
    QueryOptions(
      answers,
      (pd: PostData[_, W, ID]) => (q: Q) => fn(pd)(modifiers(pd)(q))
    )
}

object QueryOptions {
  def latest[Q, W, ID](web: W, id: ID, q: QueryOptions[Q, W, ID])(
      implicit ph: PostHistory[W, ID]
  ): Set[Q] = {
    import ph._
    findPost(web, id)
      .map {
        case (pd, preds) =>
          q.answers(pd)
            .map(Set(_))
            .getOrElse(
              preds.flatMap(pid => latest(web, pid, q).map(q.modifiers(pd)))
            )
      }
      .orElse(
        redirects(web)
          .get(id)
          .map(ids => ids.flatMap(rid => latest(web, rid, q)))
      )
      .getOrElse(Set())
  }

  def get[Q, W, ID, PList <: HList](
      qp: QueryFromPosts[Q, PList]
  )(implicit builder: BuildQuery[Q, W, ID, PList]): QueryOptions[Q, W, ID] =
    builder.build(qp)
}

sealed trait QueryFromPosts[Q, PList <: HList] {
  def addCons[P](answer: P => Option[Q]): QueryFromPosts[Q, P :: PList] =
    CaseCons(answer, this)

  def addMod[P](modifier: P => Q => Q): QueryFromPosts[Q, P :: PList] =
    ModCons(modifier, this)
}

object QueryFromPosts {
  case class Empty[Q]() extends QueryFromPosts[Q, HNil]

  case class CaseCons[Q, P, Pt <: HList](
      answer: P => Option[Q],
      tail: QueryFromPosts[Q, Pt]
  ) extends QueryFromPosts[Q, P :: Pt]

  case class ModCons[Q, P, Pt <: HList](
      modifier: P => Q => Q,
      tail: QueryFromPosts[Q, Pt]
  ) extends QueryFromPosts[Q, P :: Pt]
}

trait BuildQuery[Q, W, ID, PList <: HList] {
  def build(qp: QueryFromPosts[Q, PList]): QueryOptions[Q, W, ID]
}

object BuildQuery {
  implicit def empty[Q, W, ID]: BuildQuery[Q, W, ID, HNil] =
    new BuildQuery[Q, W, ID, HNil] {
      def build(qp: QueryFromPosts[Q, HNil]): QueryOptions[Q, W, ID] =
        QueryOptions(_ => None, _ => identity)
    }

  implicit def cons[Q, W, ID, P, PList <: HList](
      implicit pw: Postable[P, W, ID],
      tailQ: BuildQuery[Q, W, ID, PList],
      tag: TypeTag[P]
  ): BuildQuery[Q, W, ID, P :: PList] =
    new BuildQuery[Q, W, ID, P :: PList] {
      def build(qp: QueryFromPosts[Q, P :: PList]): QueryOptions[Q, W, ID] =
        qp match {
          case CaseCons(answer, tail) =>
            def headFunc(data: PostData[_, W, ID]): Option[Q] =
              if (data.pw.tag == tag) answer(data.content.asInstanceOf[P])
              else None
            tailQ.build(tail).addAnswer(headFunc(_))
          case ModCons(modifier, tail) =>
            def headFunc(data: PostData[_, W, ID]): Q => Q =
              if (data.pw.tag == tag) modifier(data.content.asInstanceOf[P])
              else identity
            tailQ.build(tail).addMod(headFunc(_))
        }
    }
}

/**
  * Have a wrapper type T for the query and make the companion object extend this class, giving query-from-posts as
  * input (whose type should be deducable)
  *
  * @param qp the query from posts
  * @param incl inclusion into the wrapper type.
  */
class QueryImplicitWrap[Q, T, PList <: HList](
    qp: QueryFromPosts[Q, PList],
    incl: Q => T
) {
  implicit def query[W, ID](
      implicit ph: PostHistory[W, ID],
      bp: BuildQuery[Q, W, ID, PList]
  ): LocalQueryable[T, W, ID] =
    new LocalQueryable[T, W, ID] {
      def getAt(web: W, id: ID, predicate: T => Boolean): Future[Vector[T]] = {
        val queryOptions: QueryOptions[Q, W, ID] = QueryOptions.get(qp)
        Future {
          QueryOptions.latest(web, id, queryOptions).toVector.map(incl)
        }
      }
    }
}

class QueryImplicit[Q, PList <: HList](
    qp: QueryFromPosts[Q, PList]
) extends QueryImplicitWrap[Q, Q, PList](qp, identity(_))

object TestCustomQuery {
  import provingground._, HoTT._
  case class TestWrap(fd: TermState)

  import HoTTPost._

  val qp = QueryFromPosts
    .Empty[TermState]
    .addCons((lp: LocalProver) => Some(lp.initState))

  val testWrap = (t: TermState) => TestWrap(t)

  object TestWrapImpl extends QueryImplicitWrap(qp, testWrap)

  val resolved: LocalQueryable[TestWrap, HoTTPostWeb, ID] = TestWrapImpl.query

  import TestWrapImpl._

  val testImp = implicitly[LocalQueryable[TestWrap, HoTTPostWeb, ID]]

  val qp1 = QueryFromPosts
    .Empty[FiniteDistribution[Term]]
    .addCons((lp: LocalProver) => Some(lp.initState.terms))

  // object FDImpl extends QueryImplicit(qp1)

  implicit val tres: LocalQueryable[FiniteDistribution[Term], HoTTPostWeb, ID] =
    new QueryImplicit(qp1).query // not clear why this needs to be implicit, while an implicit was picked up earlier

  // import FDImpl._

  val fdTest =
    implicitly[LocalQueryable[FiniteDistribution[Term], HoTTPostWeb, ID]]
}
