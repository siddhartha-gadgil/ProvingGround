package provingground.learning
import scala.concurrent._
import scala.reflect.runtime.universe._
import shapeless._

/**
 * Typeclass for being able to post with content type P in W
 */
trait Postable[P, W, ID] {
  def post(content: P, web: W, pred: Set[ID]): Future[ID]
  val tag: TypeTag[P]
}

object Postable {
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  /**
    * Making a postable
    *
    * @param postFunc the posting function
    * @return a Postable
    */
  def apply[P : TypeTag, W, ID](postFunc: (P, W, Set[ID]) => Future[ID]) : Postable[P, W, ID] = 
    Impl(postFunc)

    /**
      * A concrete implementation of postable
      *
      * @param postFunc the posting function
      */
  case class Impl[P, W, ID](postFunc: (P, W, Set[ID]) => Future[ID])(implicit val tag: TypeTag[P]) extends Postable[P, W, ID] {
        def post(content: P, web: W, pred: Set[ID]): Future[ID] = 
            postFunc(content, web, pred)
    }

    /**
      * post and return post data
      *
      * @param content content to be posted
      * @param web web where we post
      * @param pred predecessors of the post
      * @param pw postability of the type
      * @return PostData as a future
      */
  def postFuture[P, W, ID](content: P, web: W, pred: Set[ID])(implicit pw: Postable[P, W, ID]) : Future[PostData[P, W, ID]] = 
    pw.post(content, web, pred).map{id => PostData(content, id)} 

    /**
      * post in a buffer
      *
      * @return postability
      */
  implicit def bufferPostable[P: TypeTag, ID, W <: PostBuffer[P, ID]]
      : Postable[P, W, ID] =
     {
      def post(content: P, web: W, pred: Set[ID]): Future[ID] =
        web.post(content, pred)
      Impl(post)
    }
  
    /**
      * post an Option[P], posting a Unit in case of `None`
      *
      * @param pw postability of `P`
      * @param uw postability of a Unit
      * @return postability of `Option[P]`
      */
  implicit def optionPostable[P : TypeTag, W, ID](implicit pw: Postable[P, W, ID], uw: Postable[Unit, W, ID]) : Postable[Option[P], W, ID] = 
    {
      def post(content: Option[P], web: W, pred: Set[ID]): Future[ID] = 
        content.fold(uw.post((), web, pred))(c => pw.post(c, web, pred))
        Impl(post)
    }

  /**
    * post a `Try[P]` by posting `P` or an error 
    *
    * @param pw postability of `P`
    * @param ew postability of a `Throwable`
    * @return postability of `Try[P]`
    */
  implicit def tryPostable[P : TypeTag, W, ID](implicit pw: Postable[P, W, ID], ew: Postable[Throwable, W, ID]) : Postable[util.Try[P], W, ID] = 
    {
      def post(content: util.Try[P], web: W, pred: Set[ID]): Future[ID] =
        content.fold(err => ew.post(err, web, pred), c => pw.post(c, web, pred))

      Impl(post)
    }

  implicit def eitherPostable[P : TypeTag, Q: TypeTag, W, ID](implicit pw: Postable[P, W, ID], qw: Postable[Q, W, ID]) : Postable[Either[P, Q], W, ID] =
    {
     def post(content: Either[P, Q], web: W, pred: Set[ID]): Future[ID] =
      content.fold(
        fa => pw.post(fa, web, pred), 
        fb => qw.post(fb, web, pred))
    Impl(post)
    }

  implicit def pairPost[P: TypeTag, Q <: HList : TypeTag, W, ID](implicit pw: Postable[P, W, ID], qw: Postable[Q, W, ID]) : Postable[P :: Q, W, ID] =
    {
     def post(content: P :: Q, web: W, pred: Set[ID]): Future[ID] = 
      content match {
        case p :: q => 
          qw.post(q, web, pred).flatMap(id => pw.post(p, web, Set(id)))
      }
    Impl(post)
    }

  /**
    * Post a `Right[P]`, to be used to split the stream with a change or not.
    *
    * @param pw postability of `P`
    * @param uw postability of `Unit`
    * @return Postability of `Right[P]`
    */
  implicit def rightPost[X : TypeTag, P : TypeTag, W, ID](implicit pw: Postable[P, W, ID], uw: Postable[Unit, W, ID]) : Postable[Right[X, P], W, ID] = 
    {
      def post(content: Right[X, P], web: W, pred: Set[ID]): Future[ID] = 
        {
          uw.post((), web, pred).foreach(_ => ())
          pw.post(content.value, web, pred)
        }
        
        Impl(post)
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
  def getOpt[Q](implicit qw: Postable[Q, W, ID]) : Option[Q] = if (pw.tag == qw.tag) Some(content.asInstanceOf[Q]) else None 

  def getTagOpt[Q](implicit tag: TypeTag[Q]): Option[Q] = if (pw.tag == tag) Some(content.asInstanceOf[Q]) else None 
}