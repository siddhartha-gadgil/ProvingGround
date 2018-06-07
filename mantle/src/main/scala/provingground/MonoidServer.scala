package provingground.interface

import provingground._
import HoTT._
import translation._
import learning._
import ProverTasks._
import monix.execution.CancelableFuture
import ujson._
import monix.eval._
import monix.execution.Scheduler.Implicits.global
import scala.concurrent._

object MonoidServer{
  val tv = new TermEvolver(lambdaWeight = 0.0, piWeight = 0.0)

  val seek: Task[Option[Term]] = {
    import library._, MonoidSimple._
    import scala.concurrent.duration._
    theoremSearchTask(dist1,
                      tv,
                      math.pow(10.0, -6),
                      10.minutes,
                      eqM(l)(r),
                      decay = 3)
  }

  val seekResult : Task[Js.Value] =
    seek.map{
      case Some(t) =>
        Js.Obj(
          "proved" -> true,
          "term" -> TeXTranslate(t),
          "type" -> TeXTranslate(t.typ)
        )
      case None => Js.Obj("proved" -> false)
    }

  def seekResultFut : Future[Js.Value] = seekResult.runAsync

}
