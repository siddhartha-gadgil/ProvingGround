package provingground.interface

import provingground._
import HoTT._
import translation._
import learning._
import FineProverTasks._
import monix.execution.CancelableFuture
import ujson._
import monix.eval._
import monix.execution.Scheduler.Implicits.global
import scala.concurrent._

object MonoidServer {
  val tv = new TermEvolver(lambdaWeight = 0.0, piWeight = 0.0)

  val seek: Task[Option[(Term, Vector[Term])]] = {
    import library._, MonoidSimple._
    import scala.concurrent.duration._
    pprint.log("seeking proof in task")
    theoremSearchTraceTask(dist1,
                           tv,
                           math.pow(10.0, -6),
                           10.minutes,
                           eqM(l)(r),
                           decay = 3)
  }

  val seekResult: Task[ujson.Value] =
    seek.map {
      case Some((t, ls)) =>
        pprint.log("Found proof")
        val lemmaSeq =
          ls.map(
            (l) =>
              ujson.Obj(
                "term" -> TeXTranslate(l),
                "type" -> TeXTranslate(l.typ)
            )
          )
        ujson.Obj(
          "proved" -> true,
          "term"   -> TeXTranslate(t),
          "type"   -> TeXTranslate(t.typ),
          "lemmas" -> ujson.Arr(lemmaSeq: _*)
        )
      case None => ujson.Obj("proved" -> false)
    }

  def seekResultFut: Future[ujson.Value] = seekResult.runToFuture

}
