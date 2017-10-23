import learning._
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._
import library._, MonoidSimple._
import math._
import scala.concurrent.duration._
import ProverTasks._

val tv = new TermEvolver(lambdaWeight = 0.0, piWeight = 0.0)

def seek(n: Double = 6) = {
  val s = theoremSearchTask(dist1, tv, math.pow(10.0, -n), 3.minutes, eqM(l)(r), decay = 10)
  val f = s.runAsync
  f.foreach{
    case Some(t) => println(s"Found Proof ${t.fansi}\nfor Theorem ${t.typ.fansi}")
    case None => println("could not find theorem")
  }
  f
}

def seekTraced(n: Double = 6) = {
  val s = theoremSearchTraceTask(dist1, tv, math.pow(10.0, -n), 3.minutes, eqM(l)(r), decay = 10)
  val f = s.runAsync
  f.foreach{
    case Some((t, path)) =>
      println(s"Found Proof ${t.fansi}\nfor Theorem ${t.typ.fansi}")
      path.foreach((s) => println(s"Use: ${s.fansi} for Lemma ${s.typ.fansi}"))
    case None => println("could not find theorem")
  }
  f
}


def explore(n: Double = 6) =
  {
    val s = theoremsExploreTask(dist1, tv, math.pow(10.0, -n), 3.minutes, decay = 10)
  val f = s.runAsync
  f.foreach{
    (v) => v.foreach{
      case (t, p) =>
        println(s"term: ${t.fansi}" )
        println(s"type: ${t.typ.fansi}")
        println(s"weights: $p")
    }
  }
  f
}
