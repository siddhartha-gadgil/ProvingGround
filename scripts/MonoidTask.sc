import provingground._, HoTT._, translation._, FansiShow._
import learning._
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._
import library._, MonoidSimple._
import math._
import scala.concurrent.duration._
import ProverTasks._, FineProverTasks._

val tv = new TermEvolver(lambdaWeight = 0.0, piWeight = 0.0)

def seek(n: Double = 6) = {
  val s = theoremSearchTask(dist1, tv, math.pow(10.0, -n), 3.minutes, eqM(l)(r), decay = 3)
  val f = s.runAsync
  f.foreach{
    case Some(t) => println(s"Found Proof ${t.fansi}\nfor Theorem ${t.typ.fansi}")
    case None => println("could not find theorem")
  }
  f
}

def seekTraced(n: Double = 6) = {
  val s = theoremSearchTraceTask(dist1, tv, math.pow(10.0, -n), 3.minutes, eqM(l)(r), decay = 3)
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
    val s = theoremsExploreTask(dist1, tv, math.pow(10.0, -n), 3.minutes, decay = 3)
  val f = s.runAsync
  f.foreach{
    (v) => v.foreach{
        case (thm, v) =>
          println(s"theorem: ${thm.fansi}")
          println(s"proof: ${v.head._1.fansi}" )
          println(s"weight: ${v.head._2}")
          println()
    }
  }
  f
}

def exploreVars(n: Double = 6) =
  {
    val s = theoremsExploreTask(
      (dist1.map(_.replace(r, l)) + (a, 0.15) + (b, 0.08)).normalized(),
      tv, math.pow(10.0, -n), 3.minutes, decay = 3, vars = Vector(a, b))
  val f = s.runAsync
  f.foreach{
    (v) => v.foreach{
        case (thm, v) =>
          println(s"theorem: ${thm.fansi}")
          println(s"proof: ${v.head._1.fansi}" )
          println(s"weight: ${v.head._2}")
          println()
    }
  }
  f
}
