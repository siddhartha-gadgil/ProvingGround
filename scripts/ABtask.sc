import learning._
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._

import math._
import scala.concurrent.duration._
import ProverTasks._
import HoTT._


val tv = new TermEvolver()

val A = "A" :: Type
val B = "B" :: Type

val mp = A ->: (A ->: B) ->: B

val dist = FiniteDistribution.unif[Term](A, B)

def seek(n: Double = 6, decay: Double = 10) = {
  val s = theoremSearchTask(dist, tv, math.pow(10.0, -n), 3.minutes,A ->: (A ->: B) ->: B, decay = decay)
  val f = s.runAsync
  f.foreach{
    case Some(t) => println(s"Found Proof ${t.fansi}\nfor Theorem ${t.typ.fansi}")
    case None => println("could not find theorem")
  }
  f
}

def seekTraced(n: Double = 6, decay: Double = 10) = {
  val s = theoremSearchTraceTask(dist, tv, math.pow(10.0, -n), 3.minutes, A ->: (A ->: B) ->: B, decay = decay)
  val f = s.runAsync
  f.foreach{
    case Some((t, path)) =>
      println(s"Found Proof ${t.fansi}\nfor Theorem ${t.typ.fansi}")
      path.foreach((s) => println(s"Use: ${s.fansi} for Lemma ${s.typ.fansi}"))
    case None => println("could not find theorem")
  }
  f
}


def explore(n: Double = 6, decay: Double = 10, scale: Double = 1.0) =
  {
    val s = theoremsExploreTask(dist, tv, math.pow(10.0, -n), 10.minutes, scale = scale, decay = decay)
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

def exploreVars(n: Double = 6, decay: Double = 10) =
  {
    val s = theoremsExploreTraceTask(
      dist,
      tv, math.pow(10.0, -n), 3.minutes, decay = decay, vars = Vector(A, B))
  val f = s.runAsync
  f.foreach{(_) =>
    pprint.log("Traced exploration done")
  }
  f
}
