import learning._
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._
import library._, MonoidSimple._
val tv = new TermEvolver(lambdaWeight = 0.0, piWeight = 0.0)
import math._
import scala.concurrent.duration._
import ProverTasks._

def seek(n: Double = 6) = {
  val s = theoremSearchTask(dist1, tv, math.pow(10.0, -n), 3.minutes, eqM(l)(r), decay = 10)
  val f = s.runAsync
  f.foreach{
    case Some(t) => println(s"Found Proof ${t.fansi}\nfor Theorem ${t.typ.fansi}")
    case None => println("could not find theorem")
  }
  f
}
