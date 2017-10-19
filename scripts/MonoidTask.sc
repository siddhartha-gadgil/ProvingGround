import learning._
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._
import library._, MonoidSimple._
val tv = new TermEvolver(lambdaWeight = 0.0, piWeight = 0.0)
import math._
import scala.concurrent.duration._
import ProverTasks._
// val tdTask = termdistTask(dist1, tv, math.pow(10, -5), 2.minutes)
// val tdFut = tdTask.runAsync
// tdFut.value
// val tydTask = typdistTask(dist1, tv, math.pow(10, -7), 2.minutes)
// val tydFut = tydTask.runAsync
// tydFut.value
// val pt = prsmEntTask(tdTask, tydTask)
// val pf = pt.runAsync
// pf.value
val s = theoremSearchTask(dist1, tv, math.pow(10.0, -6), 3.minutes, eqM(l)(r))
val f = s.runAsync
f.foreach{
  case Some(t) => println(s"Found Proof ${t.fansi}\nfor Theorem ${t.typ.fansi}")
  case None => println("could not find theorem")
}
