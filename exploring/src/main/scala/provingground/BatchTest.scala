package provingground

import learning._, ammonite.ops._

import scala.concurrent._, duration._
import monix.execution.Scheduler.Implicits.global
import scala.io.StdIn
import monix.reactive._
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object LeanAmmTest extends App {
  import interface._, LeanInterface._
  val mods = getMods("data/group.export")
  println(mods.size)
  lazy val obs = LeanToTermMonix.observable(mods)
  var count    = 0
  lazy val fut = obs.foreach { (t) =>
    count += 1
    println(s"$count : ${t.defnMap.size}")
  }

  Await.result(fut, Duration.Inf)
}

object BatchTest /*extends App*/ {
  import scratch.FDMonixAB._

  import translation.FansiShow._

  var count = 0

  val file = home / "tmp" / "ABrun.out"

  val duration = 90.minutes

  val tick = Observable.interval(10.seconds)

  val awaitTask = tick.takeByTimespan(duration).completedL

  val timedRun = thmsObs.takeByTimespan(duration)
  //
  // timedRun.doOnComplete(
  //    {println("noted completion")
  // })

  timedRun.foreach((x) => {
    count += 1
    write.append(file, s"count: $count\nTheorems:\n${x.fansi}\n\n")
  })

  Await.result(awaitTask.runAsync, duration * 2)
}
