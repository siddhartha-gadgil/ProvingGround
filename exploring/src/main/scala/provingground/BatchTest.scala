package provingground

import learning._, ammonite.ops._

import scala.concurrent._, duration._
import monix.execution.Scheduler.Implicits.global
import scala.io.StdIn
import monix.reactive._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import ammonite.ops._

object LeanAmmTest extends App {
  import interface._, LeanInterface._
  import trepplein._
  lazy val mods = getMods("data/group.export").filter((m) =>
    !m.name.toString.startsWith("char"))
  println(mods.size)
  val defFile = pwd / "data" / "group-defs.txt"
  val indFile = pwd / "data" / "group-inds.txt"
  val errFile = pwd / "data" / "group-errors.yaml"
  def callback(mod: Modification, err: Throwable) = {
    write.append(errFile, s"""mod: "$mod"""" + "\n")
    write.append(errFile, s"""err: "$err"""" + "\n\n")
  }
  write.over(errFile, "")
  var count    = 0
  var l2tm     = LeanToTermMonix.empty
  lazy val obs = LeanToTermMonix.observable(mods, logErr = callback)
  lazy val fut = obs.foreach { (t) =>
    count += 1
    println(s"$count : ${t.defnMap.size}")
    write.over(defFile, t.defnMap.keys.mkString("", "\n", "\n"))
    write.over(indFile, t.termIndModMap.keys.mkString("", "\n", "\n"))
  }

  lazy val iter = LeanToTermMonix.iterant(mods, logErr = callback)
  lazy val task = iter.foreach { (t) =>
    count += 1
    println(s"$count : ${t.defnMap.size}")
    write.over(defFile, t.defnMap.keys.mkString("", "\n", "\n"))
    write.over(indFile, t.termIndModMap.keys.mkString("", "\n", "\n"))
  }

  lazy val iterStrict =
    LeanToTermMonix.iterant(mods, recoverAll = false)

  lazy val taskStrict = iterStrict.foreach { (t) =>
    count += 1
    println(s"$count : ${t.defnMap.size}")
    l2tm = t
  }

  def continue(n: Int = count) = {
    val it = LeanToTermMonix.iterant(mods.drop(n), l2tm, recoverAll = false)
  }

  Await.result(task.runAsync, Duration.Inf)
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
