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
  lazy val NC  = Name("char")
  lazy val NCS = Name("char_sz")
  lazy val B0  = Name("bit0")
  lazy val SO  = Name("sizeof")
  lazy val isChar: Expr => Boolean = {
    case Const(NC, _)  => true
    case Const(NCS, _) => true
    case Const(B0, _)  => true
    case Const(SO, _)  => true
    case _             => false
  }

  lazy val notChar: Modification => Boolean = {
    case df: DefMod if df.name.toString.startsWith("char") => false
    case mod                                               => !modSubExpr(mod).exists(isChar)
  }

  lazy val mods =
    getMods("data/group.export").filter(notChar)

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

  Await.result(task.runToFuture, Duration.Inf)
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

  Await.result(awaitTask.runToFuture, duration * 2)
}
