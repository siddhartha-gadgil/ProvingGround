package provingground.explore

import provingground._

import scala.concurrent.duration._

import scala.concurrent._

import library.Monoid._

import scala.io.StdIn

object MonoidLearn {
  val dedSrc = new DeducerSource(ded, dist, 30, 30, smooth)

  def shortRun = dedSrc.timedRun(10.minutes, 20.minutes, "monoid-quick")

  import WebServer._

  viewTypes ++= Set(l =:= r, leftId.typ, (b ~>: (op(r)(b) =:= b)), idUnique)
}

object MonoidLearnRun extends App {
  import ammonite.ops._

  val file = pwd / "tmp" / "blah"

  write.append(file, "reached here")

  import WebServer._

  viewTypes ++= Set(l =:= r, leftId.typ, (b ~>: (op(r)(b) =:= b)), idUnique)

//  MonoidLearn.shortRun

  println("running")

//  StdIn.readLine()
  Await.result(MonoidLearn.shortRun, 60.minutes)
}
