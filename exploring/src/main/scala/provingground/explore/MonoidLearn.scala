package provingground.explore

import provingground._

import scala.concurrent.duration._

import scala.concurrent._

import library.Monoid._

object MonoidLearn{
  val dedSrc = new DeducerSource(ded, dist, 1000000, 100000, smooth)

  val mediumRun = dedSrc.timedRun(10.hours, 20.hours, "monoid-30hours")

  import WebServer._

  viewTypes ++= Set(l =:= r, leftId.typ, (b ~>: (op(r)(b) =:= b)), idUnique)
}

object MonoidLearnRun extends App{
  import WebServer._

  viewTypes ++= Set(l =:= r, leftId.typ, (b ~>: (op(r)(b) =:= b)), idUnique)

  Await.result(MonoidLearn.mediumRun, 40.hours)
}
