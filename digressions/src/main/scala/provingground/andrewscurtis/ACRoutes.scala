package provingground.andrewscurtis

import provingground._, learning._
import FreeGroups._

import upickle.default.{write => uwrite, read => uread, _}

//import akka.actor._

//import akka.http._
//import akka.http.scaladsl._
import akka.http.scaladsl.server.Directives._

//import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global


import Hub.system

import ACMongo._

import ACBatch._

import StartData._

//import Moves._

object ACRoutes {
  val thmEvolve = path("theorem-evolution" / Segment / Segment) {
    case (name, ppress) => {
      val pres = uread[Presentation](ppress)
      val thms = thmWeights(pres, name) map ((x: LazyList[ACThm]) => uwrite(x))
      complete(thms)
    }
  }

  val thms = path("theorems" / Segment) { name =>
    val thmsOptFut = getFutOptThms(name)
    val thms =
      thmsOptFut mapp ((d: FiniteDistribution[Presentation]) => uwrite(d))
    complete(thms)
  }

  val terms = path("terms" / Segment) { name =>
    val thmsOptFDV = getFutOptFDV(name)
    val thms       = thmsOptFDV mapp ((m: FiniteDistribution[Moves]) => uwrite(m))
    complete(thms)
  }

  val moveWeights = path("move-weights" / Segment) { name =>
    val thmsOptFDM = getFutOptFDM(name)
    val thms =
      thmsOptFDM mapp ((m: FiniteDistribution[AtomicMove]) => uwrite(m))
    complete(thms)
  }

  val actors = path("actors") {
    val actors = getFutActors() map ((v: Vector[String]) => uwrite(v))
    complete(actors)
  }

  val getData = get {
    pathPrefix("data")(thms ~ thmEvolve ~ terms ~ moveWeights)
  }

  val start = post {
    path("start") {
      entity(as[String]) { d =>
        val startData = uread[StartData](d)
        val ref       = startData.run()
        complete(startData.name)
      }
    }
  }

  val quickstart = post {
    path("quickstart") {
      val ds = loadStartData()
      ds map (_.run())
      val names = ds map (_.name)
      complete(uwrite(names))
    }
  }

  val stop = post {
    path("stop") {
      FDHub.stop
      complete("stop requested")
    }
  }

  val acRoutes =
    pathPrefix("andrews-curtis")(getData ~ start ~ quickstart ~ stop)
}
