package provingground.andrewscurtis

import provingground._

import FreeGroups._
import Moves._
import MoveGenerator._
import DiffStructure._
import FiniteDistribution._
import Collections._

import FiniteDistributionLearner._
import LinearStructure._
import SimpleAcEvolution._

import akka.actor._
import ACrunner._

class ACrunner(rank: Int, size: Int, wrdCntn: Double,
    init : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
    srcRef : ActorRef
    ) extends FDactor[
      (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])
      ](ACrunner.dyn(rank, size) , ACrunner.padFeedback(rank, wrdCntn), normalize, init, srcRef)

class ACsmoothRunner(rank: Int, size: Int, wrdCntn: Double,
    init : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
    srcRef: ActorRef
    ) extends FDactor[
      (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])
      ](ACrunner.dyn(rank, size) , ACrunner.padFeedback(rank, wrdCntn), normalize, init, srcRef)


      
object ACrunner {
  def dyn(rank: Int, size: Int) = {
    sampleV(size) andthen genExtendM(allMoves(rank))
  }

  val normalize = (fd: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])) =>
    (fd._1.normalized(), fd._2.normalized())

  def feedback(rank : Int, wrdCntn: Double, strictness: Double) ={
    val projPresFn = projectV[AtomicMove, FiniteDistribution[Moves]] andthen genPresentationMoveFn(rank)
    val fb = (d : FiniteDistribution[Presentation]) => {
//      println(s"Distribution(${d.support.size}, total = ${d.norm}, ${d.total})")
//      println(d.entropyView.take(20))
      val res = d.rawfeedback(FreeGroups.Presentation.weight(wrdCntn), strictness)
//      println(s"Feedback total = ${d.total}")
//      println(res.entropyView.take(20))
      res
    }
    val pullback = fb ^: projPresFn
//    println(s"Pullback feedback total: ${pullback._2.total}")
    pullback
  }

  def smoothFeedback(rank : Int, wrdCntn: Double, strictness: Double) ={
    val projPresFn = projectV[AtomicMove, FiniteDistribution[Moves]] andthen genPresentationMoveFn(rank)
    val fb = (d : FiniteDistribution[Presentation]) => {
      d.smoothedFeedback(FreeGroups.Presentation.weight(wrdCntn), strictness)
    }
    fb ^: projPresFn
  }

  def padFeedback(rank: Int, wrdCntn: Double) =
    (strictness: Double) =>
      (x: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])) =>
        (y : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])) =>
          feedback(rank, wrdCntn, strictness)(y)

  def padSmoothback(rank: Int, wrdCntn: Double) =
    (strictness: Double) =>
      (x: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])) =>
        (y : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])) =>
          smoothFeedback(rank, wrdCntn, strictness)(y)

  def props(rank: Int, size: Int, wrdCntn: Double,
    init : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
    srcRef: ActorRef
    ) : Props = Props(
        new ACrunner(rank: Int, size: Int, wrdCntn: Double,
    init : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
    srcRef
    ) )

  def smoothProps(rank: Int, size: Int, wrdCntn: Double,
    init : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
    srcRef: ActorRef
    ) : Props = Props(
        new ACsmoothRunner(rank: Int, size: Int, wrdCntn: Double,
    init : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
    srcRef
    ) )

  def rawSpawn(name: String, rank: Int, size: Int, wrdCntn: Double,
    init : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
    srcRef: ActorRef
    ) = {
            val runner =
              Hub.system.actorOf(
                  props(rank: Int, size: Int, wrdCntn: Double,
                    init : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
                    srcRef
                 ), name)
            runner
          }

  def smoothSpawn(name: String, rank: Int, size: Int, wrdCntn: Double,
    init : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
    srcRef: ActorRef
    ) = {
            val runner =
              Hub.system.actorOf(
                  smoothProps(rank: Int, size: Int, wrdCntn: Double,
                    init : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
                    srcRef
                 ), name)
            runner
          }

  import SimpleAcEvolution._

  def initState(rank: Int) = (learnerMoves(rank), eVec)
  
  def spawnRaw(name: String, p: Param = Param(), init: Int => FiniteDistribution[AtomicMove] = learnerMoves) = {
    import p._
    import ACData.fileSave
    rawSpawn(name, rank, size, wrdCntn, (init(rank), eVec), ACData.srcRef(dir, rank))
}



  def spawnSmooth(name: String, p: Param = Param(), init: Int => FiniteDistribution[AtomicMove] = learnerMoves) = {
    import p._
    import ACData.fileSave
    smoothSpawn(name, rank, size, wrdCntn, (init(rank), eVec), ACData.srcRef(dir, rank))
}


  def spawnsRaw(name: String, mult : Int = 4, p: Param = Param(), init: Int => FiniteDistribution[AtomicMove] = learnerMoves) = {
    for (j <- 1 to mult) yield spawnRaw(name+"."+j.toString, p, init)
  }

  def spawnsSmooth(name: String, mult : Int = 4, p: Param = Param(), init: Int => FiniteDistribution[AtomicMove] = learnerMoves) = {
    for (j <- 1 to mult) yield spawnSmooth(name+"."+j.toString, p, init)
  }



   case class Param(
       rank: Int = 2, size : Int = 1000, wrdCntn: Double = 0.1,
       dir : String = "acDev"
       )

}
