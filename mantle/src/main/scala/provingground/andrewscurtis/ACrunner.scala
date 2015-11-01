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
    save : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]) => Unit
    ) extends FDactor[
      (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])
      ](ACrunner.dyn(rank, size) , ACrunner.padFeedback(rank, wrdCntn), normalize, init, (mv) =>save(mv._1, mv._2))

class ACsmoothRunner(rank: Int, size: Int, wrdCntn: Double,
    init : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
    save : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]) => Unit
    ) extends FDactor[
      (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])
      ](ACrunner.dyn(rank, size) , ACrunner.padFeedback(rank, wrdCntn), normalize, init, (mv) =>save(mv._1, mv._2))
      
object ACrunner {
  def dyn(rank: Int, size: Int) = {
    sampleV(size) andthen genExtendM(allMoves(rank))
  }

  val normalize = (fd: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])) =>
    (fd._1.normalized(), fd._2.normalized())

  def feedback(rank : Int, wrdCntn: Double, strinctness: Double) ={
    val projPresFn = projectV[AtomicMove, FiniteDistribution[Moves]] andthen genPresentationMoveFn(rank)
    val fb = (d : FiniteDistribution[Presentation]) => {
      println(s"Distribution(${d.support.size}, total = ${d.norm}, ${d.total})")
      println(d.entropyView.take(20))
      val res = d.rawfeedback(FreeGroups.Presentation.weight(wrdCntn))
      println(s"Feedback total = ${d.total}")
//      println(res.entropyView.take(20))
      res
    }
    val pullback = fb ^: projPresFn
//    println(s"Pullback feedback total: ${pullback._2.total}")
    pullback
  }

  def smoothFeedback(rank : Int, wrdCntn: Double, strinctness: Double) ={
    val projPresFn = projectV[AtomicMove, FiniteDistribution[Moves]] andthen genPresentationMoveFn(rank)
    val fb = (d : FiniteDistribution[Presentation]) => {
      d.smoothedFeedback(FreeGroups.Presentation.weight(wrdCntn))
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
    save : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]) => Unit
    ) : Props = Props(
        new ACrunner(rank: Int, size: Int, wrdCntn: Double,
    init : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
    save : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]) => Unit
    ) )

  def smoothProps(rank: Int, size: Int, wrdCntn: Double,
    init : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
    save : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]) => Unit
    ) : Props = Props(
        new ACsmoothRunner(rank: Int, size: Int, wrdCntn: Double,
    init : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
    save : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]) => Unit
    ) )

  def rawSpawn(name: String, rank: Int, size: Int, wrdCntn: Double,
    init : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
    save : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]) => Unit
    ) = {
            val runner =
              Hub.system.actorOf(
                  props(rank: Int, size: Int, wrdCntn: Double,
                    init : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
                    save : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]) => Unit
                 ), name)
            runner
          }

  def smoothSpawn(name: String, rank: Int, size: Int, wrdCntn: Double,
    init : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
    save : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]) => Unit
    ) = {
            val runner =
              Hub.system.actorOf(
                  smoothProps(rank: Int, size: Int, wrdCntn: Double,
                    init : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
                    save : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]) => Unit
                 ), name)
            runner
          }

  import SimpleAcEvolution._

  def spawnInit(name: String, p: Param = Param()) = {
    import p._
    import ACData.fileSave
    rawSpawn(name, rank, size, wrdCntn, (unifMoves(rank), eVec), fileSave(name, dir, alert))
}
  
  def spawnExtInit(name: String, p: Param = Param()) = {
    import p._
    import ACData.fileSave
    rawSpawn(name, rank, size, wrdCntn, (extendedMoves(rank), eVec), fileSave(name, dir, alert))
}

  def spawnSmooth(name: String, p: Param = Param()) = {
    import p._
    import ACData.fileSave
    smoothSpawn(name, rank, size, wrdCntn, (unifMoves(rank), eVec), fileSave(name, dir, alert))
}

  def spawnExtSmooth(name: String, p: Param = Param()) = {
    import p._
    import ACData.fileSave
    smoothSpawn(name, rank, size, wrdCntn, (extendedMoves(rank), eVec), fileSave(name, dir, alert))
} 
  
  def spawnsInit(name: String, mult : Int = 4, p: Param = Param()) = {
    for (j <- 1 to mult) yield spawnInit(name+"."+j.toString, p)
  }

  def spawnsSmooth(name: String, mult : Int = 4, p: Param = Param()) = {
    for (j <- 1 to mult) yield spawnSmooth(name+"."+j.toString, p)
  }

  def spawnsExtInit(name: String, mult : Int = 4, p: Param = Param()) = {
    for (j <- 1 to mult) yield spawnExtInit(name+"."+j.toString, p)
  }

  def spawnsExtSmooth(name: String, mult : Int = 4, p: Param = Param()) = {
    for (j <- 1 to mult) yield spawnExtSmooth(name+"."+j.toString, p)
  }
  
  
   case class Param(
       rank: Int = 2, size : Int = 1000, wrdCntn: Double = 0.5,
       dir : String = "acDev",
       alert: Unit => Unit = (_) => ()
       )

}
