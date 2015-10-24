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
      ](ACrunner.dyn(rank, size) , ACrunner.padFeedback(rank, wrdCntn), init, (mv) =>save(mv._1, mv._2))

object ACrunner {
  def dyn(rank: Int, size: Int) = {
    sampleV(size) andthen genExtendM(allMoves(rank))
  }

  def feedback(rank : Int, wrdCntn: Double, strinctness: Double) ={
    val projPresFn = projectV[AtomicMove, Moves] andthen genPresentationMoveFn(rank)
    val fb = (d : FiniteDistribution[Presentation]) => d.feedback(FreeGroups.Presentation.weight(wrdCntn))
    fb ^: projPresFn
  }

  def padFeedback(rank: Int, wrdCntn: Double) =
    (strictness: Double) =>
      (x: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])) =>
        (y : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])) =>
          feedback(rank, wrdCntn, strictness)(y)


  def props(rank: Int, size: Int, wrdCntn: Double,
    init : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
    save : (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]) => Unit
    ) : Props = Props(
        new ACrunner(rank: Int, size: Int, wrdCntn: Double,
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
          }

  import SimpleAcEvolution._

  def spawn(name: String, p: Param = Param()) = {
    import p._
    import ACData.save
    rawSpawn(name, rank, size, wrdCntn, (unifMoves(rank), eVec), save(name, dir))
}

  def spawns(name: String, mult : Int = 4, p: Param = Param()) = {
    for (j <- 1 to mult) yield spawn(name + j.toString, p)
  }

   case class Param(
       rank: Int = 2, size : Int = 1000, wrdCntn: Double = 0.3,
       dir : String = "0.5"
       )

}
