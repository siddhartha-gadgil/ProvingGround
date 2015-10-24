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
    
  def spawn(name: String, rank: Int, size: Int, wrdCntn: Double,
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
}

