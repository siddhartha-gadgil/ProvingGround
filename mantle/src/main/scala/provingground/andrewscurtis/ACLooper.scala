package provingground.andrewscurtis

import provingground._

import learning._

import FreeGroups._
//import Moves._
import MoveGenerator._
import DiffStructure._
import FiniteDistribution._
//import Collections._

import FiniteDistributionLearner._
import LinearStructure._

import akka.actor._
import ACLooper._

/**
  * The loop for learning specialized to Andrews-Curtis moves
  */
class ACLooper(
    rank: Int,
    size: Int,
    wrdCntn: Double,
    init: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
    srcRef: ActorRef,
    param: Param)
    extends FDLooper[(FiniteDistribution[AtomicMove],
                      FiniteDistribution[Moves]),
                     Param](ACLooper.dyn(rank, size),
                            ACLooper.padFeedback(rank, wrdCntn),
                            normalize,
                            init,
                            srcRef,
                            param)

class ACsmoothLooper(
    rank: Int,
    size: Int,
    wrdCntn: Double,
    init: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
    srcRef: ActorRef,
    param: Param)
    extends FDLooper[(FiniteDistribution[AtomicMove],
                      FiniteDistribution[Moves]),
                     Param](ACLooper.dyn(rank, size),
                            ACLooper.padFeedback(rank, wrdCntn),
                            normalize,
                            init,
                            srcRef,
                            param)

object ACLooper {

  /**
    * The dynamical system for andrews-curtis learning
    */
  def dyn(rank: Int, size: Int) = {
    sampleV(size) andthen genExtendM(allMoves(rank))
  }

  val normalize =
    (fd: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])) =>
      (fd._1.normalized(), (fd._2 map (_.idLast)).flatten.normalized())

  def feedback(rank: Int, wrdCntn: Double, strictness: Double) = {
    val projPresFn =
      projectV[AtomicMove, FiniteDistribution[Moves]] andthen genPresentationMoveFn(
        rank)
    val fb = (d: FiniteDistribution[Presentation]) => {
      val res =
        d.rawfeedback(FreeGroups.Presentation.weight(wrdCntn), strictness)
      res
    }
    val pullback = fb ^: projPresFn
    pullback
  }

  def smoothFeedback(rank: Int, wrdCntn: Double, strictness: Double) = {
    val projPresFn =
      projectV[AtomicMove, FiniteDistribution[Moves]] andthen genPresentationMoveFn(
        rank)
    val fb = (d: FiniteDistribution[Presentation]) => {
      d.smoothedFeedback(FreeGroups.Presentation.weight(wrdCntn), strictness)
    }
    fb ^: projPresFn
  }

  def padFeedback(rank: Int, wrdCntn: Double) =
    (strictness: Double) =>
      (x: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])) =>
        (y: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])) =>
          feedback(rank, wrdCntn, strictness)(y)

  def padSmoothback(rank: Int, wrdCntn: Double) =
    (strictness: Double) =>
      (x: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])) =>
        (y: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])) =>
          smoothFeedback(rank, wrdCntn, strictness)(y)

  def props(rank: Int,
            size: Int,
            wrdCntn: Double,
            init: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
            srcRef: ActorRef,
            param: Param): Props =
    Props(
      new ACLooper(
        rank: Int,
        size: Int,
        wrdCntn: Double,
        init: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
        srcRef,
        param))

  def smoothProps(
      rank: Int,
      size: Int,
      wrdCntn: Double,
      init: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
      srcRef: ActorRef,
      param: Param): Props =
    Props(
      new ACsmoothLooper(
        rank: Int,
        size: Int,
        wrdCntn: Double,
        init: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
        srcRef,
        param))

  def spawn(name: String,
            rank: Int,
            size: Int,
            wrdCntn: Double,
            init: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
            srcRef: ActorRef,
            param: Param) = {
    val runner = Hub.system.actorOf(
      props(rank: Int,
            size: Int,
            wrdCntn: Double,
            init: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
            srcRef,
            param),
      name)
    runner
  }

  def smoothSpawn(
      name: String,
      rank: Int,
      size: Int,
      wrdCntn: Double,
      init: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
      srcRef: ActorRef,
      param: Param) = {
    val runner = Hub.system.actorOf(
      smoothProps(
        rank: Int,
        size: Int,
        wrdCntn: Double,
        init: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
        srcRef,
        param),
      name)
    runner
  }

  def initState(rank: Int) = (learnerMoves(rank), eVec)

  /*
  def spawnRaw(name: String, p: Param = Param(), init: Int => FiniteDistribution[AtomicMove] = learnerMoves) = {
    import p._
  //  import ACData.fileSave
    spawn(name, rank, size, wrdCntn, (init(rank), eVec), ACData.srcRef(rank = p.rank), p)
}



  def spawnSmooth(name: String, p: Param = Param(), init: Int => FiniteDistribution[AtomicMove] = learnerMoves) = {
    import p._
    //import ACData.fileSave
    smoothSpawn(name, rank, size, wrdCntn, (init(rank), eVec), ACData.srcRef(rank = p.rank), p)
}


  def spawnsRaw(name: String, mult : Int = 4, p: Param = Param(), init: Int => FiniteDistribution[AtomicMove] = learnerMoves) = {
    for (j <- 1 to mult) yield spawnRaw(name+"."+j.toString, p, init)
  }

  def spawnsSmooth(name: String, mult : Int = 4, p: Param = Param(), init: Int => FiniteDistribution[AtomicMove] = learnerMoves) = {
    for (j <- 1 to mult) yield spawnSmooth(name+"."+j.toString, p, init)
  }
 */
}
