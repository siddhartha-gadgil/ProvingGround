package provingground.andrewscurtis

import Moves._

import MoveGenerator._

import DiffStructure._

import provingground._

import learning._

import Collections._

import FiniteDistribution._

import LinearStructure._

import FreeGroups._

import FiniteDistributionLearner._

import AdjDiffbleFunction._

import interface._, QDI._

import upickle.default._

import provingground.{FiniteDistribution => FD}

/**
  * @author gadgil
  */
object ACevolution {
  //  type FD[X] = FiniteDistribution[X]

  type M = AtomicMove

  type V = Moves

  type P = Presentation

  def foldPair(rank: Int): AdjDiffbleFunction[(FD[M], FD[V]), FD[P]] =
    (projectV andthen genPresentationMoveFn(rank))

  def trivMoveSeq = FiniteDistribution.uniform(Some(Moves.empty))

  def evolve(rank: Int, steps: Int = 5, initV: FD[V] = eVec) = {
    val fn = iterateDiff(allMoves(rank), steps)
    fn.func((unifMoves(rank), initV))
  }

  def step(rank: Int) = {
    genExtendM(allMoves(rank))
  }

  def presDist(r: Int)(mv: (FD[M], FD[V])) =
    mv._2 map ((v: V) => Moves.actOnTriv(r)(v).get)

  def viewAll(mv: (FD[M], FD[V]), r: Int) = {
    viewPage(fdDiv(mv._1), "tmp/movedist.html")
    viewPage(fdDiv(mv._2), "tmp/vertexdist.html")
    val presdist =
      mv._2 map ((v: V) => Moves.actOnTriv(r)(v).get.toPlainString)
    viewPage(fdDiv(presdist), "presentationdist.html")
  }

  def pickleTriple(mv: (FD[M], FD[V]), r: Int) = {
    val m = mv._1.map((m: M) => m.toPlainString).pickle
    val v = mv._2.pickle
    val presdist =
      mv._2 map ((v: V) => Moves.actOnTriv(r)(v).get.toPlainString)
    val triple  = (m, v, presdist.pickle)
    val pickled = write(triple)
    write((interface.Header.fdMVP, pickled))
  }

  def pickleInit(rank: Int) = pickleTriple((unifMoves(rank), eVec), rank)
}
