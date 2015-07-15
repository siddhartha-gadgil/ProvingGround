package provingground.andrewscurtis

import Moves._

import MoveGenerator._

import DiffStructure._

import provingground._

import Collections._

import FiniteDistribution._

import LinearStructure._

import FreeGroups._

import provingground.FiniteDistributionLearner._

import DiffbleFunction._

import QDI._

/**
 * @author gadgil
 */
object ACevolution {
  type FD[X] = FiniteDistribution[X]

  type M = AtomicMove

  type V = Moves

  type P = Presentation

  def foldPair(rank : Int) : DiffbleFunction[(FD[M], FD[V]), FD[P]] = (projectV andthen genPresentationMoveFn(rank))

  def trivMoveSeq = FiniteDistribution.uniform(Some(Moves.empty))

  val E = Weighted(Moves.empty, 1)

  lazy val eSet = FiniteDistributionSet(Set(E))

  lazy val eVec = FiniteDistributionVec(Vector(E))

  lazy val eParVec = FiniteDistributionParVec(Vector(E).par)

  def allMoves(rank: Int) = genAllMoves(rank, rank)

  def unifMoves(rank: Int) = FiniteDistribution.uniform(allMoves(rank))

  def evolve(rank: Int, steps: Int = 5, initV: FD[V] = trivMoveSeq) = {
    val fn = iterateDiff(allMoves(rank), steps)
    fn.func((unifMoves(rank), trivMoveSeq))
  }

  def step(rank: Int) = {
    genExtendM(allMoves(rank))
  }

  def presDist(r: Int)(mv : (FD[M], FD[V])) = mv._2 map ((v: V) => Moves.actOnTriv(r)(v).get)

  def viewAll(mv : (FD[M], FD[V]), r: Int) = {
    viewPage(fdDiv(mv._1), "movedist.html")
    viewPage(fdDiv(mv._2), "vertexdist.html")
    val presdist = mv._2 map ((v: V) => Moves.actOnTriv(r)(v).get.toPlainString)
    viewPage(fdDiv(presdist),
      "presentationdist.html"
    )
  }
}
