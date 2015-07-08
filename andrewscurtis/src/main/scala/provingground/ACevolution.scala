package provingground.andrewscurtis

import Moves._

import MoveGenerator._

import DiffStructure._

import provingground._
import Collections._ ; import FiniteDistribution._; import provingground._

import FreeGroups._

import provingground.FiniteDistributionLearner._

import DiffbleFunction._

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

  def allMoves(rank: Int) = genAllMoves(rank, rank)

  def unifMoves(rank: Int) = FiniteDistribution.uniform(allMoves(rank))

  def evolve(rank: Int, steps: Int = 5, initV: FD[V] = trivMoveSeq) = {
    val fn = iterateDiff(allMoves(rank), steps)
    fn.func((unifMoves(rank), trivMoveSeq))
  }
}
