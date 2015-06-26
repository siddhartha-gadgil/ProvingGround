package provingground.andrewscurtis

import provingground.Collections._
import provingground.FiniteDistributionLearner._
import provingground.DiffbleFunction._
import provingground.andrewscurtis.Moves._

object DiffStructure {

  def genMoveFn(mf: AtomicMove) = moveFn(mf)

  def genWtDyn(mf: AtomicMove) = {
    val wtdyn = weightedDyn[Moves => Option[Moves], FiniteDistribution[Moves]]
    wtdyn(mf, genMoveFn(mf))
  }

  def genCollection(lst: List[AtomicMove]) = lst map genWtDyn
}
