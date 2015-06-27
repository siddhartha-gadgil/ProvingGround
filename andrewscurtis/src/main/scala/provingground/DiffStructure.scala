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

  def addCollection(lst: List[AtomicMove]) = vbigsum(genCollection(lst))

  def genExtendM(lst: List[AtomicMove]) = extendM(addCollection(lst))

  def iterateDiff(lst: List[AtomicMove], n: Int = 5) = iterate(genExtendM(lst))(n)
}
