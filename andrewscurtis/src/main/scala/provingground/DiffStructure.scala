package provingground.andrewscurtis

import provingground.andrewscurtis.FreeGroups._
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

  def iterateDiff(lst: List[AtomicMove], iterations: Int = 5) = iterate(genExtendM(lst))(iterations)

  def genPresentationMoveFn(rank: Int) = moveFn(Moves.actOnTriv(rank)(_: Moves))

  def genProjectionMap(rank: Int, iterations: Int = 5)(lst: List[AtomicMove]) = {
    iterateDiff(lst, iterations) andthen projectV andthen genPresentationMoveFn(rank)
  }

  def getFeedback(presCntn: Double, wrdCntn: Double)(dist: FiniteDistribution[Presentation]) = {
    val baseweight = presentationWeight(_: Presentation, presCntn, wrdCntn)
    dist.feedback(baseweight)
  }

  def addFeedback(presCntn: Double, wrdCntn: Double)(dist: FiniteDistribution[Presentation]) = {
    vbigsum(List(dist, getFeedback(presCntn, wrdCntn)(dist)))
  }
}
