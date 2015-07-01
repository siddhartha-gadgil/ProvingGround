package provingground.andrewscurtis

import provingground.andrewscurtis.FreeGroups._
import provingground.Collections._
import provingground.FiniteDistributionLearner._
import provingground.DiffbleFunction._
import provingground.andrewscurtis.Moves._

object DiffStructure {

  def genMoveFn(mf: AtomicMove) = moveFn(mf)

  def genWtDyn(mf: AtomicMove) = {
    val wtdyn = weightedDyn[AtomicMove, FiniteDistribution[Moves]]
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

  def getFeedback(presCntn: Double, wrdCntn: Double, scale: Double = 1)(dist: FiniteDistribution[Presentation]) = {
    val baseweight = presentationWeight(_: Presentation, presCntn, wrdCntn)
    dist.feedback(baseweight) * scale
  }

  def conjugateByFeedback(presCntn: Double, wrdCntn: Double, scale: Double = 1)(rank: Int, iterations: Int = 5)(lst: List[AtomicMove]) = {
    val projectionMap = genProjectionMap(rank, iterations)(lst)
    projectionMap.^:(getFeedback(presCntn, wrdCntn, scale)(_: FiniteDistribution[Presentation]))
  }

  def genDynamics(conjFunc: ((FiniteDistribution[AtomicMove], FiniteDistribution[Moves])) => (FiniteDistribution[AtomicMove], FiniteDistribution[Moves]))(orig: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])) = {
    val sumRes = conjFunc(orig)
    (orig._1 ++ sumRes._1, orig._2 ++ sumRes._2)
  }
}
