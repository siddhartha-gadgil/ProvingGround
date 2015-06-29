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

  def conjugateByFeedback(presCntn: Double, wrdCntn: Double)(rank: Int, iterations: Int = 5)(lst: List[AtomicMove]) = {
    val projectionMap = genProjectionMap(rank, iterations)(lst)
    projectionMap.^:(getFeedback(presCntn, wrdCntn)(_: FiniteDistribution[Presentation]))
  }

  def genDynamics(conjFunc: ((FiniteDistribution[Moves => Option[Moves]], FiniteDistribution[Moves])) => (FiniteDistribution[Moves => Option[Moves]], FiniteDistribution[Moves]))(orig: (FiniteDistribution[Moves => Option[Moves]], FiniteDistribution[Moves]), scale: Double = 1) = {
    val sumRes = conjFunc(orig)
    (orig._1 ++ sumRes._1*scale, orig._2 ++ sumRes._2*scale)
  }
}
