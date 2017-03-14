package provingground.andrewscurtis

import provingground._

import Collections._

import FreeGroups._

import ACMongo._

import scala.concurrent.ExecutionContext.Implicits.global

class StateView(name: String,
                elems: Vector[ACElem],
                fdM: FiniteDistribution[AtomicMove]) {
  lazy val fdV = FiniteDistribution(
      elems map ((x) => Weighted(x.moves, x.weight))).flatten.normalized()

  lazy val fdP = FiniteDistribution(
      elems map ((x) => Weighted(x.pres, x.weight))).flatten.normalized()

  lazy val proofElems = elems groupBy (_.pres)

  def proofs(thm: Presentation) =
    proofElems.getOrElse(thm, Vector()) map (_.moves)

  def proofWeight(mvs: Moves) = {
    val ps = mvs.moves map (fdM(_))
    (1.0 /: ps)(_ * _)
  }

  lazy val thmWeights =
    proofElems mapValues
    ((elemVec) => (elemVec map ((elem) => elem.weight)).sum)

  lazy val proofWeightMap =
    proofElems mapValues
    ((elemVec) => (elemVec map ((elem) => proofWeight(elem.moves))).sum)

  def totalProofWeight(thm: Presentation) =
    proofWeightMap.getOrElse(thm, 0.0)

  lazy val hardnessMap = for ((thm, p) <- thmWeights) yield
    (thm, math.log(p) / math.log(proofWeightMap(thm)))

  def hardness(thm: Presentation) =
    hardnessMap.getOrElse(thm, 0.0)

  def hardThms = fdP.supp.sortBy(hardness).reverse
}

object StateView {
//  import ACFlowSaver._

//  def apply(name: String, loops: Int) = fromCasbah(name, loops)

//  def apply(name: String) = new StateView(getCurrentElems(name), FDM(name).get)

  def apply(name: String) = fromMongo(name)
  /*
  def fromCasbah(name: String, loops: Int) = {
    new StateView(name, getElems(name, loops), FDM(name).get)
  }
   */
  def fromMongo(name: String) =
    (getFutOptElems(name) flatMapp
        ((vec) => getFutOptFDM(name) mapp (new StateView(name, vec, _)))) map
    (_.get)
}
