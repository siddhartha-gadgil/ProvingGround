package provingground.andrewscurtis

import provingground._

import Collections._

import FreeGroups._

import ACMongo._

import scala.concurrent.ExecutionContext.Implicits.global

class StateView(name: String, elems: Vector[ACElem], fdM : FiniteDistribution[AtomicMove]){
  def fdV = FiniteDistribution(elems map ((x) => Weighted(x.moves, x.weight))).flatten.normalized()

  def fdP = FiniteDistribution(elems map ((x) => Weighted(x.pres, x.weight))).flatten.normalized()

  def proofs(thm: Presentation) = elems filter(_.pres == thm)

  def proofWeight(mvs: Moves) = {
    val ps = mvs.moves map (fdM(_))
    (1.0 /: ps)(_ * _)
  }

  def totalProofWeight(thm: Presentation) =
    (proofs(thm) map ((x: ACElem) => proofWeight(x.moves))).sum

  def hardness(thm: Presentation) =
    scala.util.Try(math.log(fdP(thm))/math.log(totalProofWeight(thm))).getOrElse(0.0)

  def hardThms = fdP.supp.sortBy(hardness).reverse
}

object StateView{
  import ACFlowSaver._

  def apply(name: String, loops: Int) = fromCasbah(name, loops)

//  def apply(name: String) = new StateView(getCurrentElems(name), FDM(name).get)

  def apply(name: String) = fromMongo(name)

  def fromCasbah(name: String, loops: Int) = {
    new StateView(name, getElems(name, loops), FDM(name).get)
  }

  def fromMongo(name: String) =
    (getFutOptElems(name) flatMapp ((vec) =>
      getFutOptFDM(name) mapp (new StateView(name, vec, _)))) map (_.get)
}
