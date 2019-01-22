package provingground.learning
import provingground._, HoTT._
import math._
import ProofEntropies._

case class ProofEntropies(gens: Map[Term, Double],
                          theorems: Map[Typ[Term], Double],
                          proofs: Map[Term, Double],
                          genWeight: Double) {
  val genEnt: Double = gens.values.map((p) => -p * log(p)).sum

  val thmSet: Set[Typ[Term]] =
    theorems.keySet.intersect(proofs.keySet.map(_.typ))

  def proofWeight(thm: Typ[Term]): Double =
    proofs.collect { case (x, p) if x.typ == thm => p }.sum

  val kl: Double = thmSet.map { (thm) =>
    val p = theorems(thm)
    val q = proofWeight(thm)
    log(p / q) * p
  }.sum

  def addGen(x: Term, p: Double): ProofEntropies =
    this.copy(gens = addElem(x, p, gens),
              proofs = addElem(x, p * genWeight, proofs))

  def klDiff(x: Term, p: Double): Double = kl - addGen(x, p).kl

  def genEntDiff(x: Term, p: Double): Double = addGen(x, p).genEnt - genEnt

  def gainRatio(x: Term, p: Double): Double = klDiff(x, p) / genEntDiff(x, p)
}

object ProofEntropies {
  def normalize[X](m: Map[X, Double]): Map[X, Double] = {
    val tot = m.values.sum
    m.mapValues(_ / tot)
  }

  def addElem[X](x: X, p: Double, m: Map[X, Double]): Map[X, Double] =
    normalize(m + (x -> (p + m.getOrElse(x, 0.0))))

  def fromStates(init: TermState, result: TermState, genWeight: Double) =
    ProofEntropies(init.terms.toMap,
                   result.thmsBySt.toMap,
                   result.pfDist.toMap,
                   genWeight)
}
