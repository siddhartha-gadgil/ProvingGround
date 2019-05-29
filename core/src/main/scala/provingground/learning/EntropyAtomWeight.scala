package provingground.learning
import provingground._
import HoTT._
import spire.math._
import spire.implicits._
import monix.eval._

import scala.concurrent.duration._

/**
  * Spire gradient learning for tradeoff between generation entropy and theorem-proof relative entropy
  * @param h0 the initial generation entropy
  * @param kl0 the initial relative entropy between theorems and proofs
  * @param p0 the initial theorem weight of the element
  * @param q0 the initial proof weight of the element
  * @param initWeight the weight of the initial distribution in generation of terms
  */
case class EntropyAtomWeight(
    h0: Double,
    kl0: Double,
    p0: Double,
    q0: Double,
    initWeight: Double,
    hW: Double,
    klW: Double,
) {
  implicit val jetDim: JetDim = JetDim(1)

  private val t: Jet[Double] = Jet.h[Double](0)

  def pInit(x: Double): Jet[Double] =
    1 / (1 + exp(x + t)) // Variant of logistic

  def q1(x: Double): Jet[Double] = pInit(x) * initWeight

  def h1(x: Double): Jet[Double] = {
    val p = pInit(x)
    ((1 - p) * h0) - (p * log(p)) - ((1 - p) * log(1 - p))
  }

  def kl1(x: Double): Jet[Double] = {
    val q = q1(x)
    val p = pInit(x)
    kl0 - ((1 - p0) * log(1 - p)) - (p0 * log(1 - q + (q / q0)))
  }

  def tot(x: Double): Jet[Double] = (kl1(x) *klW ) + (h1(x) * hW)

  def totShifted(x: Double, sc: Double = 1): Double =
    x - (tot(x).infinitesimal(0) * sc)

  def totIterator(x: Double, sc: Double = 1): Iterator[Double] =
    Iterator.iterate(x)(y => totShifted(y, sc))

  def iter(sc: Double = 1, prune: Boolean = false): Iterator[Double] = {
    val x = log(1.0 / q0 - 1)
    totIterator(x, sc).map { (y) =>
      1 / (1 + exp(y))
    }
  }.takeWhile(z => !prune || (z * initWeight > q0)) // truncating when a lemma has low weight

  def pairIterator(sc: Double = 1): Iterator[(Double, Option[Double])] =
    Iterator
      .iterate[(Double, Option[Double])](log(1.0 / q0 - 1) -> None) {
        case (y, _) =>
          val shift = tot(y).infinitesimal(0) * sc
          (y - shift, Some(shift.abs))
      }
      .map { case (x, s) => 1.0 / (1 + exp(x)) -> s }

  def prunedPairIterator(
      cutoff: Double,
      sc: Double = 1
  ): Iterator[(Double, Option[Double])] =
    pairIterator(sc).takeWhile {
      case (_, optShift) => optShift.forall(_ > cutoff)
    }

  def stableWeight(cutoff: Double, sc: Double = 1): Double =
    prunedPairIterator(cutoff, sc).foldLeft(q0) { case (_, (y, _)) => y }
}

object EntropyAtomWeight {

  /**
    * More natural (but verbose) description of spire gradients for entropy tradeoff
    * @param genDist the initial generating distribution
    * @param pDist the initial "theorem by proof" distribution
    * @param qDist the initial "theorem by statement" distribution
    * @param elem the lemma whose weight is learned
    * @param initWeight the weight of the initial distribution in the final one
    * @tparam A the type of objects of the initial distribution, e.g. terms
    * @tparam B the type of objects of the final distribution, e.g. types
    */
  def apply[A, B](
      genDist: FiniteDistribution[A],
      pDist: FiniteDistribution[B],
      qDist: FiniteDistribution[B],
      elem: B,
      initWeight: Double,
      hW: Double,
      klW: Double
  ): EntropyAtomWeight =
    EntropyAtomWeight(
      FiniteDistribution.entropy(genDist),
      pDist.klDivergence(qDist),
      pDist(elem),
      qDist(elem),
      initWeight,
      hW,
      klW
    )

  def evolvedLemmaGens(
      ev: EvolvedStateLike,
      hW: Double, 
      klW: Double
  ): Vector[(Typ[Term], EntropyAtomWeight)] =
    ev.result.goalThmsBySt(ev.params.goalWeight).supp
      .filter(!ev.init.terms.map(_.typ).supp.contains(_))
      .map(
        lem =>
          lem -> EntropyAtomWeight[Term, Typ[Term]](
            ev.init.terms,
            ev.result.goalThmsBySt(ev.params.goalWeight),
            ev.result.thmsByPf,
            lem,
            ev.params.termInit,
            hW,
            klW
          )
      )

  def proofWeightIter(
      ev: EvolvedStateLike,
      pf: Term,
      weight: Double,
      cutoff: Double,
      hW: Double, 
      klW: Double,
      sc: Double = 1,
      prune: Boolean = false
  ) =
    EntropyAtomWeight[Term, Typ[Term]](
      ev.init.terms,
      ev.result.goalThmsBySt(ev.params.goalWeight),
      ev.result.thmsByPf,
      pf.typ,
      ev.params.termInit * weight,hW, klW
    ).iter(sc, prune).takeWhile(_ > cutoff)

  def proofWeightTuned(
      ev: EvolvedStateLike,
      pf: Term,
      weight: Double,
      cutoff: Double,
      steps: Int,
      hW: Double, 
      klW: Double,
      sc: Double = 1,
      prune: Boolean = false
  ): Option[(Term, Double)] =
    proofWeightIter(ev, pf, weight, cutoff, hW, klW, sc, prune).toStream
      .drop(steps)
      .headOption
      .map(pf -> _)

  def tunedProofs(
      ev: EvolvedStateLike,
      pfs: Vector[(Term, Double)],
      cutoff: Double,
      steps: Int,
      hW: Double, 
      klW: Double,
      sc: Double = 1,
      prune: Boolean = false
  ): Vector[(Term, Double)] =
    pfs.map {
      case (pf, w) => proofWeightTuned(ev, pf, w, cutoff, steps, hW, klW, sc, prune)
    }.flatten

  def evolvedLemmaIters(
      ev: EvolvedStateLike,
      hW: Double, 
      klW: Double,
      sc: Double = 1,
      prune: Boolean = false
  ): Vector[(Typ[Term], Iterator[Double])] =
    evolvedLemmaGens(ev, hW, klW).map {
      case (lemma, ew) => lemma -> ew.iter(sc, prune)
    }

  def lemmaWeights(
      ev: EvolvedStateLike,
      steps: Int,
      hW: Double, 
      klW: Double,
      sc: Double = 1
  ): Vector[(Typ[Term], Double)] = {
    for {
      (tp, it) <- evolvedLemmaIters(ev, hW, klW, sc)
      p   = it.drop(steps).toStream.head
      tpW = ev.result.goalThmsBySt(ev.params.goalWeight)(tp)
      if p > tpW * ev.params.termInit
    } yield tp -> p
  }.sortBy(-_._2)

  def lemmaDist(
      ev: EvolvedStateLike,
      steps: Int,
      hW: Double, 
      klW: Double,
      sc: Double = 1
  ): FiniteDistribution[Term] =
    FiniteDistribution(lemmaWeights(ev, steps, hW, klW, sc).map {
      case (tp, p) => Weighted("lemma" :: tp, p)
    })

  def findGoal(
      init: TermState,
      base: TermState,
      tg: TermGenParams,
      steps: Int,
      maxDepth: Int,
      cutoff: Double,
      hW: Double, 
      klW: Double,
      limit: FiniteDuration = 3.minutes,
      sc: Double = 1
  ): Task[FiniteDistribution[Term]] = {
    val pfs = base.terms.filter(x => base.goals(x.typ) > 0)
    Task(pfs).flatMap {
      case p if p.support.nonEmpty => Task(p)
      case _ if maxDepth == 0      => Task(FiniteDistribution.empty)
      case _ =>
        val ev = EvolvedState(init, base, tg, cutoff)
        val td = lemmaDist(ev, steps, hW, klW, sc)
        val nextTST = tg.nextTangStateTask(
          base,
          TermState(td, FiniteDistribution.empty),
          cutoff,
          limit
        )
        nextTST.flatMap { result =>
          findGoal(init, result, tg, steps, maxDepth - 1, cutoff, hW, klW, limit, sc)
        }
    }
  }
}
