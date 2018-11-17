package provingground.learning
import provingground._

import spire.math._
import spire.implicits._

/**
* Spire gradient learning for tradeoff between generation entropy and theorem-proof relative entropy
  * @param h0 the initial generation entropy
  * @param kl0 the initial relative entropy between theorems and proofs
  * @param p0 the initial theorem weight of the element
  * @param q0 the initial proof weight of the element
  * @param initWeight the weight of the initial distribution in generation of terms
  */
case class EntropyAtomWeight(h0: Double,
                             kl0: Double,
                             p0: Double,
                             q0: Double,
                             initWeight: Double) {
  implicit val jetDim: JetDim = JetDim(1)

  val t: Jet[Double] = Jet.h[Double](0)

  def pInit(x: Double): Jet[Double] = 1 / (1 + exp(x + t))

  def q1(x: Double): Jet[Double] = pInit(x) * initWeight

  def h1(x: Double): Jet[Double] = {
    val p = pInit(x)
    ((1 - p) * h0) - (p * log(p)) - ((1 - p) * log(1 - p))
  }

  def kl1(x: Double): Jet[Double] = {
    val q = q1(x)
    kl0 - ((1 - p0) * log(1 - q)) - (p0 * log(1 - q + (q / q0)))
  }

  def rat(x: Double): Jet[Double] = kl1(x) / h1(x)

  def ratShifted(x: Double, sc: Double = 1): Double =
    x - (rat(x).infinitesimal(0) * sc)

  def ratIterator(x: Double, sc: Double = 1): Iterator[Double] =
    Iterator.iterate(x)(y => ratShifted(y, sc))

  def tot(x: Double): Jet[Double] = kl1(x) + h1(x)

  def totShifted(x: Double, sc: Double = 1): Double =
    x - (tot(x).infinitesimal(0) * sc)

  def totIterator(x: Double, sc: Double = 1): Iterator[Double] =
    Iterator.iterate(x)(y => totShifted(y, sc))
}

object EntropyAtomWeight {
  /**
  * More natural (but verbose) description of spire gradients for entropy tradeoff
    * @param genDist the initial generating distribution
    * @param pDist the initial "theorem by proof" distribution
    * @param qDist the initial "theorem by statement" distribution
    * @param elem the lemma whose weight is learned
    * @param initWeight
    * @tparam A
    * @tparam B
    * @return
    */
  def apply[A, B](genDist: FiniteDistribution[A],
                  pDist: FiniteDistribution[B],
                  qDist: FiniteDistribution[B],
                  elem: B,
                  initWeight: Double): EntropyAtomWeight =
    EntropyAtomWeight(FiniteDistribution.entropy(genDist),
                      pDist.klDivergence(qDist),
                      pDist(elem),
                      qDist(elem),
                      initWeight)
}
