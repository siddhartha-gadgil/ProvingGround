package provingground.learning
import provingground._

import spire.math._
import spire.implicits._

case class EntropyAtomWeight(h0: Double,
                             kl0: Double,
                             p0: Double,
                             q0: Double,
                             initWeight: Double) {
  implicit val jetDim: JetDim = JetDim(1)

  val t: Jet[Double] = Jet.h[Double](0)

  def p1(x: Double): Jet[Double] = 1 / (1 + exp(x + t))

  def q1(x: Double): Jet[Double] = p1(x) * initWeight

  def h1(x: Double): Jet[Double] = {
    val p = p1(x)
    ((1 - p) * h0) - (p * log(p)) - ((1 - p) * log(1 - p))
  }

  def kl1(x: Double): Jet[Double] = {
    val q = q1(x)
    kl0 - ((1 - p0) * log(1 - q)) - (p0 * log(1 + (q / q0)))
  }

  def rat(x: Double): Jet[Double] = kl1(x) / h1(x)

  def shifted(x: Double, sc: Double = 1): Double =
    x - (rat(x).infinitesimal(0) * sc)

  def iterator(x: Double, sc: Double = 1): Iterator[Double] =
    Iterator.iterate(x)(y => shifted(y, sc))
}

object EntropyAtomWeight {
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
