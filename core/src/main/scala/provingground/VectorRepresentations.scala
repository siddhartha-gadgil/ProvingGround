package provingground

import scala.annotation._
import scala.util._
import provingground.Collections._
import math._

import scala.language.implicitConversions

object VectorRepresentations {
  case class WeightVect[T](elem: T, vect: Vector[Double]) {
    def scale(s: Double) = WeightVect(elem, vect map (_ * s))

    def pos = WeightVect(elem, vect map (math.max(_, 0)))

    def norm = sqrt(vect.map((x: Double) => x * x).sum)

    def weighted = Weighted(elem, norm)
  }

  object WeightVect {
    def add(first: Vector[Double], second: Vector[Double]) =
      (first, second).zipped.map(_ + _)

    @tailrec def sum(vs: Seq[Vector[Double]], accum: Vector[Double] = Vector(0))
      : Vector[Double] =
      if (vs.isEmpty) Vector(0)
      else if (vs.tail.isEmpty) vs.head
      else sum(vs.tail, vs.head)
  }

  def randomVec(length: Int, damp: Double = 0.0) = {
    val rnd = new Random
    val raw =
      ((0 until length) map (_ => damp + (1 - damp) * rnd.nextDouble)).toVector
    val total = raw.sum
    raw map (_ * (1 / total))
  }

  lazy val rand = new Random

  case class Representation[T](rep: Vector[WeightVect[T]]) extends AnyVal {
    def pmf = rep map (_.weighted)

    def support = (Weighted.flatten(pmf) map (_.elem)).toVector

    def norm = (pmf map (_.weight.abs)).sum

    def next = Weighted.pick(pmf.toSeq, rand.nextDouble)

    def get(label: T) = rep find (_.elem == label) map (_.vect)

    def getsum(label: T) =
      WeightVect.sum((rep.toSeq filter (_.elem == label) map (_.vect)))

    def apply(label: T) = getsum(label)

    def flatdist = support map ((l) => WeightVect(l, getsum(l)))

    def flatten = Representation(flatdist)

    private def posrep(t: Double = 0.0) =
      flatdist map (_.pos) filter (_.norm > t)

    private def postotal(t: Double = 0.0) =
      ((posrep(t) map (_.norm))).sum ensuring (_ > 0)

    def normalized(t: Double = 0.0) =
      Representation(posrep(t) map (_.scale(1.0 / postotal(t))))

    def *(sc: Double) = Representation(rep map (_.scale(sc)))

    def +(elem: T, vect: Vector[Double]) =
      this ++ Representation(Vector(WeightVect(elem, vect)))

    def ++(that: Representation[T]) = {
//        val combined = (for (k <- support union that.support) yield WeightVect(k, WeightVect.add(apply(k), that(k))))
      Representation(rep ++ that.rep)
    }

    def map[S](f: T => S) = {
      val newrep = for (WeightVect(elem, vec) <- rep) yield
        WeightVect(f(elem), vec)
//        val newpmf = for (Weighted(elem, wt) <- pmf) yield Weighted(f(elem), wt) 
//        FiniteDistribution(newpmf).flatten
      Representation(newrep)
    }

    def feedback(baseweights: T => Double, damp: Double = 0.1) = {
      val rawdiff = for (Weighted(pres, prob) <- pmf) yield
        (Weighted(pres, baseweights(pres) / (baseweights(pres) * damp + prob)))
      val shift = rawdiff.map(_.weight).sum / (support.size)
      val normaldiff = for (Weighted(pres, prob) <- rawdiff) yield
        Weighted(pres, prob - shift)
      FiniteDistribution(normaldiff)
    }

    override def toString = {
      val sortedpmf = pmf.toSeq.sortBy(1 - _.weight)
      val terms = (for (Weighted(elem, wt) <- sortedpmf) yield
        (elem.toString + " : " + wt.toString + ", ")).foldLeft("")(_ + _)
      "[" + terms.dropRight(2) + "]"
    }
  }

  object Representation {
    def empty[T] = Representation[T](Vector())
  }

  implicit def VecRepVec[T] =
    LinearStructure[Representation[T]](
        Representation.empty[T], _ ++ _, (w, d) => d * w)
}
