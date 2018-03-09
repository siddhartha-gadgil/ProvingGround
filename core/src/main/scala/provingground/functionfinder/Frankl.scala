package provingground

import spire.math._
import spire.implicits._
import spire.algebra._
import scala.util.Random

object Frankl {
  case class SetSystem(sets: Set[Set[Int]]) extends AnyVal {
    def size = sets.size

    def neSets = sets.filter(!_.isEmpty)

    def properSize = neSets.size

    def suppSet = sets.foldLeft[Set[Int]](Set())(_ union _)

    def suppMaxOpt: Option[Int] =
      if (suppSet.isEmpty) None else Some(suppSet.max)

    def freq(k: Int) = sets.filter(_.contains(k)).size

    def frequencies =
      suppSet.toVector.map((k) => (k, freq(k))).sortBy { case (k, n) => -n }

    def common = suppSet.filter((k) => size <= 2 * freq(k))

    def inAll = suppSet.filter((k) => freq(k) == properSize)

    def proj(p: Int => Boolean) = SetSystem(sets.map(_.filter(p)))

    def normalize = {
      val perm: Map[Int, Int] = {
        frequencies.map(_._1).zipWithIndex.map { case (a, b) => (a, b + 1) }
      }.toMap
      val permSets: Set[Set[Int]] = sets.map((s) => s.map(perm))
      SetSystem(permSets)
    }

    def union(that: SetSystem) = SetSystem(sets union (that.sets))

    def join(that: SetSystem) =
      SetSystem(
        for { s <- sets; t <- that.sets } yield s union t
      )

    def addElem(n: Int) = SetSystem(sets.map(_ + n))

    def isUnionClosed = {
      val notClosedBools =
        for {
          s <- sets
          t <- sets
        } yield (sets.contains(s union t))
      notClosedBools.foldLeft(true)(_ && _)
    }

    def buildWith(that: SetSystem, n: Int): SetSystem = {
      val b = that.addElem(n)
      union(b).union(join(b))

    }

    def projFreqs = {
      val weightedElems =
        for {
          s <- neSets.toVector
          scVec = s.toVector.map((x) => x -> Rational(1, (s.size * properSize)))
          w <- scVec
        } yield w
      weightedElems
        .groupBy(_._1)
        .mapValues((v) => v.map(_._2).foldLeft(Rational(0))(_ + _))

    }

    def freqExp(m: Map[Int, Rational]) =
      suppSet.toVector.map((k) => freq(k) * m(k)).foldLeft(Rational(0))(_ + _)

    def franklProj = freqExp(projFreqs) * 2 >= size

    def quotAll(that: SetSystem) = proj((x) => !(that.inAll.contains(x)))

    def conjFails(that: SetSystem) = join(that).size < quotAll(that).size

    def conj2Fails(that: SetSystem) = join(that).size + 1 < quotAll(that).size

    // def conjMinFails(that: SetSystem) = join(that).size < quotAll(that).size.min(that.quotAll(this).size)
  }

  object SetSystem {
    def apply(sets: Set[Int]*): SetSystem = SetSystem(sets.toSet)

    val trivial = SetSystem(Set(Set[Int]()))

    val empty = SetSystem(Set[Set[Int]]())

    val rnd = new Random

    def getRandom(n: Int, p: Double): SetSystem =
      if (n == 0) {
        if (rnd.nextDouble < p) trivial else empty
      } else {
        getRandom(n - 1, p).buildWith(getRandom(n - 1, p), n)
      }

    def buildWithAll(coll1: Set[SetSystem],
                     coll2: Set[SetSystem],
                     n: Int): Set[SetSystem] =
      for {
        sys1 <- coll1
        sys2 <- coll2
      } yield sys1.buildWith(sys2, n)

    lazy val allSystems: Stream[Set[SetSystem]] =
      Stream.from(0).map { (n) =>
        if (n == 0) Set[SetSystem](trivial)
        else
          buildWithAll(allSystems.take(n).toSet.flatten + empty,
                       normalSystems.take(n).toSet.flatten,
                       n)
      }

    def allTill(n: Int) = allSystems.take(n).flatten.distinct

    def normalTill(n: Int) = normalSystems.take(n).flatten.distinct

    lazy val normalSystems =
      allSystems.map { (s) =>
        s.map(_.normalize)
      }

  }
}
