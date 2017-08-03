package provingground.learning
import provingground._

import provingground.{ FiniteDistribution => FD, ProbabilityDistribution => PD }

import learning.{ TangVec => T }

import cats._
import cats.implicits._

import monix.eval._
import monix.cats._

import monix.execution.Scheduler.Implicits.global
import monix.reactive._

import scala.language.higherKinds

trait TangSamples[X[_]] {
  implicit val monad: Monad[X]

  def sample[A](pd: PD[A], n: Int): X[Map[A, Int]]

  // Override for concurrency etc
  def sequence[A](v: Vector[X[A]]): X[Vector[A]] =
    Traverse[Vector].sequence[X, A](v)

  def sampFD[A](pd: PD[A], n: Int) =
    for (samp <- sample(pd, n)) yield TermEvolver.toFD(samp)

  def batchSampFD[A](pd: PD[A], batches: Int, n: Int) =
    sequence(
      (1 to batches).toVector.map((_) => sampFD(pd, n))).map((vfd) => vfd.reduce(_ ++ _).normalized())

  def tangSizes[A](n: Int)(base: FD[A]): X[Vector[(FD[A], Int)]]
}

object Samples {
  def sampleFD[A, X[_]](p: PD[A], n: Int)(implicit s: TangSamples[X]) =
    s.sampFD(p, n)

  def batchSample[A, X[_]](pd: PD[A], batches: Int, n: Int)(
    implicit
    s: TangSamples[X]) =
    s.batchSampFD(pd, batches, n)
}

trait Samples[X[_]] extends TangSamples[X] {
  def tangSizes[A](n: Int)(base: FD[A]) =
    sample(base, n).map { (samp) =>
      for { (a, n) <- samp.toVector } yield (FD.unif(a), n)
    }
}

trait MonixSamples extends Samples[Task] {
  implicit val monad = MonixSamples.monad

  override def sequence[A](v: Vector[Task[A]]) =
    Task.gatherUnordered(v) map (_.toVector)
}

object MonixSamples {
  implicit val monad = implicitly[Monad[Task]]
}
