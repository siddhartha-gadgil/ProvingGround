package provingground

import HoTT._

import HoTTgen._

import FiniteDistributionLearner.NewVertex

object HoTTgenSpec{
  /**
  * distribution with lambda, identity and application, to test for modus ponens.
  */
  val lamAppl = FiniteDistribution.unif(Move.id : Move, Move.lambda, Move.appl)

  val A = "A" :: __

  val B = "B" :: __

  val a = "a" :: A

  val ff = "ff" :: A ->: A ->: B

  /**
  * initial state to test for modus ponens.
  */
  val mpInit = FiniteDistribution.unif(A : Term, B, A ->: B)

  val mpBigInit = FiniteDistribution.unif(A : Term, B, A ->: B, "f" :: A ->: B)

  val mpSimpleInit = FiniteDistribution.unif(A : Term, B, A ->: B, "f" :: A ->: B, a)

  val vertAdd = NewVertex(a : Term)((1.0, mpInit))

  def mpEvolve(n: Int) = hottDyn(n)((lamAppl, mpInit))

  def mpBigEvolve(n: Int) = hottDyn(n)((lamAppl, mpBigInit))

  def mpSimpleEvolve(n: Int) = hottDyn(n)((lamAppl, mpSimpleInit))

  val repApplInit = FiniteDistribution.unif(a: Term, ff)

  def repApplEvolve(n: Int) = hottDyn(n)((lamAppl, repApplInit))
}
