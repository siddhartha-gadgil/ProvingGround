package provingground.scratch
import monix.execution.Scheduler.Implicits.global

import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD, _}
import HoTT._
import translation._
import learning._
import Sampler._
import FansiShow._

object DiffMonixAB {
  val A = "A" :: Type
  val B = "B" :: Type

  val fd = FD.unif[Term](A, B)

  val simpleObs = TermEvolutionStep.observable(fd)

  val thmsObs = simpleObs.map(TermEvolver.topTheorems(_, 25))

  lazy val showTheorems = thmsObs.foreach((x) => println(s"Theorems:\n${x.fansi}\n\n"))

  lazy val showEv = simpleObs.foreach(println)
}

object FDMonixAB {
  val A = "A" :: Type
  val B = "B" :: Type

  val fd = FD.unif[Term](A, B)

  val simpleObs = FineDeducerStep.observable(fd)

  val thmsObs = simpleObs.map(TermEvolver.topTheorems(_, 25))

  lazy val showTheorems = thmsObs.foreach((x) => println(s"Theorems:\n${x.fansi}\n\n"))

  lazy val showEv = simpleObs.foreach(println)
}
