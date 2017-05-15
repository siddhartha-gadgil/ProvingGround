package provingground.scratch
import monix.execution.Scheduler.Implicits.global

import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD, _}
import HoTT._
import Sampler._
import FansiShow._

object DiffMonixAB{
  val A = "A" :: Type
  val B = "B" :: Type

  val fd = FD.unif[Term](A, B)

  val simpleObs = TermEvolutionStep.observable(fd)

  val thmsObs = simpleObs.map(TermEvolver.topTheorems(_, 25))

  lazy val showTheorems = thmsObs.foreach((x) => println(x.fansi))

  lazy val showEv = simpleObs.foreach(println)
}
