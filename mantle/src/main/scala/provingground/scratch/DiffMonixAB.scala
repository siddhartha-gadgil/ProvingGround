package provingground.scratch
import monix.execution.Scheduler.Implicits.global

import provingground.{
  FiniteDistribution => FD,
  // ProbabilityDistribution => PD,
  _
}
import HoTT._
import translation._
import learning._
import Sampler._
import FansiShow._

import ammonite.ops._

object DiffMonixAB {
  val A = "A" :: Type
  val B = "B" :: Type

  val fd = FD.unif[Term](A, B)

  val simpleObs = TermEvolutionStep.observable(fd)

  val thmsObs = simpleObs.map(TermEvolver.topTheorems(_, 25))

  lazy val showTheorems =
    thmsObs.foreach((x) => println(s"Theorems:\n${x.fansi}\n\n"))

  lazy val showEv = simpleObs.foreach(println)
}

object FDMonixAB {
  val A = "A" :: Type
  val B = "B" :: Type

  val a = "a" :: A

  val b = "b" :: B

  val f = a :-> (b :-> a)

  val g = a :-> (b :-> b)

  val fd = FD.unif[Term](A, B)

  val simpleObs = FineDeducerStep.observable(fd)

  val thmsObs = simpleObs.map(TermEvolver.topTheorems(_, 25))

  lazy val showTheorems =
    thmsObs.foreach((x) => println(s"Theorems:\n${x.fansi}\n\n"))

  lazy val showEv = simpleObs.foreach(println)

  val ABfile = pwd / "data" / "ABentropy.txt"

  def update(fd: FD[Typ[Term]]) = {
    println(fd.supp.size)
    write.over(ABfile, "")
    fd.entropyVec.foreach {
      case Weighted(x, p) => write.append(ABfile, s"${x.fansi} -> $p\n")
    }
  }

  lazy val saveTheorems =
    simpleObs.map(TermEvolver.theorems).foreach(update)

  lazy val obs = FineDeducerStep.obserEv(fd)

  import math.log

  lazy val showFG =
    obs.foreach { (st) =>
      st.succFD.foreach { (nst) =>
        println(s"${-log(st.p(f))} -> ${-log(nst(f))}")
        println(s"${-log(st.p(g))} -> ${-log(nst(g))}\n\n")
      }
    }

}

object MonoidEv {
  import library.MonoidSimple._

  val obs = FineDeducerStep.observable(
    p = dist,
    fd = new FineDeducer(applnWeight = 0.2, lambdaWeight = 0, piWeight = 0),
    param = FineDeducerStep.Param(vars = Vector(a, b, c)))

  val viewThms =
    Vector(
      eqM(l)(l),
      a ~>: (b ~>: (eqM(a)(b) ->: eqM(b)(a))),
      eqM(op(l)(r))(l),
      eqM(op(l)(r))(r),
      eqM(l)(op(l)(r)),
      eqM(r)(op(l)(r)),
      eqM(l)(r)
    )

  import math.log
  lazy val showLemmas =
    obs.foreach { (fd) =>
      val thms = fd.map(_.typ)
      viewThms.foreach { (lem) =>
        println(s"lemma: ${lem.fansi} -> ${-log(fd(lem))}")
        println(s"proof: ${lem.fansi} -> ${-log(thms(lem))}")
      }
      println("\n")
    }
}
