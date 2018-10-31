package provingground.learning
import provingground._, HoTT._

import spire.algebra._
import spire.math._
import spire.implicits._

import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD}

import GeneratorVariables._

case class SpireGradient(vars: Vector[VarVal[_]],
                         p: Map[VarVal[_], Double],
                         cost: Expression) {

  /**
    * map from formal variable to its index
    */
  lazy val variableIndex: Map[VarVal[_], Int] =
    vars.zipWithIndex.toMap

  implicit val dim: JetDim = JetDim(vars.size)

  implicit val jetField: Field[Jet[Double]] = implicitly[Field[Jet[Double]]]

  /**
    * exponential multiplicative tangent jet of values of evolver variables
    */
  def spireProb(p: Map[VarVal[_], Double]): Map[VarVal[_], Jet[Double]] =
    vars.zipWithIndex.map {
      case (v, n) =>
        v -> (p.getOrElse(v, 0.0) * exp(Jet.h[Double](n)))
    }.toMap

  def spireUpdate(p: Map[VarVal[_], Double],
                  tang: Vector[Double]): Map[VarVal[_], Double] =
    vars.zipWithIndex.map {
      case (v, n) =>
        v -> (p.getOrElse(v, 0.0) * exp(tang(n)))
    }.toMap

  def spireGradShifted(epsilon: Double): Map[VarVal[_], Double] = {
    val tang = costJet.infinitesimal.toVector.map { (x) =>
      -x * epsilon
    }
    spireUpdate(p, tang)
  }

  val costJet: Jet[Double] = jet(p)(cost)

  def jet(p: Map[VarVal[_], Double])(expr: Expression): Jet[Double] =
    expr match {
      case value: VarVal[_] => spireProb(p)(value)
      case Log(exp)         => log(jet(p)(exp))
      case Sum(x, y)        => jet(p)(x) + jet(p)(y)
      case Product(x, y)    => jet(p)(x) * jet(p)(y)
      case Literal(value)   => value
      case Quotient(x, y)   => jet(p)(x) / jet(p)(y)
    }

}

object SpireGradient {
  import TermRandomVars.{Terms, Typs}
  def kl(ts: TermState): Expression = {
    val thmTot =
      ts.thmsBySt.supp
        .map { (thm) =>
          FinalVal(Elem(thm, Typs))
        }
        .reduce[Expression](_ + _)

    val pqs =
      ts.pfMap.map {
        case (thm, pfs) =>
          val pfTot =
            pfs.map((t) => FinalVal(Elem(t, Terms))).reduce[Expression](_ + _)
          (FinalVal(Elem(thm, Typs)), pfTot)
      }.toVector
    pqs
      .map { case (p, q) => (p / thmTot) * Log(q / p) }
      .reduce[Expression](_ + _)
  }

  def h(ts: Vector[Term]): Expression =
    ts.map((t) => InitialVal(Elem(t, Terms))).reduce[Expression](_ + _)

  def termGenCost(ge: GeneratorEquations[TermState, Term],
                  hW: Double = 1,
                  klW: Double = 1,
                  eqW: Double = 1): Sum =
    (kl(ge.finalState) * klW) + (h(ge.initState.terms.supp) * hW) + (ge.mse * eqW)

}

import SpireGradient._

case class TermGenCost(ge: GeneratorEquations[TermState, Term],
                       hW: Double = 1,
                       klW: Double = 1,
                       eqW: Double = 1) {
  val cost
    : Sum = (kl(ge.finalState) * klW) + (h(ge.initState.terms.supp) * hW) + (ge.mse * eqW)

  val vars: Vector[VarVal[_]] =
    (ge.elemInitVars.values.flatten.map(v => InitialVal(v): VarVal[_])++ ge.finalVars.map(v =>
      FinalVal(v): VarVal[_])).toVector

}
