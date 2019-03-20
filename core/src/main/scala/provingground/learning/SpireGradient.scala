package provingground.learning
import provingground._, HoTT._

import spire.algebra._
import spire.math._
import spire.implicits._

import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD}

import GeneratorVariables._

case class SpireGradient(
    vars: Vector[Expression],
    p: Map[Expression, Double],
    cost: Expression
) {

  /**
    * map from formal variable to its index
    */
  lazy val variableIndex: Map[Expression, Int] =
    vars.zipWithIndex.toMap

  implicit val dim: JetDim = JetDim(vars.size)

  implicit val jetField: Field[Jet[Double]] = implicitly[Field[Jet[Double]]]

  /**
    * exponential multiplicative tangent jet of values of evolver variables
    */
  def spireProb(p: Map[Expression, Double]): Map[Expression, Jet[Double]] =
    vars.zipWithIndex.map {
      case (v, n) =>
        val t: Jet[Double] = Jet.h[Double](n)
        val r: Double      = p.getOrElse(v, 0.0)
        v -> (t + r)
    }.toMap

  def spireUpdate(
      p: Map[Expression, Double],
      tang: Vector[Double]
  ): Map[Expression, Double] =
    vars.zipWithIndex.map {
      case (v, n) =>
        v -> (p.getOrElse(v, 0.0) * exp(tang(n)))
    }.toMap

  def gradient(epsilon: Double): Map[Expression, Double] = {
    val tang = costJet.infinitesimal.toVector.map { (x) =>
      -x * epsilon
    }
    spireUpdate(p, tang)
  }

  lazy val costJet: Jet[Double] = jet(p)(cost)

  def jet(p: Map[Expression, Double])(expr: Expression): Jet[Double] =
    spireProb(p).getOrElse(
      expr,
      expr match {
        case Log(exp)       => log(jet(p)(exp))
        case Exp(x)         => exp(jet(p)(x))
        case Sum(x, y)      => jet(p)(x) + jet(p)(y)
        case Product(x, y)  => jet(p)(x) * jet(p)(y)
        case Literal(value) => value
        case Quotient(x, y) => jet(p)(x) / jet(p)(y)
        case coeff          => p(coeff)

      }
    )

}

object SpireGradient {
  import TermRandomVars.{Terms, Typs}

  import GeneratorVariables._

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
    ts.map((t) => InitialVal(Elem(t, Terms))).map(p => -p * Log(p)).reduce[Expression](_ + _)

  def termGenCost(
      ge: EvolvedEquations[TermState, Term],
      hW: Double = 1,
      klW: Double = 1,
      eqW: Double = 1,
      epsilon: Double = math.pow(10, -5)
  ): Sum =
    (kl(ge.finalState) * klW) + (h(ge.initState.terms.supp) * hW) + (ge.mse(
      epsilon
    ) * eqW)

  val sd: StateDistribution[TermState, FD] = TermState.stateFD

  def elemProb[X](state: TermState)(el: Elem[X]): Double =
    sd.value(state)(el.randomVar)(el.element)

  def eventProb[X, Y](state: TermState)(ev: Event[X, Y]): Double =
    ev.sort match {
      case Sort.All()        => 1.0
      case Sort.Filter(pred) => sd.value(state)(ev.base).filter(pred).total
      case Sort.Restrict(optMap) =>
        sd.value(state)(ev.base).mapOpt(optMap).total
    }

  def pairEventProb[X1, X2, Y](
      state: TermState
  )(ev: PairEvent[X1, X2, Y]): Double =
    ev.sort match {
      case Sort.All() => 1.0
      case Sort.Filter(pred) =>
        sd.value(state)(ev.base1)
          .zip(sd.value(state)(ev.base2))
          .filter(pred)
          .total
      case Sort.Restrict(optMap) =>
        sd.value(state)(ev.base1)
          .zip(sd.value(state)(ev.base2))
          .mapOpt(optMap)
          .total
    }

  def varValue[X](initState: TermState, finalState: TermState)(
      vv: VarVal[X]
  ): Double =
    vv match {
      case FinalVal(variable) =>
        variable match {
          case el @ Elem(element, randomVar) => elemProb(finalState)(el)
          case ev @ Event(base, sort)        => eventProb(finalState)(ev)
          case ev @ PairEvent(base1, base2, sort) =>
            pairEventProb(finalState)(ev)
          case el: InIsle[X, TermState, o, Term] =>
            val (st, newBoat: Term) = el.isle.initMap(initState)
            varValue(
              st.subs(newBoat, el.boat),
              el.isle.finalMap(el.boat, finalState)
            )(FinalVal(el.isleVar))
//          case NodeCoeff(nodeFamily) => 0
        }
      case InitialVal(variable) =>
        variable match {
          case el @ Elem(element, randomVar) => elemProb(initState)(el)
          case ev @ Event(base, sort)        => eventProb(initState)(ev)
          case ev @ PairEvent(base1, base2, sort) =>
            pairEventProb(initState)(ev)
          case el: InIsle[X, TermState, o, Term] =>
            val (st, newBoat: Term) = el.isle.initMap(initState)
            varValue(
              st.subs(newBoat, el.boat),
              el.isle.finalMap(el.boat, finalState)
            )(InitialVal(el.isleVar))
//          case NodeCoeff(nodeFamily)       => 0
        }
    }
}

import SpireGradient._

abstract class TermGenEqCost(
    ge: EvolvedEquations[TermState, Term],
    hW: Double = 1,
    klW: Double = 1,
    eqW: Double = 1,
    epsilon: Double = math.pow(10, -5)
) {
  val cost
      : Expression = (kl(ge.finalState) * klW) + (h(ge.initState.terms.supp) * hW) + (ge
    .mse(epsilon) * eqW)

  lazy val vars: Vector[VarVal[_]] =
    ge.equations
      .flatMap(eq => Set(eq.lhs, eq.rhs))
      .flatMap(expr => Expression.varVals(expr))
      .toVector

  val p: Map[Expression, Double]

  lazy val spireGradient = SpireGradient(vars, p, cost)

  def grad(epsilon: Double = 1): Map[Expression, Double] =
    spireGradient.gradient(epsilon)
}

case class TermGenCost(
    ge: EvolvedEquations[TermState, Term],
    hW: Double = 1,
    klW: Double = 1,
    eqW: Double = 1,
    epsilon: Double = math.pow(10, -5)
) extends TermGenEqCost(ge, hW, klW, eqW, epsilon) {
  lazy val p: Map[Expression, Double] =
    vars.map((vv) => vv -> varValue(ge.initState, ge.finalState)(vv)).toMap
}
