package provingground.learning
import provingground._
import HoTT._
import monix.eval.Task

import scala.concurrent._
import duration._

import TermRandomVars._

import GeneratorVariables._

import EntropyAtomWeight._

case class Prover(initState: TermState,
                  tg: TermGenParams,
                  cutoff: Double,
                  limit: FiniteDuration = 3.minutes,
                  maxRatio: Double = 1.01,
                  epsilon: Double = 1.0,
                  steps: Int = 10000) {
  val nextStateT: Task[TermState] =
    tg.nextStateTask(initState, cutoff, limit).memoize

  val evolvedStateT: Task[EvolvedState] = nextStateT.map(
    result => EvolvedState(initState, result, tg, cutoff)
  ).memoize

  val mfd: MonixFiniteDistributionEq[TermState, Term] =
    MonixFiniteDistributionEq(tg.nodeCoeffSeq)

  val pairT: Task[(FiniteDistribution[Term], Set[EquationTerm])] =
    mfd.varDist(initState)(Terms, cutoff)

  val equationTermsT: Task[Set[EquationTerm]] = pairT.map(_._2).memoize

  val equationsT: Task[Set[Equation]] = equationTermsT.map { eqs =>
    groupEquations(eqs)
  }.memoize

  val expressionEvalT: Task[ExpressionEval] =
    (for {
      fs  <- nextStateT
      eqs <- equationsT
    } yield ExpressionEval(initState, fs, eqs, tg)).memoize

  val lemmaWeightsT: Task[Vector[(Typ[Term], Double)]] =
    (for {
      ev <- evolvedStateT
    } yield lemmaWeights(ev, steps, epsilon)).memoize

  val withLemmas: Task[Prover] =
    for {
      lws <- lemmaWeightsT
      lfd = FiniteDistribution(lws.map{case (tp, p) => Weighted("lemma" :: tp, p)})
      lInit = initState.copy(terms = (initState.terms ++ lfd).safeNormalized)
    } yield this.copy(initState = lInit)
}
