package provingground.learning
import provingground._
import HoTT._
import monix.eval.Task

import scala.concurrent._
import duration._

import TermRandomVars._

import GeneratorVariables._

import EntropyAtomWeight._

case class Prover(
    initState: TermState,
    tg: TermGenParams,
    cutoff: Double,
    limit: FiniteDuration = 3.minutes,
    maxRatio: Double = 1.01,
    scale: Double = 1.0,
    steps: Int = 10000,
    maxDepth: Int = 10,
    hW: Double = 1,
    klW: Double = 1
) {
  val nextStateT: Task[TermState] =
    tg.nextStateTask(initState, cutoff, limit).memoize

  val evolvedStateT: Task[EvolvedState] = nextStateT
    .map(
      result => EvolvedState(initState, result, tg, cutoff)
    )
    .memoize

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
    } yield lemmaWeights(ev, steps, scale)).memoize

  val withLemmas: Task[Prover] =
    for {
      lws <- lemmaWeightsT
      lfd = FiniteDistribution(lws.map {
        case (tp, p) => Weighted("lemma" :: tp, p)
      })
      lInit = initState.copy(terms = (initState.terms ++ lfd).safeNormalized)
    } yield this.copy(initState = lInit)

  val seekT: Task[FiniteDistribution[Term]] =
    for {
      base <- nextStateT
      goals <- findGoal(
        initState,
        base,
        tg,
        steps,
        maxDepth,
        cutoff,
        limit,
        scale
      )
    } yield goals

  val optimalInit: Task[Prover] = expressionEvalT.map{
    ev =>
      val p = ev.optimum(hW, klW)
      val td: FiniteDistribution[Term] = ExpressionEval.dist(Terms, p)
      val ts = initState.copy(terms = td)
      this.copy(initState = ts)
  }

}
