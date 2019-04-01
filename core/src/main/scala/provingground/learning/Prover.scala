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
    initState: TermState = TermState(FiniteDistribution.empty, FiniteDistribution.empty),
    tg: TermGenParams = TermGenParams(),
    cutoff: Double = math.pow(10, -4),
    limit: FiniteDuration = 3.minutes,
    maxRatio: Double = 1.01,
    scale: Double = 1.0,
    steps: Int = 10000,
    maxDepth: Int = 10,
    hW: Double = 1,
    klW: Double = 1
) {
  // Convenience for generation
  def addTerms(terms: (Term, Double)*): Prover =  {
    val total = terms.map(_._2).sum
    val td = initState.terms * (1 - total) ++ FiniteDistribution(terms.map{case (x, p) => Weighted(x, p)})
    val ts = initState.copy(terms = td)
    this.copy(initState = ts)
  }

  lazy val typesByRestrict: Prover = {
    val typd = initState.terms.collect{case tp: Typ[Term] => tp}.safeNormalized
    val ts = initState.copy(typs = typd)
    this.copy(initState = ts)
  }

  lazy val typesByMap: Prover = {
    val typd = initState.terms.map(_.typ)
    val ts = initState.copy(typs = typd)
    this.copy(initState = ts)
  }

  def addVars(terms: (Term, Double)*): Prover =  {
    val total = terms.map(_._2).sum
    val td = initState.terms * (1 - total) ++ FiniteDistribution(terms.map{case (x, p) => Weighted(x, p)})
    val allVars = initState.vars ++ terms.map(_._1).toVector
    val ctx = terms.foldLeft(initState.context){case (cx: Context, (y,_)) => cx.addVariable(y)}
    val ts = initState.copy(terms = td.safeNormalized, vars = allVars, context = ctx)
    this.copy(initState = ts)
  }

  def addTypes(typs: (Typ[Term], Double)*): Prover =  {
    val total = typs.map(_._2).sum
    val typd = initState.typs * (1 - total) ++ FiniteDistribution(typs.map{case (x, p) => Weighted(x, p)})
    val ts = initState.copy(typs = typd.safeNormalized)
    this.copy(initState = ts)
  }

  def addAxioms(typs: (Typ[Term], Double)*): Prover =  {
    val total = typs.map(_._2).sum
    val td = initState.terms * (1 - total) ++ FiniteDistribution(typs.map{case (x, p) => Weighted("axiom":: x, p)})
    val ts = initState.copy(terms = td.safeNormalized)
    this.copy(initState = ts)
  }

  def addGoals(typs: (Typ[Term], Double)*): Prover =  {
    val total = typs.map(_._2).sum
    val typd = initState.goals * (1 - total) ++ FiniteDistribution(typs.map{case (x, p) => Weighted(x, p)})
    val ts = initState.copy(goals = typd.safeNormalized)
    this.copy(initState = ts)
  }

  def addInd(typ: Typ[Term], intros: Term*)(params: Vector[Term] = Vector(), weight: Double = 1): Prover = {
    import induction._
    val str0 = ExstInducStrucs.get(typ, intros.toVector)
    val str = params.foldRight(str0){case (x, s) => ExstInducStrucs.LambdaInduc(x, s)}
    val dfn = ExstInducDefn(typ, intros.toVector, str)
    val indsNew = initState.inds * (1 - weight) ++ FiniteDistribution.unif(dfn)
    val ts = initState.copy(inds = indsNew)
    this.copy(initState = ts)
  }

  def addIndexedInd(typ: Term, intros: Term*)(params: Vector[Term] = Vector(), weight: Double = 1): Prover = {
    import induction._
    val str0 = ExstInducStrucs.getIndexed(typ, intros.toVector)
    val str = params.foldRight(str0){case (x, s) => ExstInducStrucs.LambdaInduc(x, s)}
    val dfn = ExstInducDefn(typ, intros.toVector, str)
    val indsNew = initState.inds * (1 - weight) ++ FiniteDistribution.unif(dfn)
    val ts = initState.copy(inds = indsNew)
    this.copy(initState = ts)
  }


  // Proving etc
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
