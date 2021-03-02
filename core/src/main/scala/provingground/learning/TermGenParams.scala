package provingground.learning
import provingground._
import HoTT._
import provingground.{FiniteDistribution => FD, _}
import shapeless._
import induction._

import TermRandomVars._
import monix.eval.Task
import interface._

import scala.concurrent._
import duration._
import upickle.default.{read, write}
import TermGenParams._, TermGeneratorNodes._
import ujson.Value

import scala.collection.mutable

object TermGenParams {
  def fromJson(js: ujson.Value): TermGenParams = {
    val m: mutable.Map[String, Value] = js.obj
    TermGenParams(
      appW = m("application").num,
      unAppW = m("unified-application").num,
      argAppW = m("application-by-argument").num,
      lmW = m("lambda").num,
      piW = m("pi-type").num,
      piTermW = m("pi-term").num,
      termsByTypW = m("terms-by-type").num,
      typFromFamilyW = m("type-from-family").num,
      sigmaW = m("sigma-type").num,
      recDefW = m("recursive-definition").num,
      inducDefW = m("inductive-definition").num,
      typAsCodW = m("type-as-codomain").num,
      targetInducW = m("targeted-induction").num,
      varWeight = m("variable-weight").num,
      goalWeight = m("goal-weight").num,
      typVsFamily = m("types-versus-families").num,
      negTargetW = m("negation-target").num,
      solverW = m("solver-weight").num,
      contraW = m("contradiction-weight").num
    )
  }

  import upickle.default._

  implicit val termGenParamsRW: ReadWriter[TermGenParams] =
    readwriter[ujson.Value].bimap(_.toJson, fromJson(_))

  lazy val rnd = new scala.util.Random

  def randomScale(
      tg: TermGenParams,
      scale: Double = 1.0,
      logShift: Double = 0
  ): TermGenParams = {
    def shift(v: ujson.Value): ujson.Value = {
      val sc = rnd.nextGaussian() * scale - logShift
      ujson.Num(v.num * math.exp(sc))
    }
    val jsonMap = tg.toJson.obj.view.mapValues(shift(_))
    val jsObj   = ujson.Obj(collection.mutable.LinkedHashMap(jsonMap.toSeq: _*))
    fromJson(jsObj)
  }

  val zero = TermGenParams(
    appW = 0,
    unAppW = 0,
    argAppW = 0,
    lmW = 0,
    piW = 0,
    termsByTypW = 0,
    typFromFamilyW = 0,
    sigmaW = 0,
    recDefW = 0,
    inducDefW = 0,
    typAsCodW = 0,
    targetInducW = 0,
    varWeight = 0.3,
    goalWeight = 0,
    typVsFamily = 0.5,
    negTargetW = 0,
    solverW = 0
  )

  def apple(w: Double = 0.1) = zero.copy(appW = w, unAppW = w)

}

case class TermGenParamsNodes(tg: TermGenParams)
    extends TermGeneratorNodes[TermState](
      { case (fn, arg) => applyFunc(fn.func, arg) },
      { case (fn, arg) => Unify.appln(fn.func, arg) },
      AddVar(_),
      GetVar,
      EnterIsle,
      // tg.solver
    )

object TermNodeCoeffSeq {
  def fromParams[State](tg: TermGenParams, gen: TermGeneratorNodes[State]) = {
    import tg._
    new TermNodeCoeffSeq[State](
      appW,
      unAppW,
      argAppW,
      lmW,
      piW,
      piTermW,
      termsByTypW,
      typFromFamilyW,
      sigmaW,
      recDefW,
      inducDefW,
      typAsCodW,
      targetInducW,
      varWeight,
      goalWeight,
      typVsFamily,
      negTargetW,
      solverW,
      contraW,
      solver
    ) {
      val Gen: TermGeneratorNodes[State] = gen
    }
  }
}
abstract class TermNodeCoeffSeq[State](
    appW: Double = 0.1,
    unAppW: Double = 0.1,
    argAppW: Double = 0.1,
    lmW: Double = 0.1,
    piW: Double = 0.1,
    piTermW: Double = 0,
    termsByTypW: Double = 0.05,
    typFromFamilyW: Double = 0.05,
    sigmaW: Double = 0.05,
    recDefW: Double = 0,
    inducDefW: Double = 0,
    typAsCodW: Double = 0.05,
    targetInducW: Double = 0,
    varWeight: Double = 0.3,
    goalWeight: Double = 0.7,
    typVsFamily: Double = 0.5,
    negTargetW: Double = 0,
    solverW: Double = 0,
    contraW: Double = 0,
    solver: TypSolver //= TypSolver.coreSolver
) {
  val Gen: TermGeneratorNodes[State]

  import Gen._, GeneratorNode._,
  TermRandomVars.{withTypNode => wtN, funcWithDomTermNode => fdtN}

  val termInit
      : Double = 1.0 - appW - unAppW - argAppW - lmW - termsByTypW - recDefW - inducDefW

  val typInit
      : Double = 1.0 - appW - unAppW - piW - sigmaW - typFromFamilyW - recDefW - inducDefW

  import NodeCoeffs.purge

  lazy val termNodes: NodeCoeffs[State, Double, HNil, Term] =
    purge(
      (Init(Terms)                        -> termInit) ::
        (applnNode                        -> appW) ::
        (unifApplnNode                    -> unAppW) ::
        (applnByArgNode                   -> argAppW) ::
        (lambdaNode                       -> lmW) ::
        ((piNode.|(typAsTermSort, Terms)) -> piTermW) ::
        (termsByTyps                      -> termsByTypW) ::
        (recFuncFoldedNode                -> recDefW) ::
        (inducFuncFoldedNode              -> inducDefW) ::
        Terms.target[State, Double, Term]
    )

  lazy val typNodes: NodeCoeffs[State, Double, HNil, Typ[Term]] =
    purge(
      (Init(Typs)                               -> typInit) ::
        (typApplnNode                           -> appW) ::
        (typUnifApplnNode                       -> unAppW) ::
        (piNode                                 -> piW) ::
        (sigmaNode                              -> sigmaW) ::
        (typFoldNode                            -> typFromFamilyW) ::
        ((recFuncFoldedNode.|(typSort, Typs))   -> recDefW) ::
        ((inducFuncFoldedNode.|(typSort, Typs)) -> inducDefW) ::
        Typs.target[State, Double, Typ[Term]]
    )

  lazy val inducNodes: NodeCoeffs[State, Double, HNil, ExstInducDefn] =
    (Init(InducDefns) -> 1.0) ::
      InducDefns.target[State, Double, ExstInducDefn]

  lazy val inducDomainNodes
      : NodeCoeffs[State, Double, ExstInducDefn :: HNil, Term] =
    (domainForDefnNodeFamily -> 1.0) ::
      DomForInduc.target[State, Double, Term]

  lazy val goalNodes: NodeCoeffs[State, Double, HNil, Typ[Term]] = (Init(
    Goals
  ) -> 1.0) :: Goals
    .target[State, Double, Typ[Term]]

  lazy val isleDomainsNode: NodeCoeffs[State, Double, HNil, Typ[
    Term
  ]] = (GeneratorNode
    .Map(Idty[Typ[Term]](), Typs, IsleDomains) -> 1.0) :: IsleDomains
    .target[State, Double, Typ[Term]]

  lazy val funcForCodNodes: NodeCoeffs[State, Double, Typ[Term] :: HNil, Term] =
    (codomainNodeFamily -> 1.0) ::
      FuncForCod.target[State, Double, Term]

  lazy val funcNodes: NodeCoeffs[State, Double, HNil, ExstFunc] =
    purge(
      (Init(Funcs)                                -> termInit) ::
        ((applnNode.|(funcSort, Funcs))           -> appW) ::
        ((unifApplnNode.|(funcSort, Funcs))       -> unAppW) ::
        ((applnByArgNode.|(funcSort, Funcs))      -> argAppW) ::
        ((lambdaNode.|(funcSort, Funcs))          -> lmW) ::
        ((termsByTyps.|(funcSort, Funcs))         -> termsByTypW) ::
        ((recFuncFoldedNode.|(funcSort, Funcs))   -> recDefW) ::
        ((inducFuncFoldedNode.|(funcSort, Funcs)) -> inducDefW) ::
        Funcs.target[State, Double, ExstFunc]
    )

  lazy val typFamilyNodes: NodeCoeffs[State, Double, HNil, ExstFunc] =
    purge(
      (Init(TypFamilies)                                     -> termInit) ::
        (typFamilyApplnNode                                  -> appW) ::
        (typFamilyUnifApplnNode                              -> unAppW) ::
        ((applnByArgNode.|(typFamilySort, TypFamilies))      -> argAppW) ::
        (lambdaTypFamilyNode                                 -> lmW) ::
        ((termsByTyps.|(typFamilySort, TypFamilies))         -> termsByTypW) ::
        ((recFuncFoldedNode.|(typFamilySort, TypFamilies))   -> recDefW) ::
        ((inducFuncFoldedNode.|(typFamilySort, TypFamilies)) -> inducDefW) ::
        TypFamilies.target[State, Double, ExstFunc]
    )

  lazy val termsByTypNodes: NodeCoeffs[State, Double, Typ[Term] :: HNil, Term] =
    purge(
      (TermsWithTyp.init            -> (termInit * (1 - goalWeight - typAsCodW - targetInducW - solverW))) ::
        (wtN(applnNode)             -> appW) ::
        (wtN(unifApplnNode)         -> unAppW) ::
        (wtN(applnByArgNode)        -> argAppW) ::
        (backwardTypNodeFamily      -> (termInit * goalWeight + lmW)) ::
        (curryBackwardTypNodeFamily -> (termInit * goalWeight + lmW)) :: // Warning: excess total weight
        (incl1TypNodeFamily         -> (termInit * goalWeight + lmW) / 2) ::
        (incl2TypNodeFamily         -> (termInit * goalWeight + lmW) / 2) ::
        (typAsCodNodeFamily         -> typAsCodW) ::
        (targetInducNodeFamily      -> targetInducW) ::
        (SolverNode(solver).node    -> solverW) ::
        (typViaZeroFamily           -> contraW) ::
        TermsWithTyp.target[State, Double, Term]
    )

  lazy val typOrFmlyNodes: NodeCoeffs[State, Double, HNil, Term] =
    (TypsAndFamilies.fromTyp        -> typVsFamily) ::
      (TypsAndFamilies.fromFamilies -> (1.0 - typVsFamily)) ::
      TypsAndFamilies.target[State, Double, Term]

  lazy val targTypNodes: NodeCoeffs[State, Double, HNil, Term] =
    (TargetTyps.fromGoal     -> goalWeight) ::
      (TargetTyps.fromTyp    -> (1.0 - goalWeight - negTargetW)) ::
      (TargetTyps.fromNegTyp -> negTargetW) ::
      TargetTyps.target[State, Double, Term]

  lazy val funcWithDomNodes
      : NodeCoeffs[State, Double, Typ[Term] :: HNil, ExstFunc] =
    purge(
      (FuncsWithDomain.init         -> termInit) ::
        (fdtN(applnNode)            -> appW) ::
        (fdtN(unifApplnNode)        -> unAppW) ::
        (fdtN(applnByArgNode)       -> argAppW) ::
        (lambdaForFuncWithDomFamily -> lmW) ::
        FuncsWithDomain.target[State, Double, ExstFunc]
    )

  lazy val nodeCoeffSeq: NodeCoeffSeq[State, Double] =
    funcWithDomNodes +: targTypNodes +: goalNodes +: isleDomainsNode +: inducDomainNodes +: inducNodes +: funcForCodNodes +:
      termNodes +: typNodes +: funcNodes +: typFamilyNodes +: typOrFmlyNodes +: funcWithDomNodes +: termsByTypNodes +:
      NodeCoeffSeq.Empty[State, Double]()

}
case class TermGenParams(
    appW: Double = 0.1,
    unAppW: Double = 0.1,
    argAppW: Double = 0.1,
    lmW: Double = 0.1,
    piW: Double = 0.1,
    piTermW: Double = 0,
    termsByTypW: Double = 0.05,
    typFromFamilyW: Double = 0.05,
    sigmaW: Double = 0.05,
    recDefW: Double = 0,
    inducDefW: Double = 0,
    typAsCodW: Double = 0.05,
    targetInducW: Double = 0,
    varWeight: Double = 0.3,
    goalWeight: Double = 0.7,
    typVsFamily: Double = 0.5,
    negTargetW: Double = 0,
    solverW: Double = 0,
    contraW: Double = 0,
    solver: TypSolver = TypSolver.coreSolver
) extends TermNodeCoeffSeq[TermState](
      appW,
      unAppW,
      argAppW,
      lmW,
      piW,
      piTermW,
      termsByTypW,
      typFromFamilyW,
      sigmaW,
      recDefW,
      inducDefW,
      typVsFamily,
      typVsFamily,
      varWeight,
      goalWeight,
      typVsFamily,
      negTargetW,
      solverW,
      contraW,
      solver
    ) { tg =>

  val Gen: TermGeneratorNodes[TermState] =
    // if (varWeight == 0.3 && solver == TypSolver.coreSolver)
      TermGeneratorNodes.Base
    // else TermGenParamsNodes(this)

  def coeffVal(cf: Expression.Coeff[_]) = cf.getOpt(nodeCoeffSeq)

  val toJson: ujson.Value =
    ujson.Obj(
      "application"             -> appW,
      "unified-application"     -> unAppW,
      "application-by-argument" -> argAppW,
      "lambda"                  -> lmW,
      "pi-type"                 -> piW,
      "pi-term"                 -> piTermW,
      "terms-by-type"           -> termsByTypW,
      "type-from-family"        -> typFromFamilyW,
      "sigma-type"              -> sigmaW,
      "recursive-definition"    -> recDefW,
      "inductive-definition"    -> inducDefW,
      "type-as-codomain"        -> typAsCodW,
      "targeted-induction"      -> targetInducW,
      "variable-weight"         -> varWeight,
      "goal-weight"             -> goalWeight,
      "types-versus-families"   -> typVsFamily,
      "negation-target"         -> negTargetW,
      "solver-weight"           -> solverW,
      "contradiction-weight"    -> contraW
    )

  lazy val monixFD: MonixFiniteDistribution[TermState] =
    MonixFiniteDistribution(nodeCoeffSeq, varWeight)

  lazy val monixEqFD: MonixFiniteDistributionEq[TermState] =
    MonixFiniteDistributionEq(nodeCoeffSeq, varWeight)

  def monixTangFD(baseState: TermState) =
    MonixTangentFiniteDistribution(nodeCoeffSeq, varWeight, baseState)

  def nextStateTask(
      initState: TermState,
      epsilon: Double,
      limit: FiniteDuration = 3.minutes
  ): Task[TermState] =
    for {
      terms <- monixFD.varDist(initState)(Terms, epsilon, limit)
      typs  <- monixFD.varDist(initState)(Typs, epsilon, limit)
    } yield
      TermState(
        terms,
        typs,
        initState.vars,
        initState.inds,
        initState.goals,
        initState.context
      )

  def evolvedStateTask(
      initState: TermState,
      epsilon: Double,
      limit: FiniteDuration = 3.minutes
  ): Task[EvolvedState] =
    nextStateTask(initState, epsilon, limit).map(
      result => EvolvedState(initState, result, tg, epsilon)
    )

  def nextTangStateTask(
      baseState: TermState,
      tangState: TermState,
      epsilon: Double,
      limit: FiniteDuration = 3.minutes
  ): Task[TermState] =
    for {
      terms <- monixTangFD(baseState).varDist(tangState)(Terms, epsilon, limit)
      typs  <- monixTangFD(baseState).varDist(tangState)(Typs, epsilon, limit)
    } yield TermState(terms, typs, baseState.vars, baseState.inds)

  def findProof(
      initState: TermState,
      typ: Typ[Term],
      epsilon: Double,
      limit: FiniteDuration = 3.minutes
  ): Task[FD[Term]] =
    monixFD
      .varDist(initState)(TermsWithTyp.at(typ :: HNil), epsilon, limit)
      .map(_.flatten)
}

trait EvolvedStateLike {
  val init: TermsTypThms
  val result: TermsTypThms
  val params: TermGenParams

  val goalsAttained: Set[Typ[Term]] =
    init.goals.support.intersect(result.terms.support.map(_.typ))

  val foundGoal: Boolean = goalsAttained.nonEmpty

  def goalNewThmsBySt(goalW: Double) =
    (result.typs ++ result.goals)
      .filter(
        typ =>
          result.terms.map(_.typ)(typ) > 0 && init.terms.map(_.typ)(typ) == 0
      )
      .flatten
      .safeNormalized
}

case class EvolvedState(
    init: TermState,
    result: TermState,
    params: TermGenParams,
    epsilon: Double
) extends EvolvedStateLike

object TermGenJson {

  def nextStateTask(inp: String): Task[String] = {
    val obj           = ujson.read(inp).obj
    val termGenParams = read[TermGenParams](obj("generator-parameters").str)
    val epsilon       = obj("epsilon").num
    val initState     = TermState.fromJson(obj("initial-state"))
    val task          = termGenParams.nextStateTask(initState, epsilon)
    task.map { (ts) =>
      write(ts.json)
    }
  }

  def nextTangStateTask(inp: String): Task[String] = {
    val obj           = read[ujson.Value](inp).obj
    val termGenParams = read[TermGenParams](obj("generator-parameters").str)
    val epsilon       = obj("epsilon").num
    val baseState     = TermState.fromJson(obj("initial-state"))
    val tangState     = TermState.fromJson(obj("tangent-state"))
    val task          = termGenParams.nextTangStateTask(baseState, tangState, epsilon)
    task.map((ts) => write(ts.json))
  }

  val all =
    MultiTask(
      "step"         -> nextStateTask,
      "tangent-step" -> nextTangStateTask
    )

}
