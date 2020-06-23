package provingground.learning
import provingground.HoTT._
import provingground.{FiniteDistribution => FD, _}
import shapeless._
import induction._
import provingground.learning.GeneratorNode.{Map, MapOpt}

import scala.language.higherKinds
import GeneratorNode._
import TermRandomVars._

import TermGeneratorNodes._
import scala.math.Ordering.Double.TotalOrdering

/**
  * A state, typically the initial state, for generating terms, types etc
  *
  * @param terms distribution of terms
  * @param typs distribution of types
  * @param vars variables, over which we may take closures
  * @param inds inductive type definitions
  */
case class TermState(
    terms: FD[Term],
    typs: FD[Typ[Term]],
    vars: Vector[Term] = Vector(),
    inds: FD[ExstInducDefn] = FD.empty[ExstInducDefn],
    goals: FD[Typ[Term]] = FD.empty,
    context: Context = Context.Empty
) {
  def subs(x: Term, y: Term) =
    TermState(
      terms.map(_.replace(x, y)),
      typs.map(_.replace(x, y)),
      vars.map(_.replace(x, y)),
      inds.map(_.subs(x, y)),
      goals.map(_.replace(x, y)),
      context.subs(x, y)
    )

  def ++(that: TermState) =
    TermState(
      (terms ++ that.terms).safeNormalized,
      (typs ++ that.typs).safeNormalized,
      vars ++ that.vars,
      (inds ++ that.inds).safeNormalized,
      (goals ++ that.goals).safeNormalized,
      context
    )

  def export(variables: Vector[Term]) =
    TermState(
      terms.map(t => lambdaClosure(variables)(t)),
      typs.map(t => piClosure(variables)(t)),
      variables ++ vars.map(t => lambdaClosure(variables)(t)),
      inds,
      goals.map(t => piClosure(variables)(t)),
      context
    )

  def purge(epsilon: Double) =
    TermState(
      terms.purge(epsilon).safeNormalized,
      typs.purge(epsilon).safeNormalized,
      vars,
      inds,
      goals,
      context
    )

  def withTyps(fd: FD[Typ[Term]]): TermState = this.copy(typs = fd)

  lazy val allTyps = terms.support.map(_.typ) union typs.support

  lazy val extraTyps = terms.support.map(_.typ) -- typs.support

  lazy val thmsByPf: FD[Typ[Term]] =
    terms
      .map(_.typ)
      .flatten
      .filter((t) => typs(t) + goals(t) > 0)
      .safeNormalized

  lazy val thmsBySt: FD[Typ[Term]] =
    typs.filter(thmsByPf(_) > 0).flatten.safeNormalized

  def goalThmsBySt(goalW: Double) =
    (typs ++ goals).filter(terms.map(_.typ)(_) > 0).flatten.safeNormalized

  lazy val successes: Vector[(HoTT.Typ[HoTT.Term], Double, Term)] =
    goals.filter(goal => terms.map(_.typ)(goal) > 0).pmf.map {
      case Weighted(goal, p) =>
        (goal, p, terms.support.filter(_.typ == goal).maxBy(terms(_)))
    }

  lazy val contradicted: Vector[(HoTT.Typ[HoTT.Term], Double, FD[HoTT.Term])] =
    goals.filter(goal => terms.map(_.typ)(negate(goal)) > 0).pmf.map {
      case Weighted(goal, p) => (goal, p, terms.filter(_.typ == goal))
    }

  lazy val successTerms: Set[Term] =
    terms.support.filter(t => successes.map(_._1).toSet.contains(t.typ))

  def isProd(typ: Typ[Term]) = typ match {
    case _: ProdTyp[u, v] => true
    case _                => false
  }

  lazy val unknownStatements: FD[Typ[Term]] =
    typs
      .filter(
        typ => thmsByPf(typ) == 0 && thmsByPf(negate(typ)) == 0 && !isProd(typ)
      )
      .safeNormalized

  lazy val orderedUnknowns: Vector[HoTT.Typ[HoTT.Term]] =
    unknownStatements.entropyVec.map(_.elem)

  lazy val remainingGoals: FD[Typ[Term]] =
    goals
      .filter(typ => thmsByPf(typ) == 0 && thmsByPf(negate(typ)) == 0)
      .safeNormalized

  def subGoalsFromFunc(f: Term): FD[Typ[Term]] = f match {
    case fn: FuncLike[u, v] =>
      val typ = fn.dom
      if (thmsByPf(typ) == 0) {
        val base = FD.unif(typ: Typ[Term])
        val x    = typ.Var
        val g    = fn(x.asInstanceOf[u])
        val rec =
          if (g.typ.dependsOn(x)) FD.empty[Typ[Term]]
          else subGoalsFromFunc(g)
        (base ++ rec).safeNormalized
      } else {
        val pfs = terms.filter(_.typ == typ)
        pfs.flatMap(pf => subGoalsFromFunc(fn(pf.asInstanceOf[u])))
      }
    case _ => FD.empty
  }

  lazy val thmWeights: Vector[(Typ[Term], Double, Double, Double)] =
    (for {
      Weighted(x, p) <- thmsBySt.pmf
      q = thmsByPf(x)
      h = -p / (q * math.log(q))
    } yield (x, p, q, h)).sortBy(_._4).reverse

  lazy val pfSet: Vector[Term] =
    terms.flatten.supp.filter(t => thmsBySt(t.typ) > 0)
  lazy val fullPfSet: Vector[(Term, Term)] =
    pfSet.flatMap(pf => partialLambdaClosures(vars)(pf).map((pf, _)))

  lazy val pfMap: scala.collection.immutable.Map[Typ[Term], Vector[Term]] =
    pfSet.groupBy(_.typ: Typ[Term])

  lazy val pfDist: FD[Term] =
    terms.flatten.filter(t => thmsBySt(t.typ) > 0).safeNormalized

  lazy val lemmas: Vector[(Typ[Term], Option[Term], Double)] = thmsByPf.pmf
    .map {
      case Weighted(x, q) =>
        (
          x,
          pfMap.get(x).map(v => v.maxBy(pf => terms(pf))),
          -(thmsBySt(x) / (q * math.log(q)))
        )
    }
    .sortBy(_._3)
    .reverse

  def addVar(typ: Typ[Term], varWeight: Double): (TermState, Term) = {
    val x =
      nextVar(typ, context.variables)
    val newTerms = (FD.unif(x) * varWeight) ++ (terms * (1 - varWeight))
    val newGoals: FD[Typ[Term]] = goals.map {
      case pd: PiDefn[u, v] if pd.domain == typ => pd.fibers(x.asInstanceOf[u])
      case ft: FuncTyp[u, v] if ft.dom == typ   => ft.codom
      case tp                                   => tp
    }

    lazy val newTyps =
      typOpt(x)
        .map(tp => (FD.unif(tp) * varWeight) ++ (typs * (1 - varWeight)))
        .getOrElse(typs)
    TermState(
      newTerms,
      newTyps,
      x +: vars,
      inds,
      newGoals.flatten,
      context.addVariable(x)
    ) -> x
  }

  def addTerm(x: Term, varWeight: Double = 0.3) = {
    val newTerms = (FD.unif(x) * varWeight) ++ (terms * (1 - varWeight))
    val newGoals: FD[Typ[Term]] = goals.map {
      case pd: PiDefn[u, v] if pd.domain == x.typ =>
        pd.fibers(x.asInstanceOf[u])
      case ft: FuncTyp[u, v] if ft.dom == x.typ => ft.codom
      case tp                                   => tp
    }

    lazy val newTyps =
      typOpt(x)
        .map(tp => (FD.unif(tp) * varWeight) ++ (typs * (1 - varWeight)))
        .getOrElse(typs)
    TermState(
      newTerms,
      newTyps,
      x +: vars,
      inds,
      newGoals.flatten,
      context.addVariable(x)
    ) -> x
  }

  def tangent(xs: Term*): TermState =
    this.copy(
      terms = FD.uniform(xs),
      typs = FD.empty
    )

  def distTangent(fd: FD[Term]): TermState =
    this.copy(
      terms = fd,
      typs = FD.empty
    )

  import interface._, TermJson._

  def json: ujson.Obj = {
    ujson.Obj(
      "terms" -> fdJson(terms),
      "types" -> fdJson(typs.map((t) => t: Term)),
      "variables" -> ujson.Arr(
        vars.map((t) => termToJson(t).get): _*
      ),
      "goals"                -> fdJson(goals.map((t) => t: Term)),
      "inductive-structures" -> InducJson.fdJson(inds),
      "context"              -> ContextJson.toJson(context)
    )
  }

  def inIsle(x: Term) =
    TermState(
      terms.collect {
        case l: LambdaLike[u, v] if l.variable.typ == x.typ =>
          l.value.replace(l.variable, x)
      },
      typs.collect {
        case l: PiDefn[u, v] if l.variable.typ == x.typ =>
          l.value.replace(l.variable, x)
        case ft: FuncTyp[u, v] if ft.dom == x.typ =>
          ft.codom
      },
      vars :+ x,
      inds,
      goals.collect {
        case l: PiDefn[u, v] if l.variable.typ == x.typ =>
          l.value.replace(l.variable, x)
        case ft: FuncTyp[u, v] if ft.dom == x.typ =>
          ft.codom
      },
      Context.AppendVariable(context, x)
    )

  def contextExport(ctx: Context = context) =
    TermState(
      terms.map(ctx.exportStrict(_)),
      typs.map(ctx.exportTypStrict(_)),
      vars.filterNot(ctx.variables.contains(_)),
      inds,
      goals.map(ctx.exportTypStrict(_)),
      Context.Empty
    )

  def contextInit(ctx: Context, varWeight: Double) = {
    val typVars = ctx.variables.collect { case tp: Typ[u] => tp: Typ[Term] }
    TermState(
      ((terms * (1 - varWeight)) ++ (FD
        .uniform(ctx.variables) * varWeight)).safeNormalized,
      ((typs * (1 - varWeight)) ++ (FD
        .uniform(typVars) * varWeight)).safeNormalized,
      vars ++ ctx.variables,
      inds,
      goals.mapOpt(ctx.importTypOpt(_)),
      ctx
    )
  }

  def contextImport(ctx: Context) =
    TermState(
      terms.mapOpt(ctx.importOpt(_)),
      typs.mapOpt(ctx.importTypOpt(_)),
      vars ++ ctx.variables,
      inds,
      goals.mapOpt(ctx.importTypOpt(_)),
      ctx
    )
}

object TermState {
  val zero: TermState = TermState(FD.empty, FD.empty)

  import TermRandomVars._
  def fromJson(js: ujson.Value): TermState = {
    import interface._, TermJson._
    val obj     = js.obj
    val context = ContextJson.fromJson(obj("context"))
    val terms   = jsToFD(context.inducStruct)(obj("terms"))
    val typs = jsToFD(context.inducStruct)(obj("types")).map {
      case tp: Typ[Term] => tp
    }
    val goals = jsToFD(context.inducStruct)(obj("goals")).map {
      case tp: Typ[Term] => tp
    }
    val vars = obj("variables").arr.toVector
      .map((t) => jsToTermExst(context.inducStruct)(t).get)
    val inds = FD.empty[ExstInducDefn] //InducJson.jsToFD(context.inducStruct)(obj("inductive-structures"))
    TermState(terms, typs, vars, inds, goals, context)
  }

  import upickle.default._

  implicit val termStateRW: ReadWriter[TermState] =
    readwriter[ujson.Value].bimap(_.json, fromJson(_))

  /**
    * finite distributions on terms etc
    */
  implicit val stateFD: StateDistribution[TermState, FD] =
    new StateDistribution[TermState, FD] {
      def isEmpty(state: TermState): Boolean =
        state.terms.support.isEmpty && state.typs.support.isEmpty

      def value[T](state: TermState)(randomVar: RandomVar[T]): FD[T] =
        randomVar match {
          case Terms      => state.terms.map(x => x: T)
          case Typs       => state.typs.map(x => x: T)
          case TargetTyps => state.typs.map(x => x: T)
          case Funcs      => state.terms.condMap(FuncOpt).map(x => x: T)
          case TypFamilies =>
            state.terms
              .condMap(TypFamilyOpt)
              .map(x => x: T)
          case TypsAndFamilies =>
            state.terms.conditioned(isTypFamily).map(x => x: T)
          case InducDefns                   => state.inds.map(x => x: T)
          case InducStrucs                  => state.inds.map(_.ind).map(x => x: T)
          case Goals                        => state.goals.map(x => x: T)
          case IsleDomains                  => state.typs.map(x => x: T)
          case RandomVar.AtCoord(fmly, arg) => valueAt(state)(fmly, arg)
        }

      def valueAt[Dom <: HList, T](
          state: TermState
      )(randomVarFmly: RandomVarFamily[Dom, T], fullArg: Dom): FD[T] =
        (randomVarFmly, fullArg) match {
          case (TermsWithTyp, typ :: HNil) =>
            state.terms.conditioned(_.typ == typ).map(x => x: T)
          case (FuncsWithDomain, typ :: HNil) =>
            state.terms
              .condMap(FuncOpt)
              .conditioned(_.dom == typ)
              .map(x => x: T)
          case (FuncForCod, cod :: HNil) =>
            state.terms.condMap(Unify.targetCodomain(_, cod)).map(x => x: T)
          case _ =>
            throw new IllegalArgumentException(
              s"cannot find valueAt of TermState for $randomVarFmly at $fullArg"
            )
        }
    }

  def islePairs(
      ts: TermState,
      variable: Term
  ): Set[(RandomVar.Elem[_], RandomVar.Elem[_])] = {
    val termPairs: Set[(RandomVar.Elem[_], RandomVar.Elem[_])] =
      ts.terms.support.map { x =>
        (RandomVar.Elem(Terms, x), RandomVar.Elem(Terms, variable :~> x))
      }
    val typPairs: Set[(RandomVar.Elem[_], RandomVar.Elem[_])] =
      ts.typs.support.map { x =>
        (RandomVar.Elem(Typs, x), RandomVar.Elem(Typs, variable ~>: x))
      }
    val goalPairs: Set[(RandomVar.Elem[_], RandomVar.Elem[_])] =
      ts.goals.support.map { x =>
        (RandomVar.Elem(Goals, x), RandomVar.Elem(Goals, variable ~>: x))
      }
    termPairs union typPairs union goalPairs
  }

}

object LemmaWeigths {
  import math._
  def hDiff(x: Double, h0: Double) =
    (-x * h0) - (x * log(x)) - ((1 - x) * log(1 - x))

  def klDiff(x: Double, p0: Double, q0: Double) =
    (1 - p0) * log(1 - x) + p0 * log((q0 + x) / q0)

  def entDiff(
      x: Double,
      h0: Double,
      p0: Double,
      q0: Double,
      initWeight: Double,
      hW: Double,
      klW: Double
  ) =
    (hDiff(x, h0) * hW) - (klDiff(x * initWeight, p0, q0) * klW)

  def geomStep(a: Double, b: Double, j: Int, n: Int) =
    math.pow(a, (n.toDouble - j) / n) * math.pow(b, j.toDouble / n)

  def geomRangeVec(a: Double, b: Double, n: Int): Vector[Double] =
    (0 to n).toVector.map { j =>
      geomStep(a, b, j, n)
    }

  def leastEntropy(
      h0: Double,
      p0: Double,
      q0: Double,
      initWeight: Double = 0.5,
      hW: Double = 1,
      klW: Double = 1,
      n: Int = 100
  ): Option[(Double, Double)] = {
    def f(x: Double) = entDiff(x, h0, p0, q0, initWeight, hW, klW)
    val a            = q0 / initWeight
    val b            = p0 / initWeight
    if (f(a) > 0) None
    else
      Some(
        geomRangeVec(a, b, n)
          .map { y =>
            y -> f(y)
          }
          .minBy(_._2)
      )
  }

  def lemmas(
      ts: TermState,
      h0: Double,
      initWeight: Double,
      hW: Double,
      klW: Double,
      n: Int
  ) =
    for {
      Weighted(t, p0) <- ts.thmsBySt.pmf
      (q, w) <- LemmaWeigths.leastEntropy(
        h0,
        p0,
        ts.thmsByPf(t),
        initWeight,
        hW,
        klW,
        n
      )
    } yield (t, q)

  def lemmaDistribution(
      ts: TermState,
      h0: Double,
      initWeight: Double,
      hW: Double,
      klW: Double,
      n: Int
  ): FD[Typ[Term]] =
    FD(
      lemmas(ts, h0, initWeight, hW, klW, n).map {
        case (t, p) => Weighted(t, p)
      }
    )

  def boundedLemmaDistributionOpt(
      ts: TermState,
      h0: Double,
      initWeight: Double,
      hW: Double,
      klW: Double,
      maxEntropy: Double,
      hScale: Double = 32,
      steps: Int = 10,
      n: Int = 100
  ): Option[(Double, FD[HoTT.Typ[HoTT.Term]])] = {
    def bound(fd: FD[Typ[Term]]) = FD.entropy(fd) < maxEntropy
    def fn(x: Double)            = lemmaDistribution(ts, h0, initWeight, hW * x, klW, n)
    Utils.largestAcceptable(fn, bound, 1, hScale, steps)
  }

  def boundedLemmaDistribution(
      ts: TermState,
      h0: Double,
      initWeight: Double,
      hW: Double,
      klW: Double,
      maxEntropy: Double,
      hScale: Double = 32,
      steps: Int = 10,
      n: Int = 100
  ): FD[Typ[Term]] =
    boundedLemmaDistributionOpt(
      ts,
      h0,
      initWeight,
      hW,
      klW,
      maxEntropy,
      hScale,
      steps,
      n
    ).map(_._2).getOrElse(FD.empty[Typ[Term]])
}
