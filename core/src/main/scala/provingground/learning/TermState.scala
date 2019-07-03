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

  // pprint.log(context.variables)

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

  lazy val successes: Vector[(HoTT.Typ[HoTT.Term], Double, FD[HoTT.Term])] =
    goals.filter(goal => terms.map(_.typ)(goal) > 0).pmf.map {
      case Weighted(goal, p) => (goal, p, terms.filter(_.typ == goal))
    }

  lazy val unknownStatements: FD[Typ[Term]] =
    typs
      .filter(typ => thmsByPf(typ) == 0 && thmsByPf(negate(typ)) == 0)
      .safeNormalized

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

  def addVar(typ: Typ[Term], varWeight: Double): (TermState, Term) = {
    val x =
      nextVar(typ, context.variables)
//      typ.Var
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

  def tangent(x: Term): TermState =
    this.copy(
      terms = FD.unif(x),
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

  def contextExport(ctx: Context) =
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