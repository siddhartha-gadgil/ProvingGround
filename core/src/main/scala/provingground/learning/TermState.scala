package provingground.learning
import provingground.HoTT._
import provingground.{FiniteDistribution => FD, _}
import shapeless._
import induction._
import provingground.learning.GeneratorNode.{Map, MapOpt}

import scala.language.higherKinds
import GeneratorNode._
import TermRandomVars._
import scala.collection.parallel.immutable._

import TermGeneratorNodes._
import scala.math.Ordering.Double.TotalOrdering

trait TermsTypThms { self =>
  val terms: FD[Term]
  val typs: FD[Typ[Term]]
  val vars: Vector[Term]
  val inds: FD[ExstInducDefn]
  val goals: FD[Typ[Term]]
  val context: Context

  lazy val termSet = terms.support

  lazy val typSet = typs.support

  lazy val termTypsSet = termSet.map(_.typ)

  lazy val goalSet = goals.support

  lazy val thmsByPf: FD[Typ[Term]] =
    terms
      .map(_.typ)
      .flatten
      .filter((t) => (typSet.contains(t)) || goalSet.contains(t))
      .safeNormalized

  lazy val thmsBySt: FD[Typ[Term]] =
    typs.filter(termTypsSet.contains(_)).flatten.safeNormalized

  lazy val thmWeights: Vector[(Typ[Term], Double, Double, Double)] =
    (for {
      Weighted(x, p) <- thmsBySt.pmf
      q = thmsByPf(x)
      h = -p / (q * math.log(q))
    } yield (x, p, q, h)).sortBy(_._4).reverse

  def goalThmsBySt(goalW: Double) =
    (typs ++ goals).filter(termTypsSet.contains(_)).flatten.safeNormalized

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

  def tangent(xs: Term*): TermsTypThms = new TermsTypThms {
    val terms: FD[HoTT.Term] = FD.uniform(xs.toSeq)

    val typs: FD[HoTT.Typ[HoTT.Term]] = FD.empty

    val vars: Vector[HoTT.Term] = self.vars

    val inds: FD[ExstInducDefn] = self.inds

    val goals: FD[HoTT.Typ[HoTT.Term]] = self.goals

    val context: Context = self.context

  }
}

object TermsTypThms {
  import interface._, TermJson._

  def fromJson(js: ujson.Value): TermsTypThms = new TermsTypThms {
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

    val inds: FD[ExstInducDefn] = FD.empty[ExstInducDefn]

  }
}

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
) extends TermsTypThms {
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

  lazy val termDistMap: ParMap[Term, Double] = terms.toParMap

  lazy val typDistMap = typs.toParMap

  lazy val funcDist = terms.condMap(FuncOpt)

  lazy val funcDistMap: ParMap[ExstFunc, Double] = {
    val base = termDistMap.flatMap {
      case (x, w) =>
        ExstFunc.opt(x).map { fn =>
          fn -> w
        }
    }
    val total = base.values.sum
    if (total > 0) base.mapValues(_ * (1 / total)).to(ParMap)
    else ParMap.empty
  }

  lazy val thmsSet: ParSet[Typ[Term]] = termDistMap.keySet
    .map(_.typ)
    .intersect(typDistMap.keySet union (goalSet))
    .to(ParSet)

  lazy val thmsByStParMap: ParMap[Typ[Term], Double] = {
    val base  = typDistMap.filterKeys(thmsSet.contains(_))
    val total = base.values.sum
    if (total > 0) base.mapValues(_ * (1 / total)).to(ParMap)
    else ParMap.empty
  }

  lazy val thmsByPfParMap: ParMap[Typ[Term], Double] =
    thmsSet.map(tp => tp -> termsWithTypsMap(tp).map(_._2).sum).to(ParMap)

  lazy val typFamilyDist = terms.condMap(TypFamilyOpt)

  lazy val typFamilyDistMap: ParMap[ExstFunc, Double] = {
    val base = termDistMap.flatMap {
      case (x, w) =>
        TypFamilyOpt(x).map { fn =>
          fn -> w
        }
    }
    val total = base.values.sum
    if (total > 0) base.mapValues(_ * (1 / total)).to(ParMap)
    else ParMap.empty
  }

  lazy val typOrFamilyDist = terms.conditioned(isTypFamily)

  lazy val typOrFamilyDistMap = typOrFamilyDist.toMap

  lazy val termTyps = terms.support.map(_.typ : Typ[Term])

  lazy val termWithTyps = termTyps
    .map(
      typ => (typ: Typ[Term]) -> (terms.conditioned(_.typ == typ): FD[Term])
    )
    .toMap

  lazy val termsWithTypsMap
      : ParMap[HoTT.Typ[HoTT.Term], ParMap[HoTT.Term, Double]] =
    termDistMap.groupBy(_._1.typ: Typ[Term]).map {
      case (typ, m) =>
        val total = m.values.sum
        typ -> m.map { case (x, p) => (x, p / total) }
    }
  // termTyps
  //   .map(
  //     typ =>
  //       (typ: Typ[Term]) -> (terms.conditioned(_.typ == typ): FD[Term]).toMap
  //   )
  //   .toMap

  lazy val domTotals = funcDistMap.groupBy(_._1).mapValues(_.values.sum)

  lazy val funcsWithDoms =
    termTyps
      .map(
        typ =>
          (typ: Typ[Term]) -> (terms
            .condMap(FuncOpt)
            .conditioned(_.dom == typ): FD[ExstFunc])
      )
      .toMap

  lazy val funcsWithDomsMap =
    funcDistMap.groupBy(_._1.dom: Typ[Term]).map {
      case (typ, m) =>
        val total = m.values.sum
        typ -> m.map { case (x, p) => (x, p / total) }
    }

  lazy val allTyps = termTyps union typs.support union termSet.collect { case t: Typ[u] => t : Typ[Term]}

  lazy val extraTyps = terms.support.map(_.typ) -- typs.support

  lazy val thmsByStMap = thmsBySt.toMap

  lazy val thmsByPfMap = thmsByPf.pmf.map { case Weighted(x, p) => (x, p) }.toMap

  lazy val successes: Vector[(HoTT.Typ[HoTT.Term], Double, Term)] =
    goals.filter(goal => termTypsSet.contains(goal)).pmf.map {
      case Weighted(goal, p) =>
        (goal, p, terms.support.filter(_.typ == goal).maxBy(terms(_)))
    }

  lazy val contradicted: Vector[(HoTT.Typ[HoTT.Term], Double, FD[HoTT.Term])] =
    goals.filter(goal => termTypsSet.contains(negate(goal))).pmf.map {
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
        typ =>
          !termTypsSet.contains(typ) && !termTypsSet
            .contains(negate(typ)) && !isProd(typ)
      )
      .safeNormalized

  lazy val orderedUnknowns: Vector[HoTT.Typ[HoTT.Term]] =
    unknownStatements.entropyVec.map(_.elem)

  lazy val remainingGoals: FD[Typ[Term]] =
    goals
      .filter(
        typ => !termTypsSet.contains(typ) && !termTypsSet.contains(negate(typ))
      )
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

  lazy val pfSet: Vector[Term] =
    terms.flatten.supp.filter(t => typSet.contains(t.typ))

  lazy val fullPfSet: Vector[(Term, Term)] =
    pfSet.flatMap(pf => partialLambdaClosures(vars)(pf).map((pf, _)))

  lazy val pfMap: scala.collection.immutable.Map[Typ[Term], Vector[Term]] =
    pfSet.groupBy(_.typ: Typ[Term])

  lazy val pfDist: FD[Term] =
    terms.flatten.filter(t => typSet.contains(t.typ)).safeNormalized

  lazy val bestProofs =
    pfDist.pmf
      .groupBy(_.elem.typ: Typ[Term])
      .view
      .filterKeys(termTypsSet.contains(_))
      .mapValues(_.maxBy(_.weight).elem)
      .toMap

  lazy val lemmas: Vector[(Typ[Term], Option[Term], Double)] = thmsByPf.pmf
    .map {
      case Weighted(x, q) =>
        (
          x,
          bestProofs.get(x),
          -(thmsByStMap(x) / (q * math.log(q)))
        )
    }
    .sortBy(_._3)
    .reverse

  lazy val lemmasPar: Vector[(Typ[Term], Option[Term], Double)] = thmsSet
    .map { tp =>
      val pfs = termsWithTypsMap(tp)
      val q   = pfs.map(_._2).sum
      (
        tp,
        Some(pfs.maxBy(_._2)._1),
        -(thmsByStParMap(tp) / (q * math.log(q)))
      )
    }
    .to(Vector)
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

  override def tangent(xs: Term*): TermState =
    this.copy(
      terms = FD.uniform(xs),
      typs = FD.empty
    )

  def distTangent(fd: FD[Term]): TermState =
    this.copy(
      terms = fd,
      typs = FD.empty
    )

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
  import TermRandomVars._

  def elemVal(elem: Any, randomVar: RandomVar[_]): Double =
    (elem, randomVar) match {
      case (x: Term, Terms)           => termDistMap.getOrElse(x, 0)
      case (x: Typ[u], Typs)          => typDistMap.getOrElse(x, 0)
      case (x: Typ[u], TargetTyps)    => typDistMap.getOrElse(x, 0)
      case (x: ExstFunc, Funcs)       => funcDistMap.getOrElse(x, 0)
      case (x: ExstFunc, TypFamilies) => typFamilyDistMap.getOrElse(x, 0)
      case (x: Term, TypsAndFamilies) =>
        ExstFunc
          .opt(x)
          .map(fn => typFamilyDistMap.getOrElse(fn, 0.0))
          .getOrElse(0.0) +
          typOpt(x).map(typ => typDistMap.getOrElse(typ, 0.0)).getOrElse(0.0)
      case (x: ExstInducDefn, InducDefns)    => inds(x)
      case (x: ExstInducStrucs, InducStrucs) => inds.map(_.ind)(x)
      case (x: Typ[Term], Goals)             => goals(x)
      case (x: Typ[Term], IsleDomains)       => typDistMap.getOrElse(x, 0)
      case (x, RandomVar.AtCoord(fmly: RandomVarFamily[u, t], arg)) =>
        (x, fmly, arg) match {
          case (x: Term, TermsWithTyp, (typ: Typ[u]) :: HNil) =>
            termsWithTypsMap.get(typ).map(_.getOrElse(x, 0.0)).getOrElse(0.0)
          case (x: ExstFunc, FuncsWithDomain, (typ: Typ[u]) :: HNil) =>
            funcsWithDomsMap.get(typ).map(_.getOrElse(x, 0.0)).getOrElse(0.0)
          case (x: Term, FuncForCod, (cod: Typ[u]) :: HNil) =>
            terms
              .condMap(Unify.targetCodomain(_, cod))(x)
          case _ =>
            throw new IllegalArgumentException(
              s"cannot find valueAt of TermState for $fmly at $arg"
            )
        }

    }
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
          case Terms      => state.terms.asInstanceOf[FD[T]]
          case Typs       => state.typs.asInstanceOf[FD[T]]
          case TargetTyps => state.typs.asInstanceOf[FD[T]]
          case Funcs      => state.funcDist.asInstanceOf[FD[T]]
          case TypFamilies =>
            state.typFamilyDist
              .asInstanceOf[FD[T]]
          case TypsAndFamilies =>
            state.typFamilyDist.asInstanceOf[FD[T]]
          case InducDefns                   => state.inds.asInstanceOf[FD[T]]
          case InducStrucs                  => state.inds.map(_.ind).asInstanceOf[FD[T]]
          case Goals                        => state.goals.asInstanceOf[FD[T]]
          case IsleDomains                  => state.typs.asInstanceOf[FD[T]]
          case RandomVar.AtCoord(fmly, arg) => valueAt(state)(fmly, arg)
        }

      def valueAt[Dom <: HList, T](
          state: TermState
      )(randomVarFmly: RandomVarFamily[Dom, T], fullArg: Dom): FD[T] =
        (randomVarFmly, fullArg) match {
          case (TermsWithTyp, typ :: HNil) =>
            state.termWithTyps
              .get(typ)
              .map(_.asInstanceOf[FD[T]])
              .getOrElse(FD.empty[T])
          case (FuncsWithDomain, typ :: HNil) =>
            state.funcsWithDoms
              .get(typ)
              .map(_.asInstanceOf[FD[T]])
              .getOrElse(FD.empty[T])
          case (FuncForCod, cod :: HNil) =>
            state.terms
              .condMap(Unify.targetCodomain(_, cod))
              .asInstanceOf[FD[T]]
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
