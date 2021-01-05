package provingground.learning

import provingground._, HoTT._, induction._

import scala.collection.parallel._
import shapeless._, HList._

import TermRandomVars._, TermGeneratorNodes._
import scala.collection.parallel.immutable.ParVector
import ParMapState._

case class ParMapState(
    termDist: ParMap[Term, Double],
    typDist: ParMap[Typ[Term], Double],
    vars: Vector[Term] = Vector(),
    inds: ParMap[ExstInducDefn, Double] = ParMap(),
    goalDist: ParMap[Typ[Term], Double] = ParMap(),
    context: Context = Context.Empty
) {
  lazy val funcDist: ParMap[HoTT.ExstFunc, Double] = {
    val base = termDist.flatMap {
      case (x, w) => ExstFunc.opt(x).map { _ -> w }
    }
    val total = base.values.sum
    if (total > 0) base.mapValues(_ / total).to(ParMap)
    else ParMap.empty[ExstFunc, Double]
  }

  lazy val typFamilyDist: ParMap[HoTT.ExstFunc, Double] = {
    val base = termDist.flatMap {
      case (x, w) => TypFamilyOpt(x).map { _ -> w }
    }
    val total = base.values.sum
    if (total > 0) base.mapValues(_ / total).to(ParMap)
    else ParMap.empty[ExstFunc, Double]
  }

  lazy val (termWithTypDist, typTotalMap) = {
    val base     = termDist.groupBy(_._1.typ: Typ[Term]).to(ParMap)
    val totalMap = base.mapValues(_.values.sum)
    (base.map {
      case (typ, m) => (typ, m.mapValues(_ / totalMap(typ)).to(ParMap))
    }, totalMap)
  }

  lazy val funcWithDomDist = {
    val base     = funcDist.groupBy(_._1.dom: Typ[Term]).to(ParMap)
    val totalMap = base.mapValues(_.values.sum)
    base.map {
      case (typ, m) => (typ, m.mapValues(_ / totalMap(typ)).to(ParMap))
    }
  }

  lazy val inducStrucs = inds.map { case (ind, w) => ind.ind -> w }

  def value[T](randomVar: RandomVar[T]): ParMap[T, Double] = randomVar match {
    case Terms       => termDist.to(ParMap)
    case Typs        => typDist.to(ParMap)
    case TargetTyps  => typDist.to(ParMap)
    case Goals       => goalDist.to(ParMap)
    case Funcs       => funcDist.to(ParMap)
    case TypFamilies => typFamilyDist.to(ParMap)
    case TypsAndFamilies =>
      typFamilyDist.map { case (x, w) => (x.func: Term, w) }.to(ParMap)
    case IsleDomains                  => typDist.to(ParMap)
    case InducDefns                   => inds.to(ParMap)
    case InducStrucs                  => inducStrucs.to(ParMap)
    case RandomVar.AtCoord(fmly, arg) => valueAt(fmly, arg)
  }

  def valueAt[Dom <: HList, T](
      randomVarFmly: RandomVarFamily[Dom, T],
      fullArg: Dom
  ): ParMap[T, Double] =
    (randomVarFmly, fullArg) match {
      case (TermsWithTyp, typ :: HNil)    => termWithTypDist(typ).to(ParMap)
      case (FuncsWithDomain, dom :: HNil) => funcWithDomDist(dom).to(ParMap)
      case (FuncForCod, cod :: HNil) =>
        val base = termDist.flatMap {
          case (x, w) => Unify.targetCodomain(x, cod).map(_ -> w)
        }
        val total = base.values.sum
        if (total > 0) base.mapValues(_ / total).to(ParMap)
        else ParMap.empty[T, Double]
      case _ =>
        throw new IllegalArgumentException(
          s"cannot find valueAt of ParMapState for $randomVarFmly at $fullArg"
        )
    }

  def addVar(typ: Typ[Term], varWeight: Double): (ParMapState, Term) = {
    val x =
      nextVar(typ, context.variables)
    val newTerms = termDist.mapValues(_ * (1 - varWeight)) + (x -> varWeight)
    val newGoals: ParMap[Typ[Term], Double] = makeMap(
      goalDist.to(ParVector).map {
        case (pd: PiDefn[u, v], w) if pd.domain == typ =>
          (pd.fibers(x.asInstanceOf[u]), w)
        case (ft: FuncTyp[u, v], w) if ft.dom == typ => (ft.codom, w)
        case (tp, w)                                 => (tp, w)
      }
    )

    lazy val newTyps =
      typOpt(x)
        .map(
          tp =>
            (typDist
              .mapValues(_ * (1 - varWeight)) + ((tp: Typ[Term]) -> varWeight))
              .to(ParMap)
        )
        .getOrElse(typDist)
    ParMapState(
      newTerms.to(ParMap),
      newTyps,
      x +: vars,
      inds,
      newGoals,
      context.addVariable(x)
    ) -> x
  }

  def inIsle(x: Term): ParMapState =
    ParMapState(
      termDist.collect {
        case (l: LambdaLike[u, v], w) if l.variable.typ == x.typ =>
          (l.value.replace(l.variable, x), w)
      },
      typDist.collect {
        case (l: PiDefn[u, v], w) if l.variable.typ == x.typ =>
          (l.value.replace(l.variable, x), w)
        case (ft: FuncTyp[u, v], w) if ft.dom == x.typ =>
          (ft.codom, w)
      },
      vars :+ x,
      inds,
      goalDist.collect {
        case (l: PiDefn[u, v], w) if l.variable.typ == x.typ =>
          (l.value.replace(l.variable, x), w)
        case (ft: FuncTyp[u, v], w) if ft.dom == x.typ =>
          (ft.codom, w)
      },
      Context.AppendVariable(context, x)
    )
}

object ParMapState {
  def fromTermState(ts: TermState) =
    ParMapState(
      ts.terms.toParMap,
      ts.typs.toParMap,
      ts.vars,
      ts.inds.toParMap,
      ts.goals.toParMap,
      ts.context
    )

  def makeMap[T](vec: ParVector[(T, Double)]): ParMap[T, Double] =
    vec.groupBy(_._1).mapValues(_.map(_._2).sum).to(ParMap)

  case class ParAddVar(typ: Typ[Term])
      extends (ParMapState => (Double => (ParMapState, Term))) {
    def apply(state: ParMapState): Double => (ParMapState, HoTT.Term) =
      (wt: Double) => state.addVar(typ, wt)

    override def toString(): String = s"ParAddVar($typ)"
  }

  def add[Y](m1: ParMap[Y, Double], m2: ParMap[Y, Double]): ParMap[Y, Double] =
    m1.keySet
      .union(m2.keySet)
      .map(k => k -> (m1.getOrElse(k, 0.0) + m2.getOrElse(k, 0.0)))
      .to(ParMap)

  def normalize[Y](m: ParMap[Y, Double]): ParMap[Y, Double] = {
    val total = m.values.sum
    if (total > 0) m.mapValues(_ / total) else ParMap.empty[Y, Double]
  }

  def mapMap[Y, Z](m: ParMap[Y, Double], f: Y => Z): ParMap[Z, Double] =
    makeMap(m.to(ParVector).map { case (y, p) => f(y) -> p })

  def mapMapOpt[Y, Z](
      m: ParMap[Y, Double],
      f: Y => Option[Z]
  ): ParMap[Z, Double] =
    normalize(
      makeMap(m.to(ParVector).flatMap { case (y, p) => f(y).map(_ -> p) })
    )

  def purge[Y](m: ParMap[Y, Double], cutoff: Double) =
    normalize(m.filter(_._2 > cutoff))

  case object ParEnterIsle extends ((Term, ParMapState) => ParMapState) {
    def apply(x: HoTT.Term, state: ParMapState): ParMapState = state.inIsle(x)
  }

  case class ParGenNodes(tg: TermGenParams)
      extends TermGeneratorNodes[ParMapState](
        { case (fn, arg) => applyFunc(fn.func, arg) },
        { case (fn, arg) => Unify.appln(fn.func, arg) },
        ParAddVar(_),
        GetVar,
        ParEnterIsle,
        tg.solver
      )

  def parGenNodes(tg: TermGenParams) =
    if (tg.solver == TypSolver.coreSolver) ParBaseNodes else ParGenNodes(tg)

  case object ParBaseNodes
      extends TermGeneratorNodes[ParMapState](
        { case (fn, arg) => applyFunc(fn.func, arg) },
        { case (fn, arg) => Unify.appln(fn.func, arg) },
        ParAddVar(_),
        GetVar,
        ParEnterIsle,
        TypSolver.coreSolver
      )
}

trait ExstParMap {
  type Elem

  val value: ParMap[Elem, Double]

  def cast[X]: ParMap[X, Double] = value.map {
    case (x, p) => (x.asInstanceOf[X], p)
  }
}

object ExstParMap {
  def apply[X](m: ParMap[X, Double]) = new ExstParMap {
    type Elem = X

    val value: ParMap[X, Double] = m
  }
}

import cats.Eval, scala.collection.mutable

class ParDistEqMemo {
  val varDists: mutable.Map[
    (ParMapState, RandomVar[_]),
    (Double, ExstParMap, Set[EquationNode])
  ] = mutable.Map()
  val nodeDists: mutable.Map[
    (ParMapState, GeneratorNode[_]),
    (Double, ExstParMap, Set[EquationNode])
  ] = mutable.Map()
}
