package provingground.learning
import provingground.HoTT._
import provingground.{FiniteDistribution => FD, _}
import shapeless._
import induction._
import provingground.learning.GeneratorNode.{Map, MapOpt}

import scala.language.higherKinds
import GeneratorNode._
import TermRandomVars._
import scala.util._

import TermGeneratorNodes._
import provingground.learning.GeneratorVariables.Elem
import provingground.learning.GeneratorVariables.Event
import provingground.learning.GeneratorVariables.InIsle
import provingground.learning.GeneratorVariables.PairEvent
import provingground.learning.Sort.All
import provingground.learning.Sort.Filter
import provingground.learning.Sort.Restrict
import provingground.learning.Expression.FinalVal
import provingground.learning.Expression.InitialVal
import provingground.learning.Expression.Quotient
import provingground.learning.Expression.Exp
import provingground.learning.Expression.Literal
import provingground.learning.Expression.Log
import provingground.learning.Expression.IsleScale
import provingground.learning.Expression.Coeff
import provingground.learning.Expression.Sum

object TermRandomVars {

  case object TypOpt extends (Term => Option[Typ[Term]]) {
    def apply(t: Term): Option[Typ[Term]] = typOpt(t)

    override def toString = "TypOpt"
  }

  case object FuncOpt extends (Term => Option[ExstFunc]) {
    def apply(t: Term): Option[ExstFunc] = ExstFunc.opt(t)

    override def toString = "FuncOpt"
  }

  case object TypFamilyOpt extends (Term => Option[ExstFunc]) {
    def apply(t: Term): Option[ExstFunc] =
      ExstFunc.opt(t).filter((fn) => isTypFamily(fn.func))

    override def toString = "TypFamilyOpt"
  }

  case class FuncWithDom(dom: Typ[Term]) extends (Term => Option[ExstFunc]) {
    def apply(t: Term): Option[ExstFunc] =
      ExstFunc.opt(t).filter((fn) => fn.dom == dom)

    override def toString = s"FuncWithDom($dom)"
  }

  case class WithTyp(typ: Typ[Term]) extends (Term => Boolean) {
    def apply(t: Term): Boolean = t.typ == typ

    override def toString = s"$WithTyp(typ)"
  }

  case object TypFn extends (Term => Typ[Term]) {
    def apply(tp: Term): Typ[Term] = tp.typ

    override def toString = "typeOf(_)"
  }

  case object DomFn extends (ExstFunc => Typ[Term]) {
    def apply(fn: ExstFunc): Typ[Term] = fn.dom

    override def toString = "domOf"
  }

  /**
    * distribution of terms
    */
  case object Terms extends RandomVar[Term]

  /**
    * distribution of types
    */
  case object Typs extends RandomVar[Typ[Term]]

  case object IsleDomains extends RandomVar[Typ[Term]]

  val typSort: Sort[Term, Typ[Term]] = Sort.Restrict[Term, Typ[Term]](TypOpt)

  case object Goals extends RandomVar[Typ[Term]]

  case class ContextTerms(ctx: Context) extends RandomVar[Term]

  case class ContextTyps(ctx: Context) extends RandomVar[Typ[Term]]

  def contextTermNode(ctx: Context): GeneratorNode[Term] =
    Island[Term, TermState, Term, Unit](
      ContextTerms(ctx),
      (_) => Terms,
      ts => (varWeight: Double) => (ts.contextInit(ctx, varWeight), ()),
      { case (_, term) => ctx.export(term) },
      { case (_, ts)   => ts.contextImport(ctx) }
    )

  case class PiOutput[U <: Term with Subs[U], V <: Term with Subs[V]](
      pd: PiDefn[U, V]
  ) extends (Term => RandomVar[Term]) {
    def apply(x: Term): RandomVar[Term] =
      termsWithTyp(pd.fibers(x.asInstanceOf[U]))
  }

  def withTypSort(typ: Typ[Term]): Sort[Term, Term] =
    Sort.Filter[Term](WithTyp(typ))

  def withTypNode(
      node: GeneratorNode[Term]
  ): GeneratorNodeFamily[Typ[Term] :: HNil, Term] =
    node.pi(withTypSort, TermsWithTyp)

  /**
    * family of distribution of (existential) functions with specifed domain.
    */
  case object FuncsWithDomain
      extends RandomVar.SimpleFamily[Typ[Term], ExstFunc](
        Typs,
        (typ: Typ[Term]) => Sort.Filter[ExstFunc](_.dom == typ)
      )

  // def funcWithDomSort(dom: Typ[Term]): Sort.Filter[ExstFunc] =
  //   Sort.Filter[ExstFunc](_.dom == dom)

  // def funcWithDomNode(
  //     node: GeneratorNode[ExstFunc]
  // ): GeneratorNodeFamily[Typ[Term] :: HNil, ExstFunc] =
  //   node.pi(funcWithDomSort, FuncsWithDomain)

  def funcWithDomTermNode(
      node: GeneratorNode[Term]
  ): GeneratorNodeFamily[::[Typ[Term], HNil], ExstFunc] =
    node.pi(
      (dom: Typ[Term]) => Sort.Restrict[Term, ExstFunc](FuncWithDom(dom)),
      FuncsWithDomain
    )

  /**
    * distribution of functions with a specified domain
    *
    * @param typ the domain
    * @return distribution at domain
    */
  def funcsWithDomain(typ: Typ[Term]): RandomVar[ExstFunc] =
    RandomVar.AtCoord(FuncsWithDomain, typ :: HNil)

  case object FuncsWithDomainFn extends (Typ[Term] => RandomVar[ExstFunc]) {
    def apply(typ: Typ[Term]) = RandomVar.AtCoord(FuncsWithDomain, typ :: HNil)

    override def toString = "FuncsWithDomain"
  }

  case object DomForInduc
      extends RandomVar.SimpleFamily[ExstInducDefn, Term](
        InducDefns
      )

  def domForInduc(defn: ExstInducDefn) =
    RandomVar.AtCoord(DomForInduc, defn :: HNil)

  case object FuncForCod
      extends RandomVar.SimpleFamily[Typ[Term], Term](
        Typs
      )

  def funcForCod(cod: Typ[Term]) =
    RandomVar.AtCoord(FuncForCod, cod :: HNil)

  /**
    * distribution of existential inductive structures
    */
  case object InducStrucs extends RandomVar[ExstInducStrucs]

  /**
    * distribution of existential inductive definitions
    */
  case object InducDefns extends RandomVar[ExstInducDefn]

  case class AtomVar[U](atom: U) extends RandomVar[U]

  case class FuncFoldVar(func: Term, depth: Int) extends RandomVar[Term]

  /**
    * distribution of introduction rules for an inductive type
    *
    * @param inductiveTyp the inductive type being defined
    */
  case class IntroRuleTypes(inductiveTyp: Typ[Term])
      extends RandomVar[Typ[Term]]

  /**
    * the introduction with just the target type
    *
    * @param inductiveTyp the inductive type being defined
    * @return node for inclusion
    */
  def inducHeadNode(inductiveTyp: Typ[Term]): Atom[Typ[Term]] =
    just(inductiveTyp, IntroRuleTypes(inductiveTyp))

  /**
    * distribution of `f`, `f(x)`, `f(x)(y)` etc
    *
    * @param func the function to apply
    */
  case class PartiallyApplied(func: Term) extends RandomVar[Term]

  /**
    * atomic distribution `f` for partial application
    *
    * @param f the function
    * @return node to include
    */
  def partiallyApplySelf(f: Term): Atom[Term] =
    just(f, PartiallyApplied(f))

  /**
    * iterated function types targeting a given type, typically used in introduction rules
    *
    * @param typ the final type
    */
  case class IterFuncTypTo(typ: Typ[Term]) extends RandomVar[Typ[Term]]

  /**
    * type family types, e.g. `A -> Type`, for indexed induction
    */
  val typFamilyTypes: RandomVar[Term] = IterFuncTypTo(Type)

  /**
    * distribution of types obtained by full application in a type family
    *
    * @param typF the type family
    */
  case class TypsFromFamily(typF: Term) extends RandomVar[Typ[Term]]

  /**
    * distribution of introduction rules for an indexed inductive type
    *
    * @param typF the type family
    */
  case class IndexedIntroRuleTyps(typF: Term) extends RandomVar[Typ[Term]]

  /**
    * iterated function type targetting a fully applied type family
    *
    * @param typF the type family
    */
  case class IndexedIterFuncTypTo(typF: Term) extends RandomVar[Typ[Term]]

  import shapeless._, GeneratorVariables._
  // Substituting terms in variables
  def valueSubs[U](x: Term, y: Term)(value: U): U = value match {
    case t: Term      => t.replace(x, y).asInstanceOf[U]
    case fn: ExstFunc => ExstFunc(fn.func.replace(x, y)).asInstanceOf[U]
    case l: HList =>
      l match {
        case head :: tail =>
          (valueSubs(x, y)(head) :: valueSubs(x, y)(tail)).asInstanceOf[U]
        case HNil => HNil.asInstanceOf[U]
      }
    case u => u
  }

  def randomVarSubs[U](x: Term, y: Term)(rv: RandomVar[U]): RandomVar[U] =
    rv match {
      case RandomVar.AtCoord(family, fullArg) =>
        RandomVar.AtCoord(family, valueSubs(x, y)(fullArg))
      case rv => rv
    }

  def sortSubs[U, V](x: Term, y: Term)(sort: Sort[U, V]): Sort[U, V] =
    sort match {
      case All() => sort
      case filter: Filter[U] =>
        val newPred =
          filter.pred match {
            case WithTyp(typ) =>
              WithTyp(typ.replace(x, y)).asInstanceOf[U => Boolean]
            case _ => (z: U) => filter.pred(valueSubs(y, x)(z))
          }
        Filter[U](newPred)
          .asInstanceOf[Sort[U, V]]
      case Restrict(optMap) =>
        val newOptMap = optMap match {
          case TypOpt  => TypOpt.asInstanceOf[U => Option[V]]
          case FuncOpt => FuncOpt.asInstanceOf[U => Option[V]]
          case FuncWithDom(typ) =>
            FuncWithDom(typ.replace(x, y)).asInstanceOf[U => Option[V]]
          case _ =>
            (z: U) => optMap(valueSubs(y, x)(z)).map(w => valueSubs(x, y)(w))
        }
        Restrict(
          newOptMap
        )
    }

  def isleSub[c, d](x: Term, y: Term)(
      isle: Island[c, TermState, d, Term],
      boat: Term
  ): Island[c, TermState, d, Term] = {
    val newInit =
      AddVar(boat.typ.replace(x, y))
    val newIsleOutput = isle.islandOutput match {
      case ConstRandVar(randomVar) =>
        ConstRandVar(randomVarSubs(x, y)(randomVar))
      case _ =>
        (z: Term) => randomVarSubs(x, y)(isle.islandOutput(z))
    }
    Island[c, TermState, d, Term](
      randomVarSubs(x, y)(isle.output),
      newIsleOutput,
      newInit,
      isle.export,
      isle.finalMap
    )

  }

  def variableSubs(
      x: Term,
      y: Term
  )(v: Variable[_]): Variable[_] = (v: Variable[Any]) match {
    case Elem(element, randomVar) =>
      Elem(valueSubs(x, y)(element), randomVarSubs(x, y)(randomVar))
    case ev: Event[a, b] =>
      val newBase             = randomVarSubs(x, y)(ev.base)
      val newSort: Sort[a, b] = sortSubs(x, y)(ev.sort)
      Event(newBase, newSort)
    case inIsleFull: InIsle[c, _, d, _] =>
      val inIsleTry =
        Try(inIsleFull.asInstanceOf[InIsle[c, TermState, d, Term]])
      inIsleTry.fold(
        fa => inIsleFull,
        inIsle => {
          import inIsle._
          val newBoat    = boat.replace(x, y)
          val newIsleVar = variableSubs(x, y)(isleVar)
          val newIsle    = isleSub(x, y)(isle, boat)
          InIsle(newIsleVar, newBoat, newIsle)
        }
      )

    case PairEvent(base1, base2, sort) =>
      PairEvent(
        randomVarSubs(x, y)(base1),
        randomVarSubs(x, y)(base2),
        sortSubs(x, y)(sort)
      )
  }

  def isleNormalizeVars(
      v: GeneratorVariables.Variable[_],
      vars: Vector[Term]
  ): GeneratorVariables.Variable[_] =
    v match {
      case Elem(element, randomVar) => v
      case Event(base, sort)        => v
      case inIsleFull: InIsle[c, _, d, _] =>
        val inIsleTry =
          Try(inIsleFull.asInstanceOf[InIsle[c, TermState, d, Term]])
        inIsleTry.fold(
          fa => inIsleFull,
          inIsle => {
            import inIsle._
            val newBoat = nextVar(boat.typ, vars)
            val newIsleVar = variableSubs(boat, newBoat)(
              isleNormalizeVars(isleVar, vars :+ newBoat)
            )
            val newIsle = isleSub(boat, newBoat)(isle, boat)
            InIsle(newIsleVar, newBoat, newIsle)
          }
        )
      case PairEvent(base1, base2, sort) => v
    }

  import Expression._

  def expressionMapVars(
      fn: Variable[_] => Variable[_]
  )(exp: Expression): Expression = exp match {
    case FinalVal(variable)   => FinalVal(fn(variable))
    case InitialVal(variable) => InitialVal(fn(variable))
    case Sum(x, y) =>
      Sum(
        expressionMapVars(fn)(x),
        expressionMapVars(fn)(y)
      )
    case IsleScale(boat: Term, elem) =>
      val newBoat = "%boat" :: boat.typ
      IsleScale(
        newBoat,
        Elem(
          valueSubs(boat, newBoat)(elem.element),
          randomVarSubs(boat, newBoat)(elem.randomVar)
        )
      )
    case IsleScale(boat, elem) => IsleScale(boat, elem)
    case Log(exp)              => Log(expressionMapVars(fn)(exp))
    case Literal(value)        => Literal(value)
    case Exp(exp)              => Exp(expressionMapVars(fn)(exp))
    case Coeff(node)           => Coeff(node)
    case Quotient(x, y) =>
      Quotient(
        expressionMapVars(fn)(x),
        expressionMapVars(fn)(y)
      )
    case provingground.learning.Expression.Product(x, y) =>
      Product(
        expressionMapVars(fn)(x),
        expressionMapVars(fn)(y)
      )
  }

  def expressionSubs(x: Term, y: Term)(exp: Expression): Expression =
    exp match {
      case FinalVal(variable)   => FinalVal(variableSubs(x, y)(variable))
      case InitialVal(variable) => InitialVal(variableSubs(x, y)(variable))
      case Product(a, b) =>
        Product(
          expressionSubs(x, y)(a),
          expressionSubs(x, y)(b)
        )
      case Quotient(a, b) =>
        Quotient(
          expressionSubs(x, y)(a),
          expressionSubs(x, y)(b)
        )
      case Exp(exp) =>
        Exp(expressionSubs(x, y)(exp))
      case Literal(value) => Literal(value)
      case Log(exp) =>
        Log(expressionSubs(x, y)(exp))
      case sc: IsleScale[u, v] =>
        IsleScale(
          valueSubs(x, y)(sc.boat),
          Elem(
            valueSubs(x, y)(sc.elem.element),
            randomVarSubs(x, y)(sc.elem.randomVar)
          )
        )
      case cf @ Coeff(node) => cf
      case Sum(a, b) =>
        Sum(
          expressionSubs(x, y)(a),
          expressionSubs(x, y)(b)
        )
    }

  def isleNormalizeVarExp(
      v: GeneratorVariables.Variable[_],
      rhs: Expression,
      vars: Vector[Term]
  ): (GeneratorVariables.Variable[_], Expression) =
    v match {
      case Elem(element, randomVar) =>
        val fn = v => TermRandomVars.isleNormalizeVars(v,vars)
        v -> expressionMapVars(fn)(rhs)
      case Event(base, sort) =>
        val fn = v => TermRandomVars.isleNormalizeVars(v, vars)
        v -> expressionMapVars(fn)(rhs)
      case inIsleFull: InIsle[c, _, d, _] =>
        val inIsleTry =
          Try(inIsleFull.asInstanceOf[InIsle[c, TermState, d, Term]])
        inIsleTry.fold(
          fa => inIsleFull -> rhs,
          inIsle => {
            import inIsle._
            val newBoat = nextVar(boat.typ, vars)
            val (recIsleVar, recRhs) =
              isleNormalizeVarExp(isleVar, rhs, vars :+ newBoat)
            val newIsleVar = variableSubs(boat, newBoat)(recIsleVar)
            val newRhs =
              expressionSubs(boat, newBoat)(recRhs)
            val newIsle = isleSub(boat, newBoat)(isle, boat)
            InIsle(newIsleVar, newBoat, newIsle) -> newRhs
          }
        )
      case PairEvent(base1, base2, sort) =>
      val fn = v => TermRandomVars.isleNormalizeVars(v, vars)
      v -> expressionMapVars(fn)(rhs)
    }

  def isleNormalize(eq: EquationNode, varWeight: Double = 0.3): EquationNode = {
    val fn = v => TermRandomVars.isleNormalizeVars(v, Vector())
    eq.lhs match {
      case InitialVal(variable) =>
        val (newVar, newRhs) = isleNormalizeVarExp(variable, eq.rhs, Vector())
        EquationNode(InitialVal(newVar), newRhs)
      case FinalVal(variable) =>
        val (newVar, newRhs) = isleNormalizeVarExp(variable, eq.rhs, Vector())
        EquationNode(FinalVal(newVar), newRhs)
      case _ =>
        EquationNode(
          expressionMapVars(fn)(eq.lhs),
          expressionMapVars(fn)(eq.rhs)
        )
    }

  }

  def varDepends(t: Term)(v: Variable[_]): Boolean =
    v match {
      case Elem(element: Term, randomVar) => element.dependsOn(t)
      case Elem(element, randomVar)       => false
      case Event(base, sort)              => false
      case InIsle(isleVar, boat, isle)    => varDepends(t)(isleVar)
      case PairEvent(base1, base2, sort)  => false
    }

  def equationDepends(t: Term)(eq: Equation): Boolean = {
    import Expression.varVals
    val genvars = varVals(eq.lhs).map(_.variable) union varVals(eq.lhs)
      .map(_.variable)
    genvars.exists(v => varDepends(t)(v))
  }

}
