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
import provingground.learning.GeneratorVariables.Elem
import provingground.learning.GeneratorVariables.Event
import provingground.learning.GeneratorVariables.InIsle
import provingground.learning.GeneratorVariables.PairEvent
import provingground.learning.Sort.All
import provingground.learning.Sort.Filter
import provingground.learning.Sort.Restrict

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

  def contextTermNode(ctx: Context, varWeight: Double): GeneratorNode[Term] =
    Island[Term, TermState, Term, Unit](
      ContextTerms(ctx),
      (_) => Terms,
      ts => (ts.contextInit(ctx, varWeight), ()),
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

  def funcWithDomSort(dom: Typ[Term]): Sort.Filter[ExstFunc] =
    Sort.Filter[ExstFunc](_.dom == dom)

  def funcWithDomNode(
      node: GeneratorNode[ExstFunc]
  ): GeneratorNodeFamily[Typ[Term] :: HNil, ExstFunc] =
    node.pi(funcWithDomSort, FuncsWithDomain)

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
  def valueSubs[U](x: Term, y: Term)(value: U) : U = value match {
    case t : Term => t.replace(x, y).asInstanceOf[U]
    case fn: ExstFunc => ExstFunc(fn.func.replace(x, y)).asInstanceOf[U]
    case l : HList => l match {
      case head :: tail => (valueSubs(x, y)(head) :: valueSubs(x, y)(tail)).asInstanceOf[U]
      case HNil => HNil.asInstanceOf[U]
    }
    case u => u
  }

  def randomVarSubs[U](x: Term, y: Term)(rv: RandomVar[U]) : RandomVar[U] = rv match {
    case RandomVar.AtCoord(family, fullArg) => RandomVar.AtCoord(family, valueSubs(x, y)(fullArg))
    case rv => rv
  }

  def sortSubs[U, V](x: Term, y: Term)(sort: Sort[U, V]) : Sort[U, V] = sort match {
    case All() => sort
    case filter : Filter[U] => Filter[U]((z: U) => filter.pred(valueSubs(y, x)(z))).asInstanceOf[Sort[U, V]]
    case Restrict(optMap) => Restrict((z: U) => optMap(valueSubs(y, x)(z)).map(w => valueSubs(x, y)(w)))
  }

  def variableSubs(x: Term, y: Term)(v : Variable[_], vars: Vector[Term], varWeight: Double) : Variable[_] = (v : Variable[Any]) match {
    case Elem(element, randomVar) => Elem(valueSubs(x, y)(element), randomVarSubs(x, y)(randomVar))
    case ev: Event[a, b] => 
      val newBase = randomVarSubs(x, y)(ev.base)
      val newSort: Sort[a,b] = sortSubs(x, y)(ev.sort)
      Event(newBase, newSort)
    case inIsleFull: InIsle[c, _, d, _] => 
      val inIsleTry = scala.util.Try(inIsleFull.asInstanceOf[InIsle[c, TermState, d, Term]])
      inIsleTry.fold(fa => inIsleFull,
        inIsle => {
          import inIsle._
          val newBoat = nextVar(boat.typ, vars)
          val newIsleVar = variableSubs(x, y)(isleVar, vars :+ newBoat, varWeight)
          val newInit = isle.output match {
            case RandomVar.AtCoord(family, fullArg) => fullArg match {
              case (typ: Typ[Term]) :: HNil => AddVar(typ.replace(x, y), varWeight)
              case _ => isle.initMap.asInstanceOf[(TermState => (TermState, Term) )]
            }
            case _ => isle.initMap
          }
          val newIsle = Island[c, TermState, d, Term](
            randomVarSubs(x, y)(isle.output), 
            (z: Term) => valueSubs(x, y)(isle.islandOutput(z)), 
            newInit, 
            isle.export, 
            isle.finalMap)
          InIsle(newIsleVar, newBoat, isle)
        })

    case PairEvent(base1, base2, sort) => 
      PairEvent(
        randomVarSubs(x, y)(base1),
        randomVarSubs(x, y)(base2),
        sortSubs(x, y)(sort)
      )
  }

}

