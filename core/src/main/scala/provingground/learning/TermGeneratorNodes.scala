package provingground.learning
import provingground.HoTT._
import provingground.{FiniteDistribution => FD, _}
import shapeless._
import induction._
import provingground.learning.GeneratorNode.{Map, MapOpt}

import scala.language.higherKinds

import GeneratorNode._

import TermRandomVars._

/**
  * Combining terms and subclasses to get terms, types, functions etc; these are abstract specifications,
  * to be used for generating distributions, obtaining equations etc.
  *
  * @param appln function application, assuming domain and type match
  * @param unifApplnOpt unified application of functions
  * @param addVar new state with a variable, of specified type, added
  * @param getVar a variable of a specified type
  * @tparam InitState the initial state for the dynamics, equations etc
  */
class TermGeneratorNodes[InitState](
    appln: (ExstFunc, Term) => Term,
    unifApplnOpt: (ExstFunc, Term) => Option[Term],
    addVar: Typ[Term] => InitState => (InitState, Term),
    getVar: Typ[Term] => Term
) {

  /**
    * function application with unification to get terms
    */
  val unifApplnNode: ZipMapOpt[ExstFunc, Term, Term] =
    ZipMapOpt[ExstFunc, Term, Term](
      unifApplnOpt,
      Funcs,
      Terms,
      Terms
    )

  /**
    * function application to get terms by choosing a function and then a term in its domain
    */
  val applnNode: FiberProductMap[ExstFunc, Term, Typ[Term], Term] =
    FiberProductMap[ExstFunc, Term, Typ[Term], Term](
      _.dom,
      termsWithTyp,
      appln,
      Funcs,
      Terms
    )

  /**
    * function application to get terms by choosing an argument and then a function with domain containing this.
    */
  val applnByArgNode: FiberProductMap[Term, ExstFunc, Typ[Term], Term] =
    FiberProductMap[Term, ExstFunc, Typ[Term], Term](
      _.typ,
      funcsWithDomain,
      { case (x, f) => appln(f, x) },
      Terms,
      Terms
    )

  /**
    * An island to generate lambda terms, i.e., terms are generated withing the island and exported as lambdas;
    * the initial state of the island has a new variable mixed in.
    *
    * @param typ the domain of the lambda
    * @return
    */
  def lambdaIsle(typ: Typ[Term]): Island[Term, InitState, Term, Term] =
    Island[Term, InitState, Term, Term](
      Terms,
      _ => Terms,
      addVar(typ),
      { case (x, y) => x :~> y }
    )

  /**
    * An island for targetting a (dependent) function type, with variable of the domain generated and the co-domain
    * type (more generally fibre) targeted within the island.
    *
    * @param typ the target type
    * @return optional distribution.
    */
  def lambdaIsleForTyp(
      typ: Typ[Term]): Option[Island[Term, InitState, Term, Term]] =
    typ match {
      case pd: PiDefn[u, v] =>
        Some(
          Island[Term, InitState, Term, Term](
            termsWithTyp(pd),
            x => termsWithTyp(pd.value.replace(pd.variable, x)),
            addVar(typ),
            { case (x, y) => x :~> y }
          )
        )
      case ft: FuncTyp[u, v] =>
        Some(
          Island[Term, InitState, Term, Term](
            termsWithTyp(ft),
            _ => termsWithTyp(ft.codom),
            addVar(typ),
            { case (x, y) => x :~> y }
          )
        )
      case _ => None
    }

  /**
    * lambda island for generating function with specified domain
    * @param dom the desired domain
    * @return distribution of functions
    */
  def lambdaIsleForFuncWithDomain(
      dom: Typ[Term]): Island[ExstFunc, InitState, Term, Term] =
    Island[ExstFunc, InitState, Term, Term](
      funcsWithDomain(dom),
      _ => Terms,
      addVar(dom),
      { case (x, y) => ExstFunc(x :~> y) }
    )

  /**
    * node combining lambda islands aggregated by type
    */
  val lambdaNode: FlatMap[Typ[Term], Term] =
    FlatMap(
      Typs,
      lambdaIsle,
      Terms
    )

  /**
    * nodes combining lambdas targeting types that are  (dependent) function types,
    * aggregated over all types
    */
  val lambdaByTypNodeFamily: GeneratorNodeFamily[Typ[Term] :: HNil, Term] =
    GeneratorNodeFamily.RecPiOpt[InitState, Term, Typ[Term] :: HNil, Term]({
      case typ :: HNil => lambdaIsleForTyp(typ)
    }, TermsWithTyp)

  /**
    * island to generate Pi-Types by taking variables with specified domain, similar to [[lambdaIsle]]
    * @param typ the domain for
    * @return distribution of types
    */
  def piIsle(typ: Typ[Term]): Island[Typ[Term], InitState, Typ[Term], Term] =
    Island[Typ[Term], InitState, Typ[Term], Term](
      Typs,
      _ => Typs,
      addVar(typ),
      { case (x, y) => pi(x)(y) }
    )

  /**
    * aggregate generation of Pi-types from islands
    */
  val piNode: FlatMap[Typ[Term], Typ[Term]] =
    FlatMap(
      Typs,
      piIsle,
      Typs
    )

  /**
    * recursive functions from a specific inductive structure, picking the codomain
    * @param ind the inductive structure
    * @return distribution of functions
    */
  def recFuncsForStruc(
      ind: ExstInducStruc): ZipMapOpt[Typ[Term], Typ[Term], ExstFunc] =
    ZipMapOpt[Typ[Term], Typ[Term], ExstFunc]({
      case (x, y) => ind.recOpt(x, y).flatMap(ExstFunc.opt)
    }, Typs, Typs, Funcs)

  /**
    * induction function for a specific structure, picking the type family
    * @param ind the inductive structure
    * @return distribution of functions
    */
  def inducFuncsForStruc(
      ind: ExstInducStruc): ZipMapOpt[Typ[Term], Term, ExstFunc] =
    ZipMapOpt[Typ[Term], Term, ExstFunc]({
      case (x, y) => ind.inducOpt(x, y).flatMap(ExstFunc.opt)
    }, Typs, Terms, Funcs)

  /**
    * aggregate recursion functions from inductive types
    */
  val recFuncs: FlatMap[ExstInducStruc, ExstFunc] =
    FlatMap(
      InducStrucs,
      recFuncsForStruc,
      Funcs
    )

  /**
    * aggregated induction functions from inductive types
    */
  val inducFuncs: FlatMap[ExstInducStruc, ExstFunc] =
    FlatMap(
      InducStrucs,
      inducFuncsForStruc,
      Funcs
    )

  /**
    * terms generated by first choosing type and then term with the type;
    * a form of backward reasoning
    */
  val termsByTyps: ZipFlatMap[Typ[Term], Term, Term] =
    ZipFlatMap[Typ[Term], Term, Term](
      Typs,
      (typ) => termsWithTyp(typ),
      { case (_, term) => term },
      Terms
    )

  // Generating simple inductive types, i.e. not indexed and with simple extendsions

  /**
    * extend an introduction rule type for an inductive type `W` by `W -> ...`
    * @param inductiveTyp the inductive type being defined
    * @return distribution of types for introduction rules
    */
  def selfHeadNode(
      inductiveTyp: Typ[Term]): Island[Typ[Term], InitState, Typ[Term], Term] =
    Island[Typ[Term], InitState, Typ[Term], Term](
      IntroRuleTypes(inductiveTyp), // output
      (_: Term) => IntroRuleTypes(inductiveTyp), // output from island
      addVar(inductiveTyp),
      { case (x, y) => pi(x)(y) }
    )

  /**
    * extensions of the introduction rule types for an inductive type `W` by an unrelated type `C`;
    * we may have a Pi-Type with domain `C` rather than a FuncTyp
    * there is no assumption on how the head type `typ` is generated, so this is reusable if only that is changed.
    * @param inductiveTyp the inductive type `W`
    * @param typ the type `C` by which we are extending the introduction rule type
    * @return distribution of introduction rule types
    */
  def otherHeadIsle(inductiveTyp: Typ[Term])(
      typ: Typ[Term]): Island[Typ[Term], InitState, Typ[Term], Term] =
    Island[Typ[Term], InitState, Typ[Term], Term](
      IntroRuleTypes(inductiveTyp),
      (_: Term) => IntroRuleTypes(inductiveTyp),
      addVar(typ),
      { case (x, y) => pi(x)(y) }
    )

  /**
    * aggregating extensions of inductive types by unrelated types
    * @param inductiveTyp the inductive type being generated
    * @return distribution of introduction rule types.
    */
  def otherHeadNode(inductiveTyp: Typ[Term]): FlatMap[Typ[Term], Typ[Term]] =
    FlatMap(
      Typs,
      otherHeadIsle(inductiveTyp),
      IntroRuleTypes(inductiveTyp)
    )

  /**
    * builds an inductive structure from a bunch of introduction rules for a type `inductiveTyp`,
    * we must separately specify how to build an introduction rules, as well as a vector of these.
    * @param inductiveTyp the inductve type being defined
    * @return distribution on existential inductive structures
    */
  def simpleInductiveStructure(
      inductiveTyp: Typ[Term]): Map[Vector[Typ[Term]], ExstInducStruc] =
    Map[Vector[Typ[Term]], ExstInducStruc](
      introTyps => {
        val intros = introTyps.map(getVar)
        ExstInducStruc.get(inductiveTyp, intros)
      },
      RandomVector(IntroRuleTypes(inductiveTyp)),
      InducStrucs
    )

  /**
    * builds an inductive definition, i.e. structure with type, from a bunch of introduction rules for a type `inductiveTyp`,
    * we must separately specify how to build an introduction rules, as well as a vector of these.
    * @param inductiveTyp the inductve type being defined
    * @return distribution on existential inductive structures
    */
  def simpleInductiveDefn(
      inductiveTyp: Typ[Term]): Map[Vector[Typ[Term]], ExstInducDefn] =
    Map[Vector[Typ[Term]], ExstInducDefn](
      introTyps => {
        val intros = introTyps.map(getVar)
        ExstInducDefn(inductiveTyp,
                      intros,
                      ExstInducStruc.get(inductiveTyp, intros),
                      Vector())
      },
      RandomVector(IntroRuleTypes(inductiveTyp)),
      InducDefns
    )

  // Generating iterated function types ending in `W` and indexed types

  /**
    * single function application to generate partial applications of `f`, if `f` is a function
    * @param f the function to partially apply
    * @return distribution of terms
    */
  def partiallyApply(f: Term): Option[Map[Term, Term]] =
    ExstFunc.opt(f).map { fn =>
      Map(
        (x: Term) => appln(fn, x),
        termsWithTyp(fn.dom),
        PartiallyApplied(f)
      )
    }

  /**
    * node to generate partial applications of `f` from partial applications that are functions.
    * @param f function to be applied
    * @return distribution of terms
    */
  def iteratedApply(f: Term): GeneratorNode[Term] =
    FlatMapOpt(
      PartiallyApplied(f),
      g => partiallyApply(g),
      PartiallyApplied(f)
    )

  /**
    * extending types of iterated functions targeting `W` by the type `typ`
    * @param targetTyp the target type
    * @param typ the type by which it is extended
    * @return distribution of types
    */
  def iterFuncIsle(targetTyp: Typ[Term])(
      typ: Typ[Term]): Island[Typ[Term], InitState, Typ[Term], Term] =
    Island[Typ[Term], InitState, Typ[Term], Term](
      IterFuncTypTo(targetTyp),
      (_: Term) => IntroRuleTypes(targetTyp),
      addVar(typ),
      { case (x, y) => pi(x)(y) }
    )

  /**
    * aggregating extensions of iterated function types
    * @param targetTyp target type
    * @return distribution of types
    */
  def iterFuncNode(targetTyp: Typ[Term]): FlatMap[Typ[Term], Typ[Term]] =
    FlatMap(
      Typs,
      iterFuncIsle(targetTyp),
      IterFuncTypTo(targetTyp)
    )

  /**
    * extension of the introduction rule for an inductive type `W` by a type of the form eg `A -> W`
    * @param inductiveTyp the inductive type `W`
    * @return distribution of types
    */
  def iterHeadNode(inductiveTyp: Typ[Term]): FlatMap[Typ[Term], Typ[Term]] =
    FlatMap(
      IterFuncTypTo(inductiveTyp),
      otherHeadIsle(inductiveTyp),
      IntroRuleTypes(inductiveTyp)
    )

  /**
    * types from a type family by filtering partial applications to consider only types.
    * @param typF the type family
    * @return distribution of types.
    */
  def typFromFamily(typF: Term): MapOpt[Term, Typ[Term]] =
    MapOpt[Term, Typ[Term]](
      typOpt,
      PartiallyApplied(typF),
      TypsFromFamily(typF)
    )

  /**
    * Start of an indexed introduction rule by fully applying a type family
    * @param typF the type family for the inductive type
    * @return distribution of types
    */
  def indexedInducIdNode(typF: Typ[Term]): Map[Typ[Term], Typ[Term]] =
    Map[Typ[Term], Typ[Term]](
      identity,
      TypsFromFamily(typF),
      IndexedIntroRuleTyps(typF)
    )

  /**
    * Extending an indexed introduction rule by a specific unrelated type
    * @param typF the indexed inductive type family
    * @param typ the type by which to extends
    * @return distribution on types
    */
  def indexedOtherHeadIsle(typF: Term)(
      typ: Typ[Term]): Island[Typ[Term], InitState, Typ[Term], Term] =
    Island[Typ[Term], InitState, Typ[Term], Term](
      IndexedIntroRuleTyps(typF),
      (_: Term) => IndexedIntroRuleTyps(typF),
      addVar(typ),
      { case (x, y) => pi(x)(y) }
    )

  /**
    * Aggregating extending indexed introduction rule by fully applied inductive type family.
    * @param typF the type family
    * @return distribution on types.
    */
  def indexedSelfHeadNode(typF: Term) =
    FlatMap(
      TypsFromFamily(typF),
      indexedOtherHeadIsle(typF),
      IndexedIntroRuleTyps(typF)
    )

  /**
    * aggregating extending introduction types for  indexed inductive types by unrelated types
    * @param typF the indexed type family
    * @return distribution of terms
    */
  def indexedOtherHeadNode(typF: Term) =
    FlatMap(
      Typs,
      indexedOtherHeadIsle(typF),
      IndexedIntroRuleTyps(typF)
    )

  /**
    * Extending indexed introduction rule by an iterated function ending in the inductive type family fully applied.
    * @param typ the type by which we are extending
    * @return distribution on types.
    */
  def indexedIterFuncIsle(targetTyp: Term)(
      typ: Typ[Term]): Island[Typ[Term], InitState, Typ[Term], Term] =
    Island[Typ[Term], InitState, Typ[Term], Term](
      IndexedIterFuncTypTo(targetTyp),
      (_: Term) => IndexedIntroRuleTyps(targetTyp),
      addVar(typ),
      { case (x, y) => pi(x)(y) }
    )

  /**
    * Aggregating extending indexed introduction rule by iterated function type eding in
    * fully applied inductive type family.
    * @param typF the type family
    * @return distribution on types.
    */
  def indexedIterFuncNode(targetTyp: Term): FlatMap[Typ[Term], Typ[Term]] =
    FlatMap(
      Typs,
      indexedIterFuncIsle(targetTyp),
      IndexedIterFuncTypTo(targetTyp)
    )

  /**
    * Aggregating extending introduction types for indexed inductive types by iterated function types
    * @param inductiveTyp the indexed inductive types
    * @return
    */
  def indexedIterHeadNode(inductiveTyp: Term) =
    FlatMap(
      IndexedIterFuncTypTo(inductiveTyp),
      indexedOtherHeadIsle(inductiveTyp),
      IndexedIntroRuleTyps(inductiveTyp)
    )

}

object TermRandomVars {

  /**
    * distribution of terms
    */
  case object Terms extends RandomVar[Term]

  /**
    * distribution of types
    */
  case object Typs extends RandomVar[Typ[Term]]

  /**
    * distribution of functions : as existentials, not as terms
    */
  case object Funcs extends RandomVar[ExstFunc]

  /**
    * family of distributions of terms with specified type
    */
  case object TermsWithTyp
      extends RandomVar.SimpleFamily[Typ[Term], Term](
        Typs,
        (typ: Typ[Term]) => Sort.Filter[Term](_.typ == typ)
      )

  /**
    * distribution of terms with a specific type
    * @param typ the type
    * @return distribution at type
    */
  def termsWithTyp(typ: Typ[Term]): RandomVar[Term] =
    RandomVar.AtCoord(TermsWithTyp, typ :: HNil)

  /**
    * distribution of type families
    */
  case object TypFamilies extends RandomVar[Term]

  /**
    * family of distribution of (existential) functions with specifed domain.
    */
  case object FuncsWithDomain
      extends RandomVar.SimpleFamily[Typ[Term], ExstFunc](
        Typs,
        (typ: Typ[Term]) => Sort.Filter[ExstFunc](_.dom == typ)
      )

  /**
    * distribution of functions with a specified domain
    * @param typ the domain
    * @return distribution at domain
    */
  def funcsWithDomain(typ: Typ[Term]): RandomVar[ExstFunc] =
    RandomVar.AtCoord(FuncsWithDomain, typ :: HNil)

  /**
    * distribution of existential inductive structures
    */
  case object InducStrucs extends RandomVar[ExstInducStruc]

  /**
    * distribution of existential inductive definitions
    */
  case object InducDefns extends RandomVar[ExstInducDefn]

  /**
    * atomic distribution to be included in generation
    * @param value the atom
    * @param rv random variable whose distribution is specified
    * @tparam X scala type of the random variable
    * @return node for inclusion
    */
  def just[X](value: X, rv: RandomVar[X]) =
    GeneratorNode.Atom(value, rv)

  /**
    * distribution of introduction rules for an inductive type
    * @param inductiveTyp the inductive type being defined
    */
  case class IntroRuleTypes(inductiveTyp: Typ[Term])
      extends RandomVar[Typ[Term]]

  /**
    * the introduction with just the target type
    * @param inductiveTyp the inductive type being defined
    * @return node for inclusion
    */
  def inducHeadNode(inductiveTyp: Typ[Term]): Atom[Typ[Term]] =
    just(inductiveTyp, IntroRuleTypes(inductiveTyp))

  /**
    * distribution of `f`, `f(x)`, `f(x)(y)` etc
    * @param func the function to apply
    */
  case class PartiallyApplied(func: Term) extends RandomVar[Term]

  /**
    * atomic distribution `f` for partial application
    * @param f the function
    * @return node to include
    */
  def partiallyApplySelf(f: Term): Atom[Term] =
    just(f, PartiallyApplied(f))

  /**
    * iterated function types targeting a given type, typically used in introduction rules
    * @param typ the final type
    */
  case class IterFuncTypTo(typ: Typ[Term]) extends RandomVar[Typ[Term]]

  /**
    * type family types, e.g. `A -> Type`, for indexed induction
    */
  val typFamilyTypes: RandomVar[Term] = IterFuncTypTo(Type)

  /**
    * distribution of types obtained by full application in a type family
    * @param typF the type family
    */
  case class TypsFromFamily(typF: Term) extends RandomVar[Typ[Term]]

  /**
    * distribution of introduction rules for an indexed inductive type
    * @param typF the type family
    */
  case class IndexedIntroRuleTyps(typF: Term) extends RandomVar[Typ[Term]]

  /**
    * iterated function type targetting a fully applied type family
    * @param typF the type family
    */
  case class IndexedIterFuncTypTo(typF: Term) extends RandomVar[Typ[Term]]

}

/**
  * distributions of vectors from a base distribution
  * @param base the base distribution
  * @tparam X scala type of the base
  */
case class RandomVector[X](base: RandomVar[X]) extends RandomVar[Vector[X]] {
  def empty: Atom[Vector[X]] = just[Vector[X]](Vector(), this)

  def cons: ZipMap[X, Vector[X], Vector[X]] =
    ZipMap[X, Vector[X], Vector[X]]({ case (x, ys) => x +: ys },
                                    base,
                                    this,
                                    this)
}

/**
  * A state, typically the initial state, for generating terms, types etc
  * @param terms distribution of terms
  * @param typs distribution of types
  * @param vars variables, over which we may take closures
  * @param inds inductive type definitions
  */
case class TermState(terms: FD[Term],
                     typs: FD[Typ[Term]],
                     vars: Vector[Term],
                     inds: FD[ExstInducDefn]) {
  val thmsByPf: FD[Typ[Term]] = terms.map(_.typ)
  val thmsBySt: FD[Typ[Term]] = typs.filter(thmsByPf(_) > 0)
  val pfSet: Vector[Term]     = terms.flatten.supp.filter(t => thmsBySt(t.typ) > 0)
  val fullPfSet: Vector[(Term, Term)] =
    pfSet.flatMap(pf => partialLambdaClosures(vars)(pf).map((pf, _)))

  def addVar(typ: Typ[Term], varWeight: Double): (TermState, Term) = {
    val x        = typ.Var
    val newTerms = (FD.unif(x) * varWeight) ++ (terms * (1 - varWeight))
    val newTyps =
      typOpt(x)
        .map(tp => (FD.unif(tp) * varWeight) ++ (typs * (1 - varWeight)))
        .getOrElse(typs)
    TermState(newTerms, newTyps, x +: vars, inds) -> x
  }
}

object TermState {
  import TermRandomVars._

  /**
    * finite distributions on terms etc
    */
  implicit val stateFD: StateDistribution[TermState, FD] =
    new StateDistribution[TermState, FD] {
      def value[T](state: TermState)(randomVar: RandomVar[T]): FD[T] =
        randomVar match {
          case Terms => state.terms.map(x => x: T)
          case Typs  => state.typs.map(x => x: T)
          case Funcs => state.terms.condMap(ExstFunc.opt).map(x => x: T)
          case TypFamilies =>
            state.terms.conditioned(isTypFamily).map(x => x: T)
          case InducDefns  => state.inds.map(x => x: T)
          case InducStrucs => state.inds.map(_.ind).map(x => x: T)
        }

      def valueAt[Dom <: HList, T](state: TermState)(
          randomVarFmly: RandomVarFamily[Dom, T],
          fullArg: Dom): FD[T] =
        (randomVarFmly, fullArg) match {
          case (TermsWithTyp, typ :: HNil) =>
            state.terms.conditioned(_.typ == typ).map(x => x: T)
          case (FuncsWithDomain, typ :: HNil) =>
            state.terms
              .condMap(ExstFunc.opt)
              .conditioned(_.dom == typ)
              .map(x => x: T)
        }
    }

}

case class TermGenParams(appW: Double = 0.1,
                         unAppW: Double = 0.1,
                         argAppW: Double = 0.1,
                         lmW: Double = 0.1,
                         piW: Double = 0.1,
                         varWeight: Double = 0.3,
                         vars: Vector[Term]) {
  object Gen
      extends TermGeneratorNodes[TermState](
        { case (fn, arg) => fn(arg).get },
        { case (fn, arg) => Unify.appln(fn.func, arg) },
        (typ) => (state) => state.addVar(typ, varWeight),
        _.Var
      )

  import Gen._, GeneratorNode._

  val termInit: Double = 1.0 - appW - unAppW

  val termNodes: NodeCoeffs[TermState, Term, Double, HNil, Term] =
    (Init(Terms) -> termInit) ::
      (applnNode -> appW) ::
      (unifApplnNode -> unAppW) ::
      Terms.target[TermState, Term, Double, Term]

  val nodeCoeffSeq: NodeCoeffSeq[TermState, Term, Double] =
    termNodes +:
      NodeCoeffSeq.Empty[TermState, Term, Double]
}
