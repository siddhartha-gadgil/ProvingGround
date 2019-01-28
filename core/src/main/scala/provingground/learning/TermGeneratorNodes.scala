package provingground.learning
import provingground.HoTT._
import provingground.{FiniteDistribution => FD, _}
import shapeless._
import induction._
import provingground.learning.GeneratorNode.{Map, MapOpt}

import scala.language.higherKinds
import GeneratorNode._
import TermRandomVars._
import com.sun.org.apache.xalan.internal.utils.XMLSecurityManager.Limit
import monix.eval.Task
import provingground.interface.{ContextJson, MultiTask}

import scala.concurrent._
import duration._
import monix.execution.Scheduler.Implicits.global

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
    getVar: Typ[Term] => Term,
    inIsle: (Term, InitState) => InitState
) {

  case object Appln extends ((ExstFunc, Term) => Term){
    def apply(fn: ExstFunc, arg: Term) = appln(fn, arg)

    override def toString = "Appln"
  }

  case object FlipAppln extends ((Term, ExstFunc) => Term){
    def apply(arg: Term, fn: ExstFunc) = appln(fn, arg)

    override def toString = "FlipAppn"
  }

  case object UnifApplnOpt extends ((ExstFunc, Term) => Option[Term]){
    def apply(fn: ExstFunc, arg: Term) = unifApplnOpt(fn, arg)

    override def toString = "UnifApplnOpt"
  }
  

  /**
    * function application with unification to get terms
    */
  val unifApplnNode: ZipMapOpt[ExstFunc, Term, Term] =
    ZipMapOpt[ExstFunc, Term, Term](
      UnifApplnOpt,
      Funcs,
      Terms,
      Terms
    )

  val typUnifApplnBase: ZipMapOpt[ExstFunc, Term, Term] =
    ZipMapOpt[ExstFunc, Term, Term](
      UnifApplnOpt,
      TypFamilies,
      Terms,
      Terms
    )

  val typUnifApplnNode: GeneratorNode[Typ[Term]] =
    typUnifApplnBase | (typSort, Typs)

  val typFamilyUnifApplnNode: GeneratorNode[ExstFunc] =
    typUnifApplnBase | (typFamilySort, TypFamilies)

  /**
    * function application to get terms by choosing a function and then a term in its domain
    */
  val applnNode: FiberProductMap[ExstFunc, Term, Typ[Term], Term] =
    FiberProductMap[ExstFunc, Term, Typ[Term], Term](
      DomFn,
      TermsWithTypFn,
      Appln,
      Funcs,
      Terms
    )

  val typApplnBase: FiberProductMap[ExstFunc, Term, Typ[Term], Term] =
    FiberProductMap[ExstFunc, Term, Typ[Term], Term](
      DomFn,
      TermsWithTypFn,
      Appln,
      TypFamilies,
      Terms
    )

  val typApplnNode: GeneratorNode[Typ[Term]] = typApplnBase | (typSort, Typs)

  val typFamilyApplnNode: GeneratorNode[ExstFunc] =
    typApplnBase | (typFamilySort, TypFamilies)

  /**
    * function application to get terms by choosing an argument and then a function with domain containing this.
    */
  val applnByArgNode: FiberProductMap[Term, ExstFunc, Typ[Term], Term] =
    FiberProductMap[Term, ExstFunc, Typ[Term], Term](
      TypFn,
      FuncsWithDomainFn,
      FlipAppln,
      Terms,
      Terms
    )

  /**
    * Constant random variable, for fibers for islands
    * @param randomVar the random variable
    * @tparam O scala type of the random variable
    */
  case class CRV[O](randomVar: RandomVar[O]) extends (Term => RandomVar[O]) {
    def apply(t: Term): RandomVar[O] = randomVar

    override def toString: String = randomVar.toString
  }

  case object LamApply extends ((Term, Term) => Term) {
    def apply(x: Term, y: Term): FuncLike[Term, Term] = x :~> y

    override def toString = "Lambda"
  }

  case object PiApply extends ((Term, Typ[Term]) => Typ[Term]) {
    def apply(x: Term, y: Typ[Term]): Typ[FuncLike[Term, Term]] = pi(x)(y)

    override def toString = "Pi"
  }

  case object SigmaApply extends ((Term, Typ[Term]) => Typ[Term]) {
    def apply(x: Term, y: Typ[Term]): Typ[AbsPair[Term, Term]] = sigma(x)(y)

    override def toString = "Sigma"
  }

  case object LamFunc extends ((Term, Term) => ExstFunc) {
    def apply(x: Term, y: Term): ExstFunc = ExstFunc(x :~> y)

    override def toString = "Lambda"
  }

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
      CRV(Terms),
      addVar(typ),
      LamApply,
      inIsle
    )

  case object LambdaIsle extends (Typ[Term] => Island[Term, InitState, Term, Term]){
    def apply(typ: Typ[Term]) = lambdaIsle(typ)

    override def toString = "LambdaIsle"
  }

  def lambdaTypFamilyIsle(typ: Typ[Term]): GeneratorNode[ExstFunc] =
    Island[ExstFunc, InitState, Term, Term](
      TypFamilies,
      CRV(TypsAndFamilies),
      addVar(typ),
      LamFunc,
      inIsle
    )

    case object LambdaTypFamilyIsle extends (Typ[Term] => GeneratorNode[ExstFunc]){
      def apply(typ: Typ[Term]) = lambdaTypFamilyIsle(typ)
  
      override def toString = "LambdaTypFamilyIsle"
    }

  /**
    * A node  for targeting a (dependent function) type, with variable of the domain generated and the co-domain
    * type (more generally fibre) targeted within the island.
    *
    * @param typ the target type
    * @return optional distribution.
    */
  def nodeForTyp(typ: Typ[Term]): Option[GeneratorNode[Term]] =
    typ match {
      case pd: PiDefn[u, v] =>
        Some(
          Island[Term, InitState, Term, Term](
            termsWithTyp(pd),
            PiOutput(pd),
            addVar(pd.domain),
            LamApply,
            inIsle
          )
        )
      case ft: FuncTyp[u, v] =>
        Some(
          Island[Term, InitState, Term, Term](
            termsWithTyp(ft),
            CRV(termsWithTyp(ft.codom)),
            addVar(ft.domain),
            LamApply,
            inIsle
          )
        )
      case pt: ProdTyp[u, v] =>
        Some(
          ZipMap[Term, Term, Term](
            { case (a, b) => PairTerm(a.asInstanceOf[u], b.asInstanceOf[v]) },
            termsWithTyp(pt.first),
            termsWithTyp(pt.second),
            termsWithTyp(pt)
          )
        )
      case pt: SigmaTyp[u, v] =>
        Some(
          ZipFlatMap[Term, Term, Term](
            termsWithTyp(pt.fibers.dom),
            (x) => termsWithTyp(pt.fibers(x.asInstanceOf[u])), {
              case (a, b) => pt.paircons(a.asInstanceOf[u])(b.asInstanceOf[v])
            },
            termsWithTyp(pt)
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
      CRV(Terms),
      addVar(dom),
      LamFunc,
      inIsle
    )

  /**
    * node combining lambda islands aggregated by type
    */
  val lambdaNode: FlatMap[Typ[Term], Term] =
    FlatMap(
      Typs,
      LambdaIsle,
      Terms
    )

  val lambdaTypFamilyNode: FlatMap[Typ[Term], ExstFunc] =
    FlatMap(
      Typs,
      LambdaTypFamilyIsle,
      TypFamilies
    )

  /**
    * nodes combining lambdas targeting types that are  (dependent) function types,
    * aggregated over all types
    */
  val lambdaByTypNodeFamily
    : GeneratorNodeFamily.BasePiOpt[::[Typ[Term], HNil], Term] =
    GeneratorNodeFamily.BasePiOpt[Typ[Term] :: HNil, Term]({
      case typ :: HNil => nodeForTyp(typ)
    }, TermsWithTyp)

  val lambdaForFuncWithDomFamily
    : GeneratorNodeFamily.BasePi[::[Typ[Term], HNil], ExstFunc] =
    GeneratorNodeFamily.BasePi[Typ[Term] :: HNil, ExstFunc](
      { case dom :: HNil => lambdaIsleForFuncWithDomain(dom) },
      FuncsWithDomain
    )

  /**
    * island to generate Pi-Types by taking variables with specified domain, similar to [[lambdaIsle]]
    * @param typ the domain for
    * @return distribution of types
    */
  def piIsle(typ: Typ[Term]): Island[Typ[Term], InitState, Typ[Term], Term] =
    Island[Typ[Term], InitState, Typ[Term], Term](
      Typs,
      CRV(Typs),
      addVar(typ),
      PiApply,
      inIsle
    )

  def sigmaIsle(typ: Typ[Term]): Island[Typ[Term], InitState, Typ[Term], Term] =
    Island[Typ[Term], InitState, Typ[Term], Term](
      Typs,
      CRV(Typs),
      addVar(typ),
      SigmaApply,
      inIsle
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

  val sigmaNode: FlatMap[Typ[Term], Typ[Term]] =
    FlatMap(
      Typs,
      sigmaIsle,
      Typs
    )

  def foldTypFamily(w: Term): GeneratorNode[Typ[Term]] = w match {
    case typ: Typ[Term] => Atom(typ, Typs)
    case fn: FuncLike[u, v] =>
      FlatMap(termsWithTyp(fn.dom),
              (x: Term) => foldTypFamily(fn(x.asInstanceOf[u])),
              Typs)
  }

  val typFoldNode: FlatMap[ExstFunc, Typ[Term]] =
    FlatMap(
      TypFamilies,
      (fn) => foldTypFamily(fn.term),
      Typs
    )

  def foldFunc(t: Term,
               depth: Int,
               output: RandomVar[Term]): GeneratorNode[Term] =
    if (depth < 1) Atom(t, output)
    else
      t match {
        case fn: FuncLike[u, v] =>
          FlatMap(
            termsWithTyp(fn.dom),
            (x: Term) => foldFunc(fn(x.asInstanceOf[u]), depth - 1, output),
            output)
      }

  /**
    * recursive functions from a specific inductive structure, picking the codomain
    * @param ind the inductive structure
    * @return distribution of functions
    */
  def recFuncsForStruc(
      ind: ExstInducStrucs): ZipMapOpt[Typ[Term], Typ[Term], ExstFunc] =
    ZipMapOpt[Typ[Term], Typ[Term], ExstFunc]({
      case (x, y) => ind.recOpt(x, y).flatMap(FuncOpt)
    }, Typs, Typs, Funcs)

  /**
    * induction function for a specific structure, picking the type family
    * @param ind the inductive structure
    * @return distribution of functions
    */
  def inducFuncsForStruc(
      ind: ExstInducStrucs): ZipMapOpt[Typ[Term], Term, ExstFunc] =
    ZipMapOpt[Typ[Term], Term, ExstFunc]({
      case (x, y) => ind.inducOpt(x, y).flatMap(FuncOpt)
    }, Typs, Terms, Funcs)

  /**
    * aggregate recursion functions from inductive types
    */
  val recFuncs: FlatMap[ExstInducStrucs, ExstFunc] =
    FlatMap(
      InducStrucs,
      recFuncsForStruc,
      Funcs
    )

  /**
    * aggregated induction functions from inductive types
    */
  val inducFuncs: FlatMap[ExstInducStrucs, ExstFunc] =
    FlatMap(
      InducStrucs,
      inducFuncsForStruc,
      Funcs
    )

    case object Proj2 extends ((Typ[Term], Term) => Term){
      def apply(a: Typ[Term], b: Term) = b

      override def toString = "Proj2"
    }

  /**
    * terms generated by first choosing type and then term with the type;
    * a form of backward reasoning
    */
  val termsByTyps: ZipFlatMap[Typ[Term], Term, Term] =
    ZipFlatMap[Typ[Term], Term, Term](
      TargetTyps,
      TermsWithTypFn,
      Proj2,
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
      CRV(IntroRuleTypes(inductiveTyp)), // output from island
      addVar(inductiveTyp),
      PiApply,
      inIsle
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
      CRV(IntroRuleTypes(inductiveTyp)),
      addVar(typ),
      PiApply,
      inIsle
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
      inductiveTyp: Typ[Term]): Map[Vector[Typ[Term]], ExstInducStrucs] =
    Map[Vector[Typ[Term]], ExstInducStrucs](
      introTyps => {
        val intros = introTyps.map(getVar)
        ExstInducStrucs.get(inductiveTyp, intros)
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
                      ExstInducStrucs.get(inductiveTyp, intros),
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
      CRV(IntroRuleTypes(targetTyp)),
      addVar(typ),
      PiApply,
      inIsle
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
      TypOpt,
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
      CRV(IndexedIntroRuleTyps(typF)),
      addVar(typ),
      PiApply,
      inIsle
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
      CRV(IndexedIntroRuleTyps(targetTyp)),
      addVar(typ),
      PiApply,
      inIsle
    )

  /**
    * Aggregating extending indexed introduction rule by iterated function type eding in
    * fully applied inductive type family.
    *
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

  case object TypFn extends (Term => Typ[Term]){
    def apply(tp: Term) = tp.typ

    override def toString = "typeOf(_)"
  }

  case object DomFn extends (ExstFunc => Typ[Term]){
    def apply(fn: ExstFunc) = fn.dom

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

  val typSort: Sort[Term, Typ[Term]] = Sort.Restrict[Term, Typ[Term]](TypOpt)

  case object Goals extends RandomVar[Typ[Term]]

  /**
    * distribution of functions : as existentials, not as terms
    */
  case object Funcs extends RandomVar[ExstFunc]

  val funcSort: Sort[Term, ExstFunc] =
    Sort.Restrict[Term, ExstFunc](FuncOpt)

  /**
    * family of distributions of terms with specified type
    */
  case object TermsWithTyp
      extends RandomVar.SimpleFamily[Typ[Term], Term](
        Typs,
        (typ: Typ[Term]) => Sort.Filter[Term](WithTyp(typ))
      )

  case class PiOutput[U <: Term with Subs[U], V <: Term with Subs[V]](
      pd: PiDefn[U, V])
      extends (Term => RandomVar[Term]) {
    def apply(x: Term): RandomVar[Term] =
      termsWithTyp(pd.fibers(x.asInstanceOf[U]))
  }

  def withTypSort(typ: Typ[Term]): Sort[Term, Term] =
    Sort.Filter[Term](WithTyp(typ))

  def withTypNode(
      node: GeneratorNode[Term]): GeneratorNodeFamily[Typ[Term] :: HNil, Term] =
    node.pi(withTypSort, TermsWithTyp)

  /**
    * distribution of terms with a specific type
    * @param typ the type
    * @return distribution at type
    */
  def termsWithTyp(typ: Typ[Term]): RandomVar[Term] =
    RandomVar.AtCoord(TermsWithTyp, typ :: HNil)

  case object TermsWithTypFn extends (Typ[Term] => RandomVar[Term]){
    def apply(typ: Typ[Term]) = RandomVar.AtCoord(TermsWithTyp, typ :: HNil)

    override def toString = "TermsWithTyp"
  }

  /**
    * distribution of type families
    */
  case object TypFamilies extends RandomVar[ExstFunc]

  val typFamilySort: Sort[Term, ExstFunc] =
    Sort.Restrict[Term, ExstFunc](TypFamilyOpt)

  case object TypsAndFamilies extends RandomVar[Term] {
    lazy val fromTyp: Map[Typ[Term], Term] =
      Map[Typ[Term], Term]((x) => x, Typs, TypsAndFamilies)

    lazy val fromFamilies: Map[ExstFunc, Term] =
      Map[ExstFunc, Term](_.func, TypFamilies, TypsAndFamilies)
  }

  case object TargetTyps extends RandomVar[Typ[Term]] {
    def fromGoal: Map[Typ[Term], Typ[Term]] = Map(identity, Goals, TargetTyps)

    def fromTyp: Map[Typ[Term], Typ[Term]] = Map(identity, Typs, TargetTyps)
  }

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

  def funcWithDomNode(node: GeneratorNode[ExstFunc])
    : GeneratorNodeFamily[Typ[Term] :: HNil, ExstFunc] =
    node.pi(funcWithDomSort, FuncsWithDomain)

  def funcWithDomTermNode(node: GeneratorNode[Term])
    : GeneratorNodeFamily[::[Typ[Term], HNil], ExstFunc] =
    node.pi((dom: Typ[Term]) => Sort.Restrict[Term, ExstFunc](FuncWithDom(dom)),
            FuncsWithDomain)

  /**
    * distribution of functions with a specified domain
    * @param typ the domain
    * @return distribution at domain
    */
  def funcsWithDomain(typ: Typ[Term]): RandomVar[ExstFunc] =
    RandomVar.AtCoord(FuncsWithDomain, typ :: HNil)

  case object FuncsWithDomainFn extends (Typ[Term] => RandomVar[ExstFunc]){
    def apply(typ: Typ[Term]) = RandomVar.AtCoord(FuncsWithDomain, typ :: HNil)

    override def toString = "FuncsWithDomain"
  }

  /**
    * distribution of existential inductive structures
    */
  case object InducStrucs extends RandomVar[ExstInducStrucs]

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
                     vars: Vector[Term] = Vector(),
                     inds: FD[ExstInducDefn] = FD.empty[ExstInducDefn],
                     goals: FD[Typ[Term]] = FD.empty,
                     context: Context = Context.Empty) {
  def subs(x: Term, y: Term) =
    TermState(terms.map(_.replace(x, y)),
              typs.map(_.replace(x, y)),
              vars.map(_.replace(x, y)),
              inds.map(_.subs(x, y)),
              goals.map(_.replace(x, y)),
              context.subs(x, y))

  // pprint.log(context.variables)

  lazy val thmsByPf: FD[Typ[Term]] =
    terms.map(_.typ).flatten.filter((t) => typs(t) > 0).safeNormalized
  lazy val thmsBySt: FD[Typ[Term]] =
    typs.filter(thmsByPf(_) > 0).flatten.safeNormalized

  lazy val thmWeights: Vector[(Typ[Term], Double, Double, Double)] =
    (for {
      Weighted(x, p) <- thmsBySt.pmf
      q = thmsByPf(x)
      h = -p / (q * math.log(q))
    } yield (x, p, q, h)).toVector.sortBy(_._4).reverse

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
      case tp                                   => tp
    }

    lazy val newTyps =
      typOpt(x)
        .map(tp => (FD.unif(tp) * varWeight) ++ (typs * (1 - varWeight)))
        .getOrElse(typs)
    TermState(newTerms,
              newTyps,
              x +: vars,
              inds,
              newGoals.flatten,
              context.addVariable(x)) -> x
  }

  def tangent(x: Term): TermState =
    this.copy(
      terms = FD.unif(x),
      typs = FD.empty
    )

  import interface._, TermJson._

  import ujson.Js

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
    val vars = obj("variables").arr.toVector.map((t) =>
      jsToTermExst(context.inducStruct)(t).get)
    val inds = FD.empty[ExstInducDefn] //InducJson.jsToFD(context.inducStruct)(obj("inductive-structures"))
    TermState(terms, typs, vars, inds, goals, context)
  }

  /**
    * finite distributions on terms etc
    */
  implicit val stateFD: StateDistribution[TermState, FD] =
    new StateDistribution[TermState, FD] {
      def isEmpty(state: TermState) =
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
          case RandomVar.AtCoord(fmly, arg) => valueAt(state)(fmly, arg)
        }

      def valueAt[Dom <: HList, T](state: TermState)(
          randomVarFmly: RandomVarFamily[Dom, T],
          fullArg: Dom): FD[T] =
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

    def islePairs(ts: TermState, variable: Term) = 
      {
        val termPairs : Set[(RandomVar.Elem[_], RandomVar.Elem[_])] = 
          ts.terms.support.map{x => (RandomVar.Elem(Terms, x), RandomVar.Elem(Terms, variable :~> x))}
        val typPairs : Set[(RandomVar.Elem[_], RandomVar.Elem[_])] = 
          ts.typs.support.map{x => (RandomVar.Elem(Typs, x), RandomVar.Elem(Typs, variable ~>: x))}
        val goalPairs : Set[(RandomVar.Elem[_], RandomVar.Elem[_])] = 
          ts.goals.support.map{x => (RandomVar.Elem(Goals, x), RandomVar.Elem(Goals, variable ~>: x))}
        termPairs union typPairs union goalPairs
      }

      

}

import upickle.default.{ReadWriter => RW, macroRW, read, write}

object TermGenParams {
  implicit def rw: RW[TermGenParams] = macroRW

  case class AddVar(typ: Typ[Term], wt: Double)
      extends (TermState => (TermState, Term)) {
    def apply(ts: TermState) = ts.addVar(typ, wt)

    override def toString = "AddVar"
  }

  case object GetVar extends (Typ[Term] => Term) {
    def apply(typ: Typ[Term]) = typ.Var

    override def toString = "GetVar"
  }

  case object InIsle extends ((Term, TermState) => TermState) {
    def apply(t: Term, state: TermState) = state.inIsle(t)

    override def toString = "InIsle"
  }
}

import TermGenParams._

case class TermGenParams(appW: Double = 0.1,
                         unAppW: Double = 0.1,
                         argAppW: Double = 0.1,
                         lmW: Double = 0.1,
                         piW: Double = 0.1,
                         termsByTypW: Double = 0.05,
                         typFromFamilyW: Double = 0.05,
                         sigmaW: Double = 0.05,
                         varWeight: Double = 0.3,
                         goalWeight: Double = 0.7,
                         typVsFamily: Double = 0.5) { tg =>
  object Gen
      extends TermGeneratorNodes[TermState](
        { case (fn, arg) => applyFunc(fn.func, arg) },
        { case (fn, arg) => Unify.appln(fn.func, arg) },
        AddVar(_, varWeight),
        GetVar,
        InIsle
      )

  import Gen._, GeneratorNode._,
  TermRandomVars.{withTypNode => wtN, funcWithDomTermNode => fdtN}

  val termInit: Double = 1.0 - appW - unAppW - argAppW - lmW - termsByTypW

  val typInit: Double = 1.0 - appW - unAppW - piW - sigmaW - typFromFamilyW

  val termNodes: NodeCoeffs.Cons[TermState, Term, Double, HNil, Term] =
    (Init(Terms)      -> termInit) ::
      (applnNode      -> appW) ::
      (unifApplnNode  -> unAppW) ::
      (applnByArgNode -> argAppW) ::
      (lambdaNode     -> lmW) ::
      (termsByTyps    -> termsByTypW) ::
      Terms.target[TermState, Term, Double, Term]

  val typNodes: NodeCoeffs.Cons[TermState, Term, Double, HNil, Typ[Term]] =
    (Init(Typs)         -> typInit) ::
      (typApplnNode     -> appW) ::
      (typUnifApplnNode -> unAppW) ::
      (piNode           -> piW) ::
      (sigmaNode        -> sigmaW) ::
      (typFoldNode      -> typFromFamilyW) ::
      Typs.target[TermState, Term, Double, Typ[Term]]

  val goalNodes = (Init(Goals) -> 1.0) :: Goals
    .target[TermState, Term, Double, Typ[Term]]

  val funcNodes: NodeCoeffs.Cons[TermState, Term, Double, HNil, ExstFunc] =
    (Init(Funcs)                            -> termInit) ::
      ((applnNode | (funcSort, Funcs))      -> appW) ::
      ((unifApplnNode | (funcSort, Funcs))  -> unAppW) ::
      ((applnByArgNode | (funcSort, Funcs)) -> argAppW) ::
      ((lambdaNode | (funcSort, Funcs))     -> lmW) ::
      ((termsByTyps | (funcSort, Funcs))    -> termsByTypW) ::
      Funcs.target[TermState, Term, Double, ExstFunc]

  val typFamilyNodes: NodeCoeffs.Cons[TermState, Term, Double, HNil, ExstFunc] =
    (Init(TypFamilies)                           -> termInit) ::
      (typFamilyApplnNode                        -> appW) ::
      (typFamilyUnifApplnNode                    -> unAppW) ::
      ((applnByArgNode | (typFamilySort, Funcs)) -> argAppW) ::
      (lambdaTypFamilyNode                       -> lmW) ::
      ((termsByTyps | (typFamilySort, Funcs))    -> termsByTypW) ::
      TypFamilies.target[TermState, Term, Double, ExstFunc]

  val termsByTypNodes
    : NodeCoeffs.Cons[TermState, Term, Double, Typ[Term] :: HNil, Term] =
    (TermsWithTyp.init       -> (termInit * (1 - goalWeight))) ::
      (wtN(applnNode)        -> appW) ::
      (wtN(unifApplnNode)    -> unAppW) ::
      (wtN(applnByArgNode)   -> argAppW) ::
      (lambdaByTypNodeFamily -> (termInit * goalWeight + lmW)) ::
      TermsWithTyp.target[TermState, Term, Double, Term]

  val typOrFmlyNodes: NodeCoeffs.Cons[TermState, Term, Double, HNil, Term] =
    (TypsAndFamilies.fromTyp        -> typVsFamily) ::
      (TypsAndFamilies.fromFamilies -> (1.0 - typVsFamily)) ::
      TypsAndFamilies.target[TermState, Term, Double, Term]

  val targTypNodes: NodeCoeffs.Cons[TermState, Term, Double, HNil, Term] =
    (TargetTyps.fromGoal  -> goalWeight) ::
      (TargetTyps.fromTyp -> (1.0 - goalWeight)) ::
      TargetTyps.target[TermState, Term, Double, Term]

  val funcWithDomNodes
    : NodeCoeffs.Cons[TermState, Term, Double, Typ[Term] :: HNil, ExstFunc] =
    (FuncsWithDomain.init         -> termInit) ::
      (fdtN(applnNode)            -> appW) ::
      (fdtN(unifApplnNode)        -> unAppW) ::
      (fdtN(applnByArgNode)       -> argAppW) ::
      (lambdaForFuncWithDomFamily -> lmW) ::
      FuncsWithDomain.target[TermState, Term, Double, ExstFunc]

  val nodeCoeffSeq: NodeCoeffSeq[TermState, Term, Double] =
    funcWithDomNodes +: targTypNodes +: goalNodes +:
      termNodes +: typNodes +: funcNodes +: typFamilyNodes +: typOrFmlyNodes +: funcWithDomNodes +: termsByTypNodes +:
      NodeCoeffSeq.Empty[TermState, Term, Double]()

  lazy val monixFD: MonixFiniteDistribution[TermState, Term] =
    MonixFiniteDistribution(nodeCoeffSeq)

  def monixTangFD(baseState: TermState) =
    MonixTangentFiniteDistribution(nodeCoeffSeq, baseState)

  def equationsGen(initState: TermState,
                   finalState: TermState): GeneratorEquations[TermState, Term] =
    GeneratorEquations(nodeCoeffSeq, initState, finalState)

  def spireGrad(initState: TermState,
                finalState: TermState,
                hW: Double = 1,
                klW: Double = 1,
                eqW: Double = 1): SpireGradient =
    TermGenCost(equationsGen(initState, finalState), hW, klW, eqW).spireGradient

  def nextStateTask(initState: TermState,
                    epsilon: Double,
                    limit: FiniteDuration = 3.minutes): Task[TermState] =
    for {
      terms <- monixFD.varDist(initState)(Terms, epsilon, limit)
      typs  <- monixFD.varDist(initState)(Typs, epsilon, limit)
    } yield TermState(terms, typs, initState.vars, initState.inds)

  def evolvedStateTask(initState: TermState,
                       epsilon: Double,
                       limit: FiniteDuration = 3.minutes): Task[EvolvedState] =
    nextStateTask(initState, epsilon, limit).map(
      result => EvolvedState(initState, result, tg, epsilon)
    )

  // def getEvolvedState(initState: TermState,
  //                     epsilon: Double,
  //                     limit: FiniteDuration = 3.minutes): EvolvedState =
  //   evolvedStateTask(initState, epsilon, limit).runSyncUnsafe(limit)

  def nextStateWithEqnsTask(initState: TermState,
                            epsilon: Double,
                            limit: FiniteDuration = 3.minutes)
    : Task[(TermState, Set[GeneratorVariables.Equation])] =
    nextStateTask(initState, epsilon, limit).map { finalState =>
      (finalState, equationsGen(initState, finalState).equations)
    }

  def nextTangStateTask(baseState: TermState,
                        tangState: TermState,
                        epsilon: Double,
                        vars: Vector[Term] = Vector(),
                        limit: FiniteDuration = 3.minutes): Task[TermState] =
    for {
      terms <- monixTangFD(baseState).varDist(tangState)(Terms, epsilon, limit)
      typs  <- monixTangFD(baseState).varDist(tangState)(Typs, epsilon, limit)
    } yield TermState(terms, typs, baseState.vars, baseState.inds)

  def findProof(initState: TermState,
                typ: Typ[Term],
                epsilon: Double,
                limit: FiniteDuration = 3.minutes): Task[FD[Term]] =
    monixFD
      .varDist(initState)(TermsWithTyp.at(typ :: HNil), epsilon, limit)
      .map(_.flatten)
}

case class EvolvedState(init: TermState,
                        result: TermState,
                        params: TermGenParams,
                        epsilon: Double)

import ujson.Js

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
