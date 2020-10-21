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
import ExstInducStrucs._

/**
  * Combining terms and subclasses to get terms, types, functions etc; these are abstract specifications,
  * to be used for generating distributions, obtaining equations etc.
  * The object contains helpers and other static objects.
  */
object TermGeneratorNodes {

  /**
    * Constant random variable, for fibers for islands
    *
    * @param randomVar the random variable
    * @tparam O scala type of the random variable
    */
  case class ConstRandVar[O](randomVar: RandomVar[O])
      extends (Term => RandomVar[O]) {
    def apply(t: Term): RandomVar[O] = randomVar

    override def toString: String = randomVar.toString
  }

  /**
    * Wrapper for lambda to allow equality and  `toString` to work.
    */
  case object LamApply extends ((Term, Term) => Term) {
    def apply(x: Term, y: Term): FuncLike[Term, Term] = x :~> y

    override def toString = "Lambda"
  }

  /**
    * Wrapper for Pi to allow equality and  `toString` to work.
    */
  case object PiApply extends ((Term, Typ[Term]) => Typ[Term]) {
    def apply(x: Term, y: Typ[Term]): Typ[FuncLike[Term, Term]] = pi(x)(y)

    override def toString = "Pi"
  }

  /**
    * Wrapper for Sigma to allow equality and  `toString` to work.
    */
  case object SigmaApply extends ((Term, Typ[Term]) => Typ[Term]) {
    def apply(x: Term, y: Typ[Term]): Typ[AbsPair[Term, Term]] = sigma(x)(y)

    override def toString = "Sigma"
  }

  /**
    * Wrapper for lambda giving functions to allow equality and  `toString` to work.
    */
  case object LamFunc extends ((Term, Term) => ExstFunc) {
    def apply(x: Term, y: Term): ExstFunc = ExstFunc(x :~> y)

    override def toString = "Lambda"
  }

  /**
    * Wrapper for a specific projection to allow equality and  `toString` to work.
    */
  case object Proj2 extends ((Typ[Term], Term) => Term) {
    def apply(a: Typ[Term], b: Term): Term = b

    override def toString = "Proj2"
  }

  /**
    * Wrapper for adding variable to allow equality and  `toString` to work.
    */
  case class AddVar(typ: Typ[Term])
      extends (TermState => Double => (TermState, Term)) {
    def apply(ts: TermState) = (wt: Double) => ts.addVar(typ, wt)

    override def toString = "AddVar"
  }

  /**
    * Wrapper for getting variables to allow equality and  `toString` to work.
    */
  case object GetVar extends (Typ[Term] => Term) {
    def apply(typ: Typ[Term]): Term = typ.Var

    override def toString = "GetVar"
  }

  /**
    * Wrapper for entering an isle to allow equality and  `toString` to work.
    */
  case object EnterIsle extends ((Term, TermState) => TermState) {
    def apply(t: Term, state: TermState): TermState = state.inIsle(t)

    override def toString = "EnterIsle"
  }

  /**
    * The default term generators; essentially the only one used, except the choice of variable weight, `0.3` here, may vary.
    *
    */
  case object Base
      extends TermGeneratorNodes[TermState](
        { case (fn, arg) => applyFunc(fn.func, arg) },
        { case (fn, arg) => Unify.appln(fn.func, arg) },
        (typ: Typ[Term]) => AddVar(typ),
        GetVar,
        EnterIsle
      )

  /**
    * Wrapper for first inclusion to allow equality and  `toString` to work.
    */
  case class Incl1[U <: Term with Subs[U], V <: Term with Subs[V]](
      pt: PlusTyp[U, V]
  ) extends (Term => Term) {
    def apply(x: Term) = pt.i(x.asInstanceOf[U])
  }

  /**
    * Node for the first inclusion targeting a plus type.
    */
  def incl1Node(typ: Typ[Term]): Option[GeneratorNode[Term]] =
    typ match {
      case pt: PlusTyp[u, v] =>
        Some(Map(Incl1(pt), termsWithTyp(pt.first), termsWithTyp(typ)))
      case _ => None
    }

  /**
    * Wrapper for second to allow equality and  `toString` to work.
    */
  case class Incl2[U <: Term with Subs[U], V <: Term with Subs[V]](
      pt: PlusTyp[U, V]
  ) extends (Term => Term) {
    def apply(x: Term) = pt.j(x.asInstanceOf[V])
  }

  /**
    * Node for second inclusion targeting a plus type.
    */
  def incl2Node(typ: Typ[Term]): Option[GeneratorNode[Term]] =
    typ match {
      case pt: PlusTyp[u, v] =>
        Some(Map(Incl2(pt), termsWithTyp(pt.second), termsWithTyp(typ)))
      case _ => None
    }

  /**
    * Wrapper for function application to allow equality and  `toString` to work.
    */
  case class ApplyFunc[U <: Term with Subs[U], V <: Term with Subs[V]](
      fn: Func[U, V]
  ) extends (Term => Term) {
    def apply(t: Term) = fn(t.asInstanceOf[U])

    override def toString(): String = s"ApplyFunc($fn)"
  }

  /**
    * Wrapper for pair terms to allow equality and  `toString` to work.
    */
  case class PTTerm[U <: Term with Subs[U], V <: Term with Subs[V]](
      pt: ProdTyp[U, V]
  ) extends ((Term, Term) => Term) {
    def apply(a: Term, b: Term) = PairTerm(a.asInstanceOf[U], b.asInstanceOf[V])

    override def toString(): String = s"PTTerm($pt)"
  }

  /**
    * Wrapper for dependent pair term to allow equality and  `toString` to work.
    */
  case class STTerm[U <: Term with Subs[U], V <: Term with Subs[V]](
      pt: SigmaTyp[U, V]
  ) extends ((Term, Term) => Term) {
    def apply(a: Term, b: Term) = {
      pt.paircons(a.asInstanceOf[U])(b.asInstanceOf[V])
    }

    override def toString(): String = s"STTerm($pt)"
  }

  /**
    * conditioning on being a function
    */
  val funcSort: Sort[Term, ExstFunc] =
    Sort.Restrict[Term, ExstFunc](FuncOpt)

  val typAsTermSort: Sort[Typ[Term], Term] =
    Sort.Restrict[Typ[Term], Term](TypAsTermOpt)

  /**
    * Wrapper for fiber for sigma types to allow equality and  `toString` to work.
    */
  case class STFibVar[U <: Term with Subs[U], V <: Term with Subs[V]](
      pt: SigmaTyp[U, V]
  ) extends (Term => RandomVar[Term]) {
    def apply(x: Term) = termsWithTyp(pt.fibers(x.asInstanceOf[U]))

    override def toString(): String = s"STFibVar($pt)"
  }

  /**
    * Node for generating functions that target a codomain;
    * they are not applied yet, instead form terms of a customised random variable.
    */
  def codomainNode(typ: Typ[Term]): GeneratorNode[Term] =
    MapOpt[ExstFunc, Term](
      fn => Unify.targetCodomain(fn.func, typ),
      Funcs,
      funcForCod(typ)
    )

  case object CodomainNode extends (Typ[Term] :: HNil => GeneratorNode[Term]){
    def apply(v1: HoTT.Typ[HoTT.Term] :: HNil): GeneratorNode[HoTT.Term] = codomainNode(v1.head)
  }

  /**
    * Node family for generating functions that target a codomain;
    * they are not applied yet, instead form terms of a customised random variable.
    */
  val codomainNodeFamily
      : GeneratorNodeFamily.BasePi[::[Typ[Term], HNil], Term] =
    GeneratorNodeFamily.BasePi[Typ[Term] :: HNil, Term]({
      CodomainNode
    }, FuncForCod)

  /**
    * node to target (dependent) function types by mapping domain to zero (i.e. contradict hypothesis)
    */
  def typViaZeroNodeOpt(
      typ: Typ[Term]
  ): Option[GeneratorNode.Map[HoTT.Term, HoTT.Term]] = typ match {
    case ft: FuncTyp[u, v] =>
      val A                        = ft.dom
      val a                        = A.Var
      val zeroVar: RandomVar[Term] = termsWithTyp(A ->: Zero)
      def fn(contraTerm: Term) = {
        val contra = contraTerm.asInstanceOf[Func[u, Term]]
        a :-> Zero.rec(ft.codom)(contra(a))
      }
      Some(GeneratorNode.Map(fn, zeroVar, termsWithTyp(typ)))
    case ft: PiDefn[u, v] =>
      val A                        = ft.domain
      val a                        = A.Var
      val zeroVar: RandomVar[Term] = termsWithTyp(A ->: Zero)
      def fn(contraTerm: Term) = {
        val contra = contraTerm.asInstanceOf[Func[u, Term]]
        a :-> Zero.rec(ft.fibers(a))(contra(a))
      }
      Some(GeneratorNode.Map(fn, zeroVar, termsWithTyp(typ)))
    case _ => None
  }

  case object TypViaZeroNode extends (Typ[Term] :: HNil => Option[GeneratorNode[Term]]){
    def apply(v1: HoTT.Typ[HoTT.Term] :: HNil): Option[GeneratorNode[HoTT.Term]] = typViaZeroNodeOpt(v1.head)
  }

  /**
    * node family to target (dependent) function types by mapping domain to zero (i.e. contradict hypothesis)
    */
  val typViaZeroFamily
      : GeneratorNodeFamily.BasePiOpt[HoTT.Typ[HoTT.Term] :: HNil, HoTT.Term] =
    GeneratorNodeFamily.BasePiOpt(TypViaZeroNode, TermsWithTyp)

  case object Incl1Node extends (Typ[Term] :: HNil => Option[GeneratorNode[Term]]){
    def apply(v1: HoTT.Typ[HoTT.Term] :: HNil): Option[GeneratorNode[HoTT.Term]] = incl1Node(v1.head)
  }

  case object Incl2Node extends (Typ[Term] :: HNil => Option[GeneratorNode[Term]]){
    def apply(v1: HoTT.Typ[HoTT.Term] :: HNil): Option[GeneratorNode[HoTT.Term]] = incl2Node(v1.head)
  }

  /**
    * node family to target products using inclusions
    */
  val incl1TypNodeFamily
      : GeneratorNodeFamily.BasePiOpt[::[Typ[Term], HNil], Term] =
    GeneratorNodeFamily.BasePiOpt[Typ[Term] :: HNil, Term]({
      Incl1Node
    }, TermsWithTyp)

  /**
    * node family to target products using inclusions
    */
  val incl2TypNodeFamily
      : GeneratorNodeFamily.BasePiOpt[::[Typ[Term], HNil], Term] =
    GeneratorNodeFamily.BasePiOpt[Typ[Term] :: HNil, Term]({
      Incl2Node
    }, TermsWithTyp)

  /**
    * recursive functions from a specific inductive structure, picking the codomain,
    * this is the simplest form, where the domain is not selected and so is usually wrong;
    * further just the function is returned, not with data applied
    *
    * @param ind the inductive structure
    * @return distribution of functions
    */
  def recFuncsForStrucNode(
      ind: ExstInducStrucs
  ): ZipMapOpt[Typ[Term], Typ[Term], ExstFunc] =
    ZipMapOpt[Typ[Term], Typ[Term], ExstFunc]({
      case (x, y) => ind.recOpt(x, y).flatMap(FuncOpt)
    }, Typs, Typs, Funcs)

  /**
    * Generating domains for a given inductive structure, with an inductive definition also given.
    * Examples of domains are `Nat`, `Vec(A)` and `Fin`.
    * The structure is specified separately as it may have some parameters applied, so be different from
    * the one corresponding to the definition; similarly we start with the full type family of the definition,
    * e.g. `Vec`, but fill in parameters to get e.g. `Vec(Nat)`.
    * Note that if there are no parameters the type family is returned
    */
  def domainForStruct(
      ind: ExstInducStrucs,
      fmly: Term,
      defn: ExstInducDefn
  ): Option[GeneratorNode[Term]] =
    (ind, fmly) match {
      case (_: ExstInducStrucs.OrElse, _) => None
      case (
          ExstInducStrucs.LambdaInduc(variable, structure),
          fn: FuncLike[u, v]
          ) if variable.typ == fn.dom =>
        Some(
          FlatMapOpt[Term, Term](
            termsWithTyp(fn.dom),
            x =>
              domainForStruct(
                structure.subs(variable, x),
                fn(x.asInstanceOf[u]),
                defn
              ),
            domForInduc(defn)
          )
        )
      case (ExstInducStrucs.LambdaInduc(_, _), _) => None
      case (_, t)                                 => Some(GeneratorNode.Atom(t, domForInduc(defn)))
    }

  /**
    * Generating domains for a given inductive structure, with an inductive definition also given.
    * Examples of domains are `Nat`, `Vec(A)` and `Fin`.
    */
  def domainForDefn(ind: ExstInducDefn): Option[GeneratorNode[Term]] =
    domainForStruct(ind.ind, ind.typFamily, ind)

  case object DomainForDefn extends (ExstInducDefn :: HNil => Option[GeneratorNode[Term]]){
    def apply(v1: ExstInducDefn :: HNil): Option[GeneratorNode[HoTT.Term]] = domainForDefn(v1.head)
  }

  /**
    * Node family for generating domains for a given inductive structure, with an inductive definition also given.
    * Examples of domains are `Nat`, `Vec(A)` and `Fin`.
    */
  val domainForDefnNodeFamily
      : GeneratorNodeFamily[ExstInducDefn :: HNil, Term] =
    GeneratorNodeFamily.BasePiOpt[ExstInducDefn :: HNil, Term]({
      DomainForDefn
    }, DomForInduc)

  /**
    * Node for recursive definitions targeting a specific type
    * given an inductive definition, generating a domain and
    * invoking the node getting codomain; but not the recursion data here
    */
  def targetInducFuncs(
      ind: ExstInducDefn,
      target: Typ[Term]
  ): Option[Term] =
    target match {
      case ft: FuncTyp[u, v] =>
        ind.ind.recOpt(ft.dom, ft.codom)
      case _ =>
        goalDomainFmly(ind.ind, ind.typFamily, target).flatMap {
          case (dom, targ) =>
            val fnOpt = ind.ind.inducOpt(dom, targ)
            fnOpt
        }
    }

  case class TargetInducFuncs(target: Typ[Term])
      extends (ExstInducDefn => Option[Term]) {
    def apply(v1: ExstInducDefn): Option[HoTT.Term] =
      targetInducFuncs(v1, target)
  }
}

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
    addVar: Typ[Term] => InitState => Double => (InitState, Term),
    getVar: Typ[Term] => Term,
    inIsle: (Term, InitState) => InitState,
    solver: TypSolver = TypSolver.coreSolver
) {

  /**
    * Wrapper for application to allow equality and  `toString` to work.
    */
  case object Appln extends ((ExstFunc, Term) => Term) {
    def apply(fn: ExstFunc, arg: Term): Term = appln(fn, arg)

    override def toString = "Appln"
  }

  /**
    * Wrapper for flipped application to allow equality and  `toString` to work.
    */
  case object FlipAppln extends ((Term, ExstFunc) => Term) {
    def apply(arg: Term, fn: ExstFunc): Term = appln(fn, arg)

    override def toString = "FlipAppn"
  }

  /**
    * Wrapper for unified application to allow equality and  `toString` to work.
    */
  case object UnifApplnOpt extends ((ExstFunc, Term) => Option[Term]) {
    def apply(fn: ExstFunc, arg: Term): Option[Term] = unifApplnOpt(fn, arg)

    override def toString = "UnifApplnOpt"
  }

  /**
    * function application with unification node to get terms
    */
  val unifApplnNode: ZipMapOpt[ExstFunc, Term, Term] =
    ZipMapOpt[ExstFunc, Term, Term](
      UnifApplnOpt,
      Funcs,
      Terms,
      Terms
    )

  /**
    * function application with unification starting with type families, but with output not conditioned.
    */
  val typUnifApplnBase: ZipMapOpt[ExstFunc, Term, Term] =
    ZipMapOpt[ExstFunc, Term, Term](
      UnifApplnOpt,
      TypFamilies,
      Terms,
      Terms
    )

  /**
    * function application with unification starting with type families, with output conditioned to be a type.
    */
  val typUnifApplnNode: GeneratorNode[Typ[Term]] =
    typUnifApplnBase.|(typSort, Typs)

  /**
    * function application with unification starting with type families, with output conditioned to be a type family.
    */
  val typFamilyUnifApplnNode: GeneratorNode[ExstFunc] =
    typUnifApplnBase.|(typFamilySort, TypFamilies)

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

  /**
    * function application to get terms by choosing a type family and then a term in its domain, but without conditioning
    */
  val typApplnBase: FiberProductMap[ExstFunc, Term, Typ[Term], Term] =
    FiberProductMap[ExstFunc, Term, Typ[Term], Term](
      DomFn,
      TermsWithTypFn,
      Appln,
      TypFamilies,
      Terms
    )

  /**
    * function application to get types by choosing a type family and then a term in its domain, but without conditioning
    */
  val typApplnNode: GeneratorNode[Typ[Term]] = typApplnBase.|(typSort, Typs)

  /**
    * function application to get type families by choosing a type family and then a term in its domain, but without conditioning
    */
  val typFamilyApplnNode: GeneratorNode[ExstFunc] =
    typApplnBase.|(typFamilySort, TypFamilies)

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
    * An island to generate lambda terms, i.e., terms are generated withing the island and exported as lambdas;
    * the initial state of the island has a new variable mixed in.
    *
    * @param typ the domain of the lambda
    * @return
    */
  def lambdaIsle(typ: Typ[Term]): Island[Term, InitState, Term, Term] =
    Island[Term, InitState, Term, Term](
      Terms,
      ConstRandVar(Terms),
      addVar(typ),
      LamApply,
      inIsle
    )

  /**
    * Wrapper for lambda island to allow equality and  `toString` to work.
    */
  case object LambdaIsle
      extends (Typ[Term] => Island[Term, InitState, Term, Term]) {
    def apply(typ: Typ[Term]): Island[Term, InitState, Term, Term] =
      lambdaIsle(typ)

    override def toString = "LambdaIsle"
  }

  case object PiIsle
      extends (Typ[Term] => Island[Typ[Term], InitState, Typ[Term], Term]) {
    def apply(typ: Typ[Term]): Island[Typ[Term], InitState, Typ[Term], Term] =
      piIsle(typ)

    override def toString = "PiIsle"
  }

  case object SigmaIsle
      extends (Typ[Term] => Island[Typ[Term], InitState, Typ[Term], Term]) {
    def apply(typ: Typ[Term]): Island[Typ[Term], InitState, Typ[Term], Term] =
      sigmaIsle(typ)

    override def toString = "SigmaIsle"
  }

  /**
    * An island to generate lambda terms that are type families, i.e., terms are generated withing the island and exported as lambdas;
    * the initial state of the island has a new variable mixed in.
    * Within the island types and type families are generated.
    *
    * @param typ the domain of the lambda
    * @return
    */
  def lambdaTypFamilyIsle(typ: Typ[Term]): GeneratorNode[ExstFunc] =
    Island[ExstFunc, InitState, Term, Term](
      TypFamilies,
      ConstRandVar(TypsAndFamilies),
      addVar(typ),
      LamFunc,
      inIsle
    )

  /**
    * Wrapper for lambda island tageting type families to allow equality and  `toString` to work.
    */
  case object LambdaTypFamilyIsle
      extends (Typ[Term] => GeneratorNode[ExstFunc]) {
    def apply(typ: Typ[Term]): GeneratorNode[ExstFunc] =
      lambdaTypFamilyIsle(typ)

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
            ConstRandVar(termsWithTyp(ft.codom)),
            addVar(ft.domain),
            LamApply,
            inIsle
          )
        )
      case pt: ProdTyp[u, v] =>
        Some(
          ZipMap[Term, Term, Term](
            PTTerm(pt),
            termsWithTyp(pt.first),
            termsWithTyp(pt.second),
            termsWithTyp(pt)
          )
        )
      case pt: SigmaTyp[u, v] =>
        Some(
          ZipFlatMap[Term, Term, Term](
            termsWithTyp(pt.fibers.dom),
            STFibVar(pt),
            STTerm(pt),
            termsWithTyp(pt)
          )
        )
      case _ => None
    }

  case object NodeForTyp
      extends (Typ[Term] :: HNil => Option[GeneratorNode[Term]]) {
    def apply(
        v1: HoTT.Typ[HoTT.Term] :: HNil
    ): Option[GeneratorNode[HoTT.Term]] = nodeForTyp(v1.head)
  }

  /**
    * node for targeting functions on products by currying, and also on sigma-types and co-products
    */
  def curryForTyp(typ: Typ[Term]): Option[GeneratorNode[Term]] =
    typ match {
      case ft: FuncTyp[u, v] =>
        if (ft.dom == Zero)
          Some(GeneratorNode.Atom(Zero.rec(ft.codom), termsWithTyp(typ)))
        else
          ExstInducStrucs.SimpleBase.recOpt(ft.dom, ft.codom).flatMap {
            case fn: Func[a, b] =>
              val curryDom = fn.dom
              Some(
                GeneratorNode.Map(
                  ApplyFunc(fn),
                  termsWithTyp(curryDom),
                  termsWithTyp(ft)
                )
              )
            case _ => None
          }
      case _ => None
    }

  case object CurryForTyp
      extends (Typ[Term] :: HNil => Option[GeneratorNode[Term]]) {
    def apply(
        v1: HoTT.Typ[HoTT.Term] :: HNil
    ): Option[GeneratorNode[HoTT.Term]] = curryForTyp(v1.head)
  }

  case class FoldFuncTargetNode(typ: Typ[Term])
      extends (Term => Option[GeneratorNode[Term]]) {
    def apply(v1: HoTT.Term): Option[GeneratorNode[HoTT.Term]] =
      foldFuncTargetNode(v1, typ, termsWithTyp(typ))
  }

  /**
    * Node for generating terms of a type (if possible) by multiple applications of functions tageting the type.
    */
  def foldedTargetFunctionNode(typ: Typ[Term]): FlatMapOpt[Term, Term] =
    FlatMapOpt[Term, Term](
      funcForCod(typ),
      FoldFuncTargetNode(typ),
      termsWithTyp(typ)
    )

  case object FoldedTargetFunctionNode
      extends (Typ[Term] :: HNil => FlatMapOpt[Term, Term]) {
    def apply(
        v1: HoTT.Typ[HoTT.Term] :: HNil
    ): GeneratorNode.FlatMapOpt[HoTT.Term, HoTT.Term] =
      foldedTargetFunctionNode(v1.head)
  }

  /**
    * Node for generating terms of a type (if possible) by multiple applications of functions targeting the type.
    */
  val typAsCodNodeFamily
      : GeneratorNodeFamily.BasePi[::[Typ[Term], HNil], Term] =
    GeneratorNodeFamily.BasePi[Typ[Term] :: HNil, Term]({
        FoldedTargetFunctionNode
    }, FuncForCod)

  /**
    * lambda island for generating function with specified domain
    *
    * @param dom the desired domain
    * @return distribution of functions
    */
  def lambdaIsleForFuncWithDomain(
      dom: Typ[Term]
  ): Island[ExstFunc, InitState, Term, Term] =
    Island[ExstFunc, InitState, Term, Term](
      funcsWithDomain(dom),
      ConstRandVar(Terms),
      addVar(dom),
      LamFunc,
      inIsle
    )

  case object LambdaIsleForFuncWithDomain
      extends (Typ[Term] :: HNil => Island[ExstFunc, InitState, Term, Term]) {
    def apply(
        v1: HoTT.Typ[HoTT.Term] :: HNil
    ): GeneratorNode.Island[HoTT.ExstFunc, InitState, HoTT.Term, HoTT.Term] =
      lambdaIsleForFuncWithDomain(v1.head)
  }

  /**
    * node combining lambda islands aggregated by type
    */
  val lambdaNode: FlatMap[Typ[Term], Term] =
    FlatMap(
      Typs,
      LambdaIsle,
      Terms
    )

  /**
    * node combining lambda islands targeting type families aggregated by type
    */
  val lambdaTypFamilyNode: FlatMap[Typ[Term], ExstFunc] =
    FlatMap(
      Typs,
      LambdaTypFamilyIsle,
      TypFamilies
    )

  /**
    * nodes combining backward reasoning targeting types that are  (dependent) function types,
    * aggregated over all types
    */
  val backwardTypNodeFamily
      : GeneratorNodeFamily.BasePiOpt[::[Typ[Term], HNil], Term] =
    GeneratorNodeFamily.BasePiOpt[Typ[Term] :: HNil, Term]({
      NodeForTyp
    }, TermsWithTyp)

  /**
    * nodes combining backward reasoning targeting types that are  (dependent) function types, with domain product, sigma-types or co-products
    * aggregated over all types
    */
  val curryBackwardTypNodeFamily
      : GeneratorNodeFamily.BasePiOpt[::[Typ[Term], HNil], Term] =
    GeneratorNodeFamily.BasePiOpt[Typ[Term] :: HNil, Term]({
      CurryForTyp
    }, TermsWithTyp)

  /**
    * nodes combining lambdas with given domain that are  (dependent) function types,
    * aggregated over all domains
    */
  val lambdaForFuncWithDomFamily
      : GeneratorNodeFamily.BasePi[::[Typ[Term], HNil], ExstFunc] =
    GeneratorNodeFamily.BasePi[Typ[Term] :: HNil, ExstFunc](
      LambdaIsleForFuncWithDomain,
      FuncsWithDomain
    )

  case object SolverTyp extends (Typ[Term] :: HNil => Option[GeneratorNode[Term]]){
    def apply(v1: HoTT.Typ[HoTT.Term] :: HNil): Option[GeneratorNode[HoTT.Term]] = {
      val typ = v1.head
      solver(typ)
          .map(Atom(_, termsWithTyp(typ)))
    }
  }

  /**
    * nodes for invoking (external) solvers for special types.
    */
  val solveFamily: GeneratorNodeFamily.BasePiOpt[Typ[Term] :: HNil, Term] =
    GeneratorNodeFamily.BasePiOpt[Typ[Term] :: HNil, Term]({
      SolverTyp 
    }, TermsWithTyp)

  /**
    * island to generate Pi-Types by taking variables with specified domain, similar to [[lambdaIsle]]
    *
    * @param typ the domain for
    * @return distribution of types
    */
  def piIsle(typ: Typ[Term]): Island[Typ[Term], InitState, Typ[Term], Term] =
    Island[Typ[Term], InitState, Typ[Term], Term](
      Typs,
      ConstRandVar(Typs),
      addVar(typ),
      PiApply,
      inIsle
    )

  /**
    * island to generate Sigma-Types by taking variables with specified domain, similar to [[lambdaIsle]]
    *
    * @param typ the domain for
    * @return distribution of types
    */
  def sigmaIsle(typ: Typ[Term]): Island[Typ[Term], InitState, Typ[Term], Term] =
    Island[Typ[Term], InitState, Typ[Term], Term](
      Typs,
      ConstRandVar(Typs),
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
      PiIsle,
      Typs
    )

  /**
    * aggregate generation of Sigma-types from islands
    */
  val sigmaNode: FlatMap[Typ[Term], Typ[Term]] =
    FlatMap(
      Typs,
      SigmaIsle,
      Typs
    )

  /**
    * Node for fully folding a type family to get types
    */
  def foldTypFamily(w: Term): GeneratorNode[Typ[Term]] = w match {
    case typ: Typ[Term] => Atom(typ, Typs)
    case fn: FuncLike[u, v] =>
      FlatMap(
        termsWithTyp(fn.dom),
        (x: Term) =>
          foldTypFamily(fn(x.asInstanceOf[u])), 
        Typs
      )
  }

  case object FoldTypFamily extends (ExstFunc => GeneratorNode[Typ[Term]]){
    def apply(v1: HoTT.ExstFunc): GeneratorNode[HoTT.Typ[HoTT.Term]] = foldTypFamily(v1.term)
  }

  /**
    * Node family for fully folding type families to get types
    */
  val typFoldNode: FlatMap[ExstFunc, Typ[Term]] =
    FlatMap(
      TypFamilies,
      FoldTypFamily,
      Typs
    )

  /**
    * Node for folding a function (or term) with a speficied number of arguments
    * to get terms.
    */
  def foldFuncNode(t: Term, depth: Int): GeneratorNode[Term] =
    if (depth < 1) Atom(t, AtomVar(t))
    else
      t match {
        case fn: FuncLike[u, v] =>
          FlatMap(
            termsWithTyp(fn.dom),
            (x: Term) =>
              foldFuncNode(fn(x.asInstanceOf[u]), depth - 1), 
            FuncFoldVar(t, depth)
          )
      }

  /**
    * Node for folding a function (or term) with a speficied target type
    * to optionally get terms with the target type.
    */
  def foldFuncTargetNode(
      t: Term,
      target: Typ[Term],
      output: RandomVar[Term]
  ): Option[GeneratorNode[Term]] =
    if (t.typ == target) Some(Atom(t, output))
    else
      t match {
        case fn: FuncLike[u, v] =>
          Some(
            FlatMapOpt(
              termsWithTyp(fn.dom),
              (x: Term) =>
                foldFuncTargetNode(fn(x.asInstanceOf[u]), target, output), 
              output
            )
          )
        case _ =>
          None
      }

  /**
    * Recursive definition given an inductive definition and a domain,
    * i.e., recursion function with data of the right type folded in.
    * Examples of domains are `Nat`, `Vec(A)` and `Fin`
    */
  def recFuncsFoldedGivenDomNode(
      ind: ExstInducDefn,
      dom: Term
  ): GeneratorNode[Term] =
    FlatMapOpt[Typ[Term], Term](
      Typs,
      (codom: Typ[Term]) => {
        val fnOpt = ind.ind.recOpt(dom, codom)
        fnOpt.map { fn =>
          foldFuncNode(fn, ind.intros.size)
        }
      },
      Terms
    )

  /**
    * Recursive definition given an inductive definition and a domain,
    * i.e., recursion function with data of the right type folded in.
    * Examples of domains are `Nat`, `Vec(A)` and `Fin`
    */
  def inducFuncsFoldedGivenDomNode(
      ind: ExstInducDefn,
      dom: Term
  ): GeneratorNode[Term] =
    FlatMapOpt[Term, Term](
      termsWithTyp(typFamilyTarget(dom).get),
      (codom: Term) => {
        val fnOpt = ind.ind.inducOpt(dom, codom) 
        fnOpt.map { fn =>
          foldFuncNode(fn, ind.intros.size)
        }
      },
      Terms
    )

  /**
    * Recursive definitions from a given inductive definition by generating a domain and
    * invoking the node getting codomain and recursion data
    */
  def recFuncsFolded(ind: ExstInducDefn): GeneratorNode[Term] =
    FlatMap[Term, Term](
      domForInduc(ind),
      dom =>
        recFuncsFoldedGivenDomNode(ind, dom),
      Terms
    )

  /**
    * Node for recursive definitions picking an inductive definition, generating a domain and
    * invoking the node getting codomain and recursion data
    */
  val recFuncFoldedNode: GeneratorNode[Term] =
    FlatMap[ExstInducDefn, Term](
      InducDefns,
      RecFuncsFolded, 
      Terms
    )

  /**
    * Inductive definitions from a given inductive definition by generating a domain and
    * invoking the node getting codomain and recursion data
    */
  def inducFuncsFolded(ind: ExstInducDefn): GeneratorNode[Term] =
    FlatMap[Term, Term](
      domForInduc(ind),
      dom =>
        inducFuncsFoldedGivenDomNode(ind, dom), 
      Terms
    )

  case object InducFuncsFolded extends (ExstInducDefn => GeneratorNode[Term]) {
    def apply(v1: ExstInducDefn): GeneratorNode[HoTT.Term] =
      inducFuncsFolded(v1)
  }

  case object RecFuncsFolded extends (ExstInducDefn => GeneratorNode[Term]) {
    def apply(v1: ExstInducDefn): GeneratorNode[HoTT.Term] =
      recFuncsFolded(v1)
  }

  /**
    * Node for recursive definitions targeting a specific type
    * picking an inductive definition, generating a domain and
    * invoking the node getting codomain and recursion data
    */
  def targetInducFuncsFolded(
      ind: ExstInducDefn,
      target: Typ[Term]
  ): Option[GeneratorNode[Term]] =
    target match {
      case ft: FuncTyp[u, v] =>
        val fnOpt = ind.ind.recOpt(ft.dom, ft.codom)
        fnOpt.map { fn =>
          foldFuncNode(fn, ind.intros.size)
        }
      case _ =>
        goalDomainFmly(ind.ind, ind.typFamily, target).flatMap {
          case (dom, targ) =>
            val fnOpt = ind.ind.inducOpt(dom, targ)
            fnOpt.map { fn =>
              foldFuncNode(fn, ind.intros.size)
            }
        }
    }

  case class TargetInducFuncsFolded(target: Typ[Term])
      extends (ExstInducDefn => Option[GeneratorNode[Term]]) {
    def apply(v1: ExstInducDefn): Option[GeneratorNode[HoTT.Term]] =
      targetInducFuncsFolded(v1, target)
  }

  /**
    * Node for recursive definitions given an inductive definition, generating a domain and
    * invoking the node getting codomain and recursion data
    */
  val inducFuncFoldedNode: GeneratorNode[Term] =
    FlatMap[ExstInducDefn, Term](
      InducDefns,
      InducFuncsFolded,
      Terms
    )

  /**
    * Node for recursive definitions targeting a specific type
    * picking an inductive definition, generating a domain and
    * invoking the node getting codomain; but not the recursion data here
    */
  def targetInducNode(typ: Typ[Term]): FlatMapOpt[ExstInducDefn, Term] =
    FlatMapOpt[ExstInducDefn, Term](
      InducDefns,
      TargetInducFuncsFolded(typ),
      termsWithTyp(typ)
    )

  case object TargetInducNode
      extends (Typ[Term] :: HNil => FlatMapOpt[ExstInducDefn, Term]) {
    def apply(
        v1: HoTT.Typ[HoTT.Term] :: HNil
    ): GeneratorNode.FlatMapOpt[ExstInducDefn, HoTT.Term] =
      targetInducNode(v1.head)
  }

  /**
    * Node for recursive definitions targeting a specific type
    * picking an inductive definition, generating a domain and
    * invoking the node getting codomain; but not the recursion data here
    * meant to be used for backward reasoning.
    */
  def targetInducBackNode(typ: Typ[Term]): MapOpt[ExstInducDefn, Term] =
    MapOpt[ExstInducDefn, Term](
      TargetInducFuncs(typ),
      InducDefns,
      Terms
    )

  /**
    *  Family of nodes for recursive definitions targeting a specific type
    * picking an inductive definition, generating a domain and
    * invoking the node getting codomain; but not the recursion data here
    */
  val targetInducNodeFamily
      : GeneratorNodeFamily.BasePi[Typ[Term] :: HNil, Term] =
    GeneratorNodeFamily.BasePi[Typ[Term] :: HNil, Term]({
      TargetInducNode
    }, TermsWithTyp)

  /**
    * induction function for a specific structure, picking the type family
    *
    * @param ind the inductive structure
    * @return distribution of functions
    */
  def inducFuncsForStruc(
      ind: ExstInducStrucs
  ): ZipMapOpt[Typ[Term], Term, ExstFunc] =
    ZipMapOpt[Typ[Term], Term, ExstFunc]({
      case (x, y) =>
        ind.inducOpt(x, y).flatMap(FuncOpt) // Unused
    }, Typs, Terms, Funcs)

  /**
    * aggregate recursion functions from inductive types
    */
  val recFuncs: FlatMap[ExstInducStrucs, ExstFunc] =
    FlatMap(
      InducStrucs,
      recFuncsForStrucNode, // Unused
      Funcs
    )

  /**
    * aggregated induction functions from inductive types
    */
  val inducFuncs: FlatMap[ExstInducStrucs, ExstFunc] =
    FlatMap(
      InducStrucs,
      inducFuncsForStruc, // Unused
      Funcs
    )

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
    *
    * @param inductiveTyp the inductive type being defined
    * @return distribution of types for introduction rules
    */
  def selfHeadNode(
      inductiveTyp: Typ[Term]
  ): Island[Typ[Term], InitState, Typ[Term], Term] =
    Island[Typ[Term], InitState, Typ[Term], Term](
      IntroRuleTypes(inductiveTyp),               // output
      ConstRandVar(IntroRuleTypes(inductiveTyp)), // output from island
      addVar(inductiveTyp),
      PiApply,
      inIsle
    )

  /**
    * extensions of the introduction rule types for an inductive type `W` by an unrelated type `C`;
    * we may have a Pi-Type with domain `C` rather than a FuncTyp
    * there is no assumption on how the head type `typ` is generated, so this is reusable if only that is changed.
    *
    * @param inductiveTyp the inductive type `W`
    * @param typ the type `C` by which we are extending the introduction rule type
    * @return distribution of introduction rule types
    */
  def otherHeadIsle(
      inductiveTyp: Typ[Term]
  )(typ: Typ[Term]): Island[Typ[Term], InitState, Typ[Term], Term] =
    Island[Typ[Term], InitState, Typ[Term], Term](
      IntroRuleTypes(inductiveTyp),
      ConstRandVar(IntroRuleTypes(inductiveTyp)),
      addVar(typ),
      PiApply,
      inIsle
    )

  /**
    * aggregating extensions of inductive types by unrelated types
    *
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
    *
    * @param inductiveTyp the inductve type being defined
    * @return distribution on existential inductive structures
    */
  def simpleInductiveStructure(
      inductiveTyp: Typ[Term]
  ): Map[Vector[Typ[Term]], ExstInducStrucs] =
    Map[Vector[Typ[Term]], ExstInducStrucs](
      introTyps => {
        val intros = introTyps.map(getVar)
        ExstInducStrucs.get(inductiveTyp, intros)
      }, // Unused
      RandomVector(IntroRuleTypes(inductiveTyp)),
      InducStrucs
    )

  /**
    * builds an inductive definition, i.e. structure with type, from a bunch of introduction rules for a type `inductiveTyp`,
    * we must separately specify how to build an introduction rules, as well as a vector of these.
    *
    * @param inductiveTyp the inductve type being defined
    * @return distribution on existential inductive structures
    */
  def simpleInductiveDefn(
      inductiveTyp: Typ[Term]
  ): Map[Vector[Typ[Term]], ExstInducDefn] =
    Map[Vector[Typ[Term]], ExstInducDefn](
      introTyps => {
        val intros = introTyps.map(getVar)
        ExstInducDefn(
          inductiveTyp,
          intros,
          ExstInducStrucs.get(inductiveTyp, intros)
        )
      }, // Unused
      RandomVector(IntroRuleTypes(inductiveTyp)),
      InducDefns
    )

  // Generating iterated function types ending in `W` and indexed types

  /**
    * single function application to generate partial applications of `f`, if `f` is a function
    *
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
    *
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
    *
    * @param targetTyp the target type
    * @param typ the type by which it is extended
    * @return distribution of types
    */
  def iterFuncIsle(
      targetTyp: Typ[Term]
  )(typ: Typ[Term]): Island[Typ[Term], InitState, Typ[Term], Term] =
    Island[Typ[Term], InitState, Typ[Term], Term](
      IterFuncTypTo(targetTyp),
      ConstRandVar(IntroRuleTypes(targetTyp)),
      addVar(typ),
      PiApply,
      inIsle
    )

  /**
    * aggregating extensions of iterated function types
    *
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
    *
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
    *
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
    *
    * @param typF the type family for the inductive type
    * @return distribution of types
    */
  def indexedInducIdNode(typF: Typ[Term]): Map[Typ[Term], Typ[Term]] =
    Map[Typ[Term], Typ[Term]](
      Idty(),
      TypsFromFamily(typF),
      IndexedIntroRuleTyps(typF)
    )

  /**
    * Extending an indexed introduction rule by a specific unrelated type
    *
    * @param typF the indexed inductive type family
    * @param typ the type by which to extends
    * @return distribution on types
    */
  def indexedOtherHeadIsle(
      typF: Term
  )(typ: Typ[Term]): Island[Typ[Term], InitState, Typ[Term], Term] =
    Island[Typ[Term], InitState, Typ[Term], Term](
      IndexedIntroRuleTyps(typF),
      ConstRandVar(IndexedIntroRuleTyps(typF)),
      addVar(typ),
      PiApply,
      inIsle
    )

  /**
    * Aggregating extending indexed introduction rule by fully applied inductive type family.
    *
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
    *
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
    *
    * @param typ the type by which we are extending
    * @return distribution on types.
    */
  def indexedIterFuncIsle(
      targetTyp: Term
  )(typ: Typ[Term]): Island[Typ[Term], InitState, Typ[Term], Term] =
    Island[Typ[Term], InitState, Typ[Term], Term](
      IndexedIterFuncTypTo(targetTyp),
      ConstRandVar(IndexedIntroRuleTyps(targetTyp)),
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
    *
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
