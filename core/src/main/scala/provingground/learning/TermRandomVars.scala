package provingground.learning
import provingground.HoTT._
import provingground.{FiniteDistribution => _, _}
import shapeless._
import induction._
import provingground.learning.GeneratorNode.Map

import GeneratorNode._
import scala.util._

import TermGeneratorNodes._
import provingground.learning.Sort.All
import provingground.learning.Sort.Filter
import provingground.learning.Sort.Restrict
import provingground.interface.ContextJson

object TermRandomVars {

  case object TypOpt extends (Term => Option[Typ[Term]]) {
    def apply(t: Term): Option[Typ[Term]] = typOpt(t)

    override def toString = "TypOpt"
  }

  case object TypAsTermOpt extends (Typ[Term] => Option[Term]) {
    def apply(t: Typ[Term]): Option[Term] = Some(t)

    override def toString = "TypAsTermOpt"
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

  case class FuncWithDomFilter(dom: Typ[Term]) extends (ExstFunc => Boolean) {
    def apply(v1: HoTT.ExstFunc): Boolean = v1.dom == dom

    override def toString = s"FuncWithDomFilter($dom)"
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

  /**
    * distribution of functions : as existentials (wrapping terms), not as terms
    */
  case object Funcs extends RandomVar[ExstFunc]

  /**
    * family of distributions of terms with specified type
    */
  case object TermsWithTyp
      extends RandomVar.SimpleFamily[Typ[Term], Term](
        Typs,
        (typ: Typ[Term]) => Sort.Filter[Term](WithTyp(typ))
      ) {
    def unapply(x: Any): Option[Typ[Term]] = x match {
      case RandomVar.AtCoord(fmly, (typ: Typ[Term]) :: HNil)
          if fmly == TermsWithTyp =>
        Some(typ)
      case _ => None
    }
  }

  /**
    * distribution of terms with a specific type
    *
    * @param typ the type
    * @return distribution at type
    */
  def termsWithTyp(typ: Typ[Term]): RandomVar[Term] =
    RandomVar.AtCoord(TermsWithTyp, typ :: HNil)

  /**
    * Wrapper for terms with type family to allow equality and  `toString` to work.
    */
  case object TermsWithTypFn extends (Typ[Term] => RandomVar[Term]) {
    def apply(typ: Typ[Term]) = RandomVar.AtCoord(TermsWithTyp, typ :: HNil)

    override def toString = "TermsWithTypFn"
  }

  /**
    * distribution of type families
    */
  case object TypFamilies extends RandomVar[ExstFunc]

  val typFamilySort: Sort[Term, ExstFunc] =
    Sort.Restrict[Term, ExstFunc](TypFamilyOpt)

  /**
    * distribution of types and type families
    */
  case object TypsAndFamilies extends RandomVar[Term] {
    lazy val fromTyp: Map[Typ[Term], Term] =
      Map[Typ[Term], Term](Idty(), Typs, TypsAndFamilies)

    lazy val fromFamilies: Map[ExstFunc, Term] =
      Map[ExstFunc, Term](ExstFunc.GetFunc, TypFamilies, TypsAndFamilies)
  }

  case object Negate extends (Typ[Term] => Typ[Term]) {
    def apply(v1: HoTT.Typ[HoTT.Term]): HoTT.Typ[HoTT.Term] = negate(v1)

    override def toString(): String = "Negate"
  }

  /**
    * distribution of types to target for generating terms; either a generated type or a goal.
    */
  case object TargetTyps extends RandomVar[Typ[Term]] {
    def fromGoal: Map[Typ[Term], Typ[Term]] = Map(Idty(), Goals, TargetTyps)

    def fromTyp: Map[Typ[Term], Typ[Term]] = Map(Idty(), Typs, TargetTyps)

    def fromNegTyp: Map[Typ[Term], Typ[Term]] = Map(Negate, Typs, TargetTyps)
  }

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

    override def toString(): String = s"PiOutput($pd)"
  }

  def withTypSort(typ: Typ[Term]): Sort[Term, Term] =
    Sort.Filter[Term](WithTyp(typ))

  case object WithTypSort extends (Typ[Term] => Sort[Term, Term]) {
    def apply(v1: HoTT.Typ[HoTT.Term]): Sort[HoTT.Term, HoTT.Term] =
      withTypSort(v1)

    override def toString(): String = "WithTypSort"
  }

  def withTypNode(
      node: GeneratorNode[Term]
  ): GeneratorNodeFamily[Typ[Term] :: HNil, Term] =
    node.pi(WithTypSort, TermsWithTyp)

  /**
    * family of distribution of (existential) functions with specifed domain.
    */
  case object FuncsWithDomain
      extends RandomVar.SimpleFamily[Typ[Term], ExstFunc](
        Typs,
        (typ: Typ[Term]) => Sort.Filter[ExstFunc](FuncWithDomFilter(typ))
      )

  // def funcWithDomSort(dom: Typ[Term]): Sort.Filter[ExstFunc] =
  //   Sort.Filter[ExstFunc](_.dom == dom)

  // def funcWithDomNode(
  //     node: GeneratorNode[ExstFunc]
  // ): GeneratorNodeFamily[Typ[Term] :: HNil, ExstFunc] =
  //   node.pi(funcWithDomSort, FuncsWithDomain)

  case object RestrictFuncWithDom
      extends (Typ[Term] => Sort.Restrict[Term, ExstFunc]) {
    def apply(v1: HoTT.Typ[HoTT.Term]): Restrict[HoTT.Term, HoTT.ExstFunc] =
      Sort.Restrict[Term, ExstFunc](FuncWithDom(v1))

    override def toString(): String = "RestrictFuncWithDom"
  }

  def funcWithDomTermNode(
      node: GeneratorNode[Term]
  ): GeneratorNodeFamily[::[Typ[Term], HNil], ExstFunc] =
    node.pi(
      RestrictFuncWithDom,
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

    override def toString = "FuncsWithDomainFn"
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
          case TypOpt       => TypOpt.asInstanceOf[U => Option[V]]
          case TypAsTermOpt => TypAsTermOpt.asInstanceOf[U => Option[V]]
          case FuncOpt      => FuncOpt.asInstanceOf[U => Option[V]]
          case TypFamilyOpt => TypFamilyOpt.asInstanceOf[U => Option[V]]
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
      case PiOutput(pd: PiDefn[u, v]) => PiOutput(pd.replace(x, y)).asInstanceOf[Term => RandomVar[d]]
      case _ =>
        Utils.baseLogger.warn(
          s"creating lambda random variable by substitution  for ${isle.islandOutput} in $isle; $boat gives ${isle
            .islandOutput(boat)}"
        )
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

  def variableSubs[Y](
      x: Term,
      y: Term
  )(v: Variable[Y]): Variable[Y] =
    if (x == y) v
    else
      (v: Variable[Y]) match {
        case Elem(element, randomVar) =>
          Elem(valueSubs(x, y)(element), randomVarSubs(x, y)(randomVar))
        case ev: Event[a, b] =>
          val newBase             = randomVarSubs(x, y)(ev.base)
          val newSort: Sort[a, b] = sortSubs(x, y)(ev.sort)
          Event(newBase, newSort)
        case inIsleFull: InIsle[c, cc, _, d, _] =>
          val inIsleTry =
            Try(inIsleFull.asInstanceOf[InIsle[c, cc, TermState, d, Term]])
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

  def isleNormalizeVars[Y](
      v: GeneratorVariables.Variable[Y],
      vars: Vector[Term]
  ): GeneratorVariables.Variable[Y] =
    v match {
      case Elem(element, randomVar) => v
      case Event(base, sort)        => v
      case inIsleFull: InIsle[c, y, _, d, _] =>
        val inIsleTry =
          Try(inIsleFull.asInstanceOf[InIsle[c, Y, TermState, d, Term]])
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
      fn: VariableMap
  )(exp: Expression): Expression = exp match {
    case FinalVal(variable)   => FinalVal(fn(variable))
    case InitialVal(variable) => InitialVal(fn(variable))
    case Sum(xs) =>
      Sum(
        xs.map(x => expressionMapVars(fn)(x))
      )
    case IsleScale(boat: Term) =>
      val newBoat = "%boat" :: boat.typ
      IsleScale(
        newBoat
      )
    case IsleScale(boat) => IsleScale(boat)
    case Log(exp)        => Log(expressionMapVars(fn)(exp))
    case Literal(value)  => Literal(value)
    case Exp(exp)        => Exp(expressionMapVars(fn)(exp))
    case Coeff(node)     => Coeff(node)
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
    if (x == y) exp
    else
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
            valueSubs(x, y)(sc.boat)
          )
        case cf @ Coeff(node) => cf
        case Sum(xs) =>
          Sum(
            xs.map(a => expressionSubs(x, y)(a))
          )
      }

  def isleNormalizeVarExp[Y](
      v: GeneratorVariables.Variable[Y],
      rhs: Expression,
      vars: Vector[Term]
  ): (GeneratorVariables.Variable[Y], Expression) =
    v match {
      case Elem(element, randomVar) =>
        val fn =
          new Expression.VariableMap {
            def apply[Y](
                arg: GeneratorVariables.Variable[Y]
            ): GeneratorVariables.Variable[Y] =
              TermRandomVars.isleNormalizeVars(arg, Vector())
          }
        // (v: GeneratorVariables.Variable[Y])  => TermRandomVars.isleNormalizeVars(v,Vector())
        v -> expressionMapVars(fn)(rhs)
      case Event(base, sort) =>
        val fn =
          new Expression.VariableMap {
            def apply[Y](
                arg: GeneratorVariables.Variable[Y]
            ): GeneratorVariables.Variable[Y] =
              TermRandomVars.isleNormalizeVars(arg, Vector())
          }
        v -> expressionMapVars(fn)(rhs)
      case inIsleFull: InIsle[c, cc, _, d, _] =>
        val inIsleTry =
          Try(inIsleFull.asInstanceOf[InIsle[c, cc, TermState, d, Term]])
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
        val fn =
          new Expression.VariableMap {
            def apply[Y](
                arg: GeneratorVariables.Variable[Y]
            ): GeneratorVariables.Variable[Y] =
              TermRandomVars.isleNormalizeVars(arg, Vector())
          }
        v -> expressionMapVars(fn)(rhs)
    }

  def isleNormalize(eq: EquationNode, varWeight: Double = 0.3): EquationNode = {
    val fn =
      new Expression.VariableMap {
        def apply[Y](
            arg: GeneratorVariables.Variable[Y]
        ): GeneratorVariables.Variable[Y] =
          TermRandomVars.isleNormalizeVars(arg, Vector())
      }
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
      case Elem(fn: ExstFunc, _)          => fn.func.dependsOn(t)
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

  def equationNodeDepends(t: Term)(eq: EquationNode): Boolean = {
    import Expression.varVals
    val genvars = varVals(eq.lhs).map(_.variable) union varVals(eq.rhs)
      .map(_.variable)
    genvars.exists(v => varDepends(t)(v))
  }

  val randomVarStrings: Vector[(RandomVar[_], String, String)] =
    Vector(
      (Funcs, "funcs", "func"),
      (Terms, "terms", "term"),
      (Typs, "types", "typ"),
      (TargetTyps, "target-types", "typ"),
      (Goals, "goals", "typ"),
      (TypFamilies, "type-families", "term"),
      (TypsAndFamilies, "types-and-families", "term"),
      (InducDefns, "induc-defns", "induc-defn"),
      (InducStrucs, "induc-structs", "induc-struc"),
      (IsleDomains, "isle-domains", "typs")
    )

  val rvStrMap = randomVarStrings.map { case (x, y, _) => (x, y) }.toMap

  val rvElemMap = randomVarStrings.map { case (x, _, y) => (x, y) }.toMap

  val strRvMap = randomVarStrings.map { case (x, y, _) => y -> x }.toMap

  import ujson._, provingground.interface.TermJson.{termToJson, jsonToTerm}
  import provingground.interface.InducJson._

  def randomVarToJson[X](rv: RandomVar[X]): ujson.Value =
    rv match {
      case AtomVar(atom: Term) =>
        Obj(
          "family"    -> Str("atom"),
          "atom"      -> termToJson(atom).get,
          "elem-type" -> "term"
        )
      case ContextTerms(ctx) =>
        Obj(
          "family"    -> "context-terms",
          "ctx"       -> ContextJson.toJson(ctx),
          "elem-type" -> "term"
        )
      case ContextTyps(ctx) =>
        Obj(
          "family"    -> "context-typs",
          "ctx"       -> ContextJson.toJson(ctx),
          "elem-type" -> "term"
        )
      case FuncFoldVar(func, depth) =>
        Obj(
          "family"    -> "func-fold-var",
          "func"      -> termToJson(func),
          "depth"     -> Num(depth),
          "elem-type" -> "term"
        )
      case IndexedIntroRuleTyps(typF) =>
        Obj(
          "family"    -> "indexed-intro-rules-typ",
          "typF"      -> termToJson(typF),
          "elem-type" -> "typ"
        )
      case IndexedIterFuncTypTo(typF) =>
        Obj(
          "family"    -> "indexed-iter-func-typ-to",
          "typF"      -> termToJson(typF),
          "elem-type" -> "typ"
        )
      case IntroRuleTypes(inductiveTyp) =>
        Obj(
          "family"        -> "intro-rules-typ",
          "inductive-typ" -> termToJson(inductiveTyp),
          "elem-type"     -> "typ"
        )
      case IterFuncTypTo(typ) =>
        Obj(
          "family"    -> "iter-func-typ-to",
          "typF"      -> termToJson(typ),
          "elem-type" -> "typ"
        )
      case PartiallyApplied(func) =>
        Obj(
          "family"    -> "partially-applied",
          "func"      -> termToJson(func),
          "elem-type" -> "term"
        )
      case TypsFromFamily(typF) =>
        Obj(
          "family"    -> "types-from-family",
          "typF"      -> termToJson(typF),
          "elem-type" -> "typ"
        )
      case RandomVector(base) =>
        Obj(
          "family"    -> "vector",
          "base"      -> randomVarToJson(base),
          "elem-type" -> "vector"
        )
      case RandomVar
            .AtCoord(rvF: RandomVarFamily[v, u], (head: Term) :: tail) =>
        rvF match {
          case TermsWithTyp =>
            Obj(
              "family"    -> "terms-with-type",
              "coord"     -> termToJson(head),
              "elem-type" -> "term"
            )
          case FuncsWithDomain =>
            Obj(
              "family"    -> "funcs-with-domain",
              "coord"     -> termToJson(head),
              "elem-type" -> "func"
            )
          case FuncForCod =>
            Obj(
              "family"    -> "funcs-for-cod",
              "coord"     -> termToJson(head),
              "elem-type" -> "term"
            )

        }
      case RandomVar.AtCoord(rvF, (head: ExstInducDefn) :: tail)
          if rvF == DomForInduc =>
        Obj(
          "family" -> "dom-for-induc",
          "coord"  -> upickle.default.write(head)
        )

      case randVar =>
        Obj("family" -> "single", "value" -> Str(rvStrMap(randVar)))
    }

  def jsonToRandomVar(json: ujson.Value): RandomVar[_] = {
    val obj               = json.obj
    val family            = obj("family").str
    def termAt(s: String) = jsonToTerm()(obj(s)).get
    family match {
      case "atom"          => jsonToRandomVar(obj("atom"))
      case "context-terms" => ContextTerms(ContextJson.fromJson(obj("ctx")))
      case "context-typs"  => ContextTyps(ContextJson.fromJson(obj("ctx")))
      case "func-fold-var" =>
        FuncFoldVar(termAt("func"), obj("depth").num.toInt)
      case "indexed-intro-rules-typ"  => IndexedIntroRuleTyps(termAt("typF"))
      case "indexed-iter-func-typ-to" => IndexedIterFuncTypTo(termAt("typF"))
      case "intro-rules-typ"          => IntroRuleTypes(toTyp(termAt("inductive-typ")))
      case "iter-func-typ-to"         => IterFuncTypTo(toTyp(termAt("typF")))
      case "partially-applied"        => PartiallyApplied(termAt("func"))
      case "types-from-family"        => TypsFromFamily(termAt("typF"))
      case "vector"                   => RandomVector(jsonToRandomVar(obj("base")))
      case "terms-with-type"          => termsWithTyp(toTyp(termAt("coord")))
      case "funcs-with-domain"        => funcsWithDomain(toTyp(termAt("coord")))
      case "funcs-for-cod"            => funcForCod(toTyp(termAt("coord")))
      case "dom-for-induc" =>
        domForInduc(upickle.default.read[ExstInducDefn](obj("coord")))
      case "single" => strRvMap(obj("value").str)

    }
  }

}
