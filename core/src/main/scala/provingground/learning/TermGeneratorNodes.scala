package provingground.learning
import provingground.HoTT._
import provingground.{FiniteDistribution => FD, _}
import shapeless._
import induction._
import provingground.learning.GeneratorNode.{Map, MapOpt}

import scala.language.higherKinds

import GeneratorNode._

import TermRandomVars._

class TermGeneratorNodes[InitState](
    appln: (ExstFunc, Term) => Term,
    unifApplnOpt: (ExstFunc, Term) => Option[Term],
    addVar: Typ[Term] => InitState => (InitState, Term),
    getVar: Typ[Term] => Term
) {



  val unifApplnNode: ZipMapOpt[ExstFunc, Term, Term] =
    ZipMapOpt[ExstFunc, Term, Term](
      unifApplnOpt,
      Funcs,
      Terms,
      Terms
    )

  val applnNode: FiberProductMap[ExstFunc, Term, Typ[Term], Term] =
    FiberProductMap[ExstFunc, Term, Typ[Term], Term](
      _.dom,
      termsWithTyp,
      appln,
      Funcs,
      Terms
    )

  val applnByArgNode: FiberProductMap[Term, ExstFunc, Typ[Term], Term] =
    FiberProductMap[Term, ExstFunc, Typ[Term], Term](
      _.typ,
      funcsWithDomain,
      { case (x, f) => appln(f, x) },
      Terms,
      Terms
    )

  def lambdaIsle(typ: Typ[Term]): Island[Term, InitState, Term, Term] =
    Island[Term, InitState, Term, Term](
      Terms,
      _ => Terms,
      addVar(typ),
      { case (x, y) => x :~> y }
    )

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

  def lambdaIsleForFuncWithDomain(dom: Typ[Term]): Island[ExstFunc, InitState, Term, Term] =
    Island[ExstFunc, InitState, Term, Term](
      funcsWithDomain(dom),
      _ => Terms,
      addVar(dom),
      { case (x, y) => ExstFunc(x :~> y) }
    )

  val lambdaNode: FlatMap[Typ[Term], Term] =
    FlatMap(
      Typs,
      lambdaIsle,
      Terms
    )

  val lambdaByTypNodeFamily: GeneratorNodeFamily[Typ[Term] :: HNil, Term] =
    GeneratorNodeFamily.RecPiOpt[InitState, Term, Typ[Term] :: HNil, Term]({
      case typ :: HNil => lambdaIsleForTyp(typ)
    }, TermsWithTyp)

  def piIslelambdaIsle(
      typ: Typ[Term]): Island[Typ[Term], InitState, Typ[Term], Term] =
    Island[Typ[Term], InitState, Typ[Term], Term](
      Typs,
      _ => Typs,
      addVar(typ),
      { case (x, y) => pi(x)(y) }
    )

  def recFuncs(ind: ExstInducStruc): ZipMapOpt[Typ[Term], Typ[Term], ExstFunc] =
    ZipMapOpt[Typ[Term], Typ[Term], ExstFunc]({
      case (x, y) => ind.recOpt(x, y).flatMap(ExstFunc.opt)
    }, Typs, Typs, Funcs)

  def inducFuncs(ind: ExstInducStruc): ZipMapOpt[Typ[Term], Term, ExstFunc] =
    ZipMapOpt[Typ[Term], Term, ExstFunc]({
      case (x, y) => ind.inducOpt(x, y).flatMap(ExstFunc.opt)
    }, Typs, Terms, Funcs)


  def selfHeadNode(
      inductiveTyp: Typ[Term]): Island[Typ[Term], InitState, Typ[Term], Term] =
    Island[Typ[Term], InitState, Typ[Term], Term](
      IntroRuleTypes(inductiveTyp), // output
      (t: Term) => IntroRuleTypes(inductiveTyp), // output from island
      addVar(inductiveTyp),
      { case (x, y) => pi(x)(y) }
    )

  def otherHeadIsle(inductiveTyp: Typ[Term])(
      typ: Typ[Term]): Island[Typ[Term], InitState, Typ[Term], Term] =
    Island[Typ[Term], InitState, Typ[Term], Term](
      IntroRuleTypes(inductiveTyp),
      (t: Term) => IntroRuleTypes(inductiveTyp),
      addVar(typ),
      { case (x, y) => pi(x)(y) }
    )

  def otherHeadNode(inductiveTyp: Typ[Term]): FlatMap[Typ[Term], Typ[Term]] =
    FlatMap(
      Typs,
      otherHeadIsle(inductiveTyp),
      IntroRuleTypes(inductiveTyp)
    )


  def simpleInductiveStructure(
      inductiveTyp: Typ[Term]): Map[Vector[Typ[Term]], ExstInducStruc] =
    Map[Vector[Typ[Term]], ExstInducStruc](
      introTyps =>
        {
          val intros = introTyps.map(getVar)
          ExstInducStruc.get(inductiveTyp, intros)
        },
      RandomVector(IntroRuleTypes(inductiveTyp)),
      InducStrucs
    )

  def simpleInductiveDefn(
                                inductiveTyp: Typ[Term]): Map[Vector[Typ[Term]], ExstInducDefn] =
    Map[Vector[Typ[Term]], ExstInducDefn](
      introTyps =>
        {
          val intros = introTyps.map(getVar)
          ExstInducDefn(
          inductiveTyp,
          intros,
          ExstInducStruc.get(inductiveTyp, intros),
          Vector())},
      RandomVector(IntroRuleTypes(inductiveTyp)),
      InducDefns
    )

  def partiallyApply(f: Term): Option[Map[Term, Term]] =
    ExstFunc.opt(f).map {
      fn =>
        Map(
          (x: Term) => appln(fn, x),
          termsWithTyp(fn.dom),
          PartiallyApplied(f)
        )
    }

  def iteratedApply(f: Term) =
    FlatMapOpt(
      PartiallyApplied(f),
      g => partiallyApply(g),
      PartiallyApplied(f)
    )

  def iterFuncIsle(targetTyp: Typ[Term])(
    typ: Typ[Term]): Island[Typ[Term], InitState, Typ[Term], Term] =
    Island[Typ[Term], InitState, Typ[Term], Term](
      IterFuncTypTo(targetTyp),
      (_: Term) => IntroRuleTypes(targetTyp),
      addVar(typ),
      { case (x, y) => pi(x)(y) }
    )

  def iterFuncNode(targetTyp: Typ[Term]): FlatMap[Typ[Term], Typ[Term]] =
    FlatMap(
      Typs,
      iterFuncIsle(targetTyp),
      IterFuncTypTo(targetTyp)
    )

  def iterHeadNode(inductiveTyp: Typ[Term]): FlatMap[Typ[Term], Typ[Term]] =
    FlatMap(
      IterFuncTypTo(inductiveTyp),
      otherHeadIsle(inductiveTyp),
      IntroRuleTypes(inductiveTyp)
    )

  def typFromFamily(typF: Term): MapOpt[Term, Typ[Term]] =
    MapOpt[Term, Typ[Term]](
      typOpt,
      PartiallyApplied(typF),
      TypsFromFamily(typF)
    )

  def indexedInducHeadNode(typF: Typ[Term]): Map[Typ[Term], Typ[Term]] =
    Map[Typ[Term], Typ[Term]](
      identity,
      TypsFromFamily(typF),
      IndexedIntroRuleTyps(typF)
    )

  def indexedOtherHeadIsle(typF: Term)(
    typ: Typ[Term]): Island[Typ[Term], InitState, Typ[Term], Term] =
    Island[Typ[Term], InitState, Typ[Term], Term](
      IndexedIntroRuleTyps(typF),
      (t: Term) => IndexedIntroRuleTyps(typF),
      addVar(typ),
      { case (x, y) => pi(x)(y) }
    )

  def indexedSelfHeadNode(typF: Term) =
    FlatMap(
      TypsFromFamily(typF),
      indexedOtherHeadIsle(typF),
      IndexedIntroRuleTyps(typF)
    )

  def indexedOtherHeadNode(typF: Term) =
    FlatMap(
      Typs,
      indexedOtherHeadIsle(typF),
      IndexedIntroRuleTyps(typF)
    )

  def indexedIterFuncIsle(targetTyp: Term)(
    typ: Typ[Term]): Island[Typ[Term], InitState, Typ[Term], Term] =
    Island[Typ[Term], InitState, Typ[Term], Term](
      IndexedIterFuncTypTo(targetTyp),
      (_: Term) => IndexedIntroRuleTyps(targetTyp),
      addVar(typ),
      { case (x, y) => pi(x)(y) }
    )

  def indexedIterFuncNode(targetTyp: Term): FlatMap[Typ[Term], Typ[Term]] =
    FlatMap(
      Typs,
      indexedIterFuncIsle(targetTyp),
      IndexedIterFuncTypTo(targetTyp)
    )

  def indexedIterHeadNode(inductiveTyp: Term) =
    FlatMap(
      IndexedIterFuncTypTo(inductiveTyp),
      indexedOtherHeadIsle(inductiveTyp),
      IndexedIntroRuleTyps(inductiveTyp)
    )


}

object TermRandomVars {
  case object Terms extends RandomVar[Term]

  case object Typs extends RandomVar[Typ[Term]]

  case object Funcs extends RandomVar[ExstFunc]

  case object TermsWithTyp
      extends RandomVar.SimpleFamily[Typ[Term], Term](
        Typs,
        (typ: Typ[Term]) => Sort.Filter[Term](_.typ == typ)
      )

  def termsWithTyp(typ: Typ[Term]): RandomVar[Term] =
    RandomVar.AtCoord(TermsWithTyp, typ :: HNil)

  case object TypFamilies extends RandomVar[Term]

  case object FuncsWithDomain
      extends RandomVar.SimpleFamily[Typ[Term], ExstFunc](
        Typs,
        (typ: Typ[Term]) => Sort.Filter[ExstFunc](_.dom == typ)
      )

  def funcsWithDomain(typ: Typ[Term]): RandomVar[ExstFunc] =
    RandomVar.AtCoord(FuncsWithDomain, typ :: HNil)

  case object InducStrucs extends RandomVar[ExstInducStruc]

  case object InducDefns extends RandomVar[ExstInducDefn]


  case object Star extends RandomVar[Unit]

  def just[X](value: X, rv: RandomVar[X]): GeneratorNode.Map[Unit, X] =
    GeneratorNode.Map[Unit, X]((_: Unit) => value, Star, rv)

  case class IntroRuleTypes(inductiveTyp: Typ[Term])
      extends RandomVar[Typ[Term]]

  def inducHeadNode(inductiveTyp: Typ[Term]): Map[Unit, Term] =
    just(inductiveTyp, IntroRuleTypes(inductiveTyp))



  case class PartiallyApplied(func: Term) extends RandomVar[Term]

  def partiallyApplySelf(f: Term): Map[Unit, Term] =
    just(f, PartiallyApplied(f))


  case class IterFuncTypTo(typ: Typ[Term]) extends RandomVar[Typ[Term]]

  val typFamilyTypes: RandomVar[Term] = IterFuncTypTo(Type)

  case class TypsFromFamily(typF: Term) extends RandomVar[Typ[Term]]

  case class IndexedIntroRuleTyps(typF: Term) extends RandomVar[Typ[Term]]

  case class IndexedIterFuncTypTo(typF: Term) extends RandomVar[Typ[Term]]



}

case class RandomVector[X](base: RandomVar[X]) extends RandomVar[Vector[X]]{
  def empty: Map[Unit, Vector[X]] = just[Vector[X]](Vector(), this)

  def cons: ZipMap[X, Vector[X], Vector[X]] = ZipMap[X, Vector[X], Vector[X]](
    { case (x, ys) => x +: ys },
    base,
    this,
    this)
}

case class TermState(terms: FD[Term], typs: FD[Typ[Term]], vars: Vector[Term], inds: Vector[ExstInducStruc]) {
  val thmsByPf : FD[Typ[Term]] = terms.map(_.typ)
  val thmsBySt : FD[Typ[Term]] = typs.filter(thmsByPf(_) > 0)
  val pfSet: Vector[Term] = terms.flatten.supp.filter(t => thmsBySt(t.typ) > 0)
  val fullPfSet: Vector[(Term, Term)] =
    pfSet.flatMap(pf => partialLambdaClosures(vars)(pf).map((pf, _)))
}

object TermState {
  import TermRandomVars._

  implicit val stateFD: StateDistribution[TermState, FD] =
    new StateDistribution[TermState, FD] {
      def value[T](state: TermState)(randomVar: RandomVar[T]): FD[T] =
        randomVar match {
          case Terms => state.terms.map(x => x: T)
          case Typs  => state.typs.map(x => x: T)
          case Funcs => state.terms.condMap(ExstFunc.opt).map(x => x: T)
          case TypFamilies =>
            state.terms.conditioned(isTypFamily).map(x => x: T)
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
