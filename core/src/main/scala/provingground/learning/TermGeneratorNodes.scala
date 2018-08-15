package provingground.learning
import provingground.HoTT._
import provingground.{FiniteDistribution => FD, _}
import shapeless._

import scala.language.higherKinds

class TermGeneratorNodes[InitState](
    appln: (ExstFunc, Term) => Term,
    unifApplnOpt: (ExstFunc, Term) => Option[Term],
    addVar: Typ[Term] => InitState => (InitState, Term)
) {
  import GeneratorNode._
  import TermRandomVars._

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

  def lambdaIsle(typ: Typ[Term]): Island[Term, InitState, Term, Term] =
    Island[Term, InitState, Term, Term](
      Terms,
      Terms,
      addVar(typ),
      { case (x, y) => x :~> y }
    )

  val lambdaNode: FlatMap[Typ[Term], Term] =
    FlatMap(
      Typs,
      lambdaIsle,
      Terms
    )

  def piIslelambdaIsle(
      typ: Typ[Term]): Island[Typ[Term], InitState, Typ[Term], Term] =
    Island[Typ[Term], InitState, Typ[Term], Term](
      Typs,
      Typs,
      addVar(typ),
      { case (x, y) => pi(x)(y) }
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

}

case class TermState(terms: FD[Term], typs: FD[Typ[Term]], vars: Vector[Term]){
  val thmsByPf = terms.map(_.typ)
  val thmsBySt = typs.filter(thmsByPf(_) > 0)
  val pfSet    = terms.flatten.supp.filter((t) => thmsBySt(t.typ) > 0)
  val fullPfSet = pfSet.flatMap((pf) =>
    partialLambdaClosures(vars)(pf).map((pf, _)))
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
