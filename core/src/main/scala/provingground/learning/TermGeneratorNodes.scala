package provingground.learning
import provingground._

import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD}

import learning.{TangVec => T}

import cats._
import cats.implicits._

import HoTT._

import shapeless._, HList._
import scala.language.higherKinds


class TermGeneratorNodes[InitState, D[_]](
    appln: (ExstFunc, Term) => Term,
    unifApplnOpt: (ExstFunc, Term) => Option[Term],
    addVar: Typ[Term] => (InitState => (InitState, Term))
)(implicit ctxExp: ContextExport[Term, D]) {
  import TermRandomVars._, GeneratorNode._

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

  def lambdaIsle(typ: Typ[Term]): Island[Term, Term, InitState, Term, D] =
    Island[Term, Term, InitState, Term, D](
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
      typ: Typ[Term]): Island[Typ[Term], Typ[Term], InitState, Term, D] =
    Island[Typ[Term], Typ[Term], InitState, Term, D](
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
        Sort.All[Typ[Term]](),
        (typ: Typ[Term]) => Sort.Filter[Term](_.typ == typ)
      )

  def termsWithTyp(typ: Typ[Term]) =
    RandomVar.AtCoord(TermsWithTyp, typ :: HNil)

  case object TypFamilies extends RandomVar[Term]

  case object FuncsWithDomain
      extends RandomVar.SimpleFamily[Typ[Term], ExstFunc](
        Sort.All[Typ[Term]](),
        (typ: Typ[Term]) => Sort.Filter[ExstFunc](_.dom == typ)
      )

}
