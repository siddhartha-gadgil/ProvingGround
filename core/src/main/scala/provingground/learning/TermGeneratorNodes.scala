package provingground.learning
import provingground.HoTT._
import provingground._
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
