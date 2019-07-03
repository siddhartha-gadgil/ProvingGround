package provingground.learning
import provingground._, HoTT._

import GeneratorVariables._, Expression._, TermRandomVars._, GeneratorNode._,
TermGeneratorNodes._

object DerivedEquations {
  def finalProb[X](a: X, rv: RandomVar[X]): Expression = FinalVal(Elem(a, rv))

  def conditionedProb[O, Y](
      a: O,
      input: RandomVar[O],
      output: RandomVar[Y],
      condition: Sort[O, Y]
  ) =
    Coeff(conditionedVar(input, output, condition), output) * finalProb(
      a,
      input
    )

  def asTarget(typ: Typ[Term]) =
    EquationNode(
      finalProb(typ, TargetTyps),
      Coeff(TargetTyps.fromTyp, TargetTyps) * finalProb(typ, Typs)
    )

  def targets(typs: Set[Typ[Term]]) = typs.map(asTarget(_))
}
