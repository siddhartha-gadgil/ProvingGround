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

  def targets(typs: Set[Typ[Term]]): Set[EquationNode] = typs.map(asTarget(_))

  def conditionWithTyp(t: Term): EquationNode =
    EquationNode(
      finalProb(t, termsWithTyp(t.typ)),
      conditionedProb(
        t,
        Terms,
        termsWithTyp(t.typ),
        Sort.Filter[Term](WithTyp(t.typ))
      )
    )

  def conditionAsFunction(t: Term): Set[EquationNode] =
    ExstFunc
      .opt(t)
      .map { f =>
        Set(
          EquationNode(
            finalProb(f, Funcs),
            conditionedProb(t, Terms, Funcs, funcSort)
          ),
          EquationNode(
            finalProb(f, funcsWithDomain(f.dom)),
            conditionedProb(
              t,
              Terms,
              funcsWithDomain(f.dom),
              Sort.Restrict(FuncWithDom(f.dom))
            )
          )
        )
      }
      .getOrElse(Set.empty[EquationNode])

  import TermGeneratorNodes.Base._

  def applnFlip(eqn: EquationNode): Option[EquationNode] =
    (coeffFactor(eqn.rhs), varFactors(eqn.rhs)) match {
      case (
          Some(`applnNode`),
          Vector(Elem(Funcs, f: ExstFunc), Elem(_, a: Term))
          ) =>
        Some(
          EquationNode(
            eqn.lhs,
            Coeff(applnByArgNode, Terms) * finalProb(a, Terms) * finalProb(
              f,
              funcsWithDomain(a.typ)
            )
          )
        )
        case (
            Some(`applnByArgNode`),
            Vector(Elem(Terms, a : Term), Elem(_, f: ExstFunc))
            ) =>
          Some(
            EquationNode(
              eqn.lhs,
              Coeff(applnNode, Terms) * finalProb(f, Funcs) * finalProb(
                a,
                termsWithTyp(f.dom)
              )
            )
          )
      case _ => None
    }
}
