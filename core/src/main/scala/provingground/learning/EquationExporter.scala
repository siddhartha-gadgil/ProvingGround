package provingground.learning

import provingground._, HoTT._
import TermRandomVars._, Expression._, GeneratorVariables._

class EquationExporter(val equations: Set[EquationNode], val initTerms: Set[Term]) {

  /**
    * identifying an isle variable by having an initial value, but one that is not part of the initial distribution
    *
    * @param el the element to decide
    * @return whether the element is an isle-var
    */
  def isleVar(el: Elem[_]): Boolean = el match {
      case Elem(x: Term, Terms) => valueVars.contains(InitialVal(el))  && !initTerms.contains(x)
      case _ => false
  }
    

  /**
    * Vector of all variables. This is frozen so that their indices can be used.
    */
  lazy val valueVars: Vector[Expression] =
    Expression
      .allVarVals(equations.map(_.rhs), equations.map(_.lhs))
      .map(t => t: Expression)
      .toVector

  /**
    * Terms in the final (i.e. evolved) distribution
    * * May have extra terms that evaluate to zero
    */
  lazy val finalTermSet: Set[Term] = equations
    .map(_.lhs)
    .collect {
      case FinalVal(el @ Elem(t: Term, Terms)) if !isleVar(el) => t
    }
    .toSet

  /**
    * Typs in the final (i.e. evolved) distribution
    * May have extra types that evaluate to zero
    */
  lazy val finalTypSet: Set[Typ[Term]] = equations
    .map(_.lhs)
    .collect {
      case FinalVal(el @ Elem(t: Typ[Term], Typs)) if !isleVar(el) => t
    }
    .toSet

  /**
    * equations not depending on a variable, to be used with boats
    */
  def indepEquations(variable: Term) =
    equations.filterNot(eq => TermRandomVars.equationNodeDepends(variable)(eq))

  def lambdaExportEquations(
      variable: Term
  ): Set[EquationNode] = {
    // val initState = TermState(generators(init), finalTyps)
    import GeneratorNode._, TermGeneratorNodes._
    val isle =
      Island[Term, TermState, Term, Term](
        Terms,
        ConstRandVar(Terms),
        AddVar(variable.typ),
        LamApply,
        EnterIsle
      )
    val boat  = variable
    val coeff = Coeff(Base.lambdaNode)
    val isleEqs: Set[EquationNode] =
      equations.map(_.mapVars {
        InIsle.variableMap(boat, isle)
      })
    val bridgeEqs: Set[EquationNode] = finalTermSet.map { x =>
      EquationNode(
        FinalVal(Elem(isle.export(boat, x), Terms)),
        coeff * FinalVal(
          InIsle(Elem(x, Terms), boat, isle)
        )
      )
    }
    lazy val initVarElems =
      Expression
        .allVarVals(equations.map(_.rhs), Set())
        .collect {
          case InitialVal(Elem(el, rv)) => Elem(el, rv)
        }
    val isleIn: Set[EquationNode] =
      initVarElems.map { el =>
        val rhs =
          if (boat == el.element)
            (IsleScale(boat) * -1) + Literal(1)
          else IsleScale(boat) * InitialVal(el)
        EquationNode(
          InitialVal(InIsle(el, boat, isle)),
          rhs
        )
      }
    isleEqs union (isleIn union bridgeEqs)
  }

  def piExportEquations(
      variable: Term
  ): Set[EquationNode] = {
    // val initState = TermState(generators(init), finalTyps)
    import GeneratorNode._, TermGeneratorNodes._
    val isle =
      Island[Typ[Term], TermState, Typ[Term], Term](
        Typs,
        ConstRandVar(Typs),
        AddVar(variable.typ),
        PiApply,
        EnterIsle
      )
    val boat  = variable
    val coeff = Coeff(Base.piNode)
    val isleEqs: Set[EquationNode] =
      equations.map(_.mapVars {
        InIsle.variableMap(boat, isle)
      })
    val bridgeEqs: Set[EquationNode] = finalTypSet.map { x =>
      EquationNode(
        FinalVal(Elem(isle.export(boat, x), Typs)),
        coeff * FinalVal(
          InIsle(Elem(x, Typs), boat, isle)
        )
      )
    }
    val initVarElems =
      Expression
        .allVarVals(equations.map(_.rhs), Set())
        .collect {
          case InitialVal(Elem(el, rv)) => Elem(el, rv)
        }
    val isleIn: Set[EquationNode] =
      initVarElems.map { el =>
        val rhs =
          if (boat == el.element)
            (IsleScale(boat) * -1) + Literal(1)
          else IsleScale(boat) * InitialVal(el)
        EquationNode(
          InitialVal(InIsle(el, boat, isle)),
          rhs
        )
      }
    isleEqs union (isleIn union bridgeEqs)
  }

  def piTermExportEquations(
      variable: Term
  ): Set[EquationNode] = {
    import GeneratorNode._, TermGeneratorNodes._
    val isle =
      Island[Typ[Term], TermState, Typ[Term], Term](
        Typs,
        ConstRandVar(Typs),
        AddVar(variable.typ),
        PiApply,
        EnterIsle
      )
    val boat  = variable
    val coeff = Coeff(Base.piNode.|(typAsTermSort, Terms))
    val isleEqs: Set[EquationNode] =
      equations.map(_.mapVars {
        InIsle.variableMap(boat, isle)
      })
    val bridgeEqs: Set[EquationNode] = finalTypSet.map { x =>
      EquationNode(
        FinalVal(Elem(isle.export(boat, x), Terms)),
        coeff * FinalVal(
          InIsle(Elem(x, Typs), boat, isle)
        )
      )
    }
    val initVarElems =
      Expression
        .allVarVals(equations.map(_.rhs), Set())
        .collect {
          case InitialVal(Elem(el, rv)) => Elem(el, rv)
        }
    val isleIn: Set[EquationNode] =
      initVarElems.map { el =>
        val rhs =
          if (boat == el.element)
            (IsleScale(boat) * -1) + Literal(1)
          else IsleScale(boat) * InitialVal(el)
        EquationNode(
          InitialVal(InIsle(el, boat, isle)),
          rhs
        )
      }
    isleEqs union (isleIn union bridgeEqs)
  }

  def relVariable(x: Term) = {
    val newInit = initTerms.filterNot(_.dependsOn(x))
    val eqs = piExportEquations(x) union lambdaExportEquations(
      x
    ) union indepEquations(x) union piTermExportEquations(x)
    new EquationExporter(eqs, newInit)
  }

  def export(vars: Vector[Term]): EquationExporter =
    vars match {
      case Vector() => this
      case xs :+ y  => relVariable(y).export(xs)
    }

}

object EquationExporter{
    def export(equations: Set[EquationNode], initTerms: Set[Term], vars: Vector[Term]) =
        (new EquationExporter(equations, initTerms).export(vars).equations)
}