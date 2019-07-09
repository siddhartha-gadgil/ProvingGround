package provingground.learning
import provingground._, HoTT._

import GeneratorVariables._, Expression._, TermRandomVars._, GeneratorNode._,
TermGeneratorNodes._

class DerivedEquations(
    tg: TermGeneratorNodes[TermState] = TermGeneratorNodes.Base
) {
  def finalProb[X](a: X, rv: RandomVar[X]): Expression = FinalVal(Elem(a, rv))

  def conditionedProb[O, Y](
      a: O,
      input: RandomVar[O],
      output: RandomVar[Y],
      condition: Sort[O, Y]
  ) =
    Coeff(conditionedVar(input, output, condition)) * finalProb(
      a,
      input
    )

  def asTarget(typ: Typ[Term]) =
    EquationNode(
      finalProb(typ, TargetTyps),
      Coeff(TargetTyps.fromTyp) * finalProb(typ, Typs)
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

  import tg._

  def applnFlip(eqn: EquationNode): Option[EquationNode] =
    (coeffFactor(eqn.rhs), varFactors(eqn.rhs)) match {
      case (
          Some(`applnNode`),
          Vector(Elem(Funcs, f: ExstFunc), Elem(_, a: Term))
          ) =>
        Some(
          EquationNode(
            eqn.lhs,
            Coeff(applnByArgNode) * finalProb(a, Terms) * finalProb(
              f,
              funcsWithDomain(a.typ)
            )
          )
        )
      case (
          Some(`applnByArgNode`),
          Vector(Elem(Terms, a: Term), Elem(_, f: ExstFunc))
          ) =>
        Some(
          EquationNode(
            eqn.lhs,
            Coeff(applnNode) * finalProb(f, Funcs) * finalProb(
              a,
              termsWithTyp(f.dom)
            )
          )
        )
      case _ => None
    }

  def funcFoldEqs(
      fn: Term,
      depth: Int,
      args: Vector[Term],
      result: Term
  ): Set[EquationNode] =
    if (depth < 1) Set()
    else {
      val coeff   = Coeff(tg.foldFuncNode(fn, depth))
      val x       = args.head
      val y       = fold(fn)(x)
      val tailVar = if (depth == 1) AtomVar(y) else FuncFoldVar(y, depth - 1)
      val eq = EquationNode(
        FinalVal(Elem(result, FuncFoldVar(fn, depth))),
        coeff * FinalVal(Elem(x, TermGeneratorNodes.termsWithTyp(x.typ))) * FinalVal(
          Elem(result, tailVar)
        )
      )
      funcFoldEqs(y, depth - 1, args.tail, result) + eq
    }

  def formalEquations(t: Term) : Set[EquationNode] = t match {
    case MiscAppln(fn: FuncLike[u, v], a) =>
      val f   = ExstFunc(fn)
      val lhs = finalProb(t, Terms)
      Set(
        EquationNode(
          lhs,
          Coeff(applnNode) * finalProb(f, Funcs) * finalProb(
            a,
            termsWithTyp(f.dom)
          )
        ),
        EquationNode(
          lhs,
          Coeff(applnByArgNode) * finalProb(a, Terms) * finalProb(
            f,
            funcsWithDomain(a.typ)
          )
        )
      )
    case lt: LambdaLike[u, v] =>
      val coeff   = Coeff(tg.lambdaIsle(lt.dom))
      val x = lt.variable
      val isle    = tg.lambdaIsle(lt.dom)
      Set(
        EquationNode(
              FinalVal(Elem(lt, Terms)),
              coeff * FinalVal(
                InIsle(Elem(lt.value, isle.islandOutput(x)), x, isle)
              )
            )
      )
    case pd: PiDefn[u, v] =>
    val coeff   = Coeff(tg.piIsle(pd.domain))
    val x = pd.variable
    val isle    = tg.piIsle(pd.domain)
    Set(
      EquationNode(
            FinalVal(Elem(pd, Typs)),
            coeff * FinalVal(
              InIsle(Elem(pd.value, isle.islandOutput(x)), x, isle)
            )
          )
    )
    case _ => Set()
  }
}
