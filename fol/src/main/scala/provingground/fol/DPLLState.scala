package provingground.fol

import scala.collection.immutable

final case class DPLLState(
    cnf: CNF,
    assumed: Set[Literal],
    inferred: Set[Literal],
    freeVars: List[Formula]
) {
  def simplification: Option[DPLLState] =
    cnf.findUnit
      .map(
        lit =>
          DPLLState(
            cnf.inferFrom(lit),
            assumed,
            inferred + lit,
            freeVars.filterNot(_ == lit.p)
          )
      )
      .orElse(
        cnf.findPure.map(
          pure =>
            DPLLState(
              cnf.purgePure(pure),
              assumed,
              inferred + pure,
              freeVars.filterNot(_ == pure.p)
            )
        )
      )

  def model: Option[Set[Literal]] = DPLLState.getModel(this)
}

object DPLLState {
  def apply(cnf: CNF): DPLLState =
    DPLLState(cnf.clean, Set(), Set(), cnf.varList)

  def fromFormulas(formulas: Formula*): DPLLState =
    apply(CNF.fromFormulas(formulas.toSet).clean)

  def simplify(state: DPLLState): DPLLState =
    state.simplification.map(simplify(_)).getOrElse(state)

  val rnd = new scala.util.Random()

  def getModel(state: DPLLState): Option[Set[Literal]] =
    if (state.cnf.clauses.isEmpty) Some(state.assumed union state.inferred)
    else if (state.cnf.hasContradiction) None
    else
      state.freeVars match {
        case head :: next =>
          val lit =
            if (rnd.nextDouble() < 0.5) PosLit(head)
            else
              NegLit(head)
          val base = DPLLState(
            state.cnf.inferFrom(lit),
            state.assumed + lit,
            state.inferred,
            next
          )
          getModel(simplify(base)).orElse {
            val base = DPLLState(
              state.cnf.inferFrom(lit.negate),
              state.assumed + lit.negate,
              state.inferred,
              next
            )
            getModel(simplify(base))
          }
        case immutable.Nil => None
      }
}
