package provingground.fol

import scala.collection.immutable

final case class SATState(
    cnf: CNF,
    assumed: Set[Literal],
    inferred: Set[Literal],
    freeVars: List[Formula]
) {
  def simplification: Option[SATState] =
    cnf.findUnit
      .map(
        lit =>
          SATState(
            cnf.inferFrom(lit),
            assumed,
            inferred + lit,
            freeVars.filterNot(_ == lit.p)
          )
      )
      .orElse(
        cnf.findPure.map(
          pure =>
            SATState(
              cnf.purgePure(pure),
              assumed,
              inferred + pure,
              freeVars.filterNot(_ == pure.p)
            )
        )
      )

  lazy val purified: SATState = SATState(
    CNF(cnf.withoutPure),
    assumed,
    inferred union cnf.pureLiterals,
    freeVars.filter(fmla => !cnf.pureLiterals.map(_.p).contains(fmla))
  )

  def model: Option[Set[Literal]] = SATState.getModel(this)

  def modelMap: Option[Map[Formula, Boolean]] =
    model.map(
      ls =>
        ls.map {
          case NegLit(p) => p -> false
          case PosLit(p) => p -> true
        }.toMap
    )
}

object SATState {
  def apply(cnf: CNF): SATState =
    SATState(cnf.clean, Set(), Set(), cnf.varList)

  def fromFormulas(formulas: Formula*): SATState =
    apply(CNF.fromFormulas(formulas.toSet).clean)

  def simplify(state: SATState): SATState =
    state.simplification.map(simplify(_)).getOrElse(state)

  val rnd = new scala.util.Random()

  def getModel(state: SATState): Option[Set[Literal]] =
    if (state.cnf.clauses.isEmpty) Some(state.assumed union state.inferred)
    else if (state.cnf.hasContradiction) None
    else
      state.freeVars match {
        case head :: next =>
          val lit =
            if (rnd.nextDouble() < 0.5) PosLit(head)
            else
              NegLit(head)
          val base = SATState(
            state.cnf.inferFrom(lit),
            state.assumed + lit,
            state.inferred,
            next
          )
          getModel(simplify(base)).orElse {
            val base = SATState(
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
