package provingground.fol
import scala.util._
import scala.collection.immutable

object SATSolver {
  val rnd = new scala.util.Random()

  def propagateUnits(state: SATState): (SATState, Set[ResolutionTree]) =
    state.cnf.findUnit
      .map { lit =>
        val purged        = state.cnf.clauses.filterNot(_.ls.contains(lit))
        val resultClauses = purged.map(_ - lit.negate)
        val newClauses    = resultClauses -- purged
        import ResolutionTree.{Node, Leaf}
        val inferTrees =
          newClauses.map(
            cl => Node(Leaf(Clause.atom(lit)), Leaf(cl + lit.negate), lit): ResolutionTree
          )
        val nextState = SATState(
          CNF(resultClauses),
          state.assumed,
          state.inferred + lit,
          state.freeVars.filterNot(_ == lit.p)
        )
        val (recState, recInfer) = propagateUnits(nextState)
        (recState, inferTrees union recInfer)
      }
      .getOrElse(state, Set())

  def solve(state: SATState): SATSolution =
    if (state.cnf.clauses.isEmpty)
      (SATModel(state.assumed union state.inferred))
    else if (state.cnf.hasContradiction)
      (ResolutionTree.Leaf(Clause.contradiction))
    else {
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
          ).purified
          val (refinedState: SATState, inferTrees: Set[ResolutionTree]) =
            propagateUnits(base)
          solve(refinedState) match {
            case (resolutionTree: ResolutionTree) =>
              val expandedTree = ResolutionTree.expand(resolutionTree, inferTrees)
              val liftMap      = state.cnf.inferMap(lit)
              val liftedTree   = expandedTree.lift(liftMap)
              if (liftedTree.result == Clause.contradiction) (liftedTree)
              else {
                val baseNeg = SATState(
                  state.cnf.inferFrom(lit.negate),
                  state.assumed + lit.negate,
                  state.inferred,
                  next
                ).purified
                val (
                  refinedStateNeg: SATState,
                  inferTreesNeg: Set[ResolutionTree]
                ) =
                  propagateUnits(baseNeg)
                solve(refinedStateNeg) match {
                  case (treeNeg: ResolutionTree) =>
                    val expandedTreeNeg =
                      ResolutionTree.expand(treeNeg, inferTreesNeg)
                    val liftMapNeg    = state.cnf.inferMap(lit.negate)
                    val liftedTreeNeg = expandedTreeNeg.lift(liftMapNeg)
                    if (liftedTreeNeg.result == Clause.contradiction)
                      (liftedTreeNeg)
                    else {
                      (ResolutionTree.Node(liftedTreeNeg, liftedTree, lit))
                    }
                  case (value) => (value)
                }
              }
            case (model: SATModel) => (model)
          }
        case immutable.Nil =>
          throw new Exception(
            s"no free variables but have clauses ${state.cnf}"
          )
      }
    }
}
