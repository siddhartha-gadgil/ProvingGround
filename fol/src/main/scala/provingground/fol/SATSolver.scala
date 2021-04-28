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
        val nextState            = state.copy(cnf = CNF(resultClauses))
        val (recState, recInfer) = propagateUnits(nextState)
        (recState, inferTrees union recInfer)
      }
      .getOrElse(state, Set())

  def solve(state: SATState): Either[ResolutionTree, SATModel] =
    if (state.cnf.clauses.isEmpty)
      Right(SATModel(state.assumed union state.inferred))
    else if (state.cnf.hasContradiction)
      Left(ResolutionTree.Leaf(Clause.contradiction))
    else {
      pprint.log(state)
      state.freeVars match {
        case head :: next =>
          val lit =
            if (rnd.nextDouble() < 0.5) PosLit(head)
            else
              NegLit(head)
          pprint.log(lit)
          val base = SATState(
            state.cnf.inferFrom(lit),
            state.assumed + lit,
            state.inferred,
            next
          ).purified
          pprint.log(base)
          val (refinedState: SATState, inferTrees: Set[ResolutionTree]) =
            propagateUnits(base)
          pprint.log(refinedState)
          pprint.log(inferTrees)
          solve(refinedState) match {
            case Left(value: ResolutionTree) =>
              val expandedTree = ResolutionTree.expand(value, inferTrees)
              val liftMap      = state.cnf.inferMap(lit)
              val liftedTree   = expandedTree.lift(liftMap)
              pprint.log(value)
              pprint.log(expandedTree)
              pprint.log(liftMap)
              pprint.log(liftedTree)
              pprint.log(liftedTree.result)
              if (liftedTree.result == Clause.contradiction) Left(liftedTree)
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
                pprint.log(baseNeg)
                pprint.log(refinedState)
                pprint.log(inferTreesNeg)
                solve(refinedStateNeg) match {
                  case Left(valueNeg) =>
                    val expandedTreeNeg =
                      ResolutionTree.expand(valueNeg, inferTreesNeg)
                    val liftMapNeg    = state.cnf.inferMap(lit.negate)
                    val liftedTreeNeg = expandedTreeNeg.lift(liftMapNeg)
                    pprint.log(valueNeg)
                    pprint.log(expandedTreeNeg)
                    pprint.log(liftMapNeg)
                    pprint.log(liftedTreeNeg)
                    pprint.log(liftedTreeNeg.result)
                    if (liftedTreeNeg.result == Clause.contradiction)
                      Left(liftedTreeNeg)
                    else {
                      Left(ResolutionTree.Node(liftedTreeNeg, liftedTree, lit))
                    }
                  case Right(value) => Right(value)
                }
              }
            case Right(value: SATModel) => Right(value)
          }
        case immutable.Nil =>
          throw new Exception(
            s"no free variables but have clauses ${state.cnf}"
          )
      }
    }
}
