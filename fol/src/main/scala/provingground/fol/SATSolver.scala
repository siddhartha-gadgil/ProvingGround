package provingground.fol
import scala.util._
import scala.collection.immutable

object SATSolver {
  val rnd = new scala.util.Random()

  def solve(state: SATState): Either[ResolutionTree, SATModel] =
    if (state.cnf.clauses.isEmpty)
      Right(SATModel(state.assumed union state.inferred))
    else if (state.cnf.hasContradiction)
      Left(ResolutionTree.Leaf(Clause.contradiction))
    else {
      state.freeVars match {
        case head :: next =>
          val lit =
            if (rnd.nextDouble() < 0.5) PosLit(head)
            else
              NegLit(head)
          ???
        case immutable.Nil =>
          ???
      }
    }
}
