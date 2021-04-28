package provingground.fol

sealed trait ResolutionTree {
  val result: Clause

  def lift(m: Map[Clause, Clause]): ResolutionTree

  val isContradiction: Boolean = result == Clause.contradiction
}

object ResolutionTree {
  case class Leaf(clause: Clause) extends ResolutionTree {
    val result: Clause = clause

    def lift(m: Map[Clause, Clause]): ResolutionTree =
      m.get(clause).map(Leaf(_)).getOrElse(this)

  }

  case class Node(
      positive: ResolutionTree,
      negative: ResolutionTree,
      mergeLiteral: Literal
  ) extends ResolutionTree {
    val result
        : Clause = (positive.result - mergeLiteral) | (negative.result - (mergeLiteral.negate))

    def lift(m: Map[Clause, Clause]): ResolutionTree =
      Node(positive.lift(m), negative.lift(m), mergeLiteral)
  }
}

case class SATModel(atoms: Set[Literal]) {
  val modelMap: Map[Formula, Boolean] = atoms.toVector.map {
    case NegLit(p) => p -> false
    case PosLit(p) => p -> true
  }.toMap
}
