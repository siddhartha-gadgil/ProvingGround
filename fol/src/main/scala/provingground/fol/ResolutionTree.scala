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
    require(
      positive.result.ls.contains(mergeLiteral),
      s"illegal merge with left $positive, right $negative, literal $mergeLiteral"
    )
    require(
      negative.result.ls.contains(mergeLiteral.negate),
      s"illegal merge with left $positive, right $negative, literal $mergeLiteral "
    )
    val result
        : Clause = (positive.result - mergeLiteral) | (negative.result - (mergeLiteral.negate))

    def lift(m: Map[Clause, Clause]): ResolutionTree =
      Node(positive.lift(m), negative.lift(m), mergeLiteral)
  }

  def expandStep(
      tree: ResolutionTree,
      inferTrees: Set[ResolutionTree]
  ): ResolutionTree =
    (tree match {
      case Leaf(clause) =>
        inferTrees.find(_.result == clause).getOrElse(Leaf(clause))
      case Node(positive, negative, mergeLiteral) =>
        Node(
          expand(positive, inferTrees),
          expand(negative, inferTrees),
          mergeLiteral
        )
    }).ensuring(_.result == tree.result)

  def expand(
      tree: ResolutionTree,
      inferTrees: Set[ResolutionTree]
  ): ResolutionTree = {
    val nextTree = expandStep(tree, inferTrees)
    if (nextTree == tree) nextTree else expand(nextTree, inferTrees)
  }
}

case class SATModel(atoms: Set[Literal]) {
  val modelMap: Map[Formula, Boolean] = atoms.toVector.map {
    case NegLit(p) => p -> false
    case PosLit(p) => p -> true
  }.toMap
}
