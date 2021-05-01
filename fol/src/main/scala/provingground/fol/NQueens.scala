package provingground.fol
import Formula._

case class QueenAt(row: Int, col: Int) extends SimpleFormula {
  def subs(xt: Var => Term): Formula = this

  val freeVars: Set[Var] = Set()
}

case class NQueens(n: Int) {
  def atMostOne(l: List[Formula]) =
    for {
      i <- 0 until (l.size - 1)
      j <- i + 1 until (l.size)
    } yield (!l(i)) | (!l(j))

  def rowOccupied(row: Int): Formula =
    (0 until n).map(QueenAt(row, _): Formula).reduce(_ | _)

  val rowsOccupied = (0 until n).map(rowOccupied(_))

  val rows = (0 until n).flatMap(
    i => atMostOne((0 until (n)).toList.map(QueenAt(i, _)))
  )

  val cols = (0 until n).flatMap(
    j => atMostOne((0 until (n)).toList.map(QueenAt(_, j)))
  )

  val diags = for {
    row1 <- 0 until (n)
    row2 <- (row1 + 1) until (n)
    col1 <- 0 until (n)
    col2 <- 0 until (n)
    if (row2 - row1) == (col2 - col1) || (row2 - row1) == (col1 - col2)
  } yield (!QueenAt(row1, col1)) | (!QueenAt(row2, col2))

  val cnf: CNF =
    CNF.fromFormulas((rowsOccupied ++ rows ++ cols ++ diags).toSet)

  val satState: SATState = SATState(cnf)

  lazy val solutionMap = satState.modelMap

  lazy val positions =
    solutionMap.map(
      soln =>
        soln.toVector
          .filter(_._2)
          .map(_._1)
          .collect {
            case QueenAt(row, col) => row -> col
          }
          .sortBy(_._1)
    )
}
