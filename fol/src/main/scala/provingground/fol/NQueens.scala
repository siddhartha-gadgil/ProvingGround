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

  lazy val solution: SATSolution = SATSolver.solve(satState)

  lazy val modelOpt: Option[SATModel] = solution.getModel

  val style = s"""|
                |<style>
                |    table {
                |        border-collapse: collapse;
                |      }
                |      
                |      td, th {
                |        border: 1px solid #999;
                |      }
                |</style>
                |""".stripMargin

  def modelTable(header: String = style): Option[String] = modelOpt.map { model =>
    val entries = Vector.tabulate(n, n) {
      case (i, j) =>
        if (model.modelMap(QueenAt(i, j))) "<td>&#9813;</td>"
        else "<td>&nbsp;</td>"
    }
    header + entries
      .map(_.mkString("<tr>", "", "</tr>"))
      .mkString("<table>\n", "\n", "\n</table>\n")
  }

  val modelText = modelOpt.map { model =>
    val entries = Vector.tabulate(n, n) {
      case (i, j) =>
        if (model.modelMap(QueenAt(i, j))) " \u2655 "
        else "   "
    }
    val dashes = "-" * (4 * n + 1)
    entries
      .map(_.mkString("|","|","|"))
      .mkString("\n"+dashes+"\n","\n"+dashes+"\n", "\n"+dashes+"\n")
  }


  def html(header: String = style): String = 
    modelTable(header).getOrElse(
      "<ul>"+ResolutionTree.toHMTL(solution.getProof.get)+"</ul>"
    )

  lazy val proofOpt: Option[ResolutionTree] = solution.getProof

  lazy val positions =
    modelOpt.map { model =>
      model.positives.toVector
        .collect {
          case QueenAt(row, col) => row -> col
        }
        .sortBy(_._1)
    }
}
