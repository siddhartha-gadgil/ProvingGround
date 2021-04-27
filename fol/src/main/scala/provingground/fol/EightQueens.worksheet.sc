import provingground.fol._

val q8 = NQueens(8)

val Some(positions) = q8.positions 

val slopes = 
    for {
        (row1, col1) <- positions
        (row2, col2) <- positions
        if row1 < row2 & col1 < col2
    } yield (row2 - row1) - (col2 - col1)

slopes.contains(0)

val antiSlopes = for {
        (row1, col1) <- positions
        (row2, col2) <- positions
        if row1 < row2 & col2 < col1
    } yield (row2 - row1) - (col1 - col2)

antiSlopes.contains(0)

positions.map(_._1).sortBy(identity(_))

positions.map(_._2).sortBy(identity(_))