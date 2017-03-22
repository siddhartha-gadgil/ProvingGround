package provingground

/**
  * Combining various atomic solvers to give a solver, with one or many solutions.
  *
  * Should allow parallel and serial collections
  *
  * By convention, atoms return Option[Either[P,S]] with P being progress and S success
  * The raw input should also be in P to allow recursion
  */
object RecSolver {
  trait Atomic[P, S] {
    def solve(a: P): Option[Either[P, S]]
    def apply(a: P) = solve(a)
    def apply(a: Either[P, S]): Option[Either[P, S]] = a match {
      case Left(b)  => solve(b)
      case Right(c) => Some(Right(c))
    }
  }

  trait AtomSeq[P, S] {
    def atoms: Iterable[Atomic[P, S]]

    def apply = atoms
  }

  def solve[P, S](a: P)(implicit solvers: AtomSeq[P, S]): Option[S] = {
    solveSeq(for (s <- solvers.atoms; sol <- s(a)) yield sol)(solvers)
  }

  def solveSeq[P, S](as: Iterable[Either[P, S]])(
      implicit solvers: AtomSeq[P, S]): Option[S] = {
    if (as.isEmpty) None
    else
      as.head match {
        case Right(s) => Some(s)
        case Left(p)  => solve(p)(solvers) orElse (solveSeq(as.tail)(solvers))
      }
  }

  def trySolve[P, S](s: Either[P, S])(implicit solvers: AtomSeq[P, S]) =
    s match {
      case Left(a)  => solve(a)(solvers) map (Right(_)) getOrElse (Left(a))
      case Right(a) => a
    }

  def bestNewSolutions[P, S](s: Either[P, S])(
      implicit solvers: AtomSeq[P, S]): Set[Either[P, S]] = {
    val nxt = for (sl <- solvers.atoms; p <- sl(s)) yield p
    bestSolutions(nxt.toSet)(solvers)
  }

  def bestSolutions[P, S](start: Set[Either[P, S]])(
      implicit solvers: AtomSeq[P, S]): Set[Either[P, S]] = {
    start flatMap { s =>
      if (bestNewSolutions(s)(solvers).isEmpty) Set(s)
      else bestNewSolutions(s)(solvers)
    }
  }

  def exclude[S](u: Option[S])(excl: Set[S]) = {
    for (s <- u if !(excl contains s)) yield s
  }
}
