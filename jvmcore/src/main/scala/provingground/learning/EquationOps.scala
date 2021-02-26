package provingground.learning

import provingground._
import shapeless._
import HList._
import scala.collection.immutable.Nil
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable._
import scala.concurrent._
import GeneratorVariables._, Expression._

object EquationOps {
  def group(ts: Set[EquationNode]): Set[Equation] = groupIt(ts).toSet

  def groupDirect(ts: Set[EquationNode]): Set[Equation] =
    ts.groupMapReduce(_.lhs)(_.rhs)(_ + _)
      .map { case (lhs, rhs) => Equation(lhs, rhs) }
      .toSet

  def groupIt(ts: Set[EquationNode]): Iterable[Equation] = {
    val ps = ts.par
    import scala.collection.parallel._
    // ps.tasksupport = new ForkJoinTaskSupport(
    //   new java.util.concurrent.ForkJoinPool(Utils.threadNum)
    // )
    ps.groupBy(_.lhs)
      .mapValues(s => s.map(_.rhs))
      .map {
        case (lhs, rhsV) => Equation(lhs, Sum(rhsV.toVector))
      }
  }.seq

  def groupMap(
      ts: Set[EquationNode],
      previous: ParMap[Expression, EquationNode]
  ) = {
    val ps = ts.par
    import scala.collection.parallel._
    // ps.tasksupport = new ForkJoinTaskSupport(
    //   new java.util.concurrent.ForkJoinPool(Utils.threadNum)
    // )
    val liftMap = previous.map(eqq => eqq._1 -> Equation(eqq._1, eqq._2.rhs))
    val buildMap = {
      ps.groupBy(_.lhs)
        .mapValues(s => s.map(_.rhs))
        .map {
          case (lhs, rhsV) =>
            val w = previous.get(lhs).map(n => rhsV + n.rhs).getOrElse(rhsV)
            lhs -> Equation(lhs, Sum(w.toVector))
        }
    }.toMap
    liftMap ++ buildMap
  }

  def groupFuture(
      ts: Set[EquationNode]
  )(implicit ec: ExecutionContext): Future[Set[Equation]] =
    Future
      .sequence(
        ts.groupBy(_.lhs.hashCode())
          .values
          .map(s => Future(groupIt(s).toVector))
          .toVector
      )
      .map(provingground.Utils.gatherSet(_, Set()))

  def split(eq: Equation): Set[EquationNode] = eq match {
    case Equation(lhs, Sum(ys)) =>
      ys.flatMap(y => split(Equation(lhs, y))).toSet
    // split(Equation(lhs, y1)) union (split(Equation(lhs, y2)))
    case Equation(lhs, rhs) => Set(EquationNode(lhs, rhs))
  }

  def rebuild(eqs: Set[Equation]) =
    group(eqs.flatMap(split(_)))

  def merge(eqs: Set[Equation]*) =
    rebuild(eqs.reduce(_ union _))
}
