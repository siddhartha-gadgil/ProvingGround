package provingground.learning
import provingground._, HoTT._

import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD}

import monix.eval._
import math.{log, exp}

import scala.concurrent._, duration._

object ProverTasks {
  def typdistTask(fd: FD[Term], tv: TermEvolver, cutoff: Double, maxtime: FiniteDuration) =
    Truncate.task(tv.baseEvolveTyps(fd), cutoff, maxtime).memoize

  def termdistTask(fd: FD[Term], tv: TermEvolver, cutoff: Double, maxtime: FiniteDuration) =
    Truncate.task(tv.baseEvolve(fd), cutoff, maxtime).memoize

  def termdistDerTask(fd: FD[Term], tfd: FD[Term], tv: TermEvolver, cutoff: Double, maxtime: FiniteDuration) =
    Truncate.task(tv.evolve(TangVec(fd, tfd)).vec, cutoff, maxtime).memoize

  def hExp(p: Double, q: Double) = -p / log(p) /q

  def prsmEntTask(termsTask: Task[FD[Term]], typsTask : Task[FD[Typ[Term]]]) =
    for {
      terms <- termsTask
      typs <- typsTask
      thmsByPf = terms.map(_.typ)
      thmsBySt = typs.filter(thmsByPf(_) > 0).normalized()
      pfSet = terms.flatten.supp
    } yield pfSet.map{
      (pf) => (pf, hExp(thmsBySt(pf.typ), thmsByPf(pf.typ)))
    }

  def dervecTasks(fd: FD[Term],
    tv: TermEvolver, cutoff: Double, maxtime: FiniteDuration,
    typsTask : Task[FD[Typ[Term]]], scale: Double) : Task[Vector[Task[FD[Term]]]]  =
      prsmEntTask(termdistTask(fd, tv, cutoff, maxtime), typsTask).map{
      (vec) => vec.collect{
        case (v, p) if scale * p < 1 =>
          termdistDerTask(fd, FD.unif(v), tv, scale * p, maxtime)
        }
      }

  // Abstract methods
  def inTaskVec[X, Y](tv: Task[Vector[X]], p: X => Option[Y]): Task[Option[Y]] =
      tv.flatMap{
        case Vector() => Task.pure(None)
        case x +: ys =>
          Task.eval(p(x)).flatMap{
             case Some(z) => Task.pure(Some(z))
             case None => inTaskVec(Task.eval(ys), p)
          }
      }

    def inTaskVecTask[X, Y](tv: Task[Vector[Task[X]]], p: X => Option[Y]): Task[Option[Y]] =
      tv.flatMap{
        case Vector() => Task.pure(None)
        case x +: ys =>
          x.map(p).flatMap{
             case Some(z) => Task.pure(Some(z))
             case None => inTaskVecTask(Task.eval(ys), p)
          }
      }

    def breadthFirstTask[X, Y](
      tv: Task[Vector[Task[X]]],
      p: X => Option[Y],
      spawn: X => Task[Vector[Task[X]]]): Task[Option[Y]] = {
        tv.flatMap{
          (vec) =>
             if (vec.isEmpty) Task.pure(None)
             else
              inTaskVecTask(tv, p).flatMap{
                case Some(z) => Task.pure(Some(z))
                case None =>
                  val tskVecs : Vector[Task[Vector[Task[X]]]] = vec.map(
                          (t) =>
                            t.flatMap(spawn)
                        )
                  val w : Task[Vector[Task[X]]]= Task.gather(tskVecs).map(_.flatten)
                  breadthFirstTask(w, p, spawn)
              }
        }
      }

}
