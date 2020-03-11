package provingground.learning
import provingground._, HoTT._

import spire.algebra._
import spire.implicits._

import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD}

import monix.eval._
import math._

import scala.concurrent._, duration._

import translation.FansiShow._

/**
  * A collection of functions to build provers;
  * some are abstract methods for exploring, searching etc., while others generate terms and types, sometimes as derivatives.
  * Some methods combine the two to give a ready to use function.
  * These are based on [[TermEvolver]] and `Monix`.
  */
object FineProverTasks {
  import ProverTasks._

  /**
    * evolution of types, as a  (task with value) finite distribution;
    * obtained by lazy recursive definition in [[TermEvolver]] and trunctation.
    */
  def typdistTask(fd: FD[Term],
                  tv: TermEvolver,
                  cutoff: Double,
                  maxtime: FiniteDuration,
                  vars: Vector[Term] = Vector()): Task[FD[Typ[Term]]] =
    Truncate
      .task(tv.baseEvolveTyps(fd), cutoff, maxtime)
      .memoize

  /**
    * evolution of terms, as a  (task with value) finite distribution;
    * obtained by lazy recursive definition in [[TermEvolver]] and trunctation.
    */
  def termdistTask(fd: FD[Term],
                   tv: TermEvolver,
                   cutoff: Double,
                   maxtime: FiniteDuration,
                   vars: Vector[Term] = Vector()): Task[FD[Term]] =
    Truncate
      .task(tv.baseEvolve(fd), cutoff, maxtime)
      .memoize

  /**
    * evolution of terms by the ''derivative'', as a  (task with value) finite distribution;
    * obtained by lazy recursive definition in [[TermEvolver]] and trunctation.
    * @param fd  the distribution point
    * @param tfd the tangent distribution
    * @param tv the term evolver
    */
  def termdistDerTask(fd: FD[Term],
                      tfd: FD[Term],
                      tv: TermEvolver,
                      cutoff: Double,
                      maxtime: FiniteDuration,
                      vars: Vector[Term] = Vector()): Task[FD[Term]] =
    Truncate
      .task(tv.evolve(TangVec(fd, tfd)).vec, cutoff, maxtime)
      .memoize

  /**
    * bunch of tasks using newly discovered proofs by evolving along unit tangents.
    * the cutoffs, viewed as resources, are allocated based on values of the new proofs.
    * these tasks only return the tangent distributions
    */
  def dervecTasks(base: FD[Term],
                  tv: TermEvolver,
                  termsTask: Task[FD[Term]],
                  typsTask: Task[FD[Typ[Term]]],
                  maxtime: FiniteDuration,
                  cutoff: Double,
                  scale: Double,
                  vars: Vector[Term] = Vector()): Task[Vector[Task[FD[Term]]]] =
    prsmEntTask(termsTask, typsTask, scale, vars = vars).map { (vec) =>
      {
        val scales = vec.collect {
          case (v, p) if p > cutoff && cutoff / p > 0 =>
            (v, cutoff / p)
        }
        val tot   = scales.map { case (_, x) => 1 / x }.sum
        val ratio = max(tot * cutoff, 1.0)
        scales.map {
          case (v, sc) =>
            termdistDerTask(base, FD.unif(v), tv, sc * ratio, maxtime, vars)
        }
      }
    }

  /**
    * bunch of tasks using newly discovered proofs by evolving along unit tangents.
    * the cutoffs, viewed as resources, are allocated based on values of the new proofs.
    * these tasks return the tangent distributions as well as the path to get there.
    */
  def dervecTraceTasks(base: FD[Term],
                       tv: TermEvolver,
                       termsTask: Task[FD[Term]],
                       typsTask: Task[FD[Typ[Term]]],
                       maxtime: FiniteDuration,
                       cutoff: Double,
                       scale: Double,
                       trace: Vector[Term],
                       vars: Vector[Term] = Vector())
    : Task[Vector[Task[(FD[Term], Vector[Term])]]] =
    prsmEntTask(termsTask, typsTask, scale, vars = vars).map { (vec) =>
      {
        val scales = vec.collect {
          case (v, p) if p > cutoff && cutoff / p > 0 && !(trace.contains(v)) =>
            (v, cutoff / p)
        }
        val tot   = scales.map { case (_, x) => 1 / x }.sum
        val ratio = max(tot * cutoff, 1.0)
        scales.map {
          case (v, sc) =>
            for {
              fd <- termdistDerTask(base,
                                    FD.unif(v),
                                    tv,
                                    sc * ratio,
                                    maxtime,
                                    vars)
            } yield (fd, trace :+ v)
        }
      }
    }

  /**
    * bunch of tasks using newly discovered proofs by evolving along unit tangents.
    * the cutoffs, viewed as resources, are allocated based on values of the new proofs.
    * these tasks return the tangent distributions as well as the path to get there.
    */
  def dervecMemoTasks(base: FD[Term],
                      tv: TermEvolver,
                      termsTask: Task[FD[Term]],
                      typsTask: Task[FD[Typ[Term]]],
                      maxtime: FiniteDuration,
                      cutoff: Double,
                      scale: Double,
                      trace: Vector[Term],
                      termSet: Set[Term],
                      typSet: Set[Typ[Term]],
                      vars: Vector[Term] = Vector())
    : Task[Vector[Task[(FD[Term], Vector[Term], Set[Term], Set[Typ[Term]])]]] =
    prsmEntMemoTask(termsTask, typsTask, scale, vars = vars).map {
      case (vec, newTerms, newTypes) => {
        val scales = vec.collect {
          case (v, p) if p > cutoff && cutoff / p > 0 && !(trace.contains(v)) =>
            (v, cutoff / p)
        }
        val tot   = scales.map { case (_, x) => 1 / x }.sum
        val ratio = max(tot * cutoff, 1.0)
        scales.map {
          case (v, sc) =>
            for {
              fd <- termdistDerTask(base,
                                    FD.unif(v),
                                    tv,
                                    sc * ratio,
                                    maxtime,
                                    vars)
            } yield
              (fd, trace :+ v, termSet union newTerms, typSet union newTypes)
        }
      }
    }

  /**
    * bunch of tasks using newly discovered proofs by evolving along unit tangents.
    * the cutoffs, viewed as resources, are allocated based on values of the new proofs.
    * these tasks return the tangent distributions as well as the path to get there,
    * plus weights of proofs as they are found.
    */
  def dervecWeightedTraceTasks(base: FD[Term],
                               tv: TermEvolver,
                               termsTask: Task[FD[Term]],
                               typsTask: Task[FD[Typ[Term]]],
                               maxtime: FiniteDuration,
                               cutoff: Double,
                               scale: Double,
                               trace: Vector[(Term, Double)],
                               vars: Vector[Term] = Vector())
    : Task[Vector[Task[(FD[Term], Vector[(Term, Double)])]]] =
    prsmEntTask(termsTask, typsTask, scale, vars = vars).map { (vec) =>
      {
        val scales = vec.collect {
          case (v, p) if p > cutoff && cutoff / p > 0 && !(trace.contains(v)) =>
            (v, p, cutoff / p)
        }
        val tot   = scales.map { case (_, _, x) => 1 / x }.sum
        val ratio = max(tot * cutoff, 1.0)
        scales.map {
          case (v, p, sc) =>
            for {
              fd <- termdistDerTask(base,
                                    FD.unif(v),
                                    tv,
                                    sc * ratio,
                                    maxtime,
                                    vars)
            } yield (fd, trace :+ (v -> sc * ratio))
        }
      }
    }

  /**
    * breadth-first search for a term of a given type
    *
    * @return task giving an optional term of the type.
    */
  def theoremSearchTask(fd: FD[Term],
                        tv: TermEvolver,
                        cutoff: Double,
                        maxtime: FiniteDuration,
                        goal: Typ[Term],
                        decay: Double = 1.0,
                        scale: Double = 1.0,
                        vars: Vector[Term] = Vector()): Task[Option[Term]] = {
    val typsTask  = typdistTask(fd, tv, cutoff, maxtime, vars)
    val termsTask = termdistTask(fd, tv, cutoff, maxtime, vars)
    def spawn(d: Int)(vec: FD[Term]): Task[Vector[Task[FD[Term]]]] = {
      if (fd.total == 0) Task.pure(Vector())
      else
        dervecTasks(
          fd.safeNormalized,
          tv,
          Task.eval(vec),
          typsTask,
          maxtime,
          cutoff * math.pow(decay, 1.0 + d.toDouble),
          scale,
          vars
        )
    }
    breadthFirstTask[FD[Term], Term](
      termsTask.map((fd) => Vector(Task.eval(fd))),
      (findist: FD[Term]) =>
        findist.supp
          .filter(_.typ == goal)
          .sortBy((x) => -findist(x))
          .headOption,
      spawn)
  }

  /**
    * breadth-first exploration to find alll interesting proofs.
    *
    * @return map from types to proofs with weights and steps
    */
  def theoremsExploreTask(fd: FD[Term],
                          tv: TermEvolver,
                          cutoff: Double,
                          maxtime: FiniteDuration,
                          decay: Double = 1.0,
                          scale: Double = 1.0,
                          vars: Vector[Term] = Vector())
    : Task[Map[Typ[Term], Vector[(Term, (Int, Double))]]] = {
    val typsTask  = typdistTask(fd, tv, cutoff, maxtime, vars)
    val termsTask = termdistTask(fd, tv, cutoff, maxtime, vars)
    def spawn(d: Int)(vec: FD[Term]): Task[Vector[Task[FD[Term]]]] = {
      if (fd.total == 0) Task.pure(Vector())
      else
        dervecTasks(
          fd.safeNormalized,
          tv,
          Task.eval(vec),
          typsTask,
          maxtime,
          cutoff * math.pow(decay, 1.0 + d.toDouble),
          scale,
          vars
        )
    }

    def result(d: Int)(fd: FD[Term]): Task[Vector[(Term, (Int, Double))]] =
      typsTask.map { (p) =>
        {
          val q = fd.map(_.typ)
          fd.flatten.supp.collect {
            case t if p(t.typ) > 0 && q(t.typ) > 0 && q(t.typ) < 1 =>
              (t, (d, fd(t)))
          }
        }
      }

    branchedGatherTask[FD[Term], Vector[(Term, (Int, Double))]](termsTask,
                                                                result,
                                                                _ ++ _,
                                                                spawn).map(
      (v) =>
        v.groupBy(_._1)
          .mapValues((v) => v.toVector.map(_._2).maxBy((dp) => (-dp._1, dp._2)))
          .toVector
          .groupBy(_._1.typ: Typ[Term])
          .mapValues(_.sortBy((tv) => (tv._2._1, -tv._2._2))))
  }

  /**
    * breadth-first search for a term of a given type, keeping track of the path
    *
    * @return task giving an optional term of the type together with a vector of
    * proofs of lemmas.
    */
  def theoremSearchTraceTask(
      fd: FD[Term],
      tv: TermEvolver,
      cutoff: Double,
      maxtime: FiniteDuration,
      goal: Typ[Term],
      decay: Double = 1.0,
      scale: Double = 1.0,
      vars: Vector[Term] = Vector()): Task[Option[(Term, Vector[Term])]] = {
    val typsTask  = typdistTask(fd, tv, cutoff, maxtime, vars)
    val termsTask = termdistTask(fd, tv, cutoff, maxtime, vars)
    def spawn(d: Int)(
        vecAc: (FD[Term], Vector[Term])
    ): Task[Vector[Task[(FD[Term], Vector[Term])]]] = {
      val (vec, ac) = vecAc
      if (fd.total == 0) Task.pure(Vector())
      else
        dervecTraceTasks(
          fd.safeNormalized,
          tv,
          Task.eval(vec),
          typsTask,
          maxtime,
          cutoff * math.pow(decay, 1.0 + d.toDouble),
          scale,
          ac,
          vars
        )
    }
    breadthFirstTask[(FD[Term], Vector[Term]), (Term, Vector[Term])](
      termsTask.map((fd) => Vector(Task.eval((fd, Vector.empty[Term])))),
      (findistAc: (FD[Term], Vector[Term])) =>
        findistAc._1.supp
          .filter(_.typ == goal)
          .sortBy((x) => -findistAc._1(x))
          .headOption
          .map((t) => (t, findistAc._2)),
      spawn
    )
  }

  /**
    * breadth-first search for a term of a given type, keeping track of the path
    * and the distributions along the way
    *
    * @return task giving an optional term of the type together with a vector of
    * proofs of lemmas.
    */
  def theoremSearchMemoTask(
      fd: FD[Term],
      tv: TermEvolver,
      cutoff: Double,
      maxtime: FiniteDuration,
      goal: Typ[Term],
      termSet: Set[Term],
      typSet: Set[Typ[Term]],
      decay: Double = 1.0,
      scale: Double = 1.0,
      vars: Vector[Term] = Vector()
  ): Task[Option[(Term, Vector[Term], Set[Term], Set[Typ[Term]])]] = {
    val typsTask  = typdistTask(fd, tv, cutoff, maxtime, vars)
    val termsTask = termdistTask(fd, tv, cutoff, maxtime, vars)
    def spawn(d: Int)(
        vecAc: (FD[Term], Vector[Term], Set[Term], Set[Typ[Term]])): Task[
      Vector[Task[(FD[Term], Vector[Term], Set[Term], Set[Typ[Term]])]]] = {
      val (vec, ac, termSet, typSet) = vecAc
      if (fd.total == 0) Task.pure(Vector())
      else
        dervecMemoTasks(
          fd.safeNormalized,
          tv,
          Task.eval(vec),
          typsTask,
          maxtime,
          cutoff * math.pow(decay, 1.0 + d.toDouble),
          scale,
          ac,
          termSet,
          typSet,
          vars
        )
    }
    breadthFirstTask[(FD[Term], Vector[Term], Set[Term], Set[Typ[Term]]),
                     (Term, Vector[Term], Set[Term], Set[Typ[Term]])](
      termsTask.map((fd) =>
        Vector(Task.eval((fd, Vector.empty[Term], termSet, typSet)))),
      (findistAc: (FD[Term], Vector[Term], Set[Term], Set[Typ[Term]])) =>
        findistAc._1.supp
          .filter(_.typ == goal)
          .sortBy((x) => -findistAc._1(x))
          .headOption
          .map((t) => (t, findistAc._2, findistAc._3, findistAc._4)),
      spawn
    )
  }

  /**
    * breadth-first exploration to find alll interesting proofs, keeping track
    * of lemmas
    *
    * @return task for vector of proofs with lemmas and the corresponding theorems.
    */
  def theoremsExploreTraceTask(fd: FD[Term],
                               tv: TermEvolver,
                               cutoff: Double,
                               maxtime: FiniteDuration,
                               decay: Double = 1.0,
                               scale: Double = 1.0,
                               vars: Vector[Term] = Vector())
    : Task[(Vector[(Term, Vector[(Term, Double)], (Int, Double))],
            FD[Term],
            FD[Typ[Term]])] = {
    val typsTask  = typdistTask(fd, tv, cutoff, maxtime, vars)
    val termsTask = termdistTask(fd, tv, cutoff, maxtime, vars)
    def spawn(d: Int)(vecAc: (FD[Term], Vector[(Term, Double)])) = {
      val (vec, ac) = vecAc
      if (fd.total == 0) Task.pure(Vector())
      else
        dervecWeightedTraceTasks(
          fd.safeNormalized,
          tv,
          Task.eval(vec),
          typsTask,
          maxtime,
          cutoff * math.pow(decay, 1.0 + d.toDouble),
          scale,
          ac,
          vars
        )
    }

    def result(d: Int)(fdAc: (FD[Term], Vector[(Term, Double)]))
      : Task[Vector[(Term, Vector[(Term, Double)], (Int, Double))]] =
      typsTask.map { (p) =>
        {
          val (fd, ac) = fdAc
          val q        = fd.map(_.typ)
          fd.flatten.supp.collect {
            case t if p(t.typ) > 0 && q(t.typ) > 0 && q(t.typ) < 1 =>
              (t, ac, (d, fd(t)))
          }
        }
      }

    for {
      res <- branchedGatherTask[(FD[Term], Vector[(Term, Double)]),
                                Vector[(Term,
                                        Vector[(Term, Double)],
                                        (Int, Double))]](
        termsTask.map((fd) => (fd, Vector())),
        result,
        _ ++ _,
        spawn);
      typs  <- typsTask
      terms <- termsTask

    } yield (res, terms, typs)
  }

}
