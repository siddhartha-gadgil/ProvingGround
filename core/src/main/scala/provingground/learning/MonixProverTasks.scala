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
  * A drop-in replacement for [[FineProverTasks]] to test the abstract generators approach;
  * since testing is the goal redundancies and some comments have been removed.
  */
object MonixProverTasks {
  import ProverTasks._
  import TermRandomVars._

  def typs(fd: FD[Term]): FD[Typ[Term]] = fd.mapOpt(typOpt).safeNormalized

  def typdistTask(fd: FD[Term],
                  tg: TermGenParams,
                  cutoff: Double,
                  maxtime: FiniteDuration,
                  vars: Vector[Term] = Vector()): Task[FD[Typ[Term]]] = {
    val initState = TermState(fd, typs(fd), vars)
    tg.monixFD.varDist(initState)(Typs, cutoff)
  }.memoize
  // Truncate
  //   .task(tv.baseEvolveTyps(fd), cutoff, maxtime)
  //   .memoize

  def termdistTask(fd: FD[Term],
                   tg: TermGenParams,
                   cutoff: Double,
                   maxtime: FiniteDuration,
                   vars: Vector[Term] = Vector()): Task[FD[Term]] = {
    val initState = TermState(fd, typs(fd), vars)
    tg.monixFD.varDist(initState)(Terms, cutoff)
  }
  // Truncate
  //   .task(tv.baseEvolve(fd), cutoff, maxtime)
  //   .memoize

  def termdistDerTask(fd: FD[Term],
                      tfd: FD[Term],
                      tg: TermGenParams,
                      cutoff: Double,
                      maxtime: FiniteDuration,
                      vars: Vector[Term] = Vector()): Task[FD[Term]] =
    {
      val baseState = TermState(fd, typs(fd), vars)
      val tangState = TermState(tfd, typs(tfd), vars)
      tg.monixTangFD(baseState).varDist(tangState)(Terms, cutoff)}
  // Truncate
  //   .task(tv.evolve(TangVec(fd, tfd)).vec, cutoff, maxtime)
  //   .memoize

  /**
    * bunch of tasks using newly discovered proofs by evolving along unit tangents.
    * the cutoffs, viewed as resources, are allocated based on values of the new proofs.
    * these tasks return the tangent distributions as well as the path to get there.
    */
  def dervecTraceTasks(base: FD[Term],
                       tg: TermGenParams,
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
          case (v, p) if p > cutoff && cutoff / p > 0 && !trace.contains(v) =>
            (v, cutoff / p)
        }
        val tot   = scales.map { case (_, x) => 1 / x }.sum
        val ratio = max(tot * cutoff, 1.0)
        // pprint.log(s"want: ${1/cutoff}, actual total: $tot from ${scales.size}")
        scales.map {
          case (v, sc) =>
            pprint.log("Spawning from lemma")
            pprint.log(v.fansi)
            pprint.log(v.typ.fansi)
            pprint.log(-math.log(sc * ratio))
            for {
              fd <- termdistDerTask(base,
                                    FD.unif(v),
                                    tg,
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
    * these tasks return the tangent distributions as well as the path to get there,
    * plus weights of proofs as they are found.
    */
  def dervecWeightedTraceTasks(base: FD[Term],
                               tg: TermGenParams,
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
          case (v, p) if p > cutoff && cutoff / p > 0 && !trace.contains(v) =>
            (v, p, cutoff / p)
        }
        val tot   = scales.map { case (_, _, x) => 1 / x }.sum
        val ratio = max(tot * cutoff, 1.0)
        // pprint.log(s"want: ${1/cutoff}, actual total: $tot from ${scales.size}")
        scales.map {
          case (v, p, sc) =>
            for {
              fd <- termdistDerTask(base,
                                    FD.unif(v),
                                    tg,
                                    sc * ratio,
                                    maxtime,
                                    vars)
            } yield (fd, trace :+ (v -> sc * ratio))
        }
      }
    }

  /**
    * breadth-first search for a term of a given type, keeping track of the path
    *
    * @return task giving an optional term of the type together with a vector of
    * proofs of lemmas.
    */
  def theoremSearchTraceTask(
      fd: FD[Term],
      tg: TermGenParams,
      cutoff: Double,
      maxtime: FiniteDuration,
      goal: Typ[Term],
      decay: Double = 1.0,
      scale: Double = 1.0,
      vars: Vector[Term] = Vector()): Task[Option[(Term, Vector[Term])]] = {
    val typsTask  = typdistTask(fd, tg, cutoff, maxtime, vars)
    val termsTask = termdistTask(fd, tg, cutoff, maxtime, vars)
    def spawn(d: Int)(
        vecAc: (FD[Term], Vector[Term])
    ): Task[Vector[Task[(FD[Term], Vector[Term])]]] = {
      val (vec, ac) = vecAc
//      pprint.log(s"spawning tasks:\n from ${vec.entropyView}\n types: ${vec.map(_.typ).entropyView}\n spawns: ${vec.flatten.supp.length}")
      if (fd.total == 0) Task.pure(Vector())
      else
        dervecTraceTasks(
          fd.safeNormalized,
          tg,
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
    * breadth-first exploration to find alll interesting proofs, keeping track
    * of lemmas
    *
    * @return task for vector of proofs with lemmas and the corresponding theorems.
    */
  def theoremsExploreTraceTask(fd: FD[Term],
                               tg: TermGenParams,
                               cutoff: Double,
                               maxtime: FiniteDuration,
                               decay: Double = 1.0,
                               scale: Double = 1.0,
                               vars: Vector[Term] = Vector())
    : Task[(Vector[(Term, Vector[(Term, Double)], (Int, Double))],
            FD[Term],
            FD[Typ[Term]])] = {
    val typsTask  = typdistTask(fd, tg, cutoff, maxtime, vars)
    val termsTask = termdistTask(fd, tg, cutoff, maxtime, vars)
    def spawn(d: Int)(vecAc: (FD[Term], Vector[(Term, Double)])) = {
      val (vec, ac) = vecAc
      if (fd.total == 0) Task.pure(Vector())
      else
        dervecWeightedTraceTasks(
          fd.safeNormalized,
          tg,
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
