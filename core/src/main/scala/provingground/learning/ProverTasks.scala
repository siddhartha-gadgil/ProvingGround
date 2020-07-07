package provingground.learning
import provingground._, HoTT._

import spire.algebra._
import spire.implicits._

import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD}

import monix.eval._
import math._

import scala.concurrent._, duration._

import translation.FansiShow._
import scala.math.Ordering.Double.TotalOrdering

object ProverTasks {

  /**
    * An entropy measure for parsimonious generation of terms inhabiting types.
    */
  def h0(p: Double, q: Double): Double = {
    require(q > 0, s"Entropy with p=$p, q = $q")
    -p / (q * log(q))
  }

  /**
    * Exponent of an entropy measure for parsimonious generation of terms inhabiting types;
    * low values correspond to important terms, with the `scale` increasing sensitivity.
    */
  def hExp(p: Double, q: Double, scale: Double): Double =
    math.exp(-(h0(p, q) - 1) * scale)

  /**
    * task to find proofs that are useful based on parsimony, i.e.,
    * reducing relative entropy of the final result with a cost for the
    * increase in entropy of the initial distribution.
    *
    * @param termsTask task for evolution of terms.
    * @param typsTask task for evolution of types.
    * @return task for proofs with, and sorted by, parsimonious entropies.
    */
  def prsmEntTask(termsTask: Task[FD[Term]],
                  typsTask: Task[FD[Typ[Term]]],
                  scale: Double,
                  vars: Vector[Term] = Vector()): Task[Vector[(Term, Double)]] =
    for {
      terms <- termsTask
      typs  <- typsTask
      thmsByPf = terms.map(_.typ)
      thmsBySt = typs.filter(thmsByPf(_) > 0)
      pfSet    = terms.flatten.supp.filter((t) => thmsBySt(t.typ) > 0)
      fullPfSet = pfSet.flatMap((pf) =>
        partialLambdaClosures(vars)(pf).map((pf, _)))
    } yield
      fullPfSet
        .map {
          case (pf, fullPf) =>
            (fullPf,
             hExp(thmsBySt(pf.typ),
                  thmsByPf(pf.typ),
                  scale * terms(pf) / thmsByPf(pf.typ)))
        }
        .sortBy(_._2)

  /**
    * task to find proofs that are useful based on parsimony, i.e.,
    * reducing relative entropy of the final result with a cost for the
    * increase in entropy of the initial distribution.
    * Also return terms and types
    *
    * @param termsTask task for evolution of terms.
    * @param typsTask task for evolution of types.
    * @return task for proofs with, and sorted by, parsimonious entropies.
    */
  def prsmEntMemoTask(termsTask: Task[FD[Term]],
                      typsTask: Task[FD[Typ[Term]]],
                      scale: Double,
                      vars: Vector[Term] = Vector())
    : Task[(Vector[(Term, Double)], Set[Term], Set[Typ[Term]])] =
    for {
      terms <- termsTask
      typs  <- typsTask
      thmsByPf = terms.map(_.typ)
      thmsBySt = typs.filter(thmsByPf(_) > 0)
      pfSet    = terms.flatten.supp.filter((t) => thmsBySt(t.typ) > 0)
      fullPfSet = pfSet.flatMap((pf) =>
        partialLambdaClosures(vars)(pf).map((pf, _)))
    } yield
      (fullPfSet
         .map {
           case (pf, fullPf) =>
             (fullPf,
              hExp(thmsBySt(pf.typ),
                   thmsByPf(pf.typ),
                   scale * terms(pf) / thmsByPf(pf.typ)))
         }
         .sortBy(_._2),
       terms.support,
       typs.support)

  /**
    * Looks for an element satisfying a predicate in a vector of tasks.
    * @param tv task giving a vector of tasks giving elements of X.
    * @param p an optional map, ie predicate+map
    * @return task giving optional element satisying the predicate.
    */
  def inTaskVecTask[X, Y](tv: Task[Vector[Task[X]]],
                          p: X => Option[Y]): Task[Option[Y]] =
    tv.flatMap {
      case Vector() => Task.pure(None)
      case x +: ys =>
        x.map(p).flatMap {
          case Some(z) => Task.pure(Some(z))
          case None    => inTaskVecTask(Task.eval(ys), p)
        }
    }

  /**
    * bread-first search for an element in `X` satisying a predicate-map `p`;
    * each element of `X` can spawn a vector of elements in which we also search.
    * Everything is wrapped in tasks.
    *
    * @param tv the initial vector in which we search
    * @param p the predicate to be satisfied + map; actually an optional map.
    * @param spawn new vectors in `X` spawned by an element in `X`, depeneding on the depth.
    * @param depth the depth of the current recursive step.
    */
  def breadthFirstTask[X, Y](tv: Task[Vector[Task[X]]],
                             p: X => Option[Y],
                             spawn: Int => X => Task[Vector[Task[X]]],
                             depth: Int = 0): Task[Option[Y]] = {
    tv.flatMap { (vec) =>
      if (vec.isEmpty) Task.pure(None)
      else
        inTaskVecTask(tv, p).flatMap {
          case Some(z) => Task.pure(Some(z))
          case None =>
            val tskVecs: Vector[Task[Vector[Task[X]]]] = vec.map(
              (t) => t.flatMap(spawn(depth))
            )
            val w: Task[Vector[Task[X]]] = Task.gather(tskVecs).map(_.flatten)
            breadthFirstTask(w, p, spawn, depth + 1)
        }
    }
  }

  /**
    * bread-first accumulation of results;
    * a typical example is interesting proofs, with X finite-distributions of terms.
    * each element of `X` can spawn a vector of elements in which we also search.
    * Everything is wrapped in tasks.
    *
    * @param tsk the initial element for the search
    * @param results the result, depending on an element of `X` - also the depth,
    * but this is used mainly for tracking.
    * @param spawn new vectors in `X` spawned by an element in `X`, depeneding on the depth.
    * @param depth the depth of the current recursive step.
    */
  def branchedGatherTask[X, Y](tsk: Task[X],
                               results: Int => X => Task[Y],
                               combine: (Y, Y) => Y,
                               spawn: Int => X => Task[Vector[Task[X]]],
                               depth: Int = 0): Task[Y] = {
    for {
      x <- tsk
      res = results(depth)(x)
      nextGen <- spawn(depth)(x)
      childRes: Vector[Task[Y]] = nextGen.map((tx) =>
        branchedGatherTask(tx, results, combine, spawn, depth + 1))
      comb <- childRes.foldLeft(res) {
        case (t1, t2) =>
          for {
            x1 <- t1
            x2 <- t2
          } yield combine(x1, x2)
      }
    } yield comb
  }
  def h[A](fd: FD[A]) = {
    val fd0 = fd.flatten
    fd0.supp.map((x) => -fd(x) * log(fd(x))).sum
  }

  def kl[A](p: FD[A], q: FD[A]) = {
    val p0 = p.filter(q(_) > 0).flatten.safeNormalized
    p0.supp.map { (x) =>
      p(x) * log(p(x) / q(x))
    }.sum
  }

  def minOn[A](fd: FD[A], minValue: Double, supp: Set[A]) = {
    FD(
      for {
        Weighted(x, p) <- fd.pmf
      } yield Weighted(x, if (supp.contains(x)) math.min(p, minValue) else p)
    )
  }

  def pfMatch(ev: FD[Term] => Task[FD[Term]],
              typs: Task[FD[Typ[Term]]],
              wt: Double = 1.0)(gen: FD[Term]) =
    for {
      p  <- typs
      qt <- ev(gen)
      q = qt.map(_.typ)
    } yield kl(p, q) - (h(gen) * wt)

  def pfMatchDiff(
      ev: FD[Term] => Task[FD[Term]],
      typs: Task[FD[Typ[Term]]],
      cutoff: Double,
      wt: Double = 1.0)(gen0: FD[Term], gen1: FD[Term]): Task[Double] =
    for {
      p   <- typs
      qt0 <- ev(gen0)
      qt1 <- ev(gen1)
      q0      = qt0.map(_.typ)
      q1      = qt1.map(_.typ)
      totSupp = q0.support union (q1.support)
      q0m     = minOn(q0, cutoff, totSupp)
      q1m     = minOn(q1, cutoff, totSupp)
      h0      = kl(p, q0m) - (h(gen0) * wt)
      h1      = kl(p, q1m) - (h(gen1) * wt)
    } yield h1 - h0

  def quasiGradShift[A](base: A,
                        neighbours: Vector[A],
                        derTask: (A, A) => Task[Double])(
      implicit ls: VectorSpace[A, Double]
  ): Task[A] = {
    val shiftsTask =
      Task.gather {
        for { x <- neighbours } yield {
          derTask(base, x).map { (der) =>
            -der *: (base + x)
          }
        }
      }
    shiftsTask.map { (shifts) =>
      shifts.foldLeft(base)(_ + _)
    }
  }

  def quasiGradFlowTask[A](base: A,
                           neighMap: A => Vector[A],
                           derTask: (A, A) => Task[Double],
                           halt: (A, A) => Boolean)(
      implicit ls: VectorSpace[A, Double]
  ): Task[A] = Task.tailRecM(base)(
    (x: A) =>
      quasiGradShift(x, neighMap(x), derTask).map { (y) =>
        if (halt(x, y)) Right(y) else Left(x)
    }
  )

  def selfNeighbours[A](fd: FD[A], epsilon: Double) =
    fd.flatten.supp.map { (x) =>
      (fd + (x, fd(x) * epsilon)).safeNormalized
    }

  def newNeighbours[A](fd: FD[A], pert: Vector[A], epsilon: Double) =
    pert.map { (x) =>
      (fd + (x, fd(x) * epsilon)).safeNormalized
    }

  def stabHalt(x: FD[Term], y: FD[Term], level: Double): Boolean =
    (x -- y).norm < level

}
