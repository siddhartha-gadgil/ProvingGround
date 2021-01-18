package provingground.learning

import provingground._
import HoTT._
import monix.eval.Task

import scala.concurrent._
import duration._

import TermRandomVars._

import GeneratorVariables._, Expression._

import EntropyAtomWeight._

import Utils.bestTask

import monix.tail.Iterant

import collection.mutable.ArrayBuffer
import scala.util.Success
import scala.collection.parallel._
import provingground.learning.TypSolver.LookupSolver

object StrategicProvers {
  type Successes =
    Vector[(HoTT.Typ[HoTT.Term], Double, Term)]

  type SeekResult = (
      Successes,
      Set[EquationNode],
      Set[Term]
  )

  object SeekResult {
    def or(a: SeekResult, b: SeekResult) = (
      a._1 ++ b._1,
      a._2 union b._2,
      a._3 union b._3
    )
    def collate(
        xs: Vector[(Typ[Term], SeekResult)]
    ): (SeekResult, Vector[Typ[Term]]) =
      (
        (
          xs.map(_._2).flatMap(_._1),
          xs.map(_._2).flatMap(_._2).toSet,
          xs.map(_._2).flatMap(_._3).toSet
        ),
        xs.filter(_._2._1.isEmpty).map(_._1)
      )
  }

  def formal(sc: Successes): Set[EquationNode] = {
    val termsGroups = sc.toSet
    val terms       = termsGroups.map(_._3)
    terms.flatMap(t => DE.formalEquations(t))
  }

  var currentGoal: Option[Typ[Term]] = None

  var update: Unit => Unit = (_) => ()

  def seekGoal(
      lp: LocalProver,
      typ: Typ[Term],
      terms: Set[Term],
      scale: Double = 2,
      maxSteps: Int = 100
  ): Task[SeekResult] = {
    val base =
      if (lp.tg.solverW == 0) lp.addGoals(typ -> 0.5, negate(typ) -> 0.5)
      else lp.addGoals(typ -> 0.5, negate(typ) -> 0.5).addLookup(terms)
    currentGoal = Option(typ)
    update(())
    val tasks = (1 until (maxSteps)).toVector.map { n =>
      val prover = base.sharpen(math.pow(scale, n))
      val triple = for {
        sc  <- prover.successes
        eqs <- prover.equationNodes
        // termSet <- prover.expressionEval.map(_.finalTermSet)
        finalTerms <- prover.nextState.map(_.terms.support)
      } yield (sc, eqs union formal(sc), finalTerms)
      triple
    }

    bestTask[SeekResult](tasks, p => p._1.nonEmpty)
  }.map(_.getOrElse((Vector(), Set(), Set())))

  def seekTyp(
      lp: LocalProver,
      typ: Typ[Term],
      terms: Set[Term]
  ): Task[SeekResult] = {
    val base =
      if (lp.tg.solverW == 0) lp
      else lp.addLookup(terms)
    lp.varDistEqs(TermRandomVars.termsWithTyp(typ)).map {
      case (fd, eqs) =>
        if (fd.pmf.isEmpty) (Vector(), eqs, Set())
        else {
          val best = fd.pmf.maxBy(_.weight)
          (Vector((typ, best.weight, best.elem)), eqs, fd.support)
        }
    }
  }

  def solveTyp(
      lp: LocalProver,
      typ: Typ[Term],
      terms: Set[Term]
  ): Task[SeekResult] =
    seekTyp(lp, typ, terms).flatMap { sr =>
      if (sr._1.nonEmpty) Task.now(sr)
      else seekTyp(lp, negate(typ), terms).map(nsr => SeekResult.or(sr, nsr))
    }

  def parSeekTyp(
      typ: Typ[Term],
      initState: ParMapState,
      tg: TermGenParams,
      cutoff: Double,
      terms: Set[Term]
  ): SeekResult = {
    val tgSolve =
      if (tg.solverW == 0) tg
      else {
        val solverL = tg.solver.||(LookupSolver(terms))
        tg.copy(solver = solverL)
      }
    val pde = ParDistEq.fromParams(tgSolve)
    val (dist, eqs) = pde.varDist(initState, None, false)(
      TermRandomVars.termsWithTyp(typ),
      cutoff
    )
    if (dist.isEmpty) (Vector(), eqs.to(Set), Set())
    else {
      val best = dist.maxBy(_._2)
      (Vector((typ, best._2, best._1)), eqs.to(Set), dist.keySet.to(Set))
    }
  }

  def parSolveTyp(
      typ: Typ[Term],
      initState: ParMapState,
      tg: TermGenParams,
      cutoff: Double,
      terms: Set[Term]
  ): SeekResult = {
    val sr = parSeekTyp(typ, initState, tg, cutoff, terms)
    if (sr._1.nonEmpty) sr 
    else {
      val nsr = parSeekTyp(typ, initState, tg, cutoff, terms)
      SeekResult.or(sr, nsr)
    }
  }

  val successes: ArrayBuffer[Successes] = ArrayBuffer()

  val failures: ArrayBuffer[Typ[Term]] = ArrayBuffer()

  var termSet: Set[Term] = Set()

  var equationNodes: Set[EquationNode] = Set()

  def md =
    s"""## Goal chomping status
        |
        | * current goal : $currentGoal
        | * negated current goal: ${currentGoal.map(negate)}
        | * successes : ${successes.size}
        | * failures : ${failures.size}
        | * terms : ${termSet.size}
        | * equation-nodes: ${equationNodes.size}
        | * last success : ${successes.headOption}
        | 
        | ${failures.reverse.mkString("### Failures\n\n * ", "\n * ", "\n")}
        |""".stripMargin

  // chomps goals till failure or if none are left
  def goalChomper(
      lp: LocalProver,
      typs: Vector[Typ[Term]],
      accumSucc: Successes = Vector(),
      accumEqs: Set[EquationNode] = Set(),
      accumTerms: Set[Term] = Set(),
      scale: Double = 2,
      maxSteps: Int = 100
  ): Task[
    (Successes, Set[EquationNode], Set[Term], Vector[Typ[Term]])
  ] =
    typs match {
      case Vector() => Task.now((accumSucc, accumEqs, accumTerms, Vector()))
      case typ +: ys =>
        seekGoal(lp, typ, accumTerms, scale, maxSteps).flatMap {
          case (ss, eqs, terms) =>
            if (ss.isEmpty)
              Task.now(
                (
                  accumSucc ++ ss,
                  accumEqs union eqs,
                  accumTerms union terms,
                  typ +: ys
                )
              )
            else {
              successes.append(ss)
              update(())
              goalChomper(
                lp,
                ys,
                accumSucc ++ ss,
                accumEqs union eqs,
                accumTerms union terms,
                scale,
                maxSteps
              )
            }
        }
    }

  def liberalChomper(
      lp: LocalProver,
      typs: Vector[Typ[Term]],
      accumSucc: Successes = Vector(),
      accumFail: Vector[Typ[Term]] = Vector(),
      accumEqs: Set[EquationNode] = Set(),
      accumTerms: Set[Term] = Set(),
      scale: Double = 2,
      maxSteps: Int = 100
  ): Task[
    (
        Successes,
        Vector[Typ[Term]], // failures
        Set[EquationNode],
        Set[Term]
    )
  ] =
    typs match {
      case Vector() =>
        Task.now((accumSucc, accumFail, accumEqs, accumTerms))
      case typ +: ys =>
        seekGoal(lp, typ, accumTerms, scale, maxSteps).flatMap {
          case (ss, eqs, terms) =>
            equationNodes = equationNodes union (eqs)
            termSet = termSet union (terms)
            if (ss.isEmpty) {
              failures.append(typ)
              Utils.logger.info(s"failed to prove $typ")
              update(())
              liberalChomper(
                lp,
                ys,
                accumSucc,
                accumFail :+ typ,
                accumEqs union eqs,
                accumTerms union terms,
                scale,
                maxSteps
              )
            } else {
              successes.append(ss)
              ss.foreach(
                s => Utils.logger.info(s"proved ${s._1} with proof ${s._3}")
              )
              Utils.logger.info(s"goals remaining ${ys.size}")
              update(())
              liberalChomper(
                lp,
                ys,
                accumSucc ++ ss,
                accumFail,
                accumEqs union eqs,
                accumTerms union terms,
                scale,
                maxSteps
              )
            }
        }
    }

  def parChomper(typs: Vector[Typ[Term]],
      initState: ParMapState,
      tg: TermGenParams,
      cutoff: Double,
      accumSucc: Successes = Vector(),
      accumFail: Vector[Typ[Term]] = Vector(),
      accumEqs: Set[EquationNode] = Set(),
      accumTerms: Set[Term] = Set(),
      ) : (
        Successes,
        Vector[Typ[Term]], // failures
        Set[EquationNode],
        Set[Term]
    ) = 
      typs match {
        case Vector() => (accumSucc, accumFail, accumEqs, accumTerms)
        case typ +: ys =>
          val (ss, eqs, terms) = parSolveTyp(typ, initState, tg, cutoff, accumTerms)
          if (ss.isEmpty) {
            Utils.logger.info(s"failed to prove $typ")
            parChomper(
              ys,
              initState,
              tg,
              cutoff,
              accumSucc,
              accumFail :+ typ,
              accumEqs union eqs,
              accumTerms union(terms)
            )
          }
          else 
          { 
            ss.foreach(
                s => Utils.logger.info(s"proved ${s._1} with proof ${s._3}")
              )
              Utils.logger.info(s"goals remaining ${ys.size}")
            parChomper(
              ys,
              initState,
              tg,
              cutoff,
              accumSucc ++ ss,
              accumFail,
              accumEqs union eqs,
              accumTerms union(terms)
            )
          }
      }

  def concurrentTargetChomper(
      lp: LocalProver,
      typGroups: Vector[Vector[Typ[Term]]],
      concurrency: Int = Utils.threadNum,
      accumSucc: Successes = Vector(),
      accumFail: Vector[Typ[Term]] = Vector(),
      accumEqs: Set[EquationNode] = Set(),
      accumTerms: Set[Term] = Set()
  ): Task[
    (
        Successes,
        Vector[Typ[Term]], // failures
        Set[EquationNode],
        Set[Term]
    )
  ] =
    typGroups match {
      case Vector() =>
        Task.now((accumSucc, accumFail, accumEqs, accumTerms))
      case typGroup +: ys =>
        Utils.logger.info(
          s"seeking result for group ${typGroup.mkString(" ; ")}"
        )
        val resultGroup =
          typGroup.map { typ =>
            solveTyp(lp, typ, accumTerms)
              .onErrorRecover {
                case te: TimeoutException =>
                  Utils.logger.error(te)
                  (Vector(), Set.empty[EquationNode], Set.empty[Term])
              }
              .map(typ -> _)
          }
        val result =
          Task.sequence(resultGroup).map(SeekResult.collate(_))
        result.flatMap {
          case ((ss, eqs, terms), failures) =>
            Utils.logger.info(s"proved ${ss.size} results")
            if (failures.nonEmpty)
              Utils.logger.info(s"failed to prove ${failures.mkString("\n")}")
            Utils.logger.info(s"remaining ${ys.size} groups to prove/disprove")
            equationNodes = equationNodes union (eqs)
            termSet = termSet union (terms)
            concurrentTargetChomper(
              lp,
              ys,
              concurrency,
              accumSucc ++ ss,
              accumFail ++ failures,
              accumEqs union eqs,
              accumTerms union terms
            )
        }
    }

  def targetChomper(
      lp: LocalProver,
      typs: Vector[Typ[Term]],
      accumSucc: Successes = Vector(),
      accumFail: Vector[Typ[Term]] = Vector(),
      accumEqs: Set[EquationNode] = Set(),
      accumTerms: Set[Term] = Set()
  ): Task[
    (
        Successes,
        Vector[Typ[Term]], // failures
        Set[EquationNode],
        Set[Term]
    )
  ] =
    typs match {
      case Vector() =>
        Task.now((accumSucc, accumFail, accumEqs, accumTerms))
      case typ +: ys =>
        Utils.logger.info(s"trying to prove ${typ} or ${negate(typ)}")
        solveTyp(lp, typ, accumTerms)
          .onErrorRecover {
            case te: TimeoutException =>
              Utils.logger.error(te)
              (Vector(), Set.empty[EquationNode], Set.empty[Term])
          }
          .flatMap {
            case (ss, eqs, terms) =>
              equationNodes = equationNodes union (eqs)
              termSet = termSet union (terms)
              if (ss.isEmpty) {
                Utils.logger.info(s"failed to prove $typ")
                targetChomper(
                  lp,
                  ys,
                  accumSucc,
                  accumFail :+ typ,
                  accumEqs union eqs,
                  accumTerms union terms
                )
              } else {
                ss.foreach(
                  s => Utils.logger.info(s"proved ${s._1} with proof ${s._3}")
                )
                Utils.logger.info(s"goals remaining ${ys.size}")
                targetChomper(
                  lp,
                  ys,
                  accumSucc ++ ss,
                  accumFail,
                  accumEqs union eqs,
                  accumTerms union terms
                )
              }
          }
    }
}
