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
import _root_.shapeless.Succ

object StrategicProvers {
  type Successes =
    Vector[(HoTT.Typ[HoTT.Term], Double, FiniteDistribution[HoTT.Term])]

  type SeekResult = (
      Successes,
      Set[EquationNode]
  )

  def formal(sc: Successes): Set[EquationNode] = {
    val termsGroups = sc.toSet
    val terms       = termsGroups.map(_._3).flatMap(_.support)
    terms.flatMap(t => DE.formalEquations(t))
  }

  var currentGoal: Option[Typ[Term]] = None

  var update: Unit => Unit = (_) => ()

  def seekGoal(
      lp: LocalProver,
      typ: Typ[Term],
      scale: Double = 2,
      maxSteps: Int = 100
  ): Task[SeekResult] = {
    val base = lp.addGoals(typ -> 0.5, negate(typ) -> 0.5)
    currentGoal = Option(typ)
    update(())
    val tasks = (1 until (maxSteps)).toVector.map { n =>
      val prover = base.sharpen(math.pow(scale, n))
      val pair = for {
        sc  <- prover.successes
        eqs <- prover.equationNodes
      } yield (sc, eqs union formal(sc))
      pair
    }

    bestTask[SeekResult](tasks, p => p._1.nonEmpty)
  }.map(_.getOrElse((Vector(), Set())))

  val successes: ArrayBuffer[Successes] = ArrayBuffer()

  val failures: ArrayBuffer[Typ[Term]] = ArrayBuffer()

  def md =
    s"""## Goal chomping status
        |
        | * current goal : $currentGoal
        | * successes : ${successes.size}
        | * failures : ${failures.size}
        | * last success : ${successes.headOption}
        | * last failure : ${failures.headOption}
        |""".stripMargin

  // chomps goals till failure or if none are left
  def goalChomper(
      lp: LocalProver,
      typs: Vector[Typ[Term]],
      accumSucc: Vector[Successes] = Vector(),
      accumEqs: Set[EquationNode] = Set(),
      scale: Double = 2,
      maxSteps: Int = 100
  ): Task[(Vector[Successes], Set[EquationNode], Vector[Typ[Term]])] =
    typs match {
      case Vector() => Task.now((accumSucc, accumEqs, Vector()))
      case typ +: ys =>
        seekGoal(lp, typ, scale, maxSteps).flatMap {
          case (ss, eqs) =>
            if (ss.isEmpty)
              Task.now((accumSucc :+ ss, accumEqs union eqs, typ +: ys))
            else {
              successes.append(ss)
              update(())
              goalChomper(
                lp,
                ys,
                accumSucc :+ ss,
                accumEqs union eqs,
                scale,
                maxSteps
              )
            }
        }
    }

  def liberalChomper(
      lp: LocalProver,
      typs: Vector[Typ[Term]],
      accumSucc: Vector[Successes] = Vector(),
      accumFail: Vector[Typ[Term]] = Vector(),
      accumEqs: Set[EquationNode] = Set(),
      scale: Double = 2,
      maxSteps: Int = 100
  ): Task[
    (Vector[Successes], Vector[Typ[Term]], Set[EquationNode], Vector[Typ[Term]])
  ] =
    typs match {
      case Vector() => Task.now((accumSucc, accumFail, accumEqs, Vector()))
      case typ +: ys =>
        seekGoal(lp, typ, scale, maxSteps).flatMap {
          case (ss, eqs) =>
            if (ss.isEmpty) {
              failures.append(typ)
              update(())
              liberalChomper(
                lp,
                ys,
                accumSucc,
                accumFail :+ typ,
                accumEqs union eqs,
                scale,
                maxSteps
              )
            } else {
              successes.append(ss)
              update(())
              liberalChomper(
                lp,
                ys,
                accumSucc :+ ss,
                accumFail,
                accumEqs union eqs,
                scale,
                maxSteps
              )
            }
        }
    }
}
