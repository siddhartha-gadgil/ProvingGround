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
      Set[EquationNode],
      Set[Term]
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

  val successes: ArrayBuffer[Successes] = ArrayBuffer()

  val failures: ArrayBuffer[Typ[Term]] = ArrayBuffer()

  var termSet : Set[Term] = Set()

  var equationNodes : Set[EquationNode] = Set()

  def md =
    s"""## Goal chomping status
        |
        | * current goal : $currentGoal
        | * negated current goal: ${negate(currentGoal)}
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
      accumSucc: Vector[Successes] = Vector(),
      accumEqs: Set[EquationNode] = Set(),
      scale: Double = 2,
      maxSteps: Int = 100
  ): Task[(Vector[Successes], Set[EquationNode], Vector[Typ[Term]])] =
    typs match {
      case Vector() => Task.now((accumSucc, accumEqs, Vector()))
      case typ +: ys =>
        seekGoal(lp, typ, Set(), scale, maxSteps).flatMap {
          case (ss, eqs, _) =>
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
      accumTerms: Set[Term] = Set(),
      scale: Double = 2,
      maxSteps: Int = 100
  ): Task[
    (Vector[Successes], Vector[Typ[Term]], Set[EquationNode], Set[Term], Vector[Typ[Term]])
  ] =
    typs match {
      case Vector() => Task.now((accumSucc, accumFail, accumEqs, accumTerms, Vector()))
      case typ +: ys =>
        seekGoal(lp, typ, accumTerms, scale, maxSteps).flatMap {
          case (ss, eqs, terms) =>
            equationNodes = equationNodes union(eqs)
            termSet = termSet union(terms)
            if (ss.isEmpty) {
              failures.append(typ)
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
              update(())
              liberalChomper(
                lp,
                ys,
                accumSucc :+ ss,
                accumFail,
                accumEqs union eqs,
                accumTerms union terms,
                scale,
                maxSteps
              )
            }
        }
    }
}
