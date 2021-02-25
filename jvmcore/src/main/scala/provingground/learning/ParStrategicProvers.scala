package provingground.learning

import provingground._
import HoTT._
import scala.collection.parallel._
import provingground.learning.TypSolver.LookupSolver
import scala.collection.immutable.Nil
import StrategicProvers.{SeekResult, Successes}

object ParStrategicProvers{

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
    val pde: ParDistEq = ParDistEq.fromParams(tgSolve)
    val (dist: ParMap[Term,Double], eqs: ParSet[EquationNode]) = pde.varDist(initState, None, false)(
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
      val nsr = parSeekTyp(negate(typ), initState, tg, cutoff, terms)
      SeekResult.or(sr, nsr)
    }
  }

  def polySolveTyp(
      typ: Typ[Term],
      initState: ParMapState,
      tg: TermGenParams,
      cutoffs: List[Double],
      terms: Set[Term]
  ): SeekResult = cutoffs match {
    case head :: next =>
      val sr = parSolveTyp(typ, initState, tg, head, terms)
      if (sr._1.nonEmpty) sr
      else {
        val nsr = polySolveTyp(negate(typ), initState, tg, next, terms)
        SeekResult.or(sr, nsr)
      }
    case Nil => SeekResult.empty
  }

    def parChomper(
      typs: Vector[Typ[Term]],
      initState: ParMapState,
      tg: TermGenParams,
      cutoffs: List[Double],
      accumSucc: Successes = Vector(),
      accumFail: Vector[Typ[Term]] = Vector(),
      accumEqs: Set[EquationNode] = Set(),
      accumTerms: Set[Term] = Set()
  ): (
      Successes,
      Vector[Typ[Term]], // failures
      Set[EquationNode],
      Set[Term]
  ) =
    typs match {
      case Vector() => (accumSucc, accumFail, accumEqs, accumTerms)
      case typ +: ys =>
        val (ss, eqs, terms) =
          polySolveTyp(typ, initState, tg, cutoffs, accumTerms)
        if (ss.isEmpty) {
          Utils.logger.debug(s"failed to prove $typ and ${negate(typ)}")
          parChomper(
            ys,
            initState,
            tg,
            cutoffs,
            accumSucc,
            accumFail :+ typ,
            accumEqs union eqs,
            accumTerms union (terms)
          )
        } else {
          ss.foreach(
            s => Utils.logger.debug(s"proved ${s._1} with proof ${s._3}")
          )
          Utils.logger.debug(s"goals remaining ${ys.size}")
          parChomper(
            ys,
            initState,
            tg,
            cutoffs,
            accumSucc ++ ss,
            accumFail,
            accumEqs union eqs,
            accumTerms union (terms)
          )
        }
    }


}