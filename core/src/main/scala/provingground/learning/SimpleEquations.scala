package provingground.learning
import provingground._, HoTT._

import GeneratorVariables._, Expression._, TermRandomVars._,
GeneratorNode.{Map => GMap, _}, TermGeneratorNodes._, TermGeneratorNodes.Base._
import scala.concurrent._
import monix.execution.Scheduler.Implicits.{global => monixglobal}

import DE._

object SimpleEquations {
  def groupTerms(
      fd: FiniteDistribution[Term]
  ): Map[Typ[HoTT.Term], FiniteDistribution[Term]] = {
    val suppMap: Map[Typ[Term], Set[Term]] =
      fd.support.groupBy(t => t.typ: Typ[Term])
    suppMap.map {
      case (typ, support) =>
        typ -> FiniteDistribution(support.map(x => Weighted(x, fd(x)))).safeNormalized
    }
  }

  def cutoffAppPairs(
      funcs: FiniteDistribution[ExstFunc],
      args: FiniteDistribution[Term],
      cutoff: Double
  ): Vector[(ExstFunc, Term)] = {
    val groupedArgs = groupTerms(args)
    for {
      Weighted(fn, p) <- funcs.pmf
      if p > cutoff
      argsFD <- groupedArgs.get(fn.dom).toVector
      Weighted(x, q) <- argsFD.pmf
      if (p * q > cutoff)
    } yield (fn, x)
  }

  def appEquation(fn: ExstFunc, arg: Term) = { // warning: may need more equations for conditioning
      EquationNode(
            finalProb(fn(arg), Terms),
            Coeff(applnNode) * finalProb(fn, Funcs) * finalProb(
              arg,
              termsWithTyp(fn.dom)
            )
          )
  }

  def allAppEquations(
      funcs: FiniteDistribution[ExstFunc],
      args: FiniteDistribution[Term],
      cutoff: Double
  ) = cutoffAppPairs(funcs, args, cutoff).map{case (fn, arg) => appEquation(fn, arg)}

  def unAppEquations(
      funcs: FiniteDistribution[ExstFunc],
      args: FiniteDistribution[Term],
      cutoff: Double
  ) = Future.sequence{
      for {
      Weighted(fn, p) <- funcs.pmf.toSet
      if p > cutoff
      Weighted(x, q) <- args.pmf.toSet
      if (p * q > cutoff)
    } yield Future{
        Unify.appln(fn.func, x).toSet.flatMap(
            (z : Term) => DE.formalEquations(z)
        )
    }
  }.map(_.flatten)
}
