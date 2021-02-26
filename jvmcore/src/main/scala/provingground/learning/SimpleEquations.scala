package provingground.learning
import provingground._, HoTT._

import GeneratorVariables._, Expression._, TermRandomVars._,
GeneratorNode.{Map => GMap, _}, TermGeneratorNodes._, TermGeneratorNodes.Base._
import scala.concurrent._
import monix.execution.Scheduler.Implicits.{global => monixglobal}
import monix.eval._

import DE._
import scala.concurrent._, duration._
import scala.math.Ordering.Double.TotalOrdering
import scala.util._

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
      argsFD         <- groupedArgs.get(fn.dom).toVector
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
  ) = cutoffAppPairs(funcs, args, cutoff).map {
    case (fn, arg) => appEquation(fn, arg)
  }

  def unAppEquations(
      funcs: FiniteDistribution[ExstFunc],
      args: FiniteDistribution[Term],
      cutoff: Double
  ) =
    Future
      .sequence {
        for {
          Weighted(fn, p) <- funcs.pmf.toSet
          if p > cutoff
          Weighted(x, q) <- args.pmf.toSet
          if (p * q > cutoff)
        } yield
          Future {
            Unify
              .appln(fn.func, x)
              .toSet
              .flatMap { (z: Term) =>
                val appEquations: Set[EquationNode] =
                  Set(
                    EquationNode(
                      finalProb(z, Terms),
                      Coeff(unifApplnNode) * finalProb(fn, Funcs) * finalProb(
                        x,
                        Terms
                      )
                    ),
                    EquationNode(
                      finalProb(fn, Funcs),
                      finalProb(fn.func, Terms) /
                        FinalVal(Event(Terms, Sort.Restrict(FuncOpt)))
                    ),
                    EquationNode(
                      FinalVal(Event(Terms, Sort.Restrict(FuncOpt))),
                      finalProb(fn.func, Terms)
                    ),
                    EquationNode(
                      finalProb(fn.func, Terms),
                      Coeff(Init(Terms)) * InitialVal(Elem(fn.func, Terms))
                    ),
                    EquationNode(
                      finalProb(x, Terms),
                      Coeff(Init(Terms)) * InitialVal(Elem(x, Terms))
                    )
                  )
                appEquations union (DE.formalEquations(z) union
                  (DE
                    .formalTypEquations(z.typ)))
              }
          }
      }
      .map(_.flatten)

  def taskUnAppEquations(
      funcs: FiniteDistribution[ExstFunc],
      args: FiniteDistribution[Term],
      cutoff: Double,
      prevCutoff: Option[Double],
      accumTerms: Set[Term],
      accumTyps: Set[Typ[Term]],
      limit: Long
  ): Task[(Set[EquationNode], Set[Term], Set[Typ[Term]])] = {
    val funcWeigths =
      funcs.flatten.pmf.sortBy(x => -x.weight).takeWhile(_.weight > cutoff)
    val argWeights = args.flatten.pmf.sortBy(x => -x.weight)
    Task
      .parSequence {
        for {
          Weighted(fn, p) <- funcWeigths
          // if p > cutoff
          Weighted(x, q) <- argWeights
          if (p * q > cutoff && prevCutoff
            .map(cf => cf >= p * q)
            .getOrElse(true))
        } yield
          Task {
            Unify
              .appln(fn.func, x)
              .toSet
              .map { (z: Term) =>
                val appEquations: Set[EquationNode] =
                  Set(
                    EquationNode(
                      finalProb(z, Terms),
                      Coeff(unifApplnNode) * finalProb(fn, Funcs) * finalProb(
                        x,
                        Terms
                      )
                    ),
                    EquationNode(
                      finalProb(fn, Funcs),
                      finalProb(fn.func, Terms) /
                        FinalVal(Event(Terms, Sort.Restrict(FuncOpt)))
                    ),
                    EquationNode(
                      FinalVal(Event(Terms, Sort.Restrict(FuncOpt))),
                      finalProb(fn.func, Terms)
                    ),
                    EquationNode(
                      finalProb(fn.func, Terms),
                      Coeff(Init(Terms)) * InitialVal(Elem(fn.func, Terms))
                    ),
                    EquationNode(
                      finalProb(x, Terms),
                      Coeff(Init(Terms)) * InitialVal(Elem(x, Terms))
                    )
                  )
                // val formalTermEqs: Set[EquationNode] =
                //   if (!accumTerms.contains(z)) DE.formalEquations(z) else Set()
                val formalTypEqs: Set[EquationNode] =
                  if (!accumTyps.contains(z.typ)) DE.formalTypEquations(z.typ)
                  else Set()
                (
                  (appEquations union (// formalTermEqs union
                  formalTypEqs)),
                  z,
                  z.typ: Typ[Term]
                )
              }
          }.timeout((limit / 2).millis).onErrorRecoverWith{
            case _: TimeoutException =>
              // Oh, we know about timeouts, recover it
              Task.now(Set())
          }
      }
      .map { x =>
        x.flatten
      }
      .map { y =>
        val eqnVec = y.flatMap(_._1)
        def normalized =
          JvmUtils.gatherMapSet(
            eqnVec.grouped(10000).toVector,
            Set(),
            TermData.isleNormalize(_),
            Some(limit)
          )
        (
          normalized,
          y.map(_._2).toSet -- accumTerms,
          y.map(_._3).toSet -- accumTyps
        )
      }
  }

  def timedUnAppEquations(
      funcs: FiniteDistribution[ExstFunc],
      args: FiniteDistribution[Term],
      cutoff: Double,
      maxTime: FiniteDuration,
      minCutoff: Option[Double],
      cutoffScale: Double = 2,
      accumTerms: Set[Term] = Set(),
      accumTyps: Set[Typ[Term]] = Set(),
      prevCutoff: Option[Double] = None,
      accum: Set[EquationNode] = Set()
  ): Task[Set[EquationNode]] =
    taskUnAppEquations(
      funcs,
      args,
      cutoff,
      prevCutoff,
      accumTerms,
      accumTyps,
      System.currentTimeMillis() + maxTime.toMillis
    ).timed
    // .timeout(maxTime)
    // .materialize
      .map {
        case (t, (result, newTerms, newTyps)) =>
          JvmUtils.logger.debug(s"new terms: ${newTerms.size}")
          JvmUtils.logger.debug(s"new types: ${newTyps.size}")
          val pmin = funcs.pmf.map(_.weight).filter(_ > 0).min
          val qmin = args.pmf.map(_.weight).filter(_ > 0).min
          if (t > maxTime)
            JvmUtils.logger.debug(
              s"ran for time ${t.toSeconds}, exceeding time limit $maxTime, with cutoff ${cutoff}"
            )
          else
            JvmUtils.logger.debug(
              s"ran for time ${t.toSeconds}, less than the time limit $maxTime, with cutoff ${cutoff}; running again"
            )
          if (pmin * qmin > cutoff)
            JvmUtils.logger.debug(s"all pairs considered with cutoff $cutoff")
          ((t < maxTime) && (pmin * qmin < cutoff), (result, newTerms, newTyps)) // ensuring not all pairs already used
        // case Failure(throwable) =>
        //   throwable match {
        //     case _: TimeoutException =>
        //       JvmUtils.logger.debug(s"Timed out with time limit $maxTime")
        //     case _ =>
        //       JvmUtils.logger.error(
        //         s"Instead of timeout, unexpected exception ${throwable.getMessage()}"
        //       )
        //   }
        // (
        //   false,
        //   (Set.empty[EquationNode], Set.empty[Term], Set.empty[Typ[Term]])
        // )

      }
      .flatMap {
        case (b, (result, newTerms, newTyps)) if b =>
          val newCutoff = cutoff / cutoffScale
          if (minCutoff.map(m => newCutoff > m).getOrElse(true)) timedUnAppEquations(
            funcs,
            args,
            cutoff / cutoffScale,
            maxTime,
            minCutoff,
            cutoffScale,
            accumTerms union (newTerms),
            accumTyps union (newTyps),
            Some(cutoff),
            accum union result
          )
        else 
          {
            JvmUtils.logger.debug(s"reached cutoff limit $minCutoff")
            Task(accum union(result))
          }
        case (b, (result, _, _)) =>
          Task(accum union result)
      }

}
