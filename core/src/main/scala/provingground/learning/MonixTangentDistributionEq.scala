package provingground.learning
import provingground.{FiniteDistribution => FD, _}
import shapeless.HList._
import shapeless._

import scala.language.higherKinds
import monix.eval._

import GeneratorVariables._, Expression._

import scala.concurrent._, duration._

import MonixFiniteDistributionEq._, MonixTangentFiniteDistributionEq._

object MonixTangentFiniteDistributionEq {
  def average[V](
      x: Task[(FD[V], Set[EquationNode])],
      y: Task[(FD[V], Set[EquationNode])]
  ): Task[(FD[V], Set[EquationNode])] =
    for {
      a <- x
      b <- y
    } yield (a._1 ++ b._1).safeNormalized -> (a._2 union b._2)
}

/**
  * resolving a general specification of a recursive generative model as finite distributions, depending on truncation;
  * the coefficients of the various generator nodes should be `Double`
  *
  * @param nodeCoeffSeq the various generator nodes with coefficients for various random variables and families
  * @param sd finite distributions from the initial state corresponding to random variables and families
  * @tparam State scala type of the initial state
  */
case class MonixTangentFiniteDistributionEq[State](
                                                          nodeCoeffSeq: NodeCoeffSeq[State, Double],
                                                          baseState: State,
                                                          baseEquations: Set[EquationNode],
                                                          limit: FiniteDuration = 3.minutes
)(implicit sd: StateDistribution[State, FD])
    extends GenMonixFiniteDistributionEq[State](nodeCoeffSeq, limit) {

  /**
    * update coefficients, to be used in complex islands
    *
    * @param dataSeq the new coefficients
    * @return [[MonixFiniteDistribution]] with updated coefficients
    */
  def updateAll(
      dataSeq: Seq[GeneratorNodeFamily.Value[_ <: HList, _, Double]]
  ) =
    MonixTangentFiniteDistributionEq(
      nodeCoeffSeq.updateAll(dataSeq),
      baseState,
      baseEquations
    )

  def baseVal[Y](rd: RandomVar[Y]): Task[(FD[Y], Set[EquationNode])] =
    Task(sd.value(baseState)(rd) -> baseEquations)

  /**
    * recursively determines the finite distribution given a generator node;
    * the main work is done here
    *
    * @param initState  initial state
    * @param generatorNode generator node to resolve
    * @param epsilon cutoff
    * @tparam Y values of the corresponding random variable
    * @return distribution corresponding to the `output` random variable
    */
  def nodeDist[Y](initState: State, memo: EqDistMemo[State])(
      generatorNode: GeneratorNode[Y],
      epsilon: Double,
      coeff: Expression
  ): Task[(FD[Y], Set[EquationNode])] =
    if (epsilon > 1) Task.now(FD.empty[Y] -> Set.empty[EquationNode])
    else {
      import GeneratorNode._
      generatorNode match {
        case Atom(x, _) =>
          Task(FD.unif(x), Set.empty[EquationNode])
        case Init(input) =>
          val initDist = sd.value(initState)(input)
          val eqs = initDist.support.map { x =>
            EquationNode(finalProb(x, input), coeff * initProb(x, input))
          }
          Task(initDist, eqs)
        case Map(f, input, output) =>
          varDist(initState, memo)(input, epsilon).map {
            case (fd, eqs) =>
              val meqs = fd.support.map { (x) =>
                EquationNode(
                  finalProb(f(x), output),
                  coeff * finalProb(x, input)
                )
              }
              fd.map(f).purge(epsilon) -> eqs.union(meqs)
          }
        case MapOpt(f, input, output) =>
          varDist(initState, memo)(input, epsilon).map {
            case (fd, eqs) =>
              val meqs =
                for {
                  x <- fd.support
                  y <- f(x)
                } yield
                  EquationNode(
                    finalProb(y, output),
                    coeff * finalProb(x, input)
                  )
              fd.condMap(f).purge(epsilon) -> eqs.union(meqs)
          }
        case ZipMap(f, input1, input2, output) =>
          val d1t = varDist(initState, memo)(input1, epsilon).map {
            case (fd, eqs) => fd.flatten -> eqs
          }
          val d1b = baseVal(input1)
          val d2t = varDist(initState, memo)(input2, epsilon).map {
            case (fd, eqs) => fd.flatten -> eqs
          }
          val d2b = baseVal(input2)
          val bt = Task.parZip2(d1b, d2t).map {
            case ((xd, eqx), (yd, eqy)) =>
              val meqs =
                for {
                  x <- xd.support
                  y <- yd.support
                  z = f(x, y)
                } yield
                  EquationNode(
                    finalProb(z, output),
                    coeff * finalProb(x, input1) * finalProb(y, input2)
                  )
              xd.zip(yd)
                .map { case (x, y) => f(x, y) }
                .purge(epsilon) -> (eqx union eqy union meqs)
          }
          val tb = Task.parZip2(d1t, d2b).map {
            case ((xd, eqx), (yd, eqy)) =>
              val meqs =
                for {
                  x <- xd.support
                  y <- yd.support
                  z = f(x, y)
                } yield
                  EquationNode(
                    finalProb(z, output),
                    coeff * finalProb(x, input1) * finalProb(y, input2)
                  )
              xd.zip(yd)
                .map { case (x, y) => f(x, y) }
                .purge(epsilon) -> (eqx union eqy union meqs)
          }
          average(bt, tb)
        case ZipMapOpt(f, input1, input2, output) =>
          val d1t = varDist(initState, memo)(input1, epsilon).map {
            case (fd, eqs) => fd.flatten -> eqs
          }
          val d1b = baseVal(input1)
          val d2t = varDist(initState, memo)(input2, epsilon).map {
            case (fd, eqs) => fd.flatten -> eqs
          }
          val d2b = baseVal(input2)
          val bt = Task.parZip2(d1b, d2t).map {
            case ((xd, eqx), (yd, eqy)) =>
              val meqs =
                for {
                  x <- xd.support
                  y <- yd.support
                  z <- f(x, y)
                } yield
                  EquationNode(
                    finalProb(z, output),
                    coeff * finalProb(x, input1) * finalProb(y, input2)
                  )
              xd.zip(yd)
                .condMap { case (x, y) => f(x, y) }
                .purge(epsilon) -> (eqx union eqy union meqs)
          }
          val tb = Task.parZip2(d1t, d2b).map {
            case ((xd, eqx), (yd, eqy)) =>
              val meqs =
                for {
                  x <- xd.support
                  y <- yd.support
                  z <- f(x, y)
                } yield
                  EquationNode(
                    finalProb(z, output),
                    coeff * finalProb(x, input1) * finalProb(y, input2)
                  )
              xd.zip(yd)
                .condMap { case (x, y) => f(x, y) }
                .purge(epsilon) -> (eqx union eqy union meqs)
          }
          average(bt, tb)
        case ZipFlatMap(baseInput, fiberVar, f, output) =>
          val baseDistT = varDist(initState, memo)(baseInput, epsilon).map {
            case (fd, eqs) => fd.flatten -> eqs
          }
          average(
            baseVal(baseInput).flatMap {
              case (baseDist, baseEqs) =>
                val pmfEqT =
                  baseDist.pmf
                    .map {
                      case Weighted(x1, p1) =>
                        val fiberDistEqsT =
                          varDist(initState, memo)(fiberVar(x1), epsilon / p1)
                            .map { case (fd, eqs) => fd.flatten -> eqs }
                        val tve =
                          fiberDistEqsT
                            .map {
                              case (fiberDist, fiberEqs) =>
                                val fibPMF =
                                  for {
                                    Weighted(x2, p2) <- fiberDist.pmf
                                  } yield Weighted(f(x1, x2), p1 * p2)
                                val fibEqs =
                                  (for {
                                    Weighted(x2, _) <- fiberDist.pmf
                                  } yield
                                    EquationNode(
                                      finalProb(f(x1, x2), output),
                                      coeff * finalProb(x1, baseInput) * finalProb(
                                        x2,
                                        fiberVar(x1)
                                      )
                                    )).toSet
                                (fibPMF, fibEqs union fiberEqs)
                            }
                        tve
                    }
                Task.gather(pmfEqT).map {
                  case (vveq) =>
                    FD(vveq.flatMap(_._1)) -> vveq
                      .flatMap(_._2)
                      .toSet
                      .union(baseEqs)
                }
            },
            baseDistT.flatMap {
              case (baseDist, baseEqs) =>
                val pmfEqT =
                  baseDist.pmf
                    .map {
                      case Weighted(x1, p1) =>
                        val fiberDistEqsT =
                          baseVal(fiberVar(x1))
                            .map { case (fd, eqs) => fd.flatten -> eqs }
                        val tve =
                          fiberDistEqsT
                            .map {
                              case (fiberDist, fiberEqs) =>
                                val fibPMF =
                                  for {
                                    Weighted(x2, p2) <- fiberDist.pmf
                                  } yield Weighted(f(x1, x2), p1 * p2)
                                val fibEqs =
                                  (for {
                                    Weighted(x2, _) <- fiberDist.pmf
                                  } yield
                                    EquationNode(
                                      finalProb(f(x1, x2), output),
                                      coeff * finalProb(x1, baseInput) * finalProb(
                                        x2,
                                        fiberVar(x1)
                                      )
                                    )).toSet
                                (fibPMF, fibEqs union fiberEqs)
                            }
                        tve
                    }
                Task.gather(pmfEqT).map {
                  case (vveq) =>
                    FD(vveq.flatMap(_._1)) -> vveq
                      .flatMap(_._2)
                      .toSet
                      .union(baseEqs)
                }
            }
          )
        case FlatMap(baseInput, fiberNode, output) =>
          val baseDistT = varDist(initState, memo)(baseInput, epsilon).map {
            case (fd, eqs) => fd.flatten -> eqs
          }
          average(
            baseVal(baseInput).flatMap {
              case (baseDist, baseEqs) =>
                val pmfEqT =
                  baseDist.pmf
                    .map {
                      case Weighted(x1, p1) =>
                        val node = fiberNode(x1)
                        val fiberDistEqT =
                          nodeDist(initState, memo)(node, epsilon / p1, coeff)
                            .map { case (fd, eqs) => fd.flatten -> eqs }
                        fiberDistEqT
                          .map {
                            case (fiberDist, fiberEqs) =>
                              val fibPMF =
                                fiberDist.pmf.map {
                                  case Weighted(x2, p2) => Weighted(x2, p1 * p2)
                                }
                              val fibEqs =
                                fiberDist.pmf.map {
                                  case Weighted(x2, _) =>
                                    EquationNode(
                                      finalProb(x2, output),
                                      coeff * finalProb(x1, baseInput) * finalProb(
                                        x2,
                                        fiberNode(x1).output
                                      )
                                    )
                                }.toSet
                              (fibPMF, fibEqs union fiberEqs)
                          }
                    }
                Task.gather(pmfEqT).map {
                  case (vveq) =>
                    FD(vveq.flatMap(_._1)) -> vveq
                      .flatMap(_._2)
                      .toSet
                      .union(baseEqs)
                }
            },
            baseDistT.flatMap {
              case (baseDist, baseEqs) =>
                val pmfEqT =
                  baseDist.pmf
                    .map {
                      case Weighted(x1, p1) =>
                        val node = fiberNode(x1)
                        val fiberDistEqT =
                          baseVal(node.output)
                            .map { case (fd, eqs) => fd.flatten -> eqs }
                        fiberDistEqT
                          .map {
                            case (fiberDist, fiberEqs) =>
                              val fibPMF =
                                fiberDist.pmf.map {
                                  case Weighted(x2, p2) => Weighted(x2, p1 * p2)
                                }
                              val fibEqs =
                                fiberDist.pmf.map {
                                  case Weighted(x2, _) =>
                                    EquationNode(
                                      finalProb(x2, output),
                                      coeff * finalProb(x1, baseInput) * finalProb(
                                        x2,
                                        fiberNode(x1).output
                                      )
                                    )
                                }.toSet
                              (fibPMF, fibEqs union fiberEqs)
                          }
                    }
                Task.gather(pmfEqT).map {
                  case (vveq) =>
                    FD(vveq.flatMap(_._1)) -> vveq
                      .flatMap(_._2)
                      .toSet
                      .union(baseEqs)
                }
            }
          )

        case FlatMapOpt(baseInput, fiberNodeOpt, output) =>
          val baseDistT = varDist(initState, memo)(baseInput, epsilon).map {
            case (fd, eqs) => fd.flatten -> eqs
          }
          average(
            baseVal(baseInput).flatMap {
              case (baseDist, baseEqs) =>
                val pmfEqT =
                  baseDist.pmf
                    .flatMap {
                      case wt @ Weighted(x, _) =>
                        fiberNodeOpt(x).map(node => wt -> node)
                    }
                    .map {
                      case (Weighted(x1, p1), node) =>
                        val fiberDistEqT =
                          nodeDist(initState, memo)(node, epsilon / p1, coeff)
                            .map { case (fd, eqs) => fd.flatten -> eqs }
                        fiberDistEqT
                          .map {
                            case (fiberDist, fiberEqs) =>
                              val fibPMF =
                                fiberDist.pmf.map {
                                  case Weighted(x2, p2) => Weighted(x2, p1 * p2)
                                }
                              val fibEqs =
                                fiberDist.pmf.map {
                                  case Weighted(x2, _) =>
                                    EquationNode(
                                      finalProb(x2, output),
                                      coeff * finalProb(x1, baseInput) * finalProb(
                                        x2,
                                        node.output
                                      )
                                    )
                                }.toSet
                              (fibPMF, fibEqs union fiberEqs)
                          }

                    }
                Task.gather(pmfEqT).map {
                  case (vveq) =>
                    FD(vveq.flatMap(_._1)) -> vveq.flatMap(_._2).toSet
                }
            },
            baseDistT.flatMap {
              case (baseDist, baseEqs) =>
                val pmfEqT =
                  baseDist.pmf
                    .flatMap {
                      case wt @ Weighted(x, _) =>
                        fiberNodeOpt(x).map(node => wt -> node)
                    }
                    .map {
                      case (Weighted(x1, p1), node) =>
                        val fiberDistEqT =
                          baseVal(node.output)
                            .map { case (fd, eqs) => fd.flatten -> eqs }
                        fiberDistEqT
                          .map {
                            case (fiberDist, fiberEqs) =>
                              val fibPMF =
                                fiberDist.pmf.map {
                                  case Weighted(x2, p2) => Weighted(x2, p1 * p2)
                                }
                              val fibEqs =
                                fiberDist.pmf.map {
                                  case Weighted(x2, _) =>
                                    EquationNode(
                                      finalProb(x2, output),
                                      coeff * finalProb(x1, baseInput) * finalProb(
                                        x2,
                                        node.output
                                      )
                                    )
                                }.toSet
                              (fibPMF, fibEqs union fiberEqs)
                          }

                    }
                Task.gather(pmfEqT).map {
                  case (vveq) =>
                    FD(vveq.flatMap(_._1)) -> vveq.flatMap(_._2).toSet
                }
            }
          )
        case FiberProductMap(quot, fiberVar, f, baseInput, output) =>
          val d1T = varDist(initState, memo)(baseInput, epsilon).map {
            case (fd, eqs) => fd.flatten -> eqs
          }
          val d1Tb = baseVal(baseInput)
          average(
            d1T.flatMap {
              case (d1, d1E) =>
                val byBase      = d1.pmf.groupBy { case Weighted(x, _) => quot(x) } // pmfs grouped by terms in quotient
              val baseWeights = byBase.mapValues(v => v.map(_.weight).sum) // weights of terms in the quotient
              val pmfEqT =
                byBase.map {
                  case (z, pmf1) => // `z` is in the base, `pmf1` is all terms above `z`
                    val d2T =
                      baseVal(fiberVar(z))
                        .map { case (fd, eqs) => fd.flatten -> eqs } // distribution of the fiber at `z`
                    d2T.map {
                      case (d2, d2E) =>
                        val d = FD(pmf1)
                          .zip(d2)
                          .map { case (x1, x2) => f(x1, x2) }
                          .flatten // mapped distribution over `z`
                      val eqs = FD(pmf1)
                        .zip(d2)
                        .support
                        .map {
                          case (x1, x2) =>
                            EquationNode(
                              finalProb(f(x1, x2), output),
                              coeff * finalProb(x1, baseInput)
                                * finalProb(x2, fiberVar(z))
                            )
                        }
                        (d.pmf, eqs union d2E)
                    }
                }
                Task.gather(pmfEqT).map {
                  case (vveq) =>
                    FD(vveq.flatMap(_._1)) -> vveq
                      .flatMap(_._2)
                      .toSet
                      .union(d1E)
                }
            }
            ,
            d1Tb.flatMap {
              case (d1, d1E) =>
                val byBase      = d1.pmf.groupBy { case Weighted(x, _) => quot(x) } // pmfs grouped by terms in quotient
              val baseWeights = byBase.mapValues(v => v.map(_.weight).sum) // weights of terms in the quotient
              val pmfEqT =
                byBase.map {
                  case (z, pmf1) => // `z` is in the base, `pmf1` is all terms above `z`
                    val d2T =
                      varDist(initState, memo)(fiberVar(z), epsilon / baseWeights(z))
                        .map { case (fd, eqs) => fd.flatten -> eqs } // distribution of the fiber at `z`
                    d2T.map {
                      case (d2, d2E) =>
                        val d = FD(pmf1)
                          .zip(d2)
                          .map { case (x1, x2) => f(x1, x2) }
                          .flatten // mapped distribution over `z`
                      val eqs = FD(pmf1)
                        .zip(d2)
                        .support
                        .map {
                          case (x1, x2) =>
                            EquationNode(
                              finalProb(f(x1, x2), output),
                              coeff * finalProb(x1, baseInput)
                                * finalProb(x2, fiberVar(z))
                            )
                        }
                        (d.pmf, eqs union d2E)
                    }
                }
                Task.gather(pmfEqT).map {
                  case (vveq) =>
                    FD(vveq.flatMap(_._1)) -> vveq
                      .flatMap(_._2)
                      .toSet
                      .union(d1E)
                }
            }
          )

        case tc: ThenCondition[o, Y] =>
          import tc._
          val base  = nodeDist(initState, memo)(gen, epsilon, coeff)
          val event = Event(tc.gen.output, tc.condition)
          val finEv = FinalVal(event)
          import Sort._
          condition match {
            case _: All[_] => base
            case c: Filter[_] =>
              base.map {
                case (fd, eqs) =>
                  val ceqs = fd.conditioned(c.pred).support.map { x =>
                    EquationNode(
                      finalProb(x, tc.output),
                      finalProb(x, tc.gen.output) / finEv
                    )
                  }
                  val evSupp = fd.conditioned(c.pred).support
                  val evEq: Set[EquationNode] =
                    if (evSupp.nonEmpty)
                      Set(
                        EquationNode(
                          finEv,
                          evSupp
                            .map(x => finalProb(x, tc.output))
                            .reduce[Expression](Sum)
                        )
                      )
                    else Set()
                  fd.conditioned(c.pred)
                    .purge(epsilon) -> (eqs union ceqs union evEq)
              }
            case Restrict(f) =>
              base.map {
                case (fd, eqs) =>
                  val ceqs = for {
                    x <- fd.support
                    y <- f(x)
                  } yield
                    EquationNode(
                      finalProb(y, tc.output),
                      finalProb(x, tc.gen.output) / finEv
                    )
                  val evSupp = fd.condMap(f).flatten.support
                  val evEq: Set[EquationNode] =
                    if (evSupp.nonEmpty)
                      Set(
                        EquationNode(
                          finEv,
                          evSupp
                            .map(x => finalProb(x, tc.output))
                            .reduce[Expression](Sum)
                        )
                      )
                    else Set()
                  fd.condMap(f).purge(epsilon) -> (eqs union ceqs union evEq)
              }
          }
        case isle: Island[Y, State, o, b] =>
          import isle._
          val (isleInit, boat) = initMap(initState)                             // initial condition for island, boat to row back
          val isleOut          = varDist(isleInit, memo)(islandOutput(boat), epsilon) //result for the island
          isleOut
            .map {
              case (fd, eqs) =>
                val isleEqs = eqs.map(_.mapVars((x) => InIsle(x, boat, isle)))
                val bridgeEqs = fd.support.map { x =>
                  EquationNode(
                    finalProb(export(boat, x), isle.output),
                    coeff * FinalVal(
                      InIsle(Elem(x, isle.islandOutput(boat)), boat, isle)
                    )
                  )
                }
                val initVarElems = eqs
                  .flatMap { (eq) =>
                    Expression.varVals(eq.rhs)
                  }
                  .collect {
                    case InitialVal(Elem(el, rv)) => Elem(el, rv)
                  }
                val isleIn: Set[EquationNode] =
                  initVarElems.map { el =>
                    EquationNode(
                      InitialVal(InIsle(el, boat, isle)),
                      IsleScale(boat, el) * InitialVal(el)
                    )
                  }
                // pprint.log(isleIn.size)
                fd.map(export(boat, _))
                  .purge(epsilon) -> (isleEqs union bridgeEqs union isleIn)
            } // exported result seen outside
        case isle: ComplexIsland[o, Y, State, b, Double] =>
          import isle._
          val (isleInit, boat, isleCoeffs) = initMap(initState)
          val isleOut =
            updateAll(isleCoeffs.toSeq) // coefficients changed to those for the island
              .varDist(isleInit, memo)(islandOutput(boat), epsilon)
          isleOut
            .map {
              case (fd, eqs) => fd.map(export(boat, _)).purge(epsilon) -> eqs
            } // exported result seen outside
      }
    }
}
