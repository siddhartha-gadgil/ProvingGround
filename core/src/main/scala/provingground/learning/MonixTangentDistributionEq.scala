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
  def average[V, State](
      x: Task[(FD[V], Set[EquationNode], EqDistMemo[State])],
      y: Task[(FD[V], Set[EquationNode], EqDistMemo[State])]
  ): Task[(FD[V], Set[EquationNode], EqDistMemo[State])] =
    for {
      a <- x
      b <- y
    } yield ((a._1 ++ b._1).safeNormalized, (a._2 union b._2), a._3 ++ b._3)
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
    varWeight: Double,
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
      varWeight,
      baseState,
      baseEquations
    )

  def baseVal[Y](rd: RandomVar[Y]): Task[(FD[Y], Set[EquationNode])] =
    Task((sd.value(baseState)(rd), baseEquations))

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
  def nodeDist[Y](initState: State, maxDepth: Option[Int], halted: => Boolean, memo: EqDistMemo[State] = EqDistMemo.empty[State])(
      generatorNode: GeneratorNode[Y],
      epsilon: Double,
      coeff: Expression
  ): Task[(FD[Y], Set[EquationNode], EqDistMemo[State])] =
    if (epsilon > 1) Task.now((FD.empty[Y], Set.empty[EquationNode], memo))
    else {
      val lookup =
        memo
          .getNodeDist(initState, generatorNode, epsilon)
          .map { case (fd, eq) => Task.now((fd, eq, memo)) }
      import GeneratorNode._
      val resultT
          : Task[(FD[Y], Set[EquationNode], EqDistMemo[State])] = lookup getOrElse {
        generatorNode match {
          case Atom(x, _) =>
            Task(FD.unif(x), Set.empty[EquationNode], memo)
          case Init(input) =>
            val initDist = sd.value(initState)(input)
            val eqs = initDist.support.map { x =>
              EquationNode(finalProb(x, input), coeff * initProb(x, input))
            }
            Task(initDist, eqs, memo)
          case Map(f, input, output) =>
            varDist(initState, maxDepth, halted, memo)(input, epsilon).map {
              case (fd, eqs, rm) =>
                val meqs = fd.support.map { (x) =>
                  EquationNode(
                    finalProb(f(x), output),
                    coeff * finalProb(x, input)
                  )
                }
                (fd.map(f).purge(epsilon), eqs.union(meqs), memo ++ rm)
            }
          case MapOpt(f, input, output) =>
            varDist(initState, maxDepth, halted, memo)(input, epsilon).map {
              case (fd, eqs, rm) =>
                val meqs =
                  for {
                    x <- fd.support
                    y <- f(x)
                  } yield
                    EquationNode(
                      finalProb(y, output),
                      coeff * finalProb(x, input)
                    )
                (fd.condMap(f).purge(epsilon), eqs.union(meqs), memo ++ rm)
            }
          case ZipMap(f, input1, input2, output) =>
            val d1t = varDist(initState, maxDepth.map(_ - 1), halted, memo)(input1, epsilon).map {
              case (fd, eqs, m1) => (fd.flatten, eqs, m1)
            }
            val d1b = baseVal(input1)
            val d2t = varDist(initState, maxDepth.map(_ - 1), halted, memo)(input2, epsilon).map {
              case (fd, eqs, m2) => (fd.flatten, eqs, m2)
            }
            val d2b = baseVal(input2)
            val bt = Task.parZip2(d1b, d2t).map {
              case ((xd, eqx), (yd, eqy, my)) =>
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
                (
                  xd.zip(yd)
                    .map { case (x, y) => f(x, y) }
                    .purge(epsilon),
                  (eqx union eqy union meqs),
                  memo ++ my
                )
            }
            val tb = Task.parZip2(d1t, d2b).map {
              case ((xd, eqx, mx), (yd, eqy)) =>
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
                (
                  xd.zip(yd)
                    .map { case (x, y) => f(x, y) }
                    .purge(epsilon),
                  (eqx union eqy union meqs),
                  memo ++ mx
                )
            }
            average(bt, tb)
          case ZipMapOpt(f, input1, input2, output) =>
            val d1t = varDist(initState, maxDepth.map(_ - 1), halted, memo)(input1, epsilon).map {
              case (fd, eqs, rm) => (fd.flatten, eqs, rm)
            }
            val d1b = baseVal(input1)
            val d2t = varDist(initState, maxDepth.map(_ - 1), halted, memo)(input2, epsilon).map {
              case (fd, eqs, rm) => (fd.flatten, eqs, rm)
            }
            val d2b = baseVal(input2)
            val bt = Task.parZip2(d1b, d2t).map {
              case ((xd, eqx), (yd, eqy, my)) =>
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
                (
                  xd.zip(yd)
                    .condMap { case (x, y) => f(x, y) }
                    .purge(epsilon),
                  (eqx union eqy union meqs),
                  memo ++ my
                )
            }
            val tb = Task.parZip2(d1t, d2b).map {
              case ((xd, eqx, mx), (yd, eqy)) =>
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
                (
                  xd.zip(yd)
                    .condMap { case (x, y) => f(x, y) }
                    .purge(epsilon),
                  (eqx union eqy union meqs),
                  memo ++ mx
                )
            }
            average(bt, tb)
          case ZipFlatMap(baseInput, fiberVar, f, output) =>
            val baseDistT = varDist(initState, maxDepth.map(_ - 1),  halted, memo)(baseInput, epsilon).map {
              case (fd, eqs, rm) => (fd.flatten, eqs, rm)
            }
            average(
              baseVal(baseInput).flatMap {
                case (baseDist, baseEqs) =>
                  val pmfEqT =
                    baseDist.pmf
                      .map {
                        case Weighted(x1, p1) =>
                          val fiberDistEqsT =
                            varDist(initState, maxDepth.map(_ - 1), halted, memo)(fiberVar(x1), epsilon / p1)
                              .map {
                                case (fd, eqs, rm) => (fd.flatten, eqs, rm)
                              }
                          val tve =
                            fiberDistEqsT
                              .map {
                                case (fiberDist, fiberEqs, mf) =>
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
                                  (fibPMF, fibEqs union fiberEqs, mf)
                              }
                          tve
                      }
                  Task.parSequence(pmfEqT).map {
                    case (vveq) =>
                      (
                        FD(vveq.flatMap(_._1)),
                        vveq
                          .flatMap(_._2)
                          .toSet
                          .union(baseEqs),
                        vveq.map(_._3).foldLeft(memo)(_ ++ _)
                      )
                  }
              },
              baseDistT.flatMap {
                case (baseDist, baseEqs, mb) =>
                  val pmfEqT =
                    baseDist.pmf
                      .map {
                        case Weighted(x1, p1) =>
                          val fiberDistEqsT =
                            baseVal(fiberVar(x1))
                              .map { case (fd, eqs) => (fd.flatten, eqs) }
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
                  Task.parSequence(pmfEqT).map {
                    case (vveq) =>
                      (
                        FD(vveq.flatMap(_._1)),
                        vveq
                          .flatMap(_._2)
                          .toSet
                          .union(baseEqs),
                        memo ++ mb
                      )
                  }
              }
            )
          case FlatMap(baseInput, fiberNode, output) =>
            val baseDistT = varDist(initState, maxDepth.map(_ - 1), halted, memo)(baseInput, epsilon).map {
              case (fd, eqs, rm) => (fd.flatten, eqs, rm)
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
                            nodeDist(initState, maxDepth.map(_ - 1), halted, memo)(node, epsilon / p1, coeff)
                              .map {
                                case (fd, eqs, rm) => (fd.flatten, eqs, rm)
                              }
                          fiberDistEqT
                            .map {
                              case (fiberDist, fiberEqs, mf) =>
                                val fibPMF =
                                  fiberDist.pmf.map {
                                    case Weighted(x2, p2) =>
                                      Weighted(x2, p1 * p2)
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
                                (fibPMF, fibEqs union fiberEqs, mf)
                            }
                      }
                  Task.parSequence(pmfEqT).map {
                    case (vveq) =>
                      (
                        FD(vveq.flatMap(_._1)),
                        vveq
                          .flatMap(_._2)
                          .toSet
                          .union(baseEqs),
                        vveq.map(_._3).foldLeft(memo)(_ ++ _)
                      )
                  }
              },
              baseDistT.flatMap {
                case (baseDist, baseEqs, mb) =>
                  val pmfEqT =
                    baseDist.pmf
                      .map {
                        case Weighted(x1, p1) =>
                          val node = fiberNode(x1)
                          val fiberDistEqT =
                            baseVal(node.output)
                              .map { case (fd, eqs) => (fd.flatten, eqs) }
                          fiberDistEqT
                            .map {
                              case (fiberDist, fiberEqs) =>
                                val fibPMF =
                                  fiberDist.pmf.map {
                                    case Weighted(x2, p2) =>
                                      Weighted(x2, p1 * p2)
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
                  Task.parSequence(pmfEqT).map {
                    case (vveq) =>
                      (
                        FD(vveq.flatMap(_._1)),
                        vveq
                          .flatMap(_._2)
                          .toSet
                          .union(baseEqs),
                        memo ++ mb
                      )
                  }
              }
            )

          case FlatMapOpt(baseInput, fiberNodeOpt, output) =>
            val baseDistT = varDist(initState, maxDepth.map(_ - 1), halted, memo)(baseInput, epsilon).map {
              case (fd, eqs, rm) => (fd.flatten, eqs, rm)
            }
            average(
              baseVal(baseInput).flatMap {
                case (baseDist, baseEqs) =>
                  val pmfEqT =
                    baseDist.pmf
                      .flatMap {
                        case wt @ Weighted(x, _) =>
                          fiberNodeOpt(x).map(node => (wt, node))
                      }
                      .map {
                        case (Weighted(x1, p1), node) =>
                          val fiberDistEqT =
                            nodeDist(initState, maxDepth.map(_ - 1), halted, memo)(node, epsilon / p1, coeff)
                              .map {
                                case (fd, eqs, mf) => (fd.flatten, eqs, mf)
                              }
                          fiberDistEqT
                            .map {
                              case (fiberDist, fiberEqs, mf) =>
                                val fibPMF =
                                  fiberDist.pmf.map {
                                    case Weighted(x2, p2) =>
                                      Weighted(x2, p1 * p2)
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
                                (fibPMF, fibEqs union fiberEqs, mf)
                            }

                      }
                  Task.parSequence(pmfEqT).map {
                    case (vveq) =>
                      (
                        FD(vveq.flatMap(_._1)),
                        vveq.flatMap(_._2).toSet,
                        vveq.map(_._3).foldLeft(memo)(_ ++ _)
                      )
                  }
              },
              baseDistT.flatMap {
                case (baseDist, baseEqs, mb) =>
                  val pmfEqT =
                    baseDist.pmf
                      .flatMap {
                        case wt @ Weighted(x, _) =>
                          fiberNodeOpt(x).map(node => (wt, node))
                      }
                      .map {
                        case (Weighted(x1, p1), node) =>
                          val fiberDistEqT =
                            baseVal(node.output)
                              .map { case (fd, eqs) => (fd.flatten, eqs) }
                          fiberDistEqT
                            .map {
                              case (fiberDist, fiberEqs) =>
                                val fibPMF =
                                  fiberDist.pmf.map {
                                    case Weighted(x2, p2) =>
                                      Weighted(x2, p1 * p2)
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
                  Task.parSequence(pmfEqT).map {
                    case (vveq) =>
                      (
                        FD(vveq.flatMap(_._1)),
                        vveq.flatMap(_._2).toSet,
                        memo ++ mb
                      )
                  }
              }
            )
          case FiberProductMap(quot, fiberVar, f, baseInput, output) =>
            val d1T = varDist(initState, maxDepth.map(_ - 1), halted, memo)(baseInput, epsilon).map {
              case (fd, eqs, rm) => (fd.flatten, eqs, rm)
            }
            val d1Tb = baseVal(baseInput)
            average(
              d1T
                .flatMap {
                  case (d1, d1E, m1) =>
                    val byBase = d1.pmf.groupBy {
                      case Weighted(x, _) => quot(x)
                    } // pmfs grouped by terms in quotient
                    val baseWeights = byBase.view.mapValues(v => v.map(_.weight).sum) // weights of terms in the quotient
                    val pmfEqT =
                      byBase.map {
                        case (z, pmf1) => // `z` is in the base, `pmf1` is all terms above `z`
                          val d2T =
                            baseVal(fiberVar(z))
                              .map { case (fd, eqs) => (fd.flatten, eqs) } // distribution of the fiber at `z`
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
                    Task.parSequence(pmfEqT).map {
                      case (vveq) =>
                        (
                          FD(vveq.flatMap(_._1)),
                          vveq
                            .flatMap(_._2)
                            .toSet
                            .union(d1E),
                          memo ++ m1
                        )
                    }
                },
              d1Tb
                .flatMap {
                  case (d1, d1E) =>
                    val byBase = d1.pmf.groupBy {
                      case Weighted(x, _) => quot(x)
                    } // pmfs grouped by terms in quotient
                    val baseWeights = byBase.view.mapValues(v => v.map(_.weight).sum) // weights of terms in the quotient
                    val pmfEqT =
                      byBase.map {
                        case (z, pmf1) => // `z` is in the base, `pmf1` is all terms above `z`
                          val d2T =
                            varDist(initState, maxDepth.map(_ - 1), halted, memo)(
                              fiberVar(z),
                              epsilon / baseWeights(z)
                            ).map {
                                case (fd, eqs, mf) => (fd.flatten, eqs, mf)
                              } // distribution of the fiber at `z`
                          d2T.map {
                            case (d2, d2E, m2) =>
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
                              (d.pmf, eqs union d2E, m2)
                          }
                      }
                    Task.parSequence(pmfEqT).map {
                      case (vveq) =>
                        (
                          FD(vveq.flatMap(_._1)),
                          vveq
                            .flatMap(_._2)
                            .toSet
                            .union(d1E),
                          vveq.map(_._3).foldLeft(memo)(_ ++ _)
                        )
                    }
                }
            )

          case tc: ThenCondition[o, Y] =>
            import tc._
            val base  = nodeDist(initState, maxDepth.map(_ - 1), halted, memo)(gen, epsilon, coeff)
            val event = Event(tc.gen.output, tc.condition)
            val finEv = FinalVal(event)
            import Sort._
            condition match {
              case _: All[_] => base
              case c: Filter[_] =>
                base.map {
                  case (fd, eqs, rm) =>
                    val ceqs = fd.conditioned(c.pred).support.map { x =>
                      EquationNode(
                        finalProb(x, tc.output),
                        finalProb(x, tc.gen.output) / finEv
                      )
                    }
                    val evSupp = fd.conditioned(c.pred).support
                    val evEq: Set[EquationNode] =
                      evSupp.map(
                        x =>
                          EquationNode(
                            finEv,
                            finalProb(x, tc.output)
                          )
                      )
                    (
                      fd.conditioned(c.pred)
                        .purge(epsilon),
                      (eqs union ceqs union evEq),
                      memo ++ rm
                    )
                }
              case Restrict(f) =>
                base.map {
                  case (fd, eqs, rm) =>
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
                      evSupp.map(
                        x =>
                          EquationNode(
                            finEv,
                            finalProb(x, tc.output)
                          )
                      )
                    (
                      fd.condMap(f).purge(epsilon),
                      (eqs union ceqs union evEq),
                      memo ++ rm
                    )
                }
            }
          case isle: Island[Y, State, o, b] =>
            import isle._
            val (isleInit, boat) = initMap(initState)(varWeight)                                   // initial condition for island, boat to row back
            val isleOut          = varDist(isleInit, maxDepth.map(_ - 1), halted, memo)(islandOutput(boat), epsilon) //result for the island
            isleOut
              .map {
                case (fd, eqs, rm) =>
                  val isleEqs = eqs.map(_.mapVars(InIsle.variableMap(boat, isle)))
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
                      val rhs =
                        if (boat == el.element)
                          (IsleScale(boat) * -1) + Literal(1)
                        else IsleScale(boat) * InitialVal(el)
                      EquationNode(
                        InitialVal(InIsle(el, boat, isle)),
                        rhs
                      )
                    }
                  (
                    fd.map(export(boat, _))
                      .purge(epsilon),
                    (isleEqs union bridgeEqs union isleIn),
                    memo ++ rm
                  )
              } // exported result seen outside
        }
      }
      resultT.map {
        case (fd, eq, rm) =>
          (fd, eq, memo ++ rm + (initState, generatorNode, epsilon, fd, eq))
      }
    }
}
