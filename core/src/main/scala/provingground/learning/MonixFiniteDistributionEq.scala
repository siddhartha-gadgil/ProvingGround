package provingground.learning
import provingground.{FiniteDistribution => FD, _}
import shapeless.HList._
import shapeless._

import scala.language.higherKinds
import monix.eval._

import GeneratorVariables._, Expression._

import scala.concurrent._, duration._

import MonixFiniteDistributionEq._

import scala.util.Try

import GeneratorNode.Island
import cats.kernel.Eq

object MonixFiniteDistributionEq {
  def finalProb[Y](y: Y, rv: RandomVar[Y]) = FinalVal(Elem(y, rv))

  def initProb[Y](y: Y, rv: RandomVar[Y]) = InitialVal(Elem(y, rv))

}

case class EqDistMemo[State](
    varDists: Map[(State, RandomVar[_]), (Double, FD[_], Set[EquationNode])],
    nodeDists: Map[
      (State, GeneratorNode[_]),
      (Double, FD[_], Set[EquationNode])
    ]
) {
  def getVarDist[Y](
      state: State,
      rv: RandomVar[Y],
      cutoff: Double
  ): Option[(FD[Y], Set[EquationNode])] =
    varDists.get(state -> rv).filter(_._1 <= cutoff).map {
      case (_, fd, eqs) =>
        (fd.map[Y](_.asInstanceOf[Y]).purge(cutoff).safeNormalized, eqs)
    }

  def getNodeDist[Y](
      state: State,
      node: GeneratorNode[Y],
      cutoff: Double
  ): Option[(FD[Y], Set[EquationNode])] =
    nodeDists.get(state -> node).filter(_._1 <= cutoff).map {
      case (_, fd, eqs) =>
        (fd.map[Y](_.asInstanceOf[Y]).purge(cutoff).safeNormalized, eqs)
    }

  def +[Y](
      state: State,
      rv: RandomVar[Y],
      cutoff: Double,
      fd: FD[Y],
      eqs: Set[EquationNode]
  ): EqDistMemo[State] =
    EqDistMemo(
      varDists + ((state, rv) -> getVarDist(state, rv, cutoff)
        .map { case (fd, eq) => (cutoff, fd, eq) }
        .getOrElse((cutoff, fd, eqs))),
      nodeDists
    )

  def +[Y](
      state: State,
      node: GeneratorNode[Y],
      cutoff: Double,
      fd: FD[Y],
      eqs: Set[EquationNode]
  ): EqDistMemo[State] =
    EqDistMemo(
      varDists,
      nodeDists + ((state, node) -> getNodeDist(state, node, cutoff)
        .map { case (fd, eq) => (cutoff, fd, eq) }
        .getOrElse((cutoff, fd, eqs)))
    )

  @annotation.tailrec
  final def addVars(
      v: Vector[((State, RandomVar[_]), (Double, FD[_], Set[EquationNode]))],
      accum: EqDistMemo[State] = this
  ): EqDistMemo[State] =
    v match {
      case Vector() => accum
      case ((s, r: RandomVar[u]), (c, fd, eq)) +: ys =>
        addVars(ys, accum + [u] (s, r, c, fd.asInstanceOf[FD[u]], eq))
    }

  @annotation.tailrec
  final def addNodes(
      v: Vector[
        ((State, GeneratorNode[_]), (Double, FD[_], Set[EquationNode]))
      ],
      accum: EqDistMemo[State] = this
  ): EqDistMemo[State] =
    v match {
      case Vector() => accum
      case ((s, r: GeneratorNode[u]), (c, fd, eq)) +: ys =>
        addNodes(ys, accum + [u] (s, r, c, fd.asInstanceOf[FD[u]], eq))
    }

  def ++(that: EqDistMemo[State]): EqDistMemo[State] =
    addVars(that.varDists.toVector).addNodes(that.nodeDists.toVector)

}

object EqDistMemo {
  def empty[State]: EqDistMemo[State] = EqDistMemo[State](Map.empty, Map.empty)
}

abstract class GenMonixFiniteDistributionEq[State](
    nodeCoeffSeq: NodeCoeffSeq[State, Double],
    limit: FiniteDuration
)(implicit sd: StateDistribution[State, FD]) {
  import NodeCoeffs._
  import nodeCoeffSeq.find

  /**
    * finite distribution for a random variable
    * @param initState initial state
    * @param randomVar random variable whose distribution is returned
    * @param epsilon cutoff
    * @tparam Y values of the random variable
    * @return finite distribution for the given random variable
    */
  def varDist[Y](
      initState: State,
      maxDepth: Option[Int],
      halted: => Boolean,
      memo: EqDistMemo[State] = EqDistMemo.empty[State]
  )(
      randomVar: RandomVar[Y],
      epsilon: Double
  ): Task[(FD[Y], Set[EquationNode], EqDistMemo[State])] =
    if (epsilon > 1 || halted) Task.now((FD.empty[Y], Set(), memo))
    else if (maxDepth.map(_ < 1).getOrElse(false))
      Task.now((sd.value(initState)(randomVar), Set(), memo))
    else {
      val lookup =
        memo.getVarDist(initState, randomVar, epsilon).map {
          case (fd, eq) => Task.now((fd, eq, memo))
        }
      val resultT = lookup getOrElse {
        randomVar match {
          case RandomVar.AtCoord(randomVarFmly, fullArg) =>
            varFamilyDistFunc(initState, maxDepth, halted, memo)(randomVarFmly, epsilon)(fullArg)
              .map {
                case (fd, eqs, memo) => (fd.flatten.safeNormalized, eqs, memo)
              }
              .timeout(limit)
          case _ =>
            find(randomVar)
              .map { nc =>
                nodeCoeffDist(initState, maxDepth, halted, memo)(nc, epsilon, randomVar).map {
                  case (fd, eqs, memo) => (fd.flatten.safeNormalized, eqs, memo)
                }
              }
              .getOrElse(Task.now((FD.empty[Y], Set.empty[EquationNode], memo)))
              .timeout(limit)
        }
      }
      resultT.map {
        case (fd, eq, memo) =>
          (fd, eq, memo + (initState, randomVar, epsilon, fd, eq))
      }
    }

  def nodeCoeffDist[Y](initState: State, maxDepth: Option[Int], halted: => Boolean, memo: EqDistMemo[State])(
      nodeCoeffs: NodeCoeffs[State, Double, HNil, Y],
      epsilon: Double,
      rv: RandomVar[Y]
  ): Task[(FD[Y], Set[EquationNode], EqDistMemo[State])] =
    if (epsilon > 1 || halted) Task.now((FD.empty[Y], Set.empty[EquationNode], memo))
    else
      nodeCoeffs match {
        case Target(_) => Task.now((FD.empty[Y], Set.empty[EquationNode], memo))
        case bc: Cons[State, Double, HNil, Y] =>
          val p: Double = bc.headCoeff
          val (d: Task[(FD[Y], Set[EquationNode], EqDistMemo[State])], nc) =
            bc.headGen match {
              case gen: GeneratorNode[Y] =>
                val coeff = Coeff(gen)
                (nodeDist(initState, maxDepth, halted, memo)(gen, epsilon / p, coeff) map {
                  case (fd, eqs, m) => (fd * p, eqs, m)
                }) -> Coeff(gen)
              case _ =>
                throw new IllegalArgumentException(
                  "found node family for generating a simple random variable"
                )
            }
          for {
            pa <- d
            (a, eqa, ma) = pa
            eqc = eqa.map {
              case EquationNode(lhs, rhs) => EquationNode(lhs, rhs)
            }
            pb <- nodeCoeffDist(initState, maxDepth, halted, memo)(bc.tail, epsilon, rv)
            (b, eqb, mb) = pb
          } yield (a ++ b, eqc union eqb, memo ++ ma ++ mb)
      }

  def mapsSum[X, Y](
      first: Map[X, (FD[Y], Set[EquationNode], EqDistMemo[State])],
      second: Map[X, (FD[Y], Set[EquationNode], EqDistMemo[State])]
  ): Map[X, (FD[Y], Set[EquationNode], EqDistMemo[State])] =
    (for {
      k <- first.keySet union second.keySet
      v1 = first.getOrElse(
        k,
        (FD.empty[Y], Set.empty[EquationNode], EqDistMemo.empty[State])
      )
      v2 = second.getOrElse(
        k,
        (FD.empty[Y], Set.empty[EquationNode], EqDistMemo.empty[State])
      )
    } yield (k, (v1._1 ++ v2._1, v1._2 union v2._2, v1._3 ++ v2._3))).toMap

  def nodeCoeffFamilyDist[Dom <: HList, Y](
      initState: State,
      maxDepth: Option[Int],
      halted: => Boolean,
      memo: EqDistMemo[State]
  )(
      nodeCoeffs: NodeCoeffs[State, Double, Dom, Y],
      epsilon: Double
  )(arg: Dom): Task[(FD[Y], Set[EquationNode], EqDistMemo[State])] =
    if (epsilon > 1 || halted) Task.now((FD.empty[Y], Set.empty[EquationNode], memo))
    else
      nodeCoeffs match {
        case Target(_) => Task.now((FD.empty[Y], Set.empty[EquationNode], memo))
        case bc: Cons[State, Double, Dom, Y] =>
          val p = bc.headCoeff
          for {
            pa <- nodeFamilyDistFunc(initState, maxDepth, halted, memo)(bc.headGen, epsilon / p)(
              arg
            )
            pb <- nodeCoeffFamilyDist(initState, maxDepth, halted, memo)(bc.tail, epsilon)(
              arg
            )
          } yield ((pa._1 * p) ++ pb._1, pa._2 union pb._2, pa._3 ++ pb._3)
      }

  def varFamilyDistFunc[RDom <: HList, Y](
      initState: State,
      maxDepth: Option[Int],
      halted: => Boolean,
      memo: EqDistMemo[State]
  )(
      randomVarFmly: RandomVarFamily[RDom, Y],
      epsilon: Double
  )(arg: RDom): Task[(FD[Y], Set[EquationNode], EqDistMemo[State])] =
    if (epsilon > 1 || halted) Task.now((FD.empty[Y], Set.empty[EquationNode], memo))
    else
      find(randomVarFmly)
        .map { nc =>
          nodeCoeffFamilyDist(initState, maxDepth, halted, memo)(nc, epsilon)(arg)
        }
        .getOrElse(Task.now((FD.empty[Y], Set.empty[EquationNode], memo)))

  def nodeFamilyDistFunc[Dom <: HList, Y](
      initState: State,
      maxDepth: Option[Int],
      halted: => Boolean,
      memo: EqDistMemo[State]
  )(
      generatorNodeFamily: GeneratorNodeFamily[Dom, Y],
      epsilon: Double
  )(arg: Dom): Task[(FD[Y], Set[EquationNode], EqDistMemo[State])] =
    generatorNodeFamily match {
      case node: GeneratorNode[Y] =>
        assert(
          arg == HNil,
          s"looking for coordinate $arg in $node which is not a family"
        )
        val coeff = Coeff(node)
        nodeDist(initState, maxDepth, halted, memo)(node, epsilon, coeff).map {
          case (fd, eq, m) => (fd, eq, m)
        }
      case f: GeneratorNodeFamily.Pi[Dom, Y] =>
        val coeff =
          Coeff(f.nodes(arg))
        nodeDist(initState, maxDepth, halted, memo)(f.nodes(arg), epsilon, coeff).map {
          case (fd, eq, m) => (fd, eq, m)
        } // actually a task
      case f: GeneratorNodeFamily.PiOpt[Dom, Y] =>
        f.nodesOpt(arg)
          .map { (node) =>
            val coeff = Coeff(node)
            nodeDist(initState, maxDepth, halted, memo)(node, epsilon, coeff).map {
              case (fd, eq, m) => (fd, eq, m)
            }
          }
          .getOrElse(Task.pure((FD.empty[Y], Set.empty[EquationNode], memo)))
    }

  def nodeDist[Y](initState: State, maxDepth: Option[Int], halted: => Boolean, memo: EqDistMemo[State])(
      generatorNode: GeneratorNode[Y],
      epsilon: Double,
      coeff: Expression
  ): Task[(FD[Y], Set[EquationNode], EqDistMemo[State])]

}

/**
  * resolving a general specification of a recursive generative model as finite distributions, depending on truncation;
  * the coefficients of the various generator nodes should be `Double`
  *
  * @param nodeCoeffSeq the various generator nodes with coefficients for various random variables and families
  * @param sd finite distributions from the initial state corresponding to random variables and families
  * @tparam State scala type of the initial state
  */
case class MonixFiniteDistributionEq[State](
    nodeCoeffSeq: NodeCoeffSeq[State, Double],
    varWeight: Double,
    limit: FiniteDuration = 3.minutes
)(implicit sd: StateDistribution[State, FD])
    extends GenMonixFiniteDistributionEq[State](nodeCoeffSeq, limit) {

  /**
    * update coefficients, to be used in complex islands
    * @param dataSeq the new coefficients
    * @return [[MonixFiniteDistribution]] with updated coefficients
    */
  def updateAll(
      dataSeq: Seq[GeneratorNodeFamily.Value[_ <: HList, _, Double]]
  ) =
    MonixFiniteDistributionEq(nodeCoeffSeq.updateAll(dataSeq), varWeight)

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
  def nodeDist[Y](
      initState: State,
      maxDepth: Option[Int],
      halted: => Boolean,
      memo: EqDistMemo[State] = EqDistMemo.empty[State]
  )(
      generatorNode: GeneratorNode[Y],
      epsilon: Double,
      coeff: Expression
  ): Task[(FD[Y], Set[EquationNode], EqDistMemo[State])] =
    if (epsilon > 1 || halted)
      Task.now((FD.empty[Y], Set.empty[EquationNode], EqDistMemo.empty[State]))
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
                (fd.map(f).purge(epsilon), eqs.union(meqs), rm)
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
            val d1 = varDist(initState, maxDepth.map(_ - 1), halted, memo)(input1, epsilon).map {
              case (fd, eqs, rm1) => (fd.flatten, eqs, rm1)
            }
            val d2 = varDist(initState, maxDepth.map(_ - 1), halted, memo)(input2, epsilon).map {
              case (fd, eqs, rm2) => (fd.flatten, eqs, rm2)
            }
            Task.parZip2(d1, d2).map {
              case ((xd, eqx, mx), (yd, eqy, my)) =>
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
                  memo ++ mx ++ my
                )
            }
          case ZipMapOpt(f, input1, input2, output) =>
            val d1 = varDist(initState, maxDepth.map(_ - 1), halted, memo)(input1, epsilon).map {
              case (fd, eqs, rm) => (fd.flatten, eqs, rm)
            }
            val d2 = varDist(initState, maxDepth.map(_ - 1), halted, memo)(input2, epsilon).map {
              case (fd, eqs, rm) => (fd.flatten, eqs, rm)
            }
            Task.parZip2(d1, d2).map {
              case ((xd, eqx, mx), (yd, eqy, my)) =>
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
                  memo ++ mx ++ my
                )
            }
          case ZipFlatMap(baseInput, fiberVar, f, output) =>
            val baseDistT = varDist(initState, maxDepth.map(_ - 1), halted, memo)(baseInput, epsilon).map {
              case (fd, eqs, rm) => (fd.flatten, eqs, rm)
            }
            baseDistT.flatMap {
              case (baseDist, baseEqs, mb) =>
                val pmfEqT =
                  baseDist.pmf
                    .map {
                      case Weighted(x1, p1) =>
                        val fiberDistEqsT =
                          varDist(initState, maxDepth.map(_ - 1), halted, memo)(fiberVar(x1), epsilon / p1)
                            .map { case (fd, eqs, mf) => (fd.flatten, eqs, mf) }
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
                Task.gather(pmfEqT).map {
                  case (vveq) =>
                    (
                      FD(vveq.flatMap(_._1)),
                      vveq
                        .flatMap(_._2)
                        .toSet
                        .union(baseEqs),
                      vveq.map(_._3).foldLeft(memo ++ mb)(_ ++ _)
                    )
                }
            }
          case FlatMap(baseInput, fiberNode, output) =>
            val baseDistT = varDist(initState, maxDepth.map(_ - 1), halted, memo)(baseInput, epsilon).map {
              case (fd, eqs, rm) => (fd.flatten, eqs, rm)
            }
            baseDistT.flatMap {
              case (baseDist, baseEqs, mb) =>
                val pmfEqT =
                  baseDist.pmf
                    .map {
                      case Weighted(x1, p1) =>
                        val node = fiberNode(x1)
                        val fiberDistEqT =
                          nodeDist(initState, maxDepth, halted, memo)(node, epsilon / p1, coeff)
                            .map { case (fd, eqs, mf) => (fd.flatten, eqs, mf) }
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
                Task.gather(pmfEqT).map {
                  case (vveq) =>
                    (
                      FD(vveq.flatMap(_._1)),
                      vveq
                        .flatMap(_._2)
                        .toSet
                        .union(baseEqs),
                      vveq.map(_._3).foldLeft(memo ++ mb)(_ ++ _)
                    )
                }
            }
          case FlatMapOpt(baseInput, fiberNodeOpt, output) =>
            val baseDistT = varDist(initState, maxDepth.map(_ - 1), halted, memo)(baseInput, epsilon).map {
              case (fd, eqs, mb) => (fd.flatten, eqs, mb)
            }
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
                          nodeDist(initState, maxDepth, halted, memo)(node, epsilon / p1, coeff)
                            .map { case (fd, eqs, mf) => (fd.flatten, eqs, mf) }
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
                Task.gather(pmfEqT).map {
                  case (vveq) =>
                    (
                      FD(vveq.flatMap(_._1)),
                      vveq.flatMap(_._2).toSet,
                      vveq.map(_._3).foldLeft(memo ++ mb)(_ ++ _)
                    )
                }
            }
          case FiberProductMap(quot, fiberVar, f, baseInput, output) =>
            val d1T = varDist(initState, maxDepth.map(_ - 1), halted, memo)(baseInput, epsilon).map {
              case (fd, eqs, mb) => (fd.flatten, eqs, mb)
            }
            d1T.flatMap {
              case (d1, d1E, mb) =>
                val byBase      = d1.pmf.groupBy { case Weighted(x, _) => quot(x) } // pmfs grouped by terms in quotient
                val baseWeights = byBase.mapValues(v => v.map(_.weight).sum) // weights of terms in the quotient
                val pmfEqT =
                  byBase.map {
                    case (z, pmf1) => // `z` is in the base, `pmf1` is all terms above `z`
                      val d2T =
                        varDist(initState, maxDepth.map(_ - 1), halted, memo)(
                          fiberVar(z),
                          epsilon / baseWeights(z)
                        ).map { case (fd, eqs, mf) => (fd.flatten, eqs, mf) } // distribution of the fiber at `z`
                      d2T.map {
                        case (d2, d2E, mf) =>
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
                          (d.pmf, eqs union d2E, mf)
                      }
                  }
                Task.gather(pmfEqT).map {
                  case (vveq) =>
                    (
                      FD(vveq.flatMap(_._1)),
                      vveq
                        .flatMap(_._2)
                        .toSet
                        .union(d1E),
                      vveq.map(_._3).foldLeft(memo ++ mb)(_ ++ _)
                    )
                }
            }
          case tc: ThenCondition[o, Y] =>
            import tc._
            val base  = nodeDist(initState, maxDepth, halted, memo)(gen, epsilon, coeff)
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
                    // if (evSupp.nonEmpty)
                    //   Set(
                    //     EquationNode(
                    //       finEv,
                    //       evSupp
                    //         .map(x => finalProb(x, tc.output))
                    //         .reduce[Expression](Sum)
                    //     )
                    //   )
                    // else Set()
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
                    // if (evSupp.nonEmpty)
                    //   Set(
                    //     EquationNode(
                    //       finEv,
                    //       evSupp
                    //         .map(x => finalProb(x, tc.output))
                    //         .reduce[Expression](Sum)
                    //     )
                    //   )
                    // else Set()
                    (
                      fd.condMap(f)
                        .purge(epsilon),
                      (eqs union ceqs union evEq),
                      memo ++ rm
                    )
                }
            }
          case isle: Island[Y, State, o, b] =>
            import isle._
            val (isleInit, boat) = initMap(initState)(varWeight)                                   // initial condition for island, boat to row back
            val isleOut          = varDist(isleInit, maxDepth.map(_ - 1),halted, memo)(islandOutput(boat), epsilon) //result for the island
            isleOut
              .map {
                case (fd, eqs, rm) =>
                  val isleEqs =
                    eqs.map(_.mapVars(InIsle.variableMap(boat, isle)))
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
                          (IsleScale(boat, el) * -1) + Literal(1)
                        else IsleScale(boat, el) * InitialVal(el)
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
