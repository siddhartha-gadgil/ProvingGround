package provingground.learning
import provingground.{FiniteDistribution => FD, _}
import shapeless.HList._
import shapeless._

import scala.collection.immutable
import scala.language.higherKinds
import monix.eval._

object MonixTangentFiniteDistribution {
  def average[V](x: Task[FD[V]], y: Task[FD[V]]) =
    for {
      a <- x
      b <- y
    } yield (a ++ b) * 0.5
}

/**
  * resolving a general specification of a recursive generative model as finite distributions, depending on truncation;
  * the coefficients of the various generator nodes should be `Double`
  *
  * @param nodeCoeffSeq the various generator nodes with coefficients for various random variables and families
  * @param sd finite distributions from the initial state corresponding to random variables and families
  * @tparam State scala type of the initial state
  */
case class MonixTangentFiniteDistribution[State, Boat](
    nodeCoeffSeq: NodeCoeffSeq[State, Boat, Double],
    baseState: State)(implicit sd: StateDistribution[State, FD])
    extends GenMonixFiniteDistribution[State, Boat](nodeCoeffSeq) {

  import MonixTangentFiniteDistribution._

  /**
    * update coefficients, to be used in complex islands
    * @param dataSeq the new coefficients
    * @return [[MonixFiniteDistribution]] with updated coefficients
    */
  def updateAll(
      dataSeq: Seq[GeneratorNodeFamily.Value[_ <: HList, _, Double]]) =
    MonixFiniteDistribution(nodeCoeffSeq.updateAll(dataSeq))

  import StateDistribution._

  def baseVal[Y](rd: RandomVar[Y]) = Task(sd.value(baseState)(rd))

  /**
    * recursively determines the finite distribution given a generator node;
    * the main work is done here
    *
    * @param tangentState  initial state
    * @param generatorNode generator node to resolve
    * @param epsilon cutoff
    * @tparam Y values of the corresponding random variable
    * @return distribution corresponding to the `output` random variable
    */
  def nodeDist[Y](tangentState: State)(generatorNode: GeneratorNode[Y],
                                       epsilon: Double): Task[FD[Y]] =
    if (epsilon > 1) Task.now(FD.empty[Y])
    else {
      import GeneratorNode._
      generatorNode match {
        case Atom(x, input) =>
          Task(FD.unif(x))
        case Init(input) =>
          Task(sd.value(tangentState)(input))
        case Map(f, input, _) =>
          varDist(tangentState)(input, epsilon).map(_.map(f).purge(epsilon))
        case MapOpt(f, input, _) =>
          varDist(tangentState)(input, epsilon).map(_.condMap(f).purge(epsilon))
        case ZipMap(f, input1, input2, _) =>
          val d1  = varDist(tangentState)(input1, epsilon).map(_.flatten)
          val d1b = baseVal(input1)
          val d2  = varDist(tangentState)(input2, epsilon).map(_.flatten)
          val d2b = baseVal(input2)
          average(
            d1b.zip(d2).map {
              case (xd, yd) =>
                xd.zip(yd).map { case (x, y) => f(x, y) }.purge(epsilon)
            },
            d1.zip(d2b).map {
              case (xd, yd) =>
                xd.zip(yd).map { case (x, y) => f(x, y) }.purge(epsilon)
            }
          )
        case ZipMapOpt(f, input1, input2, _) =>
          val d1  = varDist(tangentState)(input1, epsilon).map(_.flatten)
          val d1b = baseVal(input1)
          val d2  = varDist(tangentState)(input2, epsilon).map(_.flatten)
          val d2b = baseVal(input1)
          average(
            d1b.zip(d2).map {
              case (xd, yd) =>
                xd.zip(yd).condMap { case (x, y) => f(x, y) }.purge(epsilon)
            },
            d1.zip(d2b).map {
              case (xd, yd) =>
                xd.zip(yd).condMap { case (x, y) => f(x, y) }.purge(epsilon)
            }
          )
        case ZipFlatMap(baseInput, fiberVar, f, _) =>
          val baseDistT =
            varDist(tangentState)(baseInput, epsilon).map(_.flatten)
          val baseDistTb = baseVal(baseInput)
          average(
            baseDistT.flatMap { (baseDist) =>
              val pmfT =
                baseDist.pmf
                  .map {
                    case Weighted(x1, p1) =>
                      val fiberDistT =
                        baseVal(fiberVar(x1))
                        // varDist(tangentState)(fiberVar(x1), epsilon / p1)
                          .map(_.flatten)
                      val tv =
                        fiberDistT
                          .map { fiberDist =>
                            for {
                              Weighted(x2, p2) <- fiberDist.pmf
                            } yield Weighted(f(x1, x2), p1 * p2)
                          }
                      tv
                  }
              Task.gather(pmfT).map((vv) => FD(vv.flatten))
            },
            baseDistTb.flatMap { (baseDist) =>
              val pmfT =
                baseDist.pmf
                  .map {
                    case Weighted(x1, p1) =>
                      val fiberDistT =
                        varDist(tangentState)(fiberVar(x1), epsilon / p1)
                          .map(_.flatten)
                      val tv =
                        fiberDistT
                          .map { fiberDist =>
                            for {
                              Weighted(x2, p2) <- fiberDist.pmf
                            } yield Weighted(f(x1, x2), p1 * p2)
                          }
                      tv
                  }
              Task.gather(pmfT).map((vv) => FD(vv.flatten))
            }
          )
        case FlatMap(baseInput, fiberNode, _) =>
          val baseDistT =
            varDist(tangentState)(baseInput, epsilon).map(_.flatten)
          val baseDistTb = baseVal(baseInput)
          average(
            baseDistT.flatMap { (baseDist) =>
              val pmfT =
                baseDist.pmf
                  .map {
                    case Weighted(x1, p1) =>
                      val node = fiberNode(x1)
                      val fiberDistT =
                        baseVal(node.output)
                      fiberDistT
                        .map { fiberDist =>
                          fiberDist.pmf.map {
                            case Weighted(x2, p2) => Weighted(x2, p1 * p2)
                          }
                        }
                  }
              Task.gather(pmfT).map((vv) => FD(vv.flatten))
            },
            baseDistTb.flatMap { (baseDist) =>
              val pmfT =
                baseDist.pmf
                  .map {
                    case Weighted(x1, p1) =>
                      val node = fiberNode(x1)
                      val fiberDistT =
                        nodeDist(tangentState)(node, epsilon / p1)
                          .map(_.flatten)
                      fiberDistT
                        .map { fiberDist =>
                          fiberDist.pmf.map {
                            case Weighted(x2, p2) => Weighted(x2, p1 * p2)
                          }
                        }
                  }
              Task.gather(pmfT).map((vv) => FD(vv.flatten))
            }
          )
        case FlatMapOpt(baseInput, fiberNodeOpt, _) =>
          val baseDistT =
            varDist(tangentState)(baseInput, epsilon).map(_.flatten)
          val baseDistTb = baseVal(baseInput)
          average(
            baseDistT.flatMap { (baseDist) =>
              val pmfT =
                baseDist.pmf.flatMap {
                  case wt @ Weighted(x, p) => fiberNodeOpt(x).map(wt -> _)
                }
                  .map {
                    case (Weighted(x1, p1), node) =>
                      val fiberDistT =
                        baseVal(node.output)
                      fiberDistT
                        .map { fiberDist =>
                          fiberDist.pmf.map {
                            case Weighted(x2, p2) => Weighted(x2, p1 * p2)
                          }
                        }
                  }
              Task.gather(pmfT).map((vv) => FD(vv.flatten))
            },
            baseDistTb.flatMap { (baseDist) =>
              val pmfT =
                baseDist.pmf.flatMap {
                  case wt @ Weighted(x, p) => fiberNodeOpt(x).map(wt -> _)
                }
                  .map {
                    case (Weighted(x1, p1), node) =>
                      val fiberDistT =
                        nodeDist(tangentState)(node, epsilon / p1)
                          .map(_.flatten)
                      fiberDistT
                        .map { fiberDist =>
                          fiberDist.pmf.map {
                            case Weighted(x2, p2) => Weighted(x2, p1 * p2)
                          }
                        }
                  }
              Task.gather(pmfT).map((vv) => FD(vv.flatten))
            }
          )
        case FiberProductMap(quot, fiberVar, f, baseInput, _) =>
          val d1T  = varDist(tangentState)(baseInput, epsilon).map(_.flatten)
          val d1Tb = baseVal(baseInput)
          average(
            d1T.flatMap { d1 =>
              val byBase      = d1.pmf.groupBy { case Weighted(x, p) => quot(x) } // pmfs grouped by terms in quotient
//              val baseWeights = byBase.mapValues(v => v.map(_.weight).sum) // weights of terms in the quotient
              val pmfT =
                byBase.map {
                  case (z, pmf1) => // `z` is in the base, `pmf1` is all terms above `z`
                    val d2T =
                      baseVal(fiberVar(z)) // distribution of the fiber at `z`
                    d2T.map { d2 =>
                      val d = FD(pmf1)
                        .zip(d2)
                        .map { case (x1, x2) => f(x1, x2) }
                        .flatten // mapped distribution over `z`
                      d.pmf // terms in pmf over z, should be flatMapped over `z`
                    }
                }
              Task.gather(pmfT).map((vv) => FD(vv.flatten))
            },
            d1Tb.flatMap { d1 =>
              val byBase      = d1.pmf.groupBy { case Weighted(x, p) => quot(x) } // pmfs grouped by terms in quotient
              val baseWeights = byBase.mapValues(v => v.map(_.weight).sum) // weights of terms in the quotient
              val pmfT =
                byBase.map {
                  case (z, pmf1) => // `z` is in the base, `pmf1` is all terms above `z`
                    val d2T =
                      varDist(tangentState)(fiberVar(z),
                                            epsilon / baseWeights(z))
                        .map(_.flatten) // distribution of the fiber at `z`
                    d2T.map { d2 =>
                      val d = FD(pmf1)
                        .zip(d2)
                        .map { case (x1, x2) => f(x1, x2) }
                        .flatten // mapped distribution over `z`
                      d.pmf // terms in pmf over z, should be flatMapped over `z`
                    }
                }
              Task.gather(pmfT).map((vv) => FD(vv.flatten))
            }
          )
        case tc: ThenCondition[o, Y] =>
          import tc._
          val base = nodeDist(tangentState)(gen, epsilon)
          import Sort._
          condition match {
            case _: All[_]    => base
            case c: Filter[_] => base.map(_.conditioned(c.pred).purge(epsilon))
            case Restrict(f)  => base.map(_.condMap(f).purge(epsilon))
          }
        case isle: Island[Y, State, o, b] =>
          import isle._
          val (isleInit, boat) = initMap(tangentState)                          // initial condition for island, boat to row back
          val isleOut          = varDist(isleInit)(islandOutput(boat), epsilon) //result for the island
          isleOut
            .map((fd) => fd.map(export(boat, _)).purge(epsilon)) // exported result seen outside
        case isle: ComplexIsland[o, Y, State, b, Double] =>
          import isle._
          val (isleInit, boat, isleCoeffs) = initMap(tangentState)
          val isleOut =
            updateAll(isleCoeffs.toSeq) // coefficients changed to those for the island
              .varDist(isleInit)(islandOutput(boat), epsilon)
          isleOut
            .map((fd) => fd.map(export(boat, _)).purge(epsilon)) // exported result seen outside
      }
    }
}
