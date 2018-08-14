package provingground.learning
import provingground.{FiniteDistribution => FD, _}
import shapeless.HList._
import shapeless._

import scala.collection.immutable
import scala.language.higherKinds

object TangentFiniteDistribution{
  def av[X](x : FD[X], y: FD[X]) = (x ++ y) * 0.5
}

/**
  * resolving a general specification of a recursive generative model as finite distributions, depending on truncation;
  * the coefficients of the various generator nodes should be `Double`
  *
  * @param nodeCoeffSeq the various generator nodes with coefficients for various random variables and families
  * @param sd finite distributions from the initial state corresponding to random variables and families
  * @tparam State scala type of the initial state
  */
case class TangentFiniteDistribution[State, Boat](
    baseState: State,
    nodeCoeffSeq: NodeCoeffSeq[State, Boat, Double])(
    implicit sd: StateDistribution[State, FD]) extends GenTruncatedFiniteDistribution[State, Boat](nodeCoeffSeq) {

  /**
    * update coefficients, to be used in complex islands
    * @param dataSeq the new coefficients
    * @return [[TruncatedFiniteDistribution]] with updated coefficients
    */
  def updateAll(
      dataSeq: Seq[GeneratorNodeFamily.Value[_ <: HList, _, Double]]) =
    TangentFiniteDistribution(baseState, nodeCoeffSeq.updateAll(dataSeq))

    import StateDistribution._
    import TangentFiniteDistribution._

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
                                    epsilon: Double): FD[Y] =
    if (epsilon > 1) FD.empty[Y]
    else {
      import GeneratorNode._
      generatorNode match {
        case Init(input) =>
          value(tangentState)(input).purge(epsilon)
        case Map(f, input, _) =>
          varDist(tangentState)(input, epsilon).map(f).purge(epsilon)
        case MapOpt(f, input, _) =>
          varDist(tangentState)(input, epsilon).condMap(f).purge(epsilon)
        case ZipMap(f, input1, input2, _) =>
          val d1t = varDist(tangentState)(input1, epsilon).flatten
          val d2t = varDist(tangentState)(input2, epsilon).flatten
          val d1b = value(baseState)(input1)
          val d2b = value(baseState)(input2)
          av(
            d1b.zip(d2t).map { case (x, y) => f(x, y) },
            d1t.zip(d2b).map { case (x, y) => f(x, y) }
          ).purge(epsilon)
        case ZipMapOpt(f, input1, input2, _) =>
          val d1 = varDist(tangentState)(input1, epsilon).flatten
          val d2 = varDist(tangentState)(input2, epsilon).flatten
          val d1b = value(baseState)(input1)
          val d2b = value(baseState)(input2)
          av(d1.zip(d2b).condMap { case (x, y) => f(x, y) },
          d1b.zip(d2).condMap { case (x, y) => f(x, y) }
            ).purge(epsilon)
        case ZipFlatMap(baseInput, fiberVar, f, _) =>
          val baseDist = varDist(tangentState)(baseInput, epsilon).flatten
          val pmf1 =
            for {
              Weighted(x1, p1) <- value(baseState)(baseInput).pmf
              fiberDist = varDist(tangentState)(fiberVar(x1), epsilon / p1).flatten
              Weighted(x2, p2) <- fiberDist.pmf
            } yield Weighted(f(x1, x2), p1 * p2)
          val pmf2 =
            for {
              Weighted(x1, p1) <- baseDist.pmf
              fiberDist = value(baseState)(fiberVar(x1))
              Weighted(x2, p2) <- fiberDist.pmf
            } yield Weighted(f(x1, x2), p1 * p2)
          av(FD(pmf1), FD(pmf2)).flatten.purge(epsilon)
        case FlatMap(baseInput, fiberNode, _) =>
          val baseDist = varDist(tangentState)(baseInput, epsilon).flatten
          val pmf1 =
            for {
              Weighted(x1, p1) <- value(baseState)(baseInput).pmf
              fiberDist = nodeDist(tangentState)(fiberNode(x1), epsilon / p1).flatten
              Weighted(x2, p2) <- fiberDist.pmf
            } yield Weighted(x2, p1 * p2)
          val pmf2 =
            for {
              Weighted(x1, p1) <- baseDist.pmf
              fiberDist = value(baseState)(fiberNode(x1).output)
              Weighted(x2, p2) <- fiberDist.pmf
            } yield Weighted(x2, p1 * p2)
          av(FD(pmf1), FD(pmf2)).flatten.purge(epsilon)
        case FlatMapOpt(baseInput, fiberNodeOpt, _) =>
        val baseDist = varDist(tangentState)(baseInput, epsilon).flatten
        val pmf1 =
          for {
            Weighted(x1, p1) <- value(baseState)(baseInput).pmf
            (node : GeneratorNode[Y]) <- fiberNodeOpt(x1).toSet
            fiberDist = nodeDist(tangentState)(node, epsilon / p1).flatten
            Weighted(x2, p2) <- fiberDist.pmf
          } yield Weighted(x2, p1 * p2)
        val pmf2 =
          for {
            Weighted(x1, p1) <- baseDist.pmf
            (node : GeneratorNode[Y]) <- fiberNodeOpt(x1).toSet
            fiberDist = value(baseState)(node.output)
            Weighted(x2, p2) <- fiberDist.pmf
          } yield Weighted(x2, p1 * p2)
        av(FD(pmf1), FD(pmf2)).flatten.purge(epsilon)
        case FiberProductMap(quot, fiberVar, f, baseInput, _) =>
          val d1          = value(baseState)(baseInput)
          val byBase      = d1.pmf.groupBy { case Weighted(x, p) => quot(x) } // pmfs grouped by terms in quotient
          val baseWeights = byBase.mapValues(v => v.map(_.weight).sum) // weights of terms in the quotient
          val pmf1: immutable.Iterable[Weighted[Y]] =
            for {
              (z, pmf1) <- byBase // `z` is in the base, `pmf1` is all terms above `z`
              d2 = varDist(tangentState)(fiberVar(z), epsilon / baseWeights(z)).flatten // distribution of the fiber at `z`
              d = FD(pmf1)
                .zip(d2)
                .map { case (x1, x2) => f(x1, x2) }
                .flatten // mapped distribution over `z`
              wtd <- d.pmf // terms in pmf over z, should be flatMapped over `z`
            } yield wtd
          val d1T          = varDist(tangentState)(baseInput, epsilon).flatten
          val byBaseT      = d1.pmf.groupBy { case Weighted(x, p) => quot(x) } // pmfs grouped by terms in quotient
          val baseWeightsT = byBase.mapValues(v => v.map(_.weight).sum) // weights of terms in the quotient
          val pmf2: immutable.Iterable[Weighted[Y]] =
            for {
              (z, pmf1) <- byBase // `z` is in the base, `pmf1` is all terms above `z`
              d2 = value(baseState)(fiberVar(z)) // distribution of the fiber at `z`
              d = FD(pmf1)
                .zip(d2)
                .map { case (x1, x2) => f(x1, x2) }
                .flatten // mapped distribution over `z`
              wtd <- d.pmf // terms in pmf over z, should be flatMapped over `z`
            } yield wtd
          av(FD(pmf1.toVector), FD(pmf2.toVector) ).purge(epsilon)
        case tc: ThenCondition[o, Y] =>
          import tc._
          val base = nodeDist(tangentState)(gen, epsilon)
          import Sort._
          condition match {
            case _: All[_]    => base
            case c: Filter[_] => base.conditioned(c.pred).purge(epsilon)
            case Restrict(f)  => base.condMap(f).purge(epsilon)
          }
        case isle: Island[Y, State, o, b] =>
          import isle._
          val (isleInit, boat) = initMap(tangentState)                       // initial condition for island, boat to row back
          val isleOut          = varDist(isleInit)(islandOutput, epsilon) //result for the island
          isleOut
            .map(export(boat, _))
            .purge(epsilon) // exported result seen outside
        case isle: ComplexIsland[o, Y, State, b, Double] =>
          import isle._
          val (isleInit, boat, isleCoeffs) = initMap(tangentState)
          val isleOut =
            updateAll(isleCoeffs.toSeq) // coefficients changed to those for the island
              .varDist(isleInit)(islandOutput, epsilon)
          isleOut.map(export(boat, _)).purge(epsilon)
      }
    }
}
