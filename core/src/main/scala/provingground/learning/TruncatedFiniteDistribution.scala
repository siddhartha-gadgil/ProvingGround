package provingground.learning
import provingground.{FiniteDistribution => FD, _}
import shapeless.HList._
import shapeless._

import scala.collection.immutable
import scala.language.higherKinds

object TruncatedFiniteDistribution {
  import GeometricDistribution._

  object Geom
      extends TruncatedFiniteDistribution[VarValueSet[FD]](
        nodeCoeffSeq,
        0.3
      )

  def geomFD(epsilon: Double): FD[Int] =
    Geom.varDist(initState)(GeomVar, epsilon)
}

abstract class GenTruncatedFiniteDistribution[State](
    nodeCoeffSeq: NodeCoeffSeq[State, Double])(
    implicit sd: StateDistribution[State, FD]) {
  import NodeCoeffs._
  import StateDistribution._
  import nodeCoeffSeq.find

  /**
    * finite distribution for a random variable
    * @param initState initial state
    * @param randomVar random variable whose distribution is returned
    * @param epsilon cutoff
    * @tparam Y values of the random variable
    * @return finite distribution for the given random variable
    */
  def varDist[Y](initState: State)(randomVar: RandomVar[Y],
                                   epsilon: Double): FD[Y] =
    if (epsilon > 1) FD.empty[Y]
    else
      randomVar match {
        case RandomVar.AtCoord(randomVarFmly, fullArg) =>
          varFamilyDist(initState)(randomVarFmly, epsilon)
            .getOrElse(fullArg, FD.empty[Y])
            .flatten
            .safeNormalized
        case _ =>
          find(randomVar)
            .map { nc =>
              nodeCoeffDist(initState)(nc, epsilon).flatten.safeNormalized
            }
            .getOrElse(FD.empty[Y])
      }

  /**
    * finite distribution for a list of random variables
    * @param initState initial state
    * @param vl list of random variables
    * @param epsilon cutoff
    * @tparam Dom the `HList` giving the type of the variable list
    * @return finite distribution of `Dom`
    */
  def varListDist[Dom <: HList](initState: State)(vl: RandomVarList[Dom],
                                                  epsilon: Double): FD[Dom] =
    vl match {
      case RandomVarList.Nil => FD.unif(HNil)
      case RandomVarList.Cons(head, tail) =>
        varDist(initState)(head, epsilon)
          .zip(varListDist(initState)(tail, epsilon))
          .map { case (x, y) => x :: y }
    }

  def nodeCoeffDist[Y](initState: State)(
      nodeCoeffs: NodeCoeffs[State, Double, HNil, Y],
      epsilon: Double): FD[Y] =
    if (epsilon > 1) FD.empty[Y]
    else
      nodeCoeffs match {
        case Target(_) => FD.empty[Y]
        case bc: Cons[State, Double, HNil, Y] =>
          val p: Double = bc.headCoeff
          val d: FD[Y] =
            bc.headGen match {
              case gen: GeneratorNode[Y] =>
                nodeDist(initState)(gen, epsilon / p) * p
              case _ =>
                throw new IllegalArgumentException(
                  "found node family for generating a simple random variable")
            }
          d ++ nodeCoeffDist(initState)(bc.tail, epsilon)
      }

  def mapsSum[X, Y](first: Map[X, FD[Y]],
                    second: Map[X, FD[Y]]): Map[X, FD[Y]] =
    (for {
      k <- first.keySet union second.keySet
      v = first.getOrElse(k, FD.empty[Y]) ++ second.getOrElse(k, FD.empty[Y])
    } yield (k, v)).toMap

  def nodeCoeffFamilyMap[Dom <: HList, Y](initState: State)(
      nodeCoeffs: NodeCoeffs[State, Double, Dom, Y],
      baseDist: FD[Dom],
      epsilon: Double): Map[Dom, FD[Y]] =
    if (epsilon > 1) Map()
    else
      nodeCoeffs match {
        case Target(_) => Map()
        case bc: BaseCons[State, Double, Dom, Y] =>
          val p = bc.headCoeff
          mapsSum(nodeFamilyDist(initState)(bc.headGen, baseDist, epsilon),
                  nodeCoeffFamilyMap(initState)(bc.tail,
                                                baseDist,
                                                epsilon / (1.0 - p)))
        case rc: RecCons[State, Double, Dom, Y] =>
          val p = rc.headCoeff
          mapsSum(nodeFamilyDist(initState)(rc.headGen, baseDist, epsilon),
                  nodeCoeffFamilyMap(initState)(rc.tail,
                                                baseDist,
                                                epsilon / (1.0 - p)))
      }

  def varFamilyDist[RDom <: HList, Y](initState: State)(
      randomVarFmly: RandomVarFamily[RDom, Y],
      epsilon: Double): Map[RDom, FD[Y]] =
    if (epsilon > 1) Map()
    else
      find(randomVarFmly)
        .map { nc =>
          val base = varListDist(initState)(nc.output.polyDomain, epsilon)
          nodeCoeffFamilyMap(initState)(nc, base, epsilon)
        }
        .getOrElse(Map())

  def nodeFamilyDist[Dom <: HList, Y](initState: State)(
      generatorNodeFamily: GeneratorNodeFamily[Dom, Y],
      baseDist: FD[Dom],
      epsilon: Double): Map[Dom, FD[Y]] =
    generatorNodeFamily match {
      case node: GeneratorNode[Y] =>
        Map(HNil -> nodeDist(initState)(node, epsilon))
      case fml: GeneratorNodeFamily.Pi[Dom, Y] =>
        val kvs: Vector[(Dom, FD[Y])] =
          for {
            Weighted(arg, p) <- baseDist.pmf
            d = nodeDist(initState)(fml.nodes(arg), epsilon / p)
          } yield arg -> d
        kvs.toMap
      case fml: GeneratorNodeFamily.PiOpt[Dom, Y] =>
        val kvs: Vector[(Dom, FD[Y])] =
          for {
            Weighted(arg, p) <- baseDist.pmf
            node             <- fml.nodesOpt(arg)
            d = nodeDist(initState)(node, epsilon / p)
          } yield arg -> d
        kvs.toMap
    }

  def nodeDist[Y](initState: State)(generatorNode: GeneratorNode[Y],
                                    epsilon: Double): FD[Y]

}

/**
  * resolving a general specification of a recursive generative model as finite distributions, depending on truncation;
  * the coefficients of the various generator nodes should be `Double`
  *
  * @param nodeCoeffSeq the various generator nodes with coefficients for various random variables and families
  * @param sd finite distributions from the initial state corresponding to random variables and families
  * @tparam State scala type of the initial state
  */
case class TruncatedFiniteDistribution[State](
    nodeCoeffSeq: NodeCoeffSeq[State, Double], varWeight: Double)(
    implicit sd: StateDistribution[State, FD])
    extends GenTruncatedFiniteDistribution[State](nodeCoeffSeq) {

  /**
    * update coefficients, to be used in complex islands
    * @param dataSeq the new coefficients
    * @return [[TruncatedFiniteDistribution]] with updated coefficients
    */
  def updateAll(
      dataSeq: Seq[GeneratorNodeFamily.Value[_ <: HList, _, Double]]) =
    TruncatedFiniteDistribution(nodeCoeffSeq.updateAll(dataSeq), varWeight)

  import StateDistribution._

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
  def nodeDist[Y](initState: State)(generatorNode: GeneratorNode[Y],
                                    epsilon: Double): FD[Y] =
    if (epsilon > 1) FD.empty[Y]
    else {
      import GeneratorNode._
      generatorNode match {
        case Atom(x, input) =>
          FD.unif(x)
        case Init(input) =>
          value(initState)(input).purge(epsilon)
        case Map(f, input, _) =>
          varDist(initState)(input, epsilon).map(f).purge(epsilon)
        case MapOpt(f, input, _) =>
          varDist(initState)(input, epsilon).condMap(f).purge(epsilon)
        case ZipMap(f, input1, input2, _) =>
          val d1 = varDist(initState)(input1, epsilon).flatten
          val d2 = varDist(initState)(input2, epsilon).flatten
          d1.zip(d2).map { case (x, y) => f(x, y) }.purge(epsilon)
        case ZipMapOpt(f, input1, input2, _) =>
          val d1 = varDist(initState)(input1, epsilon).flatten
          val d2 = varDist(initState)(input2, epsilon).flatten
          d1.zip(d2).condMap { case (x, y) => f(x, y) }.purge(epsilon)
        case ZipFlatMap(baseInput, fiberVar, f, _) =>
          val baseDist = varDist(initState)(baseInput, epsilon).flatten
          val pmf =
            for {
              Weighted(x1, p1) <- baseDist.pmf
              fiberDist = varDist(initState)(fiberVar(x1), epsilon / p1).flatten
              Weighted(x2, p2) <- fiberDist.pmf
            } yield Weighted(f(x1, x2), p1 * p2)
          FD(pmf).flatten.purge(epsilon)
        case FlatMap(baseInput, fiberNode, _) =>
          val baseDist = varDist(initState)(baseInput, epsilon).flatten
          val pmf =
            for {
              Weighted(x1, p1) <- baseDist.pmf
              fiberDist = nodeDist(initState)(fiberNode(x1), epsilon / p1).flatten
              Weighted(x2, p2) <- fiberDist.pmf
            } yield Weighted(x2, p1 * p2)
          FD(pmf).flatten.purge(epsilon)
        case FlatMapOpt(baseInput, fiberNodeOpt, _) =>
          val baseDist = varDist(initState)(baseInput, epsilon).flatten
          val pmf =
            for {
              Weighted(x1, p1) <- baseDist.pmf
              node             <- fiberNodeOpt(x1).toVector
              fiberDist = nodeDist(initState)(node, epsilon / p1).flatten
              Weighted(x2, p2) <- fiberDist.pmf
            } yield Weighted(x2, p1 * p2)
          FD(pmf).flatten.purge(epsilon)
        case FiberProductMap(quot, fiberVar, f, baseInput, _) =>
          val d1          = varDist(initState)(baseInput, epsilon).flatten
          val byBase      = d1.pmf.groupBy { case Weighted(x, p) => quot(x) } // pmfs grouped by terms in quotient
          val baseWeights = byBase.mapValues(v => v.map(_.weight).sum) // weights of terms in the quotient
          val pmf: immutable.Iterable[Weighted[Y]] =
            for {
              (z, pmf1) <- byBase // `z` is in the base, `pmf1` is all terms above `z`
              d2 = varDist(initState)(fiberVar(z), epsilon / baseWeights(z)).flatten // distribution of the fiber at `z`
              d = FD(pmf1)
                .zip(d2)
                .map { case (x1, x2) => f(x1, x2) }
                .flatten // mapped distribution over `z`
              wtd <- d.pmf // terms in pmf over z, should be flatMapped over `z`
            } yield wtd
          FD(pmf.toVector).purge(epsilon)
        case tc: ThenCondition[o, Y] =>
          import tc._
          val base = nodeDist(initState)(gen, epsilon)
          import Sort._
          condition match {
            case _: All[_]    => base
            case c: Filter[_] => base.conditioned(c.pred).purge(epsilon)
            case Restrict(f)  => base.condMap(f).purge(epsilon)
          }
        case isle: Island[Y, State, o, b] =>
          import isle._
          val (isleInit, boat) = initMap(initState)(varWeight)                             // initial condition for island, boat to row back
          val isleOut          = varDist(isleInit)(islandOutput(boat), epsilon) //result for the island
          isleOut
            .map(export(boat, _))
            .purge(epsilon) // exported result seen outside
      }
    }
}
