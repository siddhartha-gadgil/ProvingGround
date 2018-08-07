package provingground.learning
import provingground._
import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD}
import shapeless._
import HList._

import scala.collection.immutable
import scala.language.higherKinds

object TruncatedFiniteDistribution{
  import GeometricDistribution._

  object Geom extends TruncatedFiniteDistribution[VarValueSet[FD]](
    nodeCoeffSeq
  )

  def truncFD(epsilon: Double) : FD[Int] =
    Geom.varDist(initState)(GeomVar, epsilon)
}

class TruncatedFiniteDistribution[State](
    nodeCoeffSeq: NodeCoeffSeq[State, Double])(implicit sd: StateDistribution[State, FD]) {
  import nodeCoeffSeq.{outputs, find}

  import NodeCoeffs._, StateDistribution._

  def varDist[Y](initState: State)(randomVar: RandomVar[Y],
                                             epsilon: Double): FD[Y] =
    if (epsilon > 1) FD.empty[Y]
    else
      randomVar match {
        case RandomVar.AtCoord(randomVarFmly, fullArg) =>
          varFamilyDist(initState)(randomVarFmly, epsilon)
            .getOrElse(fullArg, FD.empty[Y])
        case _ =>
          find(randomVar)
            .map { (nc) =>
              nodeCoeffDist(initState)(nc, epsilon)
            }
            .getOrElse(FD.empty[Y])
      }

  def varListDist[Dom <: HList](initState: State)(vl: RandomVarList[Dom], epsilon : Double) : FD[Dom] =
    vl match {
      case RandomVarList.Nil => FD.unif(HNil)
      case RandomVarList.Cons(head, tail) =>
        varDist(initState)(head, epsilon).zip(varListDist(initState)(tail, epsilon)).map{case (x, y) => x :: y}
    }

  def nodeCoeffDist[Y](initState: State)(
      nodeCoeffs: NodeCoeffs[State, Double, HNil, Y],
      epsilon: Double): FD[Y] =
    if (epsilon > 1) FD.empty[Y]
    else
      nodeCoeffs match {
        case Target(_) => FD.empty[Y]
        case bc: BaseCons[State, Double, HNil, Y] =>
          val p = bc.headCoeff
          val d =
            bc.headGen match {
              case gen: GeneratorNode[Y] =>
                nodeDist(initState)(gen, epsilon / p) * p
              case _ => throw new IllegalArgumentException("found family while looking for node")
            }
          d ++ nodeCoeffDist(initState)(bc.tail, epsilon / (1.0 - p))
        case rc: RecCons[State, Double, HNil, Y] =>
          val p = rc.headCoeff
          val d =
            rc.headGen match {
              case gen: GeneratorNode[Y] =>
                nodeDist(initState)(gen, epsilon / p) * p
              case _ => throw new IllegalArgumentException("found family while looking for node")
            }
          d ++ nodeCoeffDist(initState)(rc.tail, epsilon / (1.0 - p))
      }

  def mapsSum[X, Y](first: Map[X, FD[Y]], second: Map[X, FD[Y]]): Map[X, FD[Y]] =
    (for {
      k <- first.keySet union second.keySet
      v = first.getOrElse(k, FD.empty[Y]) ++ second.getOrElse(k, FD.empty[Y])
    } yield (k, v)).toMap

  def nodeCoeffFamilyMap[Dom <: HList, Y](initState: State)(
    nodeCoeffs: NodeCoeffs[State, Double, Dom, Y], baseDist : FD[Dom],
    epsilon: Double): Map[Dom, FD[Y]] =
    if (epsilon > 1) Map()
    else
      nodeCoeffs match {
        case Target(_) => Map()
        case bc: BaseCons[State, Double, Dom, Y] =>
          val p = bc.headCoeff
          mapsSum(
            nodeFamilyDist(initState)(bc.headGen, baseDist, epsilon),
            nodeCoeffFamilyMap(initState)(bc.tail, baseDist, epsilon / (1.0 - p)) )
        case rc: RecCons[State, Double, Dom, Y] =>
          val p = rc.headCoeff
          mapsSum(
            nodeFamilyDist(initState)(rc.headGen, baseDist, epsilon),
            nodeCoeffFamilyMap(initState)(rc.tail, baseDist, epsilon / (1.0 - p)) )
      }

  def varFamilyDist[RDom <: HList, Y](initState: State)(
      randomVarFmly: RandomVarFamily[RDom, Y],
      epsilon: Double): Map[RDom, FD[Y]] =
    if (epsilon > 0) Map()
    else
      find(randomVarFmly)
        .map { (nc) =>
          val base = varListDist(initState)(nc.output.polyDomain, epsilon)
          nodeCoeffFamilyMap(initState)(nc, base, epsilon)
        }
        .getOrElse(Map())

  def nodeFamilyDist[Dom <: HList, Y](initState: State)(generatorNodeFamily: GeneratorNodeFamily[Dom, Y], baseDist : FD[Dom],
                                                                  epsilon: Double): Map[Dom, FD[Y]] =
    generatorNodeFamily match {
      case node : GeneratorNode[Y] => Map(HNil -> nodeDist(initState)(node, epsilon))
      case GeneratorNodeFamily.BasePi(nodes, _) =>
        val kvs: Vector[(Dom, FD[Y])] =
          for {
            Weighted(arg, p) <- baseDist.pmf
            d = nodeDist(initState)(nodes(arg), epsilon/ p)
          } yield arg -> d
        kvs.toMap
      case GeneratorNodeFamily.RecPi(nodes, _) =>
        val kvs: Vector[(Dom, FD[Y])] =
          for {
            Weighted(arg, p) <- baseDist.pmf
            d = nodeDist(initState)(nodes(arg), epsilon/ p)
          } yield arg -> d
        kvs.toMap
    }

  def nodeDist[Y](initState: State)(generatorNode: GeneratorNode[Y],
                                              epsilon: Double): FD[Y] =
    if (epsilon > 1) FD.empty[Y]
    else {
      import GeneratorNode._
      generatorNode match {
        case Init(input)           => value(initState)(input).purge(epsilon)
        case Map(f, input, output) => varDist(initState)(input, epsilon).map(f)
        case MapOpt(f, input, output) =>
          varDist(initState)(input, epsilon).condMap(f)
        case ZipMap(f, input1, input2, output) =>
          val d1 = varDist(initState)(input1, epsilon).flatten
          val d2 = varDist(initState)(input2, epsilon).flatten
          d1.zip(d2).map { case (x, y) => f(x, y) }.purge(epsilon)
        case ZipMapOpt(f, input1, input2, output) =>
          val d1 = varDist(initState)(input1, epsilon).flatten
          val d2 = varDist(initState)(input2, epsilon).flatten
          d1.zip(d2).condMap { case (x, y) => f(x, y) }.purge(epsilon)
        case ZipFlatMap(baseInput, fiberVar, f, output) =>
          val baseDist = varDist(initState)(baseInput, epsilon).flatten
          val pmf =
            for {
              Weighted(x1, p1) <- baseDist.pmf
              fiberDist = varDist(initState)(fiberVar(x1), epsilon / p1).flatten
              Weighted(x2, p2) <- fiberDist.pmf
            } yield Weighted(f(x1, x2), p1 * p2)
          FiniteDistribution(pmf).flatten
        case FlatMap(baseInput, fiberNode, output) =>
          val baseDist = varDist(initState)(baseInput, epsilon).flatten
          val pmf =
            for {
              Weighted(x1, p1) <- baseDist.pmf
              fiberDist = nodeDist(initState)(fiberNode(x1), epsilon / p1).flatten
              Weighted(x2, p2) <- fiberDist.pmf
            } yield Weighted(x2, p1 * p2)
          FiniteDistribution(pmf).flatten
        case FiberProductMap(quot, fiberVar, f, baseInput, output) =>
          val d1          = varDist(initState)(baseInput, epsilon).flatten
          val byBase      = d1.pmf.groupBy { case Weighted(x, p) => quot(x) }
          val baseWeights = byBase.mapValues((v) => v.map(_.weight).sum)
          val pmf: immutable.Iterable[Weighted[Y]] =
            for {
              (z, pmf1) <- byBase
              d2 = varDist(initState)(fiberVar(z), epsilon).flatten
              d  = FD(pmf1).zip(d2).map { case (x1, x2) => f(x1, x2) }.flatten
              wtd <- d.pmf
            } yield wtd
          FD(pmf.toVector)
        case tc: BaseThenCondition[v, o, Y] =>
          import tc._
          val base = nodeDist(initState)(gen, epsilon)
          import Sort._
          condition match {
            case _: All[_]    => base
            case c: Filter[_] => base.conditioned(c.pred)
            case Restrict(f)  => base.condMap(f)
          }
        case isle: Island[Y, State, o, b] =>
          import isle._
          val (isleInit, boat) = initMap(initState)
          val isleOut          = varDist(isleInit)(islandOutput, epsilon)
          isleOut.map(export(boat, _))
        case ComplexIsland(islandOutput, output, initMap, export) =>
          ??? // implement later
      }
    }
}
