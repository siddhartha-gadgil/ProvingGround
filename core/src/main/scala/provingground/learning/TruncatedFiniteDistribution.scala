package provingground.learning
import provingground._
import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD}
import learning.{TangVec => T}
import cats._
import cats.implicits._
import HoTT._
import shapeless._
import HList._

import scala.collection.immutable
import scala.language.higherKinds

class TruncatedFiniteDistribution(
    nodeCoeffSeq: NodeCoeffSeq[VarValueSet[FD], Double]) {
  import nodeCoeffSeq.{outputs, find}

  import NodeCoeffs._, StateDistribution._

  def varDist[Y](initState: VarValueSet[FD])(randomVar: RandomVar[Y],
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

  def nodeCoeffDist[RDom <: HList, Y](initState: VarValueSet[FD])(
      nodeCoeffs: NodeCoeffs[VarValueSet[FD], Double, RDom, Y],
      epsilon: Double): FD[Y] =
    if (epsilon > 1) FD.empty[Y]
    else
      nodeCoeffs match {
        case Target(_) => FD.empty[Y]
        case bc: BaseCons[i, o, VarValueSet[FD], Double, HNil, Y] =>
          val p = bc.headCoeff
          val d =
            bc.headGen match {
              case gen: GeneratorNode[Y] =>
                nodeDist(initState)(gen, epsilon / p) * p
            }
          d ++ nodeCoeffDist(initState)(bc.tail, epsilon / (1.0 - p))
        case rc: RecCons[i, o, VarValueSet[FD], Double, HNil, Y] =>
          val p = rc.headCoeff
          val d =
            rc.headGen match {
              case gen: GeneratorNode[Y] =>
                nodeDist(initState)(gen, epsilon / p) * p
            }
          d ++ nodeCoeffDist(initState)(rc.tail, epsilon / (1.0 - p))
      }

  def varFamilyDist[RDom <: HList, Y](initState: VarValueSet[FD])(
      randomVarFmly: RandomVarFamily[RDom, Y],
      epsilon: Double): Map[RDom, FD[Y]] =
    if (epsilon > 0) Map()
    else
      find(randomVarFmly)
        .map {
          case Target(_) => Map.empty[RDom, FD[Y]]
          case bc: BaseCons[i, o, VarValueSet[FD], Double, RDom, Y] =>
            ???
          case rc: RecCons[i, o, VarValueSet[FD], Double, RDom, Y] =>
            ???
        }
        .getOrElse(Map())

  def nodeDist[Y](initState: VarValueSet[FD])(generatorNode: GeneratorNode[Y],
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
        case tc: ThenCondition[v, o, Y] =>
          import tc._
          val base = nodeDist(initState)(gen, epsilon)
          import Sort._
          condition match {
            case _: All[_]    => base
            case c: Filter[_] => base.conditioned(c.pred)
            case Restrict(f)  => base.condMap(f)
          }
        case isle: Island[o, Y, VarValueSet[FD], b] =>
          import isle._
          val (isleInit, boat) = initMap(initState)
          val isleOut          = varDist(isleInit)(islandOutput, epsilon)
          isleOut.map(export(boat, _))
        case ComplexIsland(islandOutput, output, initMap, export) =>
          ??? // implement later
      }
    }
}
