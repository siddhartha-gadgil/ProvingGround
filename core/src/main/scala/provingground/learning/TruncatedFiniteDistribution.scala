package provingground.learning
import provingground._

import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD}

import learning.{TangVec => T}

import cats._
import cats.implicits._

import HoTT._

import shapeless._, HList._
import scala.language.higherKinds

class TruncatedFiniteDistribution(nodeCoeffSeq: NodeCoeffSeq[VarValueSet[FD], Double]){
  import nodeCoeffSeq.{outputs, find}

  import NodeCoeffs._

  def varDist[Y](randomVar: RandomVar[Y], epsilon: Double) : FD[Y] =
    if (epsilon > 1) FD.empty[Y]
    else find(randomVar).map{
      case Target(_) => FD.empty[Y]
      case bc : BaseCons[i, o, VarValueSet[FD], Double, HNil, Y] =>
        ???
      case rc : RecCons[i, o, VarValueSet[FD], Double, HNil, Y] =>
        ???
    }.getOrElse(FD.empty[Y])

  def nodeDist[Y](generatorNode: GeneratorNode[Y], epsilon: Double): FD[Y] =
    if (epsilon > 1) FD.empty[Y]
    else generatorNode match{
      case _ => ???
    }
}