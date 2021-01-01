package provingground.learning

import provingground._, HoTT._

import GeneratorVariables._, Expression._

import scala.collection.parallel._
import shapeless._
import TermRandomVars._, NodeCoeffs._
import ParMapState._

trait RecParDistEq {
  val nodeCoeffSeq: NodeCoeffSeq[ParMapState, Double]

  def nodeDist[Y](
      initState: ParMapState,
      maxDepth: Option[Int],
      halted: => Boolean
  )(
      generatorNode: GeneratorNode[Y],
      epsilon: Double,
      coeff: Expression
  ): (ParMap[Y, Double], ParSet[EquationNode])

  def nodeFamilyDistFunc[Dom <: HList, Y](
      initState: ParMapState,
      maxDepth: Option[Int],
      halted: => Boolean
  )(
      generatorNodeFamily: GeneratorNodeFamily[Dom, Y],
      epsilon: Double
  )(arg: Dom): (ParMap[Y, Double], ParSet[EquationNode]) =
    generatorNodeFamily match {
      case node: GeneratorNode[Y] =>
        assert(
          arg == HNil,
          s"looking for coordinate $arg in $node which is not a family"
        )
        val coeff = Coeff.get(node)
        nodeDist(initState, maxDepth, halted)(node, epsilon, coeff)
      case f: GeneratorNodeFamily.Pi[Dom, Y] =>
        val coeff =
          Coeff.get(f.nodes(arg))
        nodeDist(initState, maxDepth, halted)(f.nodes(arg), epsilon, coeff)
      case f: GeneratorNodeFamily.PiOpt[Dom, Y] =>
        f.nodesOpt(arg)
          .map { (node) =>
            val coeff = Coeff.get(node)
            nodeDist(initState, maxDepth, halted)(node, epsilon, coeff)

          }
          .getOrElse((ParMap.empty[Y, Double], ParSet.empty[EquationNode]))
    }

  def nodeCoeffFamilyDist[Dom <: HList, Y](
      initState: ParMapState,
      maxDepth: Option[Int],
      halted: => Boolean
  )(
      nodeCoeffs: NodeCoeffs[ParMapState, Double, Dom, Y],
      epsilon: Double
  )(arg: Dom): (ParMap[Y, Double], ParSet[EquationNode]) =
    if (epsilon > 1 || halted)
      (ParMap.empty[Y, Double], ParSet.empty[EquationNode])
    else
      nodeCoeffs match {
        case Target(_) => (ParMap.empty[Y, Double], ParSet.empty[EquationNode])
        case bc: Cons[ParMapState, Double, Dom, Y] =>
          val p = bc.headCoeff

          val pa = nodeFamilyDistFunc(initState, maxDepth, halted)(
            bc.headGen,
            epsilon / p
          )(
            arg
          )
          val pb =
            nodeCoeffFamilyDist(initState, maxDepth, halted)(bc.tail, epsilon)(
              arg
            )
          (add(pa._1.mapValues(_ * p).to(ParMap), pb._1), pa._2 union pb._2)
      }

  def varFamilyDistFunc[RDom <: HList, Y](
      initState: ParMapState,
      maxDepth: Option[Int],
      halted: => Boolean
  )(
      randomVarFmly: RandomVarFamily[RDom, Y],
      epsilon: Double
  )(arg: RDom): (ParMap[Y, Double], ParSet[EquationNode]) =
    if (epsilon > 1 || halted)
      (ParMap.empty[Y, Double], ParSet.empty[EquationNode])
    else
      nodeCoeffSeq
        .find(randomVarFmly)
        .map { nc =>
          nodeCoeffFamilyDist(initState, maxDepth, halted)(nc, epsilon)(
            arg
          )
        }
        .getOrElse((ParMap.empty[Y, Double], ParSet.empty[EquationNode]))

  def nodeCoeffDist[Y](
      initState: ParMapState,
      maxDepth: Option[Int],
      halted: => Boolean
  )(
      nodeCoeffs: NodeCoeffs[ParMapState, Double, HNil, Y],
      epsilon: Double,
      rv: RandomVar[Y]
  ): (ParMap[Y, Double], ParSet[EquationNode]) =
    if (epsilon > 1 || halted)
      (ParMap.empty[Y, Double], ParSet.empty[EquationNode])
    else
      nodeCoeffs match {
        case Target(_) => (ParMap.empty[Y, Double], ParSet.empty[EquationNode])
        case bc: Cons[ParMapState, Double, HNil, Y] =>
          val p: Double = bc.headCoeff
          val (d: (ParMap[Y, Double], ParSet[EquationNode]), nc) =
            bc.headGen match {
              case gen: GeneratorNode[Y] =>
                val coeff = Coeff.get(gen) // FIXME : should be outer coefficient
                val (dist, eqs) = nodeDist(initState, maxDepth, halted)(
                  gen,
                  epsilon / p,
                  coeff
                )
                (dist.mapValues(_ * p), eqs) -> Coeff.get(gen)

              case _ =>
                throw new IllegalArgumentException(
                  "found node family for generating a simple random variable"
                )
            }
          val (a, eqa) = d
          val pb = nodeCoeffDist(initState, maxDepth, halted)(
            bc.tail,
            epsilon,
            rv
          )
          val (b, eqb) = pb
          (add(a, b), eqa union eqb)
      }

  def varDist[Y](
      initState: ParMapState,
      maxDepth: Option[Int],
      halted: => Boolean
  )(
      randomVar: RandomVar[Y],
      epsilon: Double
  ): (ParMap[Y, Double], ParSet[EquationNode]) =
    if (epsilon > 1 || halted)
      (ParMap.empty[Y, Double], ParSet.empty[EquationNode])
    else if (maxDepth.map(_ < 1).getOrElse(false))
      (initState.value(randomVar), ParSet())
    else {

      val result =
        randomVar match {
          case RandomVar.AtCoord(randomVarFmly, fullArg) =>
            val (dist, eqs) = varFamilyDistFunc(initState, maxDepth, halted)(
              randomVarFmly,
              epsilon
            )(fullArg)
            (normalize(dist), eqs)
          case _ =>
            nodeCoeffSeq
              .find(randomVar)
              .map { nc =>
                val (fd, eqs) = nodeCoeffDist(initState, maxDepth, halted)(
                  nc,
                  epsilon,
                  randomVar
                )
                (normalize(fd), eqs)

              }
              .getOrElse((ParMap.empty[Y, Double], ParSet.empty[EquationNode]))
        }
      result
    }
}

class ParDistEq(
    nodeCoeffSeqParam: NodeCoeffSeq[ParMapState, Double],
    varWeight: Double
) extends RecParDistEq {
  val nodeCoeffSeq: NodeCoeffSeq[ParMapState, Double] = nodeCoeffSeqParam

  def nodeDist[Y](
      initState: ParMapState,
      maxDepth: Option[Int],
      halted: => Boolean
  )(
      generatorNode: GeneratorNode[Y],
      epsilon: Double,
      coeff: Expression
  ): (ParMap[Y, Double], ParSet[EquationNode]) = ???
}
