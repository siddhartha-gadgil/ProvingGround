package provingground.learning
import provingground.{FiniteDistribution => FD}
import shapeless._
import HList._
import provingground.learning.GeneratorNode.{FlatMap, Island, ThenCondition}

import scala.language.higherKinds

/**
  * resolving a general specification of a recursive generative model as finite distributions, depending on truncation;
  * the coefficients of the various generator nodes should be `Double`
  *
  * @param nodeCoeffSeq the various generator nodes with coefficients for various random variables and families
  * @param sd finite distributions from the initial state corresponding to random variables and families
  * @tparam State scala type of the initial state
  */
case class GeneratorVariables[State, Boat](
    nodeCoeffSeq: NodeCoeffSeq[State, Boat, Double],
    initState: State)(implicit sd: StateDistribution[State, FD]) {

  def varSupport[Y](rv: RandomVar[Y]): Set[Y] =
    StateDistribution.value(initState)(rv).support

  def varListSupport[Dom <: HList](rvs: RandomVarList[Dom]): Set[Dom] =
    rvs match {
      case RandomVarList.Nil => Set(HNil)
      case RandomVarList.Cons(head, tail) =>
        val headSet = varSupport(head)
        val tailSet = varListSupport(tail)
        for {
          x <- headSet
          y <- tailSet
        } yield x :: y
    }
  import GeneratorVariables._

  def varFamilyVars[Dom <: HList, Y](
      rvF: RandomVarFamily[Dom, Y]): Set[GeneratorVariables.Variable[_]] =
    for {
      x <- varListSupport(rvF.polyDomain)
      dist = StateDistribution.valueAt(initState)(rvF, x)
      y <- dist.support
    } yield Elem(y, rvF.at(x))

  lazy val outputVars: Set[Variable[_]] =
    nodeCoeffSeq.outputs.toSet.flatMap(
      (rvF: RandomVarFamily[_ <: HList, _]) => varFamilyVars(rvF)
    )

  def generatorVars[Y](generatorNode: GeneratorNode[Y]): Set[Variable[_]] =
    generatorNode match {
      case tc: ThenCondition[o, Y] =>
        Set(GeneratorVariables.Event(tc.gen.output, tc.condition))
      case fm: FlatMap[o, Y] =>
        varSupport(fm.baseInput).flatMap((x) => generatorVars(fm.fiberNode(x)))
      case isle: Island[Y, State, o, b] =>
        import isle._
        val (isleInit, boat) = initMap(initState)
        val isleVars: Set[Variable[_]] =
          GeneratorVariables(nodeCoeffSeq, isleInit).allVars
        isleVars.map((x) => GeneratorVariables.InIsle(x, boat))
      case _ => Set()
    }

  def generatorFamilyVars[Dom <: HList, O](
      generatorNodeFamily: GeneratorNodeFamily[Dom, O],
      output: RandomVarFamily[Dom, O]): Set[Variable[_]] =
    generatorNodeFamily match {
      case p: GeneratorNodeFamily.Pi[Dom, O] =>
        varListSupport(output.polyDomain).flatMap((x) =>
          generatorVars(p.nodes(x)))
      case node: GeneratorNode[O] => generatorVars(node)
    }

  def nodeSeqVars(nc: NodeCoeffSeq[State, Boat, Double]): Set[Variable[_]] =
    nc match {
      case _: NodeCoeffSeq.Empty[State, Boat, Double] => Set()
      case ncs: NodeCoeffSeq.Cons[State, Boat, Double, rd, y] =>
        nodeSeqVars(ncs.tail) union {
          ncs.head.nodeFamilies.flatMap(nf =>
            generatorFamilyVars[rd, y](nf, ncs.head.output))
        }
    }

  lazy val allVars: Set[Variable[_]] = outputVars union nodeSeqVars(
    nodeCoeffSeq)

}

object GeneratorVariables {

  /**
    * a variable representing a probability
    * @tparam Y objects in the distribution
    */
  sealed trait Variable[+Y]

  case class Elem[Y](element: Y, randomVar: RandomVar[Y]) extends Variable[Y]

  case class Event[X, Y](base: RandomVar[X], sort: Sort[X, Y])
      extends Variable[Y]

  case class InIsle[Y, Boat](isleVar: Variable[Y], boat: Boat)
      extends Variable[Y]

  case class NodeCoeff[RDom <: HList, Y](nodeFamily: GeneratorNodeFamily[RDom, Y]) extends Variable[Unit]

  sealed trait Expression{
    def +(that: Expression): Sum = Sum(this, that)

    def *(that: Expression): Product = Product(this, that)

    def /(that: Expression): Quotient = Quotient(this, that)
  }

  case class VarProb[+Y](variable : Variable[Y], prob: Double) extends Expression

  case class Sum(x: Expression, y: Expression) extends Expression

  case class Product(x: Expression, y: Expression) extends Expression

  case class Literal(value: Double) extends Expression

  case class Quotient(x: Expression, y: Expression) extends Expression

}
