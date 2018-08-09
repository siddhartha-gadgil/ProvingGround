package provingground.learning
import provingground._
import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD}
import shapeless._
import HList._
import provingground.learning.GeneratorNode.Island

import scala.collection.immutable
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
    nodeCoeffSeq: NodeCoeffSeq[State, Boat, Double], initState: State)(
    implicit sd: StateDistribution[State,  FD]) {

  def varSupport[Y](rv: RandomVar[Y]): Set[Y] =
    StateDistribution.value(initState)(rv).support

  def varListSupport[Dom <: HList](rvs : RandomVarList[Dom]): Set[Dom] =
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

  def varFamilyVars[Dom <: HList, Y](rvF : RandomVarFamily[Dom, Y]) : Set[GeneratorVariables.Variable[_]] =
    for {
      x <- varListSupport(rvF.polyDomain)
      dist = StateDistribution.valueAt(initState)(rvF, x)
      y<- dist.support
    } yield Prob(y, rvF.at(x))

  lazy val outputVars: Set[Variable[_]] =
    nodeCoeffSeq.outputs.toSet.flatMap(
      (rvF: RandomVarFamily[_ <: HList, _]) => varFamilyVars(rvF)
    )

}

object GeneratorVariables {

  /**
    * a variable representing a probability
    * @tparam Y objects in the distribution
    */
  trait Variable[Y]

  case class Prob[Y](element: Y, randomVar : RandomVar[Y]) extends Variable[Y]

  case class Event[X, Y](base: RandomVar[X], sort : Sort[X, Y]) extends Variable[Y]

  case class InIsle[Y, InitState, O, Boat](isleVar: Variable[Y], isle : Island[Y, InitState, O, Boat], boat: Boat) extends Variable[Y]



}
