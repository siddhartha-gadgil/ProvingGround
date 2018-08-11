package provingground.learning
import provingground.{FiniteDistribution => FD}
import shapeless._
import HList._
import provingground.learning.GeneratorNode.{FlatMap, Island, ThenCondition}

import scala.language.higherKinds
import GeneratorVariables._

case class GeneratorEquations[State, Boat](
    nodeCoeffSeq: NodeCoeffSeq[State, Boat, Double],
    vars: Set[Variable[_]]) {

  val elemVars: Map[RandomVar[_], Set[Elem[_]]] =
    vars.collect { case e: Elem[u] => e }.groupBy(_.randomVar)

  def finalProbs[Y](rv: RandomVar[Y]): Set[(Y, FinalVal[_])] =
    elemVars
      .getOrElse(rv, Set())
      .map((v) => v.element.asInstanceOf[Y] -> FinalVal(v))

  def initProbs[Y](rv: RandomVar[Y]): Set[(Y, InitialVal[_])] =
    elemVars
      .getOrElse(rv, Set())
      .map((v) => v.element.asInstanceOf[Y] -> InitialVal(v))


  def finalListProb[Dom <: HList](
      rvl: RandomVarList[Dom]): Set[(Dom, Expression)] = rvl match {
    case RandomVarList.Nil => Set(HNil -> Literal(1))
    case RandomVarList.Cons(head, tail) =>
      for {
        (x, p)  <- finalProbs(head)
        (ys, q) <- finalListProb(tail)
      } yield (x :: ys, p * q)
  }

  import GeneratorNode.{Map => _, _}

  def nodeEquationTerms[Y](
      node: GeneratorNode[Y]): (Set[EquationTerm], Set[Equation]) =
    node match {
      case Init(input) =>
        val eqTerms =
          initProbs(input).map {
            case (x, _) =>
              EquationTerm(FinalVal(Elem(x, input)), InitialVal(Elem(x, input)))
          }
        (eqTerms, Set())
      case GeneratorNode.Map(f, input, output) =>
        val eqTerms =
          finalProbs(input).map {
            case (x, p) =>
              EquationTerm(FinalVal(Elem(x, output)), p)
          }
        (eqTerms, Set())

      case _ => ???
    }
}
