package provingground.learning
import provingground._

import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD}

import learning.{TangVec => T}

import cats._
import cats.implicits._

import HoTT._

import shapeless._, HList._
import scala.language.higherKinds

/**
  * A `sort`, i.e. type refining a scala type.
  * Can also be used for conditioning, giving one distribution from another.
  */
sealed trait Sort[S, T]

object Sort {
  case class True[S]() extends Sort[S, S]

  case class Filter[S](pred: S => Boolean) extends Sort[S, S]

  case class Restrict[S, T](optMap: S => Option[T]) extends Sort[S, T]

  val AllTerms = True[Term]
}

sealed trait SortList[S, U <: HList] {
  def ::[X](that: Sort[S, X]) = SortList.Cons(that, this)
}

object SortList {
  case class Nil[S]() extends SortList[S, HNil]

  case class Cons[S, X, U <: HList](head: Sort[S, X],
                                    tail: SortList[S, U])
      extends SortList[S, X :: U]
}

class RandomVarFamily[Dom <: HList, O](val rangeFamily: Dom => Sort[_, O] = (d: Dom) => Sort.True[O])

class RandomVar[O](val range: Sort[_, O] = Sort.True[O]) extends RandomVarFamily[HNil, O]((_) => range)


object RandomVar{
  class SimpleFamily[U, O](rangeFamily: U => Sort[_, O] = (x: U) => Sort.True[O]) extends RandomVarFamily[U :: HNil, O](
    {case x :: HNil => rangeFamily(x)}
  )

  case class Instance[Dom <: HList, O](family: RandomVarFamily[Dom, O], fullArg: Dom) extends RandomVar(family.rangeFamily(fullArg))
}


sealed trait RandomVarList[U <: HList] {
  def ::[X](that: RandomVar[X]) = RandomVarList.Cons(that, this)
}


object RandomVarList {
  case object Nil extends RandomVarList[HNil]

  case class Cons[X, U <: HList](head: RandomVar[X],
                                    tail: RandomVarList[U])
      extends RandomVarList[X :: U]
}

sealed trait GeneratorNodeFamily[Dom <: HList, O] {
  val output: RandomVarFamily[Dom, O]
}


object GeneratorNodeFamily {
  case class Pi[Dom <: HList, O](nodes : Dom => GeneratorNode[O], output: RandomVarFamily[Dom, O]) extends GeneratorNodeFamily[Dom, O]
}

/**
  * A formal node for describing recursive generation. Can have several inputList,
  * encoded as an HList, but has just one output.
  */
sealed trait GeneratorNode[O] extends GeneratorNodeFamily[HNil, O]{
  val output: RandomVar[O]
}

sealed trait BaseGeneratorNode[I<: HList, O] extends GeneratorNode[O]{
  val inputList: RandomVarList[I]
}

object GeneratorNode {
  def simplePi[U, O](nodes : U => GeneratorNode[O], output: RandomVarFamily[U :: HNil, O]) =
    GeneratorNodeFamily.Pi[U :: HNil, O]({case x :: HNil => nodes(x)}, output)

  case class Init[X](input: RandomVar[X]) extends BaseGeneratorNode[X :: HNil, X]{
    val output = input
    val inputList = input :: RandomVarList.Nil
  }

  case class ConditionedInit[X, Y](input: RandomVar[X], output: RandomVar[Y]
  ) extends BaseGeneratorNode[X :: HNil, Y]{
    val inputList = input :: RandomVarList.Nil
  }

  case class Map[X, Y](f: X => Y,
                          input: RandomVar[X],
                          output: RandomVar[Y])
      extends BaseGeneratorNode[X :: HNil, Y] {
    val inputList = input :: RandomVarList.Nil
  }

  case class MapOpt[X, Y](f: X => Option[Y],
                             input: RandomVar[X],
                             output: RandomVar[Y])
      extends BaseGeneratorNode[X :: HNil, Y] {
    val inputList = input :: RandomVarList.Nil
  }

  case class ZipMap[X1, X2, Y](f: (X1, X2) => Y,
                                  input1: RandomVar[X1],
                                  input2: RandomVar[X2],
                                  output: RandomVar[Y])
      extends BaseGeneratorNode[X1 :: X2 :: HNil, Y] {
    val inputList = input1 :: input2 :: RandomVarList.Nil
  }

  case class ZipMapOpt[X1, X2, Y](f: (X1, X2) => Option[Y],
                                     input1: RandomVar[X1],
                                     input2: RandomVar[X2],
                                     output: RandomVar[Y])
      extends BaseGeneratorNode[X1 :: X2 :: HNil, Y] {
    val inputList = input1 :: input2 :: RandomVarList.Nil
  }

  case class FiberProductMap[X1, X2, Z, Y](
                                  quot: X1 => Z,
                                  fiberVar: Y => RandomVar[X2],
                                  f: (X1, X2) => Y,
                                  baseInput: RandomVar[X1],
                                  output: RandomVar[Y])
      extends GeneratorNode[Y]

  case class ZipFlatMap[X1, X2, Y](
                                  baseInput: RandomVar[X1],
                                  fiberVar: X1 => RandomVar[X2],
                                  f: (X1, X2) => Y,
                                  output: RandomVar[Y])
      extends GeneratorNode[Y]

  case class ThenCondition[O, Y](
      gen: GeneratorNode[ O],
      condition: RandomVar[Y]
  ) extends GeneratorNode[Y] {
    val output   = condition
  }

  case class Island[O, Y, State, Boat, D[_]](
      islandOutput: RandomVar[O],
      output: RandomVar[Y],
      initMap: State => (State, Boat),
      export: (Boat, O) => Y
  )(implicit dists: DistributionState[State, D]) extends GeneratorNode[Y]

}

/**
 * typeclass for providing distributions from a state and
 * modifying a state from distributions
 */
trait DistributionState[State, D[_]]{
  val randomVars : Set[RandomVar[_]]

  val randomVarFamilies : Set[RandomVarFamily[_ <: HList, _]]

  def value[T](state : State)(randomVar: RandomVar[T]) : D[T]

  def valueAt[Dom <: HList, T](state : State)(randomVarFmly: RandomVarFamily[Dom, T], fullArg: Dom) : D[T]

  def update(values: RandomVarValues[D])(init: State): State
}

trait RandomVarValues[D[_]]{
  def valueAt[Dom <: HList, T](randomVarFmly: RandomVarFamily[Dom, T], fullArg: Dom) : D[T]

  def value[T](randomVar: RandomVar[T]) : D[T]
}

/**
 * typeclass for being able to condition
 */
trait Conditioning[D[_]]{
  def condition[S, T](sort: Sort[S, T]) : D[S] => D[T]
}

case object Conditioning{
  def flatten[S, D[_]](implicit cd: Conditioning[D]) : D[Option[S]] => D[S] =
    cd.condition(Sort.Restrict[Option[S], S](identity))
}

object TermRandomVars{
  case object Terms extends RandomVar[Term]

  case object Typ extends RandomVar[Typ[Term]]

  case object Funcs extends RandomVar[ExstFunc]
  // import RandomVar._
  //
  // val terms = True[Term]
  //
  // val typs = Restrict(typOpt)
  //
  // val funcs = Restrict(ExstFunc.opt)
  //
  // def termsWithTyp(typ: Term) = Filter((t: Term) => t.typ == typ)
  //
  // val typFamilies = Filter(isTypFamily)
  //
  // def funcsWithDomain(typ: Typ[Term]) = Restrict((t: Term) => ExstFunc.opt(t).filter(_.dom == typ))
}
