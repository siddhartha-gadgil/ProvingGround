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
  * A condition, corresponding to which we get a distribution from an existing one.
  * Can also be used for conditioning, giving one distribution from another.
  */
sealed trait Condition[S, T]

object Condition {
  case class True[S]() extends Condition[S, S]

  case class Filter[S](pred: S => Boolean) extends Condition[S, S]

  case class Restrict[S, T](optMap: S => Option[T]) extends Condition[S, T]

  val AllTerms = True[Term]
}

sealed trait ConditionList[S, U <: HList] {
  def ::[X](that: Condition[S, X]) = ConditionList.Cons(that, this)
}

object ConditionList {
  case class Nil[S]() extends ConditionList[S, HNil]

  case class Cons[S, X, U <: HList](head: Condition[S, X],
                                    tail: ConditionList[S, U])
      extends ConditionList[S, X :: U]
}

/**
  * A formal node for describing recursive generation. Can have several inputs,
  * encoded as an HList, but has just one output.
  */
sealed trait GeneratorNode[S,  O] {
  val codomain: Condition[S, O]
}

sealed trait BaseGeneratorNode[S, I<: HList, O] extends GeneratorNode[S, O]{
  val polyDomain: ConditionList[S, I]
}

object GeneratorNode {
  case class Map[S, X, Y](f: X => Y,
                          domain: Condition[S, X],
                          codomain: Condition[S, Y])
      extends BaseGeneratorNode[S, X :: HNil, Y] {
    val polyDomain = domain :: ConditionList.Nil[S]
  }

  case class MapOpt[S, X, Y](f: X => Option[Y],
                             domain: Condition[S, X],
                             codomain: Condition[S, Y])
      extends BaseGeneratorNode[S, X :: HNil, Y] {
    val polyDomain = domain :: ConditionList.Nil[S]
  }

  case class ZipMap[S, X1, X2, Y](f: (X1, X2) => Y,
                                  domain1: Condition[S, X1],
                                  domain2: Condition[S, X2],
                                  codomain: Condition[S, Y])
      extends BaseGeneratorNode[S, X1 :: X2 :: HNil, Y] {
    val polyDomain = domain1 :: domain2 :: ConditionList.Nil[S]
  }

  case class ZipMapOpt[S, X1, X2, Y](f: (X1, X2) => Option[Y],
                                     domain1: Condition[S, X1],
                                     domain2: Condition[S, X2],
                                     codomain: Condition[S, Y])
      extends BaseGeneratorNode[S, X1 :: X2 :: HNil, Y] {
    val polyDomain = domain1 :: domain2 :: ConditionList.Nil[S]
  }

  case class ThenCondition[S, O, Y](
      gen: GeneratorNode[S,  O],
      condition: Condition[S, Y]
  ) extends GeneratorNode[S, Y] {
    val codomain   = condition
  }

  case class Island[S, O, Y, State, Boat, D[_]](
      islandCodomain: Condition[S, O],
      codomain: Condition[S, Y],
      initMap: State => (State, Boat),
      export: (Boat, O) => Y
  )(implicit dists: Distibutions[S, State, D]) extends GeneratorNode[S, Y]

}

/**
 * typeclass for providing distributions from a state
 */
trait Distibutions[S, State, D[_]]{
  def distribution[T](condition: Condition[S, T])(state : State) : D[T]
}

/**
 * typeclass for being able to condition
 */
trait Conditioning[D[_]]{
  def condition[S, T](c: Condition[S, T]) : D[S] => D[T]
}

case object Conditioning{
  def flatten[S, D[_]](implicit cd: Conditioning[D]) : D[Option[S]] => D[S] =
    cd.condition(Condition.Restrict[Option[S], S](identity))
}
