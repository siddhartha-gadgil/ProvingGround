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
  * A sort, corresponding to which we get a distribution from an existing one.
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

/**
  * A formal node for describing recursive generation. Can have several inputs,
  * encoded as an HList, but has just one output.
  */
sealed trait GeneratorNode[S,  O] {
  val codomain: Sort[S, O]
}

sealed trait BaseGeneratorNode[S, I<: HList, O] extends GeneratorNode[S, O]{
  val polyDomain: SortList[S, I]
}

object GeneratorNode {
  case class Init[S, X](domain: Sort[S, X]) extends BaseGeneratorNode[S, X :: HNil, X]{
    val codomain = domain
    val polyDomain = domain :: SortList.Nil[S]
  }

  case class ConditionedInit[S, X, Y](domain: Sort[S, X], codomain: Sort[S, Y]
  ) extends BaseGeneratorNode[S, X :: HNil, Y]{
    val polyDomain = domain :: SortList.Nil[S]
  }

  case class Map[S, X, Y](f: X => Y,
                          domain: Sort[S, X],
                          codomain: Sort[S, Y])
      extends BaseGeneratorNode[S, X :: HNil, Y] {
    val polyDomain = domain :: SortList.Nil[S]
  }

  case class MapOpt[S, X, Y](f: X => Option[Y],
                             domain: Sort[S, X],
                             codomain: Sort[S, Y])
      extends BaseGeneratorNode[S, X :: HNil, Y] {
    val polyDomain = domain :: SortList.Nil[S]
  }

  case class ZipMap[S, X1, X2, Y](f: (X1, X2) => Y,
                                  domain1: Sort[S, X1],
                                  domain2: Sort[S, X2],
                                  codomain: Sort[S, Y])
      extends BaseGeneratorNode[S, X1 :: X2 :: HNil, Y] {
    val polyDomain = domain1 :: domain2 :: SortList.Nil[S]
  }

  case class ZipMapOpt[S, X1, X2, Y](f: (X1, X2) => Option[Y],
                                     domain1: Sort[S, X1],
                                     domain2: Sort[S, X2],
                                     codomain: Sort[S, Y])
      extends BaseGeneratorNode[S, X1 :: X2 :: HNil, Y] {
    val polyDomain = domain1 :: domain2 :: SortList.Nil[S]
  }

  case class FiberProduct[S, X1, X2, Z, Y](
                                  quot: X1 => Z,
                                  fiberDomain: Y => Sort[S, X2],
                                  f: (X1, X2) => Y,
                                  baseDomain: Sort[S, X1],
                                  codomain: Sort[S, Y])
      extends GeneratorNode[S, Y]

  case class ThenCondition[S, O, Y](
      gen: GeneratorNode[S,  O],
      condition: Sort[S, Y]
  ) extends GeneratorNode[S, Y] {
    val codomain   = condition
  }

  case class Island[S, O, Y, State, Boat, D[_]](
      islandCodomain: Sort[S, O],
      codomain: Sort[S, Y],
      initMap: State => (State, Boat),
      export: (Boat, O) => Y
  )(implicit dists: DistributionState[S, State, D]) extends GeneratorNode[S, Y]

}

/**
 * typeclass for providing distributions from a state and
 * modifying a state from distributions
 */
trait DistributionState[S, State, D[_]]{
  def distributions[T](sort: Sort[S, T])(state : State) : D[T]

  def update(dists: Distributions[S, D])(init: State): State
}

trait Distributions[S, D[_]]{
  def dist[T](sort: Sort[S, T]) : D[S]
}

/**
 * typeclass for being able to condition
 */
trait Conditioning[D[_]]{
  def condition[S, T](c: Sort[S, T]) : D[S] => D[T]
}

case object Conditioning{
  def flatten[S, D[_]](implicit cd: Conditioning[D]) : D[Option[S]] => D[S] =
    cd.condition(Sort.Restrict[Option[S], S](identity))
}

object TermSorts{
  import Sort._

  val terms = True[Term]

  val typs = Restrict(typOpt)

  val funcs = Restrict(ExstFunc.opt)

  def termsWithTyp(typ: Term) = Filter((t: Term) => t.typ == typ)

  val typFamilies = Filter(isTypFamily)

  def funcsWithDomain(typ: Typ[Term]) = Restrict((t: Term) => ExstFunc.opt(t).filter(_.dom == typ))
}
