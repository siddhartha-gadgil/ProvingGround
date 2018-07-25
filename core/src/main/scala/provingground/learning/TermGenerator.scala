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
  * @tparam T scala type elements of this Sort
  */
sealed trait Sort[+T]

object Sort {
  /**
   * Sort of all terms with given scala type
   */
  case class All[S]() extends Sort[S]

  /**
   * Sort given by predicate
   */
  case class Filter[S](pred: S => Boolean) extends Sort[S]

  /**
   * Sort as image of an optional map, which should be injective in the `Some(_)` case
   */
  case class Restrict[S, T](optMap: S => Option[T]) extends Sort[T]

  /**
   * Sort of all HoTT terms
   */
  val AllTerms = All[Term]
}

/**
 * List of Sorts, used as domains for families.
 */
sealed trait SortList[U <: HList] {
  def ::[X](that: Sort[X]) = SortList.Cons(that, this)
}

object SortList {
  case object Nil extends SortList[HNil]

  case class Cons[X, U <: HList](head: Sort[X], tail: SortList[U])
      extends SortList[X :: U]
}

/**
 * A formal family of Random Variables up to equality of distribution.
 * May actually have representations instead of distributions, for example.
 *
 */
class RandomVarFamily[Dom <: HList, +O](val polyDomain: SortList[Dom],
                                        val rangeFamily: Dom => Sort[O] =
                                          (d: Dom) => Sort.All[O])

object RandomVarFamily {
  /**
   * The associated distribution for a random variable family,
   * For example if `D[_]` is [[FiniteDistribution]], and `randVars` represents
   * terms with a given type, then `value` is a function from types to
   * finite distributions on terms.
   *
   * @param randVars  the family of random variables for which the distribution is specified
   * @param value function giving distributions associated to the random variables
   * @tparam Dom the domain of the family
   * @tparam D the distribution (could be representation) that is associated to the random variable
   */
  case class Value[Dom <: HList, O, D[_]](
      randVars: RandomVarFamily[Dom, O],
      value: Dom => D[O]
  )
}

/**
 * A formal Random Variable up to equality of distribution.
 * May actually have representations instead of distributions, for example.
 *
 */
class RandomVar[+O](val range: Sort[O] = Sort.All[O])
    extends RandomVarFamily[HNil, O](SortList.Nil, (_) => range)

object RandomVar {
  /**
   * convenience class to avoid {{HNil}}
   */
  class SimpleFamily[U, O](domain: Sort[U],
                           rangeFamily: U => Sort[O] = (x: U) => Sort.All[O])
      extends RandomVarFamily[U :: HNil, O](
        domain :: SortList.Nil,
        { case x :: HNil => rangeFamily(x) }
      )

  /**
   * The random variable at a specific domain point of a family.
   */
  case class AtCoord[Dom <: HList, O](family: RandomVarFamily[Dom, O],
                                       fullArg: Dom)
      extends RandomVar(family.rangeFamily(fullArg))

  /**
   * The associated distribution for a random variable,
   * For example if `D[_]` is [[FiniteDistribution]], and `randVars` represents
   * functions, then `value` is
   * finite distributions on functions.
   *
   * @param randVar  the random variable for which the distribution is specified
   * @param value function giving distributions associated to the random variables
   * @tparam D the distribution (could be representation) that is associated to the random variable
   */
  case class Value[O, D[_]](randVar: RandomVar[O], value: D[O])
}

/**
 * List of random variables, e.g. input of simple generator nodes.
 */
sealed trait RandomVarList[U <: HList] {
  def ::[X](that: RandomVar[X]) = RandomVarList.Cons(that, this)
}

object RandomVarList {
  case object Nil extends RandomVarList[HNil]

  case class Cons[X, U <: HList](head: RandomVar[X], tail: RandomVarList[U])
      extends RandomVarList[X :: U]
}

/**
 * Family of (recursive) generation functions, either a function giving a family
 * or a single {{GeneratorNode}}, which is the interesting case.
 */
sealed trait GeneratorNodeFamily[Dom <: HList, +O] {
  /**
   * the family of random variables for which this is a generation.
   */
  val outputFamily: RandomVarFamily[Dom, O]
}

object GeneratorNodeFamily {
  /**
   * A family of recursive generation functions, given as a function.
   */
  case class Pi[Dom <: HList, +O](nodes: Dom => GeneratorNode[O],
                                  outputFamily: RandomVarFamily[Dom, O])
      extends GeneratorNodeFamily[Dom, O]
}

/**
  * A formal node for describing recursive generation. Can have several inputList,
  * encoded as an HList, but has just one output.
  */
sealed trait GeneratorNode[+O] extends GeneratorNodeFamily[HNil, O] {
  val output: RandomVar[O]
  val outputFamily = output
}

sealed trait BaseGeneratorNode[I <: HList, O] extends GeneratorNode[O] {
  val inputList: RandomVarList[I]
}

object GeneratorNode {
  def simplePi[U, O](nodes: U => GeneratorNode[O],
                     outputFamily: RandomVarFamily[U :: HNil, O]) =
    GeneratorNodeFamily
      .Pi[U :: HNil, O]({ case x :: HNil => nodes(x) }, outputFamily)

  case class Init[X](input: RandomVar[X])
      extends BaseGeneratorNode[X :: HNil, X] {
    val output    = input
    val inputList = input :: RandomVarList.Nil
  }

  case class ConditionedInit[X, Y](input: RandomVar[X], output: RandomVar[Y])
      extends BaseGeneratorNode[X :: HNil, Y] {
    val inputList = input :: RandomVarList.Nil
  }

  case class Map[X, Y](f: X => Y, input: RandomVar[X], output: RandomVar[Y])
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

  case class FiberProductMap[X1, X2, Z, Y](quot: X1 => Z,
                                           fiberVar: Z => RandomVar[X2],
                                           f: (X1, X2) => Y,
                                           baseInput: RandomVar[X1],
                                           output: RandomVar[Y])
      extends GeneratorNode[Y]

  case class ZipFlatMap[X1, X2, Y](baseInput: RandomVar[X1],
                                   fiberVar: X1 => RandomVar[X2],
                                   f: (X1, X2) => Y,
                                   output: RandomVar[Y])
      extends GeneratorNode[Y]

  case class FlatMap[X, Y](
      baseInput: RandomVar[X],
      fiberVar: X => GeneratorNode[Y],
      output: RandomVar[Y]
  )

  case class ThenCondition[O, Y](
      gen: GeneratorNode[O],
      condition: RandomVar[Y]
  ) extends GeneratorNode[Y] {
    val output = condition
  }

  case class Island[O, Y, State, Boat, D[_]](
      islandOutput: RandomVar[O],
      output: RandomVar[Y],
      initMap: State => (State, Boat),
      export: (Boat, O) => Y
  )(implicit dists: DistributionState[State, D])
      extends GeneratorNode[Y]

}

/**
  * typeclass for providing distributions from a state and
  * modifying a state from distributions
  */
trait DistributionState[State, D[_]] {

  def value[T](state: State)(randomVar: RandomVar[T]): D[T]

  def valueAt[Dom <: HList, T](
      state: State)(randomVarFmly: RandomVarFamily[Dom, T], fullArg: Dom): D[T]

  def update(values: RandomVarValues[D])(init: State): State
}

trait StateEvolver[State] {
  def next(init: State): State
}

class DistributionStateEvolver[State, D[_]](
    nextDist: State => RandomVarValues[D])(
    implicit ds: DistributionState[State, D])
    extends StateEvolver[State] {
  def next(init: State): State = ds.update(nextDist(init))(init)
}

trait RandomVarValues[D[_]] {
  def valueAt[Dom <: HList, T](randomVarFmly: RandomVarFamily[Dom, T],
                               fullArg: Dom): D[T]

  def value[T](randomVar: RandomVar[T]): D[T]

  def support[T](randomVar: RandomVar[T])(implicit supp: Support[D]) =
    supp.support(value(randomVar))

  def fullArgSet[U <: HList](
      l: SortList[U],
      varGroups: Map[Sort[_], Set[RandomVar[_]]]
  )(implicit supp: Support[D]): Set[U] =
    l match {
      case SortList.Nil => Set(HNil)
      case SortList.Cons(head, tail) =>
        val headSets: Set[RandomVar[Any]] = varGroups(head)
        val headSupps                     = headSets.map((rv) => supp.support(value(rv)))
        val headSet                       = headSupps.reduce(_ union _)
        for {
          x  <- headSet
          ys <- fullArgSet(tail, varGroups)
        } yield x :: ys
    }
}

abstract class RandomVarMemo[D[_]](
    val randomVarVals: Set[RandomVar.Value[_, D]])(
    implicit emp: Empty[D])
    extends RandomVarValues[D] {
  def value[T](randomVar: RandomVar[T]): D[T] =
    randomVarVals
      .find(_.randVar == randomVar)
      .map(_.value.asInstanceOf[D[T]])
      .getOrElse(emp.empty[T])

  def valueAt[Dom <: HList, T](randomVarFmly: RandomVarFamily[Dom, T],
                               fullArg: Dom): D[T] =
    value(RandomVar.AtCoord(randomVarFmly, fullArg))
}

trait NodeCoefficients[V] {
  def value[O](node: GeneratorNode[O]): V

  def valueAt[Dom <: HList, T](nodes: GeneratorNodeFamily[Dom, T]): V
}

trait Empty[D[_]] {
  def empty[A]: D[A]
}

case class GeneratorData[V](
  nodeCoeffs: Vector[(GeneratorNode[_], V)],
  nodeFamilyCoeffs: Vector[(GeneratorNodeFamily[_ <: HList, _], V)],
  varsToResolve: Vector[RandomVar[_]]
)

case class MemoState[D[_], V, C](
    randVarVals: Set[RandomVar.Value[_, D]],
    genData: GeneratorData[V],
    varFamiliesToResolve: Vector[RandomVarFamily[_<: HList, _]],
    context: C) {
  val rangeSet = randVarVals.map(_.randVar.range)

  val rangeGroups: Map[Sort[_], Set[RandomVar[_]]] =
    randVarVals.map(_.randVar).groupBy(_.range)
}

object MemoState {
  implicit def safeExplDistState[D[_], V, P](
      implicit emp: Empty[D]): DistributionState[MemoState[D, V, P], D] =
    new DistributionState[MemoState[D, V, P], D] {
      def value[T](state: MemoState[D, V, P])(
          randomVar: RandomVar[T]): D[T] =
        state.randVarVals
          .find(_.randVar == randomVar)
          .map(_.value.asInstanceOf[D[T]])
          .getOrElse(emp.empty[T])

      def valueAt[Dom <: HList, T](state: MemoState[D, V, P])(
          randomVarFmly: RandomVarFamily[Dom, T],
          fullArg: Dom): D[T] =
        value(state)(RandomVar.AtCoord(randomVarFmly, fullArg))

      def update(values: RandomVarValues[D])(
          init: MemoState[D, V, P]): MemoState[D, V, P] =
        values match {
          case memo: RandomVarMemo[D] =>
            init.copy(randVarVals = memo.randomVarVals)
          case _ =>
            val randomVarVals =
              for {
                RandomVar.Value(rv, value) <- init.randVarVals
                newVal = values.value(rv)
              } yield RandomVar.Value(rv, newVal)

            init.copy(randVarVals = randomVarVals)
        }
    }
}

/**
  * typeclass for being able to condition
  */
trait Conditioning[D[_]] {
  def condition[S, T](sort: Sort[T]): D[S] => D[T]
}

case object Conditioning {
  def flatten[S, D[_]](implicit cd: Conditioning[D]): D[Option[S]] => D[S] =
    cd.condition(Sort.Restrict[Option[S], S](identity))
}

trait Support[D[_]] {
  def support[T](dist: D[T]): Set[T]
}

object TermRandomVars {
  case object Terms extends RandomVar[Term]

  case object Typs extends RandomVar[Typ[Term]]

  case object Funcs extends RandomVar[ExstFunc]

  case object TermsWithTyp
      extends RandomVar.SimpleFamily[Typ[Term], Term](
        Sort.All[Typ[Term]],
        (typ: Typ[Term]) => Sort.Filter[Term](_.typ == typ)
      )

  def termsWithTyp(typ: Typ[Term]) =
    RandomVar.AtCoord(TermsWithTyp, typ :: HNil)

  case object TypFamilies extends RandomVar[Term]

  case object FuncsWithDomain
      extends RandomVar.SimpleFamily[Typ[Term], ExstFunc](
        Sort.All[Typ[Term]],
        (typ: Typ[Term]) => Sort.Filter[ExstFunc](_.dom == typ)
      )

}

class TermGeneratorNodes[State, D[_]](
    appln: (ExstFunc, Term) => Term,
    unifApplnOpt: (ExstFunc, Term) => Option[Term],
    addVar: Typ[Term] => (State => (State, Term))
)(implicit dists: DistributionState[State, D]) {
  import TermRandomVars._, GeneratorNode._

  val unifApplnNode = ZipMapOpt[ExstFunc, Term, Term](
    unifApplnOpt,
    Funcs,
    Terms,
    Terms
  )

  val applnNode =
    FiberProductMap[ExstFunc, Term, Typ[Term], Term](
      _.dom,
      termsWithTyp,
      appln,
      Funcs,
      Terms
    )

  def lambdaIsle(typ: Typ[Term]) =
    Island[Term, Term, State, Term, D](
      Terms,
      Terms,
      addVar(typ),
      { case (x, y) => x :~> y }
    )

  val lambdaNode =
    FlatMap(
      Typs,
      lambdaIsle,
      Terms
    )

  def piIslelambdaIsle(typ: Typ[Term]) =
    Island[Typ[Term], Typ[Term], State, Term, D](
      Typs,
      Typs,
      addVar(typ),
      { case (x, y) => pi(x)(y) }
    )
}
