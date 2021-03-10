package provingground.learning
import provingground._
import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD}
import HoTT._
import shapeless._
import HList._
import provingground.learning.GeneratorNode.{Map => _, _}
import provingground.learning.GeneratorNodeFamily.BasePi

import scala.language.implicitConversions

/**
  * A `sort`, i.e. type refining a scala type.
  * Can also be used for conditioning, giving one distribution from another.
  * @tparam T scala type elements of this Sort
  */
sealed trait Sort[-S, +T] {
  val pred: S => Boolean
}

object Sort {

  /**
    * Sort of all terms with given scala type
    */
  case class All[S]() extends Sort[S, S] {
    val pred: S => Boolean = (_) => true
  }

  /**
    * Sort given by predicate
    */
  case class Filter[S](pred: S => Boolean) extends Sort[S, S]

  /**
    * Sort as image of an optional map, which should be injective in the `Some(_)` case
    */
  case class Restrict[S, T](optMap: S => Option[T]) extends Sort[S, T] {
    val pred: S => Boolean = (s: S) => optMap(s).isDefined
  }

  /**
    * Sort of all HoTT terms
    */
  val AllTerms: Sort[Term, Term] = All[Term]()
}

/**
  * List of Sorts, used as domains for families.
  */
sealed trait SortList[U <: HList] {
  def ::[Z, X](that: Sort[Z, X]) = SortList.Cons(that, this)
}

object SortList {
  case object Nil extends SortList[HNil]

  case class Cons[Z, X, U <: HList](head: Sort[Z, X], tail: SortList[U])
      extends SortList[X :: U]
}

/**
  * A formal family of Random Variables up to equality of distribution.
  * May actually have representations instead of distributions, for example.
  *
  */
class RandomVarFamily[Dom <: HList, +O](
    val polyDomain: RandomVarList[Dom],
    val rangeFamily: Dom => Sort[_, O] = (_: Dom) => Sort.All[O]()
) {
  def target[State, V, Y >: O]: NodeCoeffs.Target[State, V, Dom, Y] =
    NodeCoeffs.Target[State, V, Dom, Y](this)

  def at(x: Dom) = RandomVar.AtCoord(this, x)

  def init[Y >: O] = BasePi(RandomVarFamily.InitFunc[Dom, O, Y](this), this)
}

object RandomVarFamily {
  case class InitFunc[Dom <: HList, O, Y >: O](base : RandomVarFamily[Dom, O]) extends (Dom => GeneratorNode.Init[Y]){
    def apply(v1: Dom): GeneratorNode.Init[Y] = GeneratorNode.Init[Y](RandomVar.AtCoord(base, v1))
  }

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
      value: Map[Dom, D[O]]
  )
}

/**
  * A formal Random Variable up to equality of distribution.
  * May actually have representations instead of distributions, for example.
  *
  */
class RandomVar[+O](val range: Sort[_, O] = Sort.All[O]())
    extends RandomVarFamily[HNil, O](RandomVarList.Nil, _ => range)

object RandomVar {

  /**
    * convenience class to avoid {{HNil}}
    */
  class SimpleFamily[U, O](
      domain: RandomVar[U],
      rangeFamily: U => Sort[_, O] = (_: U) => Sort.All[O]()
  ) extends RandomVarFamily[U :: HNil, O](
        domain :: RandomVarList.Nil,
        { case x :: HNil => rangeFamily(x) }
      )

  /**
    * The random variable at a specific domain point of a family.
    */
  case class AtCoord[Dom <: HList, +O](
      family: RandomVarFamily[Dom, O],
      fullArg: Dom
  ) extends RandomVar(family.rangeFamily(fullArg))

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

  case class Element[O](randVar: RandomVar[O], value: O)
}

/**
  * distributions of vectors from a base distribution
  *
  * @param base the base distribution
  * @tparam X scala type of the base
  */
case class RandomVector[X](base: RandomVar[X]) extends RandomVar[Vector[X]] {
  def empty: Atom[Vector[X]] = just[Vector[X]](Vector(), this)

  def cons: ZipMap[X, Vector[X], Vector[X]] =
    ZipMap[X, Vector[X], Vector[X]](
      { case (x, ys) => x +: ys },
      base,
      this,
      this
    )
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

// sealed trait BaseGeneratorNodeFamily[Dom <: HList, +O]
//     extends GeneratorNodeFamily[Dom, O]

sealed trait RecursiveGeneratorNodeFamily[Dom <: HList, State, +O]
    extends GeneratorNodeFamily[Dom, O]

object GeneratorNodeFamily {

  case class Value[Dom <: HList, O, V](
      family: GeneratorNodeFamily[Dom, O],
      value: V
  )

  sealed trait Pi[Dom <: HList, +O] extends GeneratorNodeFamily[Dom, O] {
    val nodes: Dom => GeneratorNode[O]
    val outputFamily: RandomVarFamily[Dom, O]
  }

  /**
    * A family of recursive generation functions, given as a function.
    */
  case class RecPi[State, Dom <: HList, +O](
      nodes: Dom => RecursiveGeneratorNode[State, O],
      outputFamily: RandomVarFamily[Dom, O]
  ) extends RecursiveGeneratorNodeFamily[Dom, State, O]
      with Pi[Dom, O]

  /**
    * A family of recursive generation functions, given as a function.
    */
  case class BasePi[Dom <: HList, +O](
      nodes: Dom => GeneratorNode[O],
      outputFamily: RandomVarFamily[Dom, O]
  ) extends GeneratorNodeFamily[Dom, O]
      with Pi[Dom, O]

  sealed trait PiOpt[Dom <: HList, +O] extends GeneratorNodeFamily[Dom, O] {
    val nodesOpt: Dom => Option[GeneratorNode[O]]
    val outputFamily: RandomVarFamily[Dom, O]
  }

  /**
    * A family of recursive generation functions, given as a function.
    */
  case class RecPiOpt[State, Dom <: HList, +O](
      nodesOpt: Dom => Option[RecursiveGeneratorNode[State, O]],
      outputFamily: RandomVarFamily[Dom, O]
  ) extends RecursiveGeneratorNodeFamily[Dom, State, O]
      with PiOpt[Dom, O]

  /**
    * A family of recursive generation functions, given as a function.
    */
  case class BasePiOpt[Dom <: HList, +O](
      nodesOpt: Dom => Option[GeneratorNode[O]],
      outputFamily: RandomVarFamily[Dom, O]
  ) extends GeneratorNodeFamily[Dom, O]
      with PiOpt[Dom, O]

  def simplePiOpt[D, O](
      nodesOpt: D => Option[GeneratorNode[O]],
      outputFamily: RandomVarFamily[D :: HNil, O]
  ) = BasePiOpt[D :: HNil, O]({ case x :: HNil => nodesOpt(x) }, outputFamily)

}

/**
  * A formal node for describing recursive generation. Can have several inputList,
  * encoded as an HList, but has just one output.
  */
sealed trait GeneratorNode[+O] extends GeneratorNodeFamily[HNil, O] {
  val output: RandomVar[O]
  val outputFamily: RandomVar[O] = output

  def |[S >: O, T](
      condition: Sort[S, T],
      output: RandomVar[T]
  ): GeneratorNode[T] =
    BaseThenCondition[S, T](this, output, condition)

  def piFmly[Dom <: HList, S >: O, T](
      conditionFamily: Dom => Sort[S, T],
      outputFamily: RandomVarFamily[Dom, T]
  ): GeneratorNodeFamily[Dom, T] =
    GeneratorNodeFamily.BasePi(
      (x: Dom) =>
        BaseThenCondition(this, outputFamily.at(x), conditionFamily(x)),
      outputFamily
    )

  def pi[D, S >: O, T](
      conditionFamily: D => Sort[S, T],
      outputFamily: RandomVarFamily[D :: HNil, T]
  ): GeneratorNodeFamily[D :: HNil, T] =
    // piFmly({ case x :: HNil => conditionFamily(x) }, outputFamily)
    GeneratorNodeFamily.BasePi(
      GeneratorNode.ConditionFunc(this, conditionFamily, outputFamily),
      outputFamily
    )
    // GeneratorNodeFamily.BasePi(
    //   {case x :: HNil =>
    //     BaseThenCondition(this, outputFamily.at(x :: HNil), conditionFamily(x))},
    //   outputFamily
    // )
}

sealed trait RecursiveGeneratorNode[State, +O]
    extends GeneratorNode[O]
    with RecursiveGeneratorNodeFamily[HNil, State, O]
object GeneratorNode {

  case class ConditionFunc[O, D, S >: O, T](
      base: GeneratorNode[O],
      conditionFamily: D => Sort[S, T],
      outputFamily: RandomVarFamily[D :: HNil, T]
  ) extends (D :: HNil => BaseThenCondition[O, T]) {
    def apply(v1: D :: HNil): BaseThenCondition[O, T] =
      BaseThenCondition[O, T](
        base,
        outputFamily.at(v1),
        conditionFamily(v1.head)
      )
  }

  /**
    * generator node for simply using the initial distribution.
    * @param input the random variable whose distribution we extract.
    */
  case class Init[X](input: RandomVar[X]) extends GeneratorNode[X] {
    val output: RandomVar[X]                   = input
    val inputList: RandomVarList.Cons[X, HNil] = input :: RandomVarList.Nil
  }

  /**
    *  atomic value taken by a random variable
    * @param value the value
    * @tparam X the scala type of the random variable
    */
  case class Atom[X](value: X, output: RandomVar[X]) extends GeneratorNode[X]

  /**
    * atomic distribution to be included in generation
    *
    * @param value the atom
    * @param rv random variable whose distribution is specified
    * @tparam X scala type of the random variable
    * @return node for inclusion
    */
  def just[X](value: X, rv: RandomVar[X]) =
    Atom(value, rv)

  /**
    * generator node for mapping
    * @param f the function
    * @param input the input random variable
    * @param output the output random variable
    * @tparam X scala type of the input random variable
    * @tparam Y scala type of the output random variable
    */
  case class Map[X, Y](f: X => Y, input: RandomVar[X], output: RandomVar[Y])
      extends GeneratorNode[Y] {
    val inputList: RandomVarList.Cons[X, HNil] = input :: RandomVarList.Nil
  }

  /**
    * generator node for optionally mapping, to be conditioned by non-trivial outputs (if any)
    * @param f the optional function
    * @param input the input random variable
    * @param output the output random variable
    * @tparam X scala type of the input random variable
    * @tparam Y scala type of the output random variable
    */
  case class MapOpt[X, Y](
      f: X => Option[Y],
      input: RandomVar[X],
      output: RandomVar[Y]
  ) extends GeneratorNode[Y] {
    val inputList: RandomVarList.Cons[X, HNil] = input :: RandomVarList.Nil
  }

  /**
    * generator node for combining using a binary operation
    * @param f the binary operation
    * @param input1 the first input random variable
    * @param input2 the second input random variable
    * @param output the output random variable
    * @tparam X1 scala type of the first input random variable
    * @tparam X2 scala type of the second input random variable
    * @tparam Y scala type of the output random variable
    */
  case class ZipMap[X1, X2, Y](
      f: (X1, X2) => Y,
      input1: RandomVar[X1],
      input2: RandomVar[X2],
      output: RandomVar[Y]
  ) extends GeneratorNode[Y] {
    val inputList
        : RandomVarList.Cons[X1, ::[X2, HNil]] = input1 :: input2 :: RandomVarList.Nil
  }

  /**
    * generator node for combining using an optional binary operation, should be conditioned on output being non-trivial
    * @param f the optional binary operation
    * @param input1 the first input random variable
    * @param input2 the second input random variable
    * @param output the output random variable
    * @tparam X1 scala type of the first input random variable
    * @tparam X2 scala type of the second input random variable
    * @tparam Y scala type of the output random variable
    */
  case class ZipMapOpt[X1, X2, Y](
      f: (X1, X2) => Option[Y],
      input1: RandomVar[X1],
      input2: RandomVar[X2],
      output: RandomVar[Y]
  ) extends GeneratorNode[Y] {
    val inputList
        : RandomVarList.Cons[X1, ::[X2, HNil]] = input1 :: input2 :: RandomVarList.Nil
  }

  case class FiberProductMap[X1, X2, Z, Y](
      quot: X1 => Z,
      fiberVar: Z => RandomVar[X2],
      f: (X1, X2) => Y,
      baseInput: RandomVar[X1],
      output: RandomVar[Y]
  ) extends GeneratorNode[Y] {
    val inputList: RandomVarList.Cons[X1, HNil] = baseInput :: RandomVarList.Nil
  }

  case class ZipFlatMap[X1, X2, Y](
      baseInput: RandomVar[X1],
      fiberVar: X1 => RandomVar[X2],
      f: (X1, X2) => Y,
      output: RandomVar[Y]
  ) extends GeneratorNode[Y] {
    val inputList: RandomVarList.Cons[X1, HNil] = baseInput :: RandomVarList.Nil
  }

  /**
    * flat-maps a family of generator nodes, such as lambda islands corresponding to different types.
    * @param baseInput the random variable for the parameter of the family
    * @param fiberNode the node for each parameter
    * @param output the output random variable
    * @tparam X scala type of the parametrizing random variable
    * @tparam Y scala type of the output random variable
    */
  case class FlatMap[X, Y](
      baseInput: RandomVar[X],
      fiberNode: X => GeneratorNode[Y],
      output: RandomVar[Y]
  ) extends GeneratorNode[Y] {
    val inputList: RandomVarList.Cons[X, HNil] = baseInput :: RandomVarList.Nil
  }

  case class FlatMapOpt[X, Y](
      baseInput: RandomVar[X],
      fiberNodeOpt: X => Option[GeneratorNode[Y]],
      output: RandomVar[Y]
  ) extends GeneratorNode[Y] {
    val inputList: RandomVarList.Cons[X, HNil] = baseInput :: RandomVarList.Nil
  }

  /**
    * compose a generator node with conditioning
    * @tparam O scala type of the original output
    * @tparam Y scala type of the final output
    */
  sealed trait ThenCondition[O, Y] extends GeneratorNode[Y] {
    val gen: GeneratorNode[O]

    val output: RandomVar[Y]

    val condition: Sort[O, Y]
  }

  case class BaseThenCondition[O, Y](
      gen: GeneratorNode[O],
      output: RandomVar[Y],
      condition: Sort[O, Y]
  ) extends GeneratorNode[Y]
      with ThenCondition[O, Y]

  /**
    * Wrapper for identity to allow equality and  `toString` to work.
    */
  case class Idty[A]() extends (A => A) {
    def apply(a: A) = a

    override def toString = "Identity"
  }

  def conditionedVar[O, Y](
      input: RandomVar[O],
      output: RandomVar[Y],
      condition: Sort[O, Y]
  ) = BaseThenCondition(Map[O, O](Idty(), input, input), output, condition)

  case class RecursiveThenCondition[State, O, Y](
      gen: RecursiveGeneratorNode[State, O],
      output: RandomVar[Y],
      condition: Sort[O, Y]
  ) extends RecursiveGeneratorNode[State, Y]
      with ThenCondition[O, Y]

  /**
    * An `island`, i.e., a full map from initial state to all required distributions,
    * which is used in recursive definitions where we recurse on not just the final state
    * but also the dynamics. One enters the island using a `boat` which is returned, and must exit by the same boat.
    *
    * @param islandOutput the variable whose distribution from the island is used
    * @param output the final output, as seen from the outside
    * @param initMap the map from the initial state on the outside to the initial state in the island
    * @param export the object (e.g. term) level map applied when mapping a distribution obtained in the island to the outside; this is specific to the island
    * @tparam O the scala type of the final output
    * @tparam Y the scala type of the island output
    * @tparam InitState the initial state
    * @tparam Boat scala type of the `boat`
    */
  case class Island[Y, InitState, O, Boat](
      output: RandomVar[Y],
      islandOutput: Boat => RandomVar[O],
      initMap: InitState => Double => (InitState, Boat),
      export: (Boat, O) => Y,
      finalMap: (Boat, InitState) => InitState
  ) extends RecursiveGeneratorNode[InitState, Y]

  /*
   * An `island`, i.e., a full map from initial state to all required distributions,
   * which is used in recursive definitions where we recurse on not just the final state
   * but also the dynamics. One enters the island using a `boat` which is returned, and must exit by the same boat.
   *
   * @param islandOutput the variable whose distribution from the island is used
   * @param output the final output, as seen from the outside
   * @param initMap the map from the initial state on the outside to the initial state in the island
   * @param export the object (e.g. term) level map applied when mapping a distribution obtained in the island to the outside; this is specific to the island
   * @tparam O the scala type of the final output
   * @tparam Y the scala type of the island output
   * @tparam InitState the initial state
   * @tparam Boat scala type of the `boat`
   */
  // case class ComplexIsland[O, +Y, InitState, Boat, V](
  //     islandOutput: Boat => RandomVar[O],
  //     output: RandomVar[Y],
  //     initMap: InitState => (InitState,
  //                            Boat,
  //                            Set[GeneratorNodeFamily.Value[_ <: HList, _, V]]),
  //     export: (Boat, O) => Y,
  //     finalMap: (Boat, InitState) => InitState
  // ) extends RecursiveGeneratorNode[InitState, Y]

}

/**
  * changes in the distribution-like object other than those induced by object level map while exporting from an island
  * typically a change in context for variables representing terms in a context
  * @tparam Boat the boat for an island
  * @tparam D the distribution-like object
  */
trait ContextExport[Boat, D[_]] {
  def export[A]: (Boat, D[A]) => D[A]
}

object ContextExport {

  /**
    * No context change during export
    * @tparam Boat the boat for an island
    * @tparam D the distribution-like object
    *
    */
  def id[Boat, D[_]]: ContextExport[Boat, D] {
    def export[A]: (Boat, D[A]) => D[A]
  } = new ContextExport[Boat, D] {
    def export[A]: (Boat, D[A]) => D[A] = { case (_, d) => d }
  }

  /**
    * no change in context for finite distributions
    */
  implicit val fdTerm: ContextExport[Term, FD] =
    id[Term, FiniteDistribution]

  /**
    * no change in context for probability distributions
    */
  implicit val pdTerm: ContextExport[Term, PD] =
    id[Term, PD]

  /**
    * change in context typically corresponding to variables representing terms in a lambda-context
    * @tparam F the field; jet space for spire, outputs for tensorflow
    *
    */
  implicit def ctxMapExport[F]: ContextExport[
    Term,
    ({
      type D[A] = Map[(A, Vector[Term]), F]
    })#D
  ] =
    new ContextExport[Term, ({ type D[A] = Map[(A, Vector[Term]), F] })#D] {
      def export[A]
          : (Term, Map[(A, Vector[Term]), F]) => Map[(A, Vector[Term]), F] = {
        case (x, m) =>
          for {
            ((a, ctx), value) <- m
          } yield (a, x +: ctx) -> value
      }
    }

  type CtxDbl[A] = Map[(A, Vector[Term]), Double]

  val dblCtxShift: ContextExport[Term, CtxDbl] =
    implicitly[ContextExport[Term, CtxDbl]] // just a test
}

case class VarValueSet[D[_]](
    randVarVals: Set[RandomVar.Value[_, D]],
    randVarFamilyVals: Set[RandomVarFamily.Value[_ <: HList, _, D]]
)

/**
  * typeclass for providing distributions from a state
  */
trait StateDistribution[State, D[_]] {

  def value[T](state: State)(randomVar: RandomVar[T]): D[T]

  def valueAt[Dom <: HList, T](
      state: State
  )(randomVarFmly: RandomVarFamily[Dom, T], fullArg: Dom): D[T]

  def isEmpty(state: State): Boolean
}

object StateDistribution {
  implicit def varValueStateDist[D[_], V](
      implicit emp: Empty[D]
  ): StateDistribution[VarValueSet[D], D] =
    new StateDistribution[VarValueSet[D], D] {
      def value[T](state: VarValueSet[D])(randomVar: RandomVar[T]): D[T] =
        state.randVarVals
          .find(_.randVar == randomVar)
          .map(_.value.asInstanceOf[D[T]])
          .getOrElse(emp.empty[T])

      def valueAt[Dom <: HList, T](
          state: VarValueSet[D]
      )(randomVarFmly: RandomVarFamily[Dom, T], fullArg: Dom): D[T] =
        state.randVarFamilyVals
          .find(_.randVars == randomVarFmly)
          .flatMap(_.value.asInstanceOf[Map[Dom, D[T]]].get(fullArg))
          .getOrElse(emp.empty[T])

      def isEmpty(state: VarValueSet[D]) =
        state.randVarVals.isEmpty && state.randVarFamilyVals.isEmpty
    }

  def value[State, T, D[_]](
      state: State
  )(randomVar: RandomVar[T])(implicit sd: StateDistribution[State, D]): D[T] =
    sd.value(state)(randomVar)

  def valueAt[Dom <: HList, State, T, D[_]](state: State)(
      randomVarFmly: RandomVarFamily[Dom, T],
      fullArg: Dom
  )(implicit sd: StateDistribution[State, D]): D[T] =
    sd.valueAt(state)(randomVarFmly, fullArg)

}

/**
  * typeclass for providing distributions from a state and
  * modifying a state from distributions
  */
trait DistributionState[State, D[_]] extends StateDistribution[State, D] {

  def update(values: RandomVarValues[D])(init: State): State
}

object DistributionState {
  implicit def fdVar[O](randVar: RandomVar[O]): DistributionState[FD[O], FD] =
    new DistributionState[FD[O], FD] {
      def value[T](state: FD[O])(randomVar: RandomVar[T]): FD[T] =
        if (randVar == randomVar) state.asInstanceOf[FD[T]] else FD.empty[T]

      def valueAt[Dom <: HList, T](
          state: FD[O]
      )(randomVarFmly: RandomVarFamily[Dom, T], fullArg: Dom): FD[T] =
        FD.empty[T]

      def isEmpty(state: FiniteDistribution[O]) = state.support.isEmpty

      def update(values: RandomVarValues[FD])(init: FD[O]): FD[O] =
        values.value(randVar)
    }

}

trait StateEvolver[State] {
  def next(init: State): State
}

class DistributionStateEvolver[State, D[_]](
    nextDist: State => RandomVarValues[D]
)(implicit ds: DistributionState[State, D])
    extends StateEvolver[State] {
  def next(init: State): State = ds.update(nextDist(init))(init)
}

trait RandomVarValues[D[_]] {
  def valueAt[Dom <: HList, T](
      randomVarFmly: RandomVarFamily[Dom, T],
      fullArg: Dom
  ): D[T]

  def value[T](randomVar: RandomVar[T]): D[T]

  def support[T](randomVar: RandomVar[T])(implicit supp: Support[D]): Set[T] =
    supp.support(value(randomVar))

  def fullArgSet[U <: HList](
      l: SortList[U],
      varGroups: Map[Sort[_, _], Set[RandomVar[_]]]
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
    val randomVarVals: Set[RandomVar.Value[_, D]]
)(implicit emp: Empty[D])
    extends RandomVarValues[D] {
  def value[T](randomVar: RandomVar[T]): D[T] =
    randomVarVals
      .find(_.randVar == randomVar)
      .map(_.value.asInstanceOf[D[T]])
      .getOrElse(emp.empty[T])

  def valueAt[Dom <: HList, T](
      randomVarFmly: RandomVarFamily[Dom, T],
      fullArg: Dom
  ): D[T] =
    value(RandomVar.AtCoord(randomVarFmly, fullArg))
}

trait Empty[D[_]] {
  def empty[A]: D[A]
}

object Empty {
  implicit def fdEmpty: Empty[FD] {
    def empty[A]: FD[A]
  } = new Empty[FiniteDistribution] {
    def empty[A]: FD[A] = FiniteDistribution.empty[A]
  }
}

case class GeneratorData[V](
    nodeCoeffs: Vector[(GeneratorNodeFamily[_ <: HList, _], V)], // change this to something depending on state
    varsToResolve: Vector[RandomVar[_]]
)

sealed trait NodeCoeffSeq[State, V] {
  def +:[RDom <: HList, Y](head: NodeCoeffs[State, V, RDom, Y]) =
    NodeCoeffSeq.Cons(head, this)

  val nodeFamilies: Set[GeneratorNodeFamily[_ <: HList, _]]

  def find[RDom <: HList, Y](
      randomVar: RandomVarFamily[RDom, Y]
  ): Option[NodeCoeffs[State, V, RDom, Y]]

  val outputs: Vector[RandomVarFamily[_ <: HList, _]]

  def update[RDom <: HList, Y](
      data: GeneratorNodeFamily.Value[RDom, Y, V]
  ): NodeCoeffSeq[State, V]

  def updateAll(
      dataSeq: Seq[GeneratorNodeFamily.Value[_ <: HList, _, V]]
  ): NodeCoeffSeq[State, V] =
    dataSeq.foldLeft(this)(_ update _)

  def getCoeff[RDom <: HList, Y](gen: GeneratorNodeFamily[RDom, Y]): Option[V]

}

object NodeCoeffSeq {
  case class Empty[State, V]() extends NodeCoeffSeq[State, V] {
    def find[RDom <: HList, Y](
        randomVar: RandomVarFamily[RDom, Y]
    ): Option[NodeCoeffs[State, V, RDom, Y]] = None

    val outputs: Vector[RandomVarFamily[_ <: HList, _]] = Vector()

    val nodeFamilies: Set[GeneratorNodeFamily[_ <: HList, _]] = Set()

    def update[RDom <: HList, Y](
        data: GeneratorNodeFamily.Value[RDom, Y, V]
    ): Empty[State, V] =
      this

    def getCoeff[RDom <: HList, Y](
        gen: GeneratorNodeFamily[RDom, Y]
    ): Option[V] = None

  }

  case class Cons[State, V, RDom <: HList, Y](
      head: NodeCoeffs[State, V, RDom, Y],
      tail: NodeCoeffSeq[State, V]
  ) extends NodeCoeffSeq[State, V] {
    def find[VarRDom <: HList, VarY](
        randomVar: RandomVarFamily[VarRDom, VarY]
    ): Option[NodeCoeffs[State, V, VarRDom, VarY]] =
      tail
        .find(randomVar)
        .orElse(
          if (head.output == randomVar)
            Some(head.asInstanceOf[NodeCoeffs[State, V, VarRDom, VarY]])
          else None
        )

    val outputs
        : Vector[RandomVarFamily[_ <: HList, _]] = head.output +: tail.outputs

    val nodeFamilies: Set[GeneratorNodeFamily[_ <: HList, _]] =
      tail.nodeFamilies union head.nodeFamilies.map(
        (x) => x: GeneratorNodeFamily[_ <: HList, _]
      )

    def update[D <: HList, O](
        data: GeneratorNodeFamily.Value[D, O, V]
    ): NodeCoeffSeq[State, V] =
      this.copy(head = head.update(data))

    def getCoeff[RDom <: HList, Y](
        gen: GeneratorNodeFamily[RDom, Y]
    ): Option[V] =
      tail.getCoeff(gen).orElse(head.getCoeff(gen))

  }
}

sealed trait NodeCoeffs[State, V, RDom <: HList, Y] {
  val output: RandomVarFamily[RDom, Y]

  val nodeFamilies: Set[GeneratorNodeFamily[RDom, Y]]

  def updateOpt(
      data: GeneratorNodeFamily.Value[RDom, Y, V]
  ): Option[NodeCoeffs[State, V, RDom, Y]]

  import scala.util.Try

  def update[D <: HList, O](
      data: GeneratorNodeFamily.Value[D, O, V]
  ): NodeCoeffs[State, V, RDom, Y] =
    Try {
      updateOpt(data.asInstanceOf[GeneratorNodeFamily.Value[RDom, Y, V]])
    }.toOption.flatten.getOrElse(this)

  def updateAll(
      dataSeq: Seq[GeneratorNodeFamily.Value[_ <: HList, _, V]]
  ): NodeCoeffs[State, V, RDom, Y] =
    dataSeq.foldLeft(this)(_ update _)

  import NodeCoeffs._
  def ::(
      head: (GeneratorNodeFamily[RDom, Y], V)
  ) = Cons(head._1, head._2, this)

  def getCoeff[RD <: HList, YY](gen: GeneratorNodeFamily[RD, YY]): Option[V]

//  def ::(head: (RecursiveGeneratorNodeFamily[RDom, State, Y], V)) =
//    RecCons(head._1, head._2, this)
}

object NodeCoeffs {
  case class Target[State, V, RDom <: HList, Y](
      output: RandomVarFamily[RDom, Y]
  ) extends NodeCoeffs[State, V, RDom, Y] {
    def updateOpt(
        data: GeneratorNodeFamily.Value[RDom, Y, V]
    ): Option[NodeCoeffs[State, V, RDom, Y]] = None

    val nodeFamilies: Set[GeneratorNodeFamily[RDom, Y]] = Set()

    def getCoeff[RD <: HList, YY](gen: GeneratorNodeFamily[RD, YY]): Option[V] =
      None

  }

  sealed trait Cons[State, V, RDom <: HList, Y]
      extends NodeCoeffs[State, V, RDom, Y] {
    val headGen: GeneratorNodeFamily[RDom, Y]
    val headCoeff: V
    val tail: NodeCoeffs[State, V, RDom, Y]

    def getCoeff[RD <: HList, YY](gen: GeneratorNodeFamily[RD, YY]): Option[V] =
      tail.getCoeff(gen).orElse {
        if (gen == headGen) Some(headCoeff) else None
      }

  }

  object Cons {
    def apply[State, V, RDom <: HList, Y](
        headGen: GeneratorNodeFamily[RDom, Y],
        headCoeff: V,
        tail: NodeCoeffs[State, V, RDom, Y]
    ): Cons[State, V, RDom, Y] =
      headGen match {
        case value: GeneratorNodeFamily[RDom, Y] =>
          BaseCons(value, headCoeff, tail)
        // case value: RecursiveGeneratorNodeFamily[RDom, State, Y] => RecCons(value, headCoeff, tail)
      }

  }

  def purge[State, RDom <: HList, Y](
      nc: NodeCoeffs[State, Double, RDom, Y]
  ): NodeCoeffs[State, Double, RDom, Y] = nc match {
    case Target(output) => nc
    case BaseCons(headGen, headCoeff, tail) =>
      if (headCoeff > 0) BaseCons(headGen, headCoeff, purge(tail))
      else purge(tail)
    case RecCons(headGen, headCoeff, tail) =>
      if (headCoeff > 0) RecCons(headGen, headCoeff, purge(tail))
      else purge(tail)
  }

  case class BaseCons[State, V, RDom <: HList, Y](
      headGen: GeneratorNodeFamily[RDom, Y],
      headCoeff: V,
      tail: NodeCoeffs[State, V, RDom, Y]
  ) extends Cons[State, V, RDom, Y] {
    val output: RandomVarFamily[RDom, Y] = tail.output

//    require(
//      headGen.outputFamily == output,
//      s"cannot add node with output ${headGen.outputFamily} to sequence with output $output")

    val nodeFamilies
        : Set[GeneratorNodeFamily[RDom, Y]] = tail.nodeFamilies + headGen

    def updateOpt(
        data: GeneratorNodeFamily.Value[RDom, Y, V]
    ): Option[BaseCons[State, V, RDom, Y]] =
      if (data.family == headGen) Some(this.copy(headCoeff = data.value))
      else None
  }

  case class RecCons[State, V, RDom <: HList, Y](
      headGen: RecursiveGeneratorNodeFamily[RDom, State, Y],
      headCoeff: V,
      tail: NodeCoeffs[State, V, RDom, Y]
  ) extends Cons[State, V, RDom, Y] {
    val output: RandomVarFamily[RDom, Y] = tail.output

//    require(
//      headGen.outputFamily == output,
//      s"cannot add node with output ${headGen.outputFamily} to sequence with output $output")

    val nodeFamilies
        : Set[GeneratorNodeFamily[RDom, Y]] = tail.nodeFamilies + headGen

    def updateOpt(
        data: GeneratorNodeFamily.Value[RDom, Y, V]
    ): Option[RecCons[State, V, RDom, Y]] =
      if (data.family == headGen) Some(this.copy(headCoeff = data.value))
      else None
  }
}

case class MemoState[D[_], V, C](
    randVarVals: Set[RandomVar.Value[_, D]],
    genData: GeneratorData[V],
    // varFamiliesToResolve: Vector[RandomVarFamily[_ <: HList, _]],
    context: C
)

object MemoState {
  implicit def safeMemoDistState[D[_], V, P](
      implicit emp: Empty[D]
  ): DistributionState[MemoState[D, V, P], D] =
    new DistributionState[MemoState[D, V, P], D] {
      def value[T](state: MemoState[D, V, P])(randomVar: RandomVar[T]): D[T] =
        state.randVarVals
          .find(_.randVar == randomVar)
          .map(_.value.asInstanceOf[D[T]])
          .getOrElse(emp.empty[T])

      def isEmpty(state: MemoState[D, V, P]) = state.randVarVals.isEmpty

      def valueAt[Dom <: HList, T](
          state: MemoState[D, V, P]
      )(randomVarFmly: RandomVarFamily[Dom, T], fullArg: Dom): D[T] =
        value(state)(RandomVar.AtCoord(randomVarFmly, fullArg))

      def update(
          values: RandomVarValues[D]
      )(init: MemoState[D, V, P]): MemoState[D, V, P] =
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
  def condition[S, T](sort: Sort[S, T]): D[S] => D[T]
}

case object Conditioning {
  def flatten[S, D[_]](implicit cd: Conditioning[D]): D[Option[S]] => D[S] =
    cd.condition(Sort.Restrict[Option[S], S](identity))
}

trait Support[D[_]] {
  def support[T](dist: D[T]): Set[T]
}

/**
  * An example, the geometric distribution in an abstract form
  */
object GeometricDistribution {
  case object GeomVar extends RandomVar[Int]

  import GeneratorNode._

  val init: Init[Int] = Init(GeomVar)

  val shift: Map[Int, Int] = Map((n: Int) => n + 1, GeomVar, GeomVar)

  val nodeCoeffs: NodeCoeffs[VarValueSet[FD], Double, HNil, Int] =
    (init, 0.5) :: (shift, 0.5) :: GeomVar
      .target[VarValueSet[FD], Double, Int]

  val nodeCoeffSeq
      : NodeCoeffSeq.Cons[VarValueSet[FD], Double, HNil, Int] = nodeCoeffs +: NodeCoeffSeq
    .Empty[VarValueSet[FD], Double]()

  val initState: VarValueSet[FD] =
    VarValueSet[FD](Set(RandomVar.Value[Int, FD](GeomVar, FD.unif(0))), Set())

}
