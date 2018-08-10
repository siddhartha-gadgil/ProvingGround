package provingground.learning
import provingground._
import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD}
import HoTT._
import shapeless._
import HList._
import provingground.learning.GeneratorNode.{
  BaseThenCondition,
  RecursiveThenCondition
}

import scala.language.{higherKinds, implicitConversions, reflectiveCalls}

/**
  * A `sort`, i.e. type refining a scala type.
  * Can also be used for conditioning, giving one distribution from another.
  * @tparam T scala type elements of this Sort
  */
sealed trait Sort[-S, +T]

object Sort {

  /**
    * Sort of all terms with given scala type
    */
  case class All[S]() extends Sort[S, S]

  /**
    * Sort given by predicate
    */
  case class Filter[S](pred: S => Boolean) extends Sort[S, S]

  /**
    * Sort as image of an optional map, which should be injective in the `Some(_)` case
    */
  case class Restrict[S, T](optMap: S => Option[T]) extends Sort[S, T]

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
class RandomVarFamily[Dom <: HList, +O](val polyDomain: RandomVarList[Dom],
                                        val rangeFamily: Dom => Sort[_, O] =
                                          (_: Dom) => Sort.All[O]()) {
  def target[State, Boat, V, Y >: O]
    : NodeCoeffs.Target[State, Boat, V, Dom, Y] =
    NodeCoeffs.Target[State, Boat, V, Dom, Y](this)

  def at(x: Dom) = RandomVar.AtCoord(this, x)
}

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
  class SimpleFamily[U, O](domain: RandomVar[U],
                           rangeFamily: U => Sort[_, O] = (_: U) =>
                             Sort.All[O]())
      extends RandomVarFamily[U :: HNil, O](
        domain :: RandomVarList.Nil,
        { case x :: HNil => rangeFamily(x) }
      )

  /**
    * The random variable at a specific domain point of a family.
    */
  case class AtCoord[Dom <: HList, +O](family: RandomVarFamily[Dom, O],
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

sealed trait BaseGeneratorNodeFamily[Dom <: HList, +O]
    extends GeneratorNodeFamily[Dom, O]

sealed trait RecursiveGeneratorNodeFamily[Dom <: HList, State, Boat, +O]
    extends GeneratorNodeFamily[Dom, O]

object GeneratorNodeFamily {

  case class Value[Dom <: HList, O, V](family: GeneratorNodeFamily[Dom, O],
                                       value: V)

  sealed trait Pi[Dom <: HList, +O] {
    val nodes: Dom => GeneratorNode[O]
    val outputFamily: RandomVarFamily[Dom, O]
  }

  /**
    * A family of recursive generation functions, given as a function.
    */
  case class RecPi[State, Boat, Dom <: HList, +O](
      nodes: Dom => RecursiveGeneratorNode[State, Boat, O],
      outputFamily: RandomVarFamily[Dom, O])
      extends RecursiveGeneratorNodeFamily[Dom, State, Boat, O]
      with Pi[Dom, O]

  /**
    * A family of recursive generation functions, given as a function.
    */
  case class BasePi[Dom <: HList, I <: HList, +O](
      nodes: Dom => BaseGeneratorNode[I, O],
      outputFamily: RandomVarFamily[Dom, O])
      extends BaseGeneratorNodeFamily[Dom, O]
      with Pi[Dom, O]

}

/**
  * A formal node for describing recursive generation. Can have several inputList,
  * encoded as an HList, but has just one output.
  */
sealed trait GeneratorNode[+O] extends GeneratorNodeFamily[HNil, O] {
  val output: RandomVar[O]
  val outputFamily: RandomVar[O] = output

  def |[S >: O, T](condition: Sort[S, T],
                   output: RandomVar[T]): GeneratorNode[T]
}

sealed trait BaseGeneratorNode[I <: HList, +O]
    extends GeneratorNode[O]
    with BaseGeneratorNodeFamily[HNil, O] {
  val inputList: RandomVarList[I]

  def |[S >: O, T](condition: Sort[S, T],
                   output: RandomVar[T]): BaseGeneratorNode[I, T] =
    BaseThenCondition[I, S, T](this, output, condition)

}

sealed trait RecursiveGeneratorNode[State, Boat, +O]
    extends GeneratorNode[O]
    with RecursiveGeneratorNodeFamily[HNil, State, Boat, O] {
  def |[S >: O, T](
      condition: Sort[S, T],
      output: RandomVar[T]): RecursiveGeneratorNode[State, Boat, T] =
    RecursiveThenCondition[State, Boat, S, T](gen = this,
                                              output = output,
                                              condition = condition)
}

object GeneratorNode {

  /**
    * generator node for simply using the initial distribution.
    * @param input the random variable whose distribution we extract.
    */
  case class Init[X](input: RandomVar[X])
      extends BaseGeneratorNode[X :: HNil, X] {
    val output: RandomVar[X]                   = input
    val inputList: RandomVarList.Cons[X, HNil] = input :: RandomVarList.Nil
  }

  /**
    * generator node for mapping
    * @param f the function
    * @param input the input random variable
    * @param output the output random variable
    * @tparam X scala type of the input random variable
    * @tparam Y scala type of the output random variable
    */
  case class Map[X, Y](f: X => Y, input: RandomVar[X], output: RandomVar[Y])
      extends BaseGeneratorNode[X :: HNil, Y] {
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
  case class MapOpt[X, Y](f: X => Option[Y],
                          input: RandomVar[X],
                          output: RandomVar[Y])
      extends BaseGeneratorNode[X :: HNil, Y] {
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
  case class ZipMap[X1, X2, Y](f: (X1, X2) => Y,
                               input1: RandomVar[X1],
                               input2: RandomVar[X2],
                               output: RandomVar[Y])
      extends BaseGeneratorNode[X1 :: X2 :: HNil, Y] {
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
  case class ZipMapOpt[X1, X2, Y](f: (X1, X2) => Option[Y],
                                  input1: RandomVar[X1],
                                  input2: RandomVar[X2],
                                  output: RandomVar[Y])
      extends BaseGeneratorNode[X1 :: X2 :: HNil, Y] {
    val inputList
      : RandomVarList.Cons[X1, ::[X2, HNil]] = input1 :: input2 :: RandomVarList.Nil
  }

  case class FiberProductMap[X1, X2, Z, Y](quot: X1 => Z,
                                           fiberVar: Z => RandomVar[X2],
                                           f: (X1, X2) => Y,
                                           baseInput: RandomVar[X1],
                                           output: RandomVar[Y])
      extends BaseGeneratorNode[X1 :: HNil, Y] {
    val inputList: RandomVarList.Cons[X1, HNil] = baseInput :: RandomVarList.Nil
  }

  case class ZipFlatMap[X1, X2, Y](baseInput: RandomVar[X1],
                                   fiberVar: X1 => RandomVar[X2],
                                   f: (X1, X2) => Y,
                                   output: RandomVar[Y])
      extends BaseGeneratorNode[X1 :: HNil, Y] {
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
  ) extends BaseGeneratorNode[X :: HNil, Y] {
    val inputList: RandomVarList.Cons[X, HNil] = baseInput :: RandomVarList.Nil
  }

  sealed trait ThenCondition[O, Y] extends GeneratorNode[Y] {
    val gen: GeneratorNode[O]

    val output: RandomVar[Y]

    val condition: Sort[O, Y]
  }

  /**
    * compose a generator node with conditioning
    * @param gen the original node
    * @param output the output random variable
    * @param condition the condition
    * @tparam O scala type of the original output
    * @tparam Y scala type of the final output
    */
  case class BaseThenCondition[V <: HList, O, Y](
      gen: BaseGeneratorNode[V, O],
      output: RandomVar[Y],
      condition: Sort[O, Y]
  ) extends BaseGeneratorNode[V, Y]
      with ThenCondition[O, Y] {
    val inputList: RandomVarList[V] = gen.inputList
  }

  case class RecursiveThenCondition[State, Boat, O, Y](
      gen: RecursiveGeneratorNode[State, Boat, O],
      output: RandomVar[Y],
      condition: Sort[O, Y]
  ) extends RecursiveGeneratorNode[State, Boat, Y]
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
  case class Island[+Y, InitState, O, Boat](
      output: RandomVar[Y],
      islandOutput: RandomVar[O],
      initMap: InitState => (InitState, Boat),
      export: (Boat, O) => Y
  ) extends RecursiveGeneratorNode[InitState, Boat, Y]

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
  case class ComplexIsland[O, +Y, InitState, Boat, V](
      islandOutput: RandomVar[O],
      output: RandomVar[Y],
      initMap: InitState => (InitState,
                             Boat,
                             Set[GeneratorNodeFamily.Value[_ <: HList, _, V]]),
      export: (Boat, O) => Y
  ) extends RecursiveGeneratorNode[InitState, Boat, Y]

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
  implicit def ctxMapExport[F]
    : ContextExport[Term,
                    ({
                      type D[A] = Map[(A, Vector[Term]), F]
                    })#D] =
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
    randVarFamilyVals: Set[RandomVarFamily.Value[_ <: HList, _, D]])

/**
  * typeclass for providing distributions from a state
  */
trait StateDistribution[State, D[_]] {

  def value[T](state: State)(randomVar: RandomVar[T]): D[T]

  def valueAt[Dom <: HList, T](
      state: State)(randomVarFmly: RandomVarFamily[Dom, T], fullArg: Dom): D[T]
}

object StateDistribution {
  implicit def varValueStateDist[D[_], V](
      implicit emp: Empty[D]): StateDistribution[VarValueSet[D], D] =
    new StateDistribution[VarValueSet[D], D] {
      def value[T](state: VarValueSet[D])(randomVar: RandomVar[T]): D[T] =
        state.randVarVals
          .find(_.randVar == randomVar)
          .map(_.value.asInstanceOf[D[T]])
          .getOrElse(emp.empty[T])

      def valueAt[Dom <: HList, T](state: VarValueSet[D])(
          randomVarFmly: RandomVarFamily[Dom, T],
          fullArg: Dom): D[T] =
        state.randVarFamilyVals
          .find(_.randVars == randomVarFmly)
          .flatMap(_.value.asInstanceOf[Map[Dom, D[T]]].get(fullArg))
          .getOrElse(emp.empty[T])
    }

  def value[State, T, D[_]](state: State)(randomVar: RandomVar[T])(
      implicit sd: StateDistribution[State, D]): D[T] =
    sd.value(state)(randomVar)

  def valueAt[Dom <: HList, State, T, D[_]](state: State)(
      randomVarFmly: RandomVarFamily[Dom, T],
      fullArg: Dom)(implicit sd: StateDistribution[State, D]): D[T] =
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

      def valueAt[Dom <: HList, T](state: FD[O])(
          randomVarFmly: RandomVarFamily[Dom, T],
          fullArg: Dom): FD[T] = FD.empty[T]

      def update(values: RandomVarValues[FD])(init: FD[O]): FD[O] =
        values.value(randVar)
    }

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
    val randomVarVals: Set[RandomVar.Value[_, D]])(implicit emp: Empty[D])
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

sealed trait NodeCoeffSeq[State, Boat, V] {
  def ::[RDom <: HList, Y](head: NodeCoeffs[State, Boat, V, RDom, Y]) =
    NodeCoeffSeq.Cons(head, this)

  val nodeFamilies: Set[GeneratorNodeFamily[_ <: HList, _]]

  def find[RDom <: HList, Y](randomVar: RandomVarFamily[RDom, Y])
    : Option[NodeCoeffs[State, Boat, V, RDom, Y]]

  val outputs: Vector[RandomVarFamily[_ <: HList, _]]

  def update[RDom <: HList, Y](
      data: GeneratorNodeFamily.Value[RDom, Y, V]): NodeCoeffSeq[State, Boat, V]

  def updateAll(dataSeq: Seq[GeneratorNodeFamily.Value[_ <: HList, _, V]])
    : NodeCoeffSeq[State, Boat, V] =
    dataSeq.foldLeft(this)(_ update _)

}

object NodeCoeffSeq {
  case class Empty[State, Boat, V]() extends NodeCoeffSeq[State, Boat, V] {
    def find[RDom <: HList, Y](randomVar: RandomVarFamily[RDom, Y])
      : Option[NodeCoeffs[State, Boat, V, RDom, Y]] = None

    val outputs: Vector[RandomVarFamily[_ <: HList, _]] = Vector()

    val nodeFamilies: Set[GeneratorNodeFamily[_ <: HList, _]] = Set()

    def update[RDom <: HList, Y](
        data: GeneratorNodeFamily.Value[RDom, Y, V]): Empty[State, Boat, V] =
      this
  }

  case class Cons[State, Boat, V, RDom <: HList, Y](
      head: NodeCoeffs[State, Boat, V, RDom, Y],
      tail: NodeCoeffSeq[State, Boat, V])
      extends NodeCoeffSeq[State, Boat, V] {
    def find[VarRDom <: HList, VarY](randomVar: RandomVarFamily[VarRDom, VarY])
      : Option[NodeCoeffs[State, Boat, V, VarRDom, VarY]] =
      tail
        .find(randomVar)
        .orElse(
          if (head.output == randomVar)
            Some(head.asInstanceOf[NodeCoeffs[State, Boat, V, VarRDom, VarY]])
          else None
        )

    val outputs
      : Vector[RandomVarFamily[_ <: HList, _]] = head.output +: tail.outputs

    val nodeFamilies: Set[GeneratorNodeFamily[_ <: HList, _]] =
      tail.nodeFamilies union head.nodeFamilies.map((x) =>
        x: GeneratorNodeFamily[_ <: HList, _])

    def update[D <: HList, O](data: GeneratorNodeFamily.Value[D, O, V])
      : NodeCoeffSeq[State, Boat, V] =
      this.copy(head = head.update(data))
  }
}

sealed trait NodeCoeffs[State, Boat, V, RDom <: HList, Y] {
  val output: RandomVarFamily[RDom, Y]

  val nodeFamilies: Set[GeneratorNodeFamily[RDom, Y]]

  def updateOpt(data: GeneratorNodeFamily.Value[RDom, Y, V])
    : Option[NodeCoeffs[State, Boat, V, RDom, Y]]

  import scala.util.Try

  def update[D <: HList, O](data: GeneratorNodeFamily.Value[D, O, V])
    : NodeCoeffs[State, Boat, V, RDom, Y] =
    Try {
      updateOpt(data.asInstanceOf[GeneratorNodeFamily.Value[RDom, Y, V]])
    }.toOption.flatten.getOrElse(this)

  def updateAll(dataSeq: Seq[GeneratorNodeFamily.Value[_ <: HList, _, V]])
    : NodeCoeffs[State, Boat, V, RDom, Y] =
    dataSeq.foldLeft(this)(_ update _)

  import NodeCoeffs._
  def ::(
      head: (BaseGeneratorNodeFamily[RDom, Y], V)
  ) = BaseCons(head._1, head._2, this)

  def ::(head: (RecursiveGeneratorNodeFamily[RDom, State, Boat, Y], V)) =
    RecCons(head._1, head._2, this)
}

object NodeCoeffs {
  case class Target[State, Boat, V, RDom <: HList, Y](
      output: RandomVarFamily[RDom, Y]
  ) extends NodeCoeffs[State, Boat, V, RDom, Y] {
    def updateOpt(data: GeneratorNodeFamily.Value[RDom, Y, V])
      : Option[NodeCoeffs[State, Boat, V, RDom, Y]] = None

    val nodeFamilies: Set[GeneratorNodeFamily[RDom, Y]] = Set()
  }

  case class BaseCons[State, Boat, V, RDom <: HList, Y](
      headGen: BaseGeneratorNodeFamily[RDom, Y],
      headCoeff: V,
      tail: NodeCoeffs[State, Boat, V, RDom, Y]
  ) extends NodeCoeffs[State, Boat, V, RDom, Y] {
    val output: RandomVarFamily[RDom, Y] = tail.output

    val nodeFamilies
      : Set[GeneratorNodeFamily[RDom, Y]] = tail.nodeFamilies + headGen

    def updateOpt(data: GeneratorNodeFamily.Value[RDom, Y, V])
      : Option[BaseCons[State, Boat, V, RDom, Y]] =
      if (data.family == headGen) Some(this.copy(headCoeff = data.value))
      else None
  }

  case class RecCons[State, Boat, V, RDom <: HList, Y](
      headGen: RecursiveGeneratorNodeFamily[RDom, State, Boat, Y],
      headCoeff: V,
      tail: NodeCoeffs[State, Boat, V, RDom, Y]
  ) extends NodeCoeffs[State, Boat, V, RDom, Y] {
    val output: RandomVarFamily[RDom, Y] = tail.output

    val nodeFamilies
      : Set[GeneratorNodeFamily[RDom, Y]] = tail.nodeFamilies + headGen

    def updateOpt(data: GeneratorNodeFamily.Value[RDom, Y, V])
      : Option[RecCons[State, Boat, V, RDom, Y]] =
      if (data.family == headGen) Some(this.copy(headCoeff = data.value))
      else None
  }
}

case class MemoState[D[_], V, C](randVarVals: Set[RandomVar.Value[_, D]],
                                 genData: GeneratorData[V],
                                 // varFamiliesToResolve: Vector[RandomVarFamily[_ <: HList, _]],
                                 context: C)

object MemoState {
  implicit def safeMemoDistState[D[_], V, P](
      implicit emp: Empty[D]): DistributionState[MemoState[D, V, P], D] =
    new DistributionState[MemoState[D, V, P], D] {
      def value[T](state: MemoState[D, V, P])(randomVar: RandomVar[T]): D[T] =
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

  val nodeCoeffs: NodeCoeffs[VarValueSet[FD], Unit, Double, HNil, Int] =
    (init, 0.5) :: (shift, 0.5) :: GeomVar
      .target[VarValueSet[FD], Unit, Double, Int]

  val nodeCoeffSeq: NodeCoeffSeq.Cons[VarValueSet[FD],
                                      Unit,
                                      Double,
                                      HNil,
                                      Int] = nodeCoeffs :: NodeCoeffSeq
    .Empty[VarValueSet[FD], Unit, Double]()

  val initState: VarValueSet[FD] =
    VarValueSet[FD](Set(RandomVar.Value[Int, FD](GeomVar, FD.unif(0))), Set())

}
