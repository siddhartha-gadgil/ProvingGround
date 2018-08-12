package provingground.learning
import provingground.{FiniteDistribution => FD}
import shapeless._
import HList._
import provingground.learning.GeneratorNode.{FlatMap, Island, ThenCondition}

import scala.language.higherKinds
import GeneratorVariables._

case class GeneratorEquations[State, Boat](
    nodeCoeffSeq: NodeCoeffSeq[State, Boat, Double],
    initState: State)(implicit sd: StateDistribution[State, FD]) {
  val vars: Set[Variable[_]] =
    GeneratorVariables(nodeCoeffSeq, initState).allVars

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

  lazy val recurrenceEquations: Set[Equation] = nodeCoeffSeqEquations(
    nodeCoeffSeq)

  lazy val equations
    : Set[Equation] = recurrenceEquations union eventEquations union pairEventEquations union totalProbEquations

  def nodeCoeffSeqEquations(
      ncs: NodeCoeffSeq[State, Boat, Double]): Set[Equation] = ncs match {
    case NodeCoeffSeq.Empty() => Set()
    case NodeCoeffSeq.Cons(head, tail) =>
      nodeCoeffsEquations(head) union nodeCoeffSeqEquations(tail)
  }

  def nodeCoeffsEquations[Dom <: HList, Y](
      nodeCoeffs: NodeCoeffs[State, Boat, Double, Dom, Y]): Set[Equation] = {
    val (terms, eqs) = nodeCoeffsEquationTerms(nodeCoeffs)
    groupEquations(terms) union eqs
  }

  def nodeCoeffsEquationTerms[Dom <: HList, Y](
      nodeCoeffs: NodeCoeffs[State, Boat, Double, Dom, Y])
    : (Set[EquationTerm], Set[Equation]) =
    nodeCoeffs match {
      case NodeCoeffs.Target(output) => (Set(), Set())
      case bc: NodeCoeffs.Cons[State, Boat, Double, Dom, Y] =>
        val (hts, hes) = bc.headGen match {
          case gen: GeneratorNode[Y] =>
            nodeEquationTerms(gen)
          case _ => throw new Exception("node family with output not a family")
        }
        val (tts, tes) = nodeCoeffsEquationTerms(bc.tail)
        (hts.map(_ * bc.headCoeff) union tts, hes union tes)
    }

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
              EquationTerm(FinalVal(Elem(f(x), output)), p)
          }
        (eqTerms, Set())
      case MapOpt(f, input, output) =>
        val eqTerms =
          finalProbs(input).flatMap {
            case (x, p) =>
              f(x).map { y =>
                val condition = FinalVal(Event(input, Sort.Restrict(f)))
                EquationTerm(FinalVal(Elem(y, output)), p / condition)
              }
          }
        (eqTerms, Set())
      case ZipMap(f, input1, input2, output) =>
        val eqTerms =
          for {
            (x1, p1) <- finalProbs(input1)
            (x2, p2) <- finalProbs(input2)
          } yield EquationTerm(FinalVal(Elem(f(x1, x2), output)), p1 * p2)
        (eqTerms, Set())
      case zm: ZipMapOpt[o1, o2, Y] =>
        val eqTerms =
          for {
            (x1, p1) <- finalProbs(zm.input1)
            (x2, p2) <- finalProbs(zm.input2)
            condition = FinalVal(
              PairEvent(zm.input1, zm.input2, Sort.Restrict[(o1, o2), Y] {
                case (x1, x2) => zm.f(x1, x2)
              }))
            y <- zm.f(x1, x2)
          } yield
            EquationTerm(FinalVal(Elem(y, zm.output)), p1 * p2 / condition)
        (eqTerms, Set())
      case FlatMap(baseInput, fiberNode, output) =>
        val eqTerms: Set[EquationTerm] = for {
          (x, p) <- finalProbs(baseInput)
          eqT    <- nodeEquationTerms(fiberNode(x))._1
        } yield eqT * p
        val eqns: Set[Equation] = for {
          (x, p) <- finalProbs(baseInput)
          eqn    <- nodeEquationTerms(fiberNode(x))._2
        } yield eqn
        (eqTerms, eqns)
      case ZipFlatMap(baseInput, fiberVar, f, output) =>
        val eqTerms =
          for {
            (x1, p1) <- finalProbs(baseInput)
            (x2, p2) <- finalProbs(fiberVar(x1))
          } yield EquationTerm(FinalVal(Elem(f(x1, x2), output)), p1 * p2)
        (eqTerms, Set())
      case FiberProductMap(quot, fiberVar, f, baseInput, output) =>
        val d1     = finalProbs(baseInput)
        val byBase = d1.groupBy { case (x, _) => quot(x) } // final probs grouped by terms in quotient
        val baseWeights = byBase.mapValues(
          v =>
            v.map(_._2)
              .reduce[Expression](_ + _)) // weights of terms in the quotient
        val eqT =
          for {
            (z, pmf1) <- byBase // `z` is in the base, `pmf1` is all terms above `z`
            d2 = finalProbs(fiberVar(z)) // distribution of the fiber at `z`
            d = d1.zip(d2).map {
              case ((x1, p1), (x2, p2)) => (f(x1, x2), p1 * p2)
            }
            (y, p) <- d
          } yield EquationTerm(FinalVal(Elem(y, output)), p)
        (eqT.toSet, Set())
      case tc: ThenCondition[o, Y] =>
        val base = finalProbs(tc.gen.output)
        val eqT = tc.condition match {
          case Sort.All() =>
            base.map {
              case (x, p) => EquationTerm(FinalVal(Elem(x, tc.output)), p)
            }
          case s @ Sort.Filter(pred) =>
            val condition = FinalVal(Event(tc.gen.output, s))
            for {
              (x, p) <- base
              if pred(x)
            } yield EquationTerm(FinalVal(Elem(x, tc.output)), p / condition)
          case s @ Sort.Restrict(optMap) =>
            val condition = FinalVal(Event(tc.gen.output, s))
            for {
              (x, p) <- base
              y      <- optMap(x)
            } yield EquationTerm(FinalVal(Elem(x, tc.output)), p / condition)
        }
        (eqT, Set())
      case isle: Island[Y, State, o, b] =>
        val (isleInit, boat)             = isle.initMap(initState)
        val isleEq                       = GeneratorEquations(nodeCoeffSeq, isleInit)
        val isleEquations: Set[Equation] = isleEq.equations.map(_.useBoat(boat))
        val isleFinalProb                = isleEq.finalProbs(isle.islandOutput)
        val eqTerms =
          for {
            (x, FinalVal(p)) <- isleFinalProb
            y = isle.export(boat, x)
          } yield
            EquationTerm(FinalVal(Elem(y, isle.output)),
                         FinalVal(GeneratorVariables.InIsle(p, boat)))
        (eqTerms, isleEquations)
      case isle: ComplexIsland[o, Y, State, Boat, Double] =>
        val (isleInit, boat, _)          = isle.initMap(initState)
        val isleEq                       = GeneratorEquations(nodeCoeffSeq, isleInit)
        val isleEquations: Set[Equation] = isleEq.equations.map(_.useBoat(boat))
        val isleFinalProb                = isleEq.finalProbs(isle.islandOutput)
        val eqTerms =
          for {
            (x, FinalVal(p)) <- isleFinalProb
            y = isle.export(boat, x)
          } yield
            EquationTerm(FinalVal(Elem(y, isle.output)),
                         FinalVal(GeneratorVariables.InIsle(p, boat)))
        (eqTerms, isleEquations)

    }

  val finalProbVars: Map[RandomVar[Any], Set[Expression]] = vars
    .collect {
      case p @ GeneratorVariables.Elem(_, randomVar) => randomVar -> p
    }
    .groupBy(_._1)
    .mapValues(v => v.map { case (_, x) => FinalVal(x) })

  val finalProbTotals: Set[Expression] =
    finalProbVars.mapValues(_.reduce(_ + _)).values.toSet

  val totalProbEquations: Set[Equation] =
    finalProbTotals.map(t => Equation(t, Literal(1)))

  def eventTotal[X, Y](ev: Event[X, Y]): Expression = {
    val elemProbs: Set[Expression] = finalProbs(ev.base).map {
      case (x, p) if ev.sort.pred(x) => p
    }
    elemProbs.reduce(_ + _)
  }

  def pairEventTotal[X1, X2, Y](ev: PairEvent[X1, X2, Y]): Expression = {
    val elemProbs: Set[Expression] = for {
      (x1, p1) <- finalProbs(ev.base1)
      (x2, p2) <- finalProbs(ev.base2)
      if (ev.sort.pred(x1, x2))
    } yield p1 * p2
    elemProbs.reduce(_ + _)
  }

  val eventEquations: Set[Equation] = vars.collect {
    case ev: Event[x, y] => Equation(eventTotal(ev), Literal(1))
  }

  val pairEventEquations: Set[Equation] = vars.collect {
    case ev: PairEvent[x1, x2, y] => Equation(pairEventTotal(ev), Literal(1))
  }

}
