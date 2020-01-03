package provingground.learning
import provingground.{FiniteDistribution => FD}
import shapeless._
import HList._
import provingground.learning.GeneratorNode.{FlatMap, Island, ThenCondition}

import scala.language.higherKinds
import GeneratorVariables._, Expression._

import scala.util.Try

case class GeneratorEquations[State](
    nodeCoeffSeq: NodeCoeffSeq[State, Double],
    varWeight: Double,
    initState: State,
    finalState: State)(implicit sd: StateDistribution[State, FD]) extends EvolvedEquations[State] {
  pprint.log(initState)
  pprint.log(finalState)

  lazy val initVars: Set[Variable[_]] =
    GeneratorVariables(nodeCoeffSeq, initState).allVars

  lazy val elemInitVars: Map[RandomVar[_], Set[Elem[_]]] =
    initVars.collect { case e: Elem[u] => e }.groupBy(_.randomVar)

  def elemInitVarList[Dom <: HList](
      vl: RandomVarList[Dom]): Set[ElemList[Dom]] =
    vl match {
      case RandomVarList.Nil => Set(ElemList.Empty)
      case rl: RandomVarList.Cons[u, v] =>
        for {
          el <- elemInitVars.getOrElse(rl.head, Set())
          l  <- elemInitVarList(rl.tail)
        } yield (ElemList.Cons(el, l).asInstanceOf[ElemList[Dom]])
    }

  def elemFinalVarList[Dom <: HList](
      vl: RandomVarList[Dom]): Set[ElemList[Dom]] =
    vl match {
      case RandomVarList.Nil => Set(ElemList.Empty)
      case rl: RandomVarList.Cons[u, v] =>
        for {
          el <- elemFinalVars.getOrElse(rl.head, Set())
          l  <- elemFinalVarList(rl.tail)
        } yield (ElemList.Cons(el, l).asInstanceOf[ElemList[Dom]])
    }

  lazy val finalVars: Set[Variable[_]] =
    GeneratorVariables(nodeCoeffSeq, finalState).allVars

  lazy val elemFinalVars: Map[RandomVar[_], Set[Elem[_]]] =
    finalVars.collect { case e: Elem[u] => e }.groupBy(_.randomVar)

  def finalElemValOpt[Y](element: Y, rv: RandomVar[Y]): Option[FinalVal[Y]] =
    if (elemFinalVars.values.toSet.flatten.contains(Elem(element, rv)))
      Some(FinalVal(Elem(element, rv)))
    else None

  def finalProbs[Y](rv: RandomVar[Y]): Set[(Y, FinalVal[_])] =
    elemFinalVars
      .getOrElse(rv, Set())
      .map((v) => v.element.asInstanceOf[Y] -> FinalVal(v))

  def finalProbSet[Dom <: HList, Y](
      rvF: RandomVarFamily[Dom, Y]): Map[HList, Set[FinalVal[Any]]] =
    (finalVars collect {
      case e @ Elem(element, RandomVar.AtCoord(family, fullArg))
          if family == rvF =>
        fullArg -> FinalVal(e)
    }).groupBy(_._1).mapValues(s => s.map(_._2)).toMap

  def finalElemIndices[Dom <: HList, Y](
      rvF: RandomVarFamily[Dom, Y]): Set[Dom] =
    finalVars collect {
      case Elem(element, RandomVar.AtCoord(family, fullArg)) if family == rvF =>
        fullArg.asInstanceOf[Dom]
    }

  def initProbs[Y](rv: RandomVar[Y]): Set[(Y, InitialVal[_])] =
    elemInitVars
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

  lazy val equations: Set[Equation] =
    recurrenceEquations union eventEquations union pairEventEquations union totalProbEquations


  def nodeCoeffSeqEquations(
      ncs: NodeCoeffSeq[State, Double]): Set[Equation] = ncs match {
    case NodeCoeffSeq.Empty() => Set()
    case NodeCoeffSeq.Cons(head, tail) =>
      nodeCoeffsEquations(head) union nodeCoeffSeqEquations(tail)
  }

  def nodeCoeffsEquations[Dom <: HList, Y](
      nodeCoeffs: NodeCoeffs[State, Double, Dom, Y]): Set[Equation] =
    nodeCoeffs.output match {
      case _: RandomVar[Y] =>
        val (terms, eqs) = nodeCoeffsEquationTerms(nodeCoeffs, HNil)
        Equation.group(terms) union eqs
      case fmly =>
        finalElemIndices(nodeCoeffs.output).flatMap { x =>
          val (terms, eqs) = nodeCoeffsEquationTerms(nodeCoeffs, x)
          Equation.group(terms) union eqs
        }
    }

  def nodeCoeffsEquationTerms[Dom <: HList, Y](
      nodeCoeffs: NodeCoeffs[State, Double, Dom, Y],
      x: Dom): (Set[EquationNode], Set[Equation]) =
    nodeCoeffs match {
      case NodeCoeffs.Target(output) => (Set(), Set())
      case bc: NodeCoeffs.Cons[State, Double, Dom, Y] =>
        val (hts, hes) = bc.headGen match {
          case gen: GeneratorNode[Y] =>
            nodeEquationTerms(gen)
          case pf: GeneratorNodeFamily.Pi[Dom, Y] =>
            nodeEquationTerms(pf.nodes(x))
          case pf: GeneratorNodeFamily.PiOpt[Dom, Y] =>
            pf.nodesOpt(x)
              .map(nodeEquationTerms)
              .getOrElse(Set.empty[EquationNode] -> Set.empty[Equation])
          case _ =>
            throw new Exception(
              s"node family ${bc.headGen} with output ${bc.output} in $nodeCoeffs not a family")
        }
        val (tts, tes) = nodeCoeffsEquationTerms(bc.tail, x)
        (hts.map(_ * bc.headCoeff) union tts, hes union tes)
    }

  def nodeEquationTerms[Y](
      node: GeneratorNode[Y]): (Set[EquationNode], Set[Equation]) =
    node match {
      case Init(input) =>
        val eqTerms =
          initProbs(input).map {
            case (x, _) =>
              EquationNode(FinalVal(Elem(x, input)), InitialVal(Elem(x, input)))
          }
        (eqTerms, Set())
      case Atom(x, input) =>
        val eqTerm =
          EquationNode(FinalVal(Elem(x, input)), FinalVal(Elem(x, input)))
        (Set(eqTerm), Set())
      case GeneratorNode.Map(f, input, output) =>
        val eqTerms =
          finalProbs(input).flatMap {
            case (x, p) =>
              finalElemValOpt(f(x), output).map(EquationNode(_, p))
          }
        (eqTerms, Set())
      case MapOpt(f, input, output) =>
        val eqTerms =
          finalProbs(input).flatMap {
            case (x, p) =>
              f(x).flatMap { y =>
                val condition = FinalVal(Event(input, Sort.Restrict(f)))
                finalElemValOpt(y, output).map(EquationNode(_, p / condition))
              }
          }
        (eqTerms, Set())
      case ZipMap(f, input1, input2, output) =>
        val eqTerms =
          for {
            (x1, p1) <- finalProbs(input1)
            (x2, p2) <- finalProbs(input2)
            fve      <- finalElemValOpt(f(x1, x2), output)
          } yield EquationNode(fve, p1 * p2)
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
            y   <- zm.f(x1, x2)
            fve <- finalElemValOpt(y, zm.output)
          } yield EquationNode(fve, p1 * p2 / condition)
        (eqTerms, Set())
      case FlatMap(baseInput, fiberNode, output) =>
        val eqTerms: Set[EquationNode] = for {
          (x, p) <- finalProbs(baseInput)
//          _ = pprint.log(s"$fiberNode($x) = ${fiberNode(x)}")
          eqT <- nodeEquationTerms(fiberNode(x))._1
        } yield eqT * p
        val eqns: Set[Equation] = for {
          (x, p) <- finalProbs(baseInput)
          eqn    <- nodeEquationTerms(fiberNode(x))._2
        } yield eqn
        (eqTerms, eqns)
      case FlatMapOpt(baseInput, fiberNodeOpt, output) =>
        val eqTerms: Set[EquationNode] = for {
          (x, p)                   <- finalProbs(baseInput)
          (node: GeneratorNode[Y]) <- fiberNodeOpt(x).toSet
          _ = pprint.log(s"$fiberNodeOpt($x) = Some($node)")
          eqT <- nodeEquationTerms(node)._1
        } yield eqT * p
        val eqns: Set[Equation] = for {
          (x, p)                   <- finalProbs(baseInput)
          (node: GeneratorNode[Y]) <- fiberNodeOpt(x).toSet
          eqn                      <- nodeEquationTerms(node)._2
        } yield eqn
        (eqTerms, eqns)
      case ZipFlatMap(baseInput, fiberVar, f, output) =>
        val eqTerms =
          for {
            (x1, p1) <- finalProbs(baseInput)
            (x2, p2) <- finalProbs(fiberVar(x1))
            fve      <- finalElemValOpt(f(x1, x2), output)
          } yield EquationNode(fve, p1 * p2)
        (eqTerms, Set())
      case FiberProductMap(quot, fiberVar, f, baseInput, output) =>
        val d1     = finalProbs(baseInput)
        val byBase = d1.groupBy { case (x, _) => quot(x) } // final probs grouped by terms in quotient
        val baseWeights = byBase.mapValues(
          v =>
            v.map(_._2)
              .reduce[Expression](_ + _)).toMap // weights of terms in the quotient
        val eqT =
          for {
            (z, pmf1) <- byBase // `z` is in the base, `pmf1` is all terms above `z`
            d2 = finalProbs(fiberVar(z)) // distribution of the fiber at `z`
            _  = pprint.log(z)
            _  = pprint.log(fiberVar(z))
            d = pmf1.zip(d2).map {
              case ((x1, p1), (x2, p2)) =>
                Try((f(x1, x2), p1 * p2)).fold(fa => {
                  pprint.log(x1)
                  pprint.log(x2)
                  pprint.log(f)
                  pprint.log(node)
                  throw fa
                }, fb => fb)
            }
            (y, p) <- d
            fve    <- finalElemValOpt(y, output)
          } yield EquationNode(fve, p)
        (eqT.toSet, Set())
      case tc: ThenCondition[o, Y] =>
        val base = finalProbs(tc.gen.output)
        val eqT = tc.condition match {
          case Sort.All() =>
            for {
              (x, p) <- base

              fve <- finalElemValOpt(x, tc.output)
            } yield EquationNode(fve, p)

          case s @ Sort.Filter(pred) =>
            val condition = FinalVal(Event(tc.gen.output, s))
            for {
              (x, p) <- base
              if pred(x)
              fve <- finalElemValOpt(x, tc.output)
            } yield EquationNode(fve, p / condition)
          case s @ Sort.Restrict(optMap) =>
            val condition = FinalVal(Event(tc.gen.output, s))
            for {
              (x, p) <- base
              y      <- optMap(x)
              fve    <- finalElemValOpt(x, tc.output)
            } yield EquationNode(fve, p / condition)
        }
        (eqT, Set())
      case isle: Island[y, State, o, b] =>
        val (isleInit, boat) = isle.initMap(initState)(varWeight)
        val isleEq = GeneratorEquations(nodeCoeffSeq,
                                        varWeight,
                                        isleInit,
                                        isle.finalMap(boat, finalState))
        val isleEquations: Set[Equation] =
          isleEq.equations.map(_.useBoat(boat, isle))
        val isleFinalProb = isleEq.finalProbs(isle.islandOutput(boat))
        val eqTerms =
          for {
            (x, FinalVal(p)) <- isleFinalProb
            y = isle.export(boat, x)
            fve <- finalElemValOpt(y, isle.output)
          } yield
            EquationNode(fve,
                         FinalVal(GeneratorVariables.InIsle(p, boat, isle)))
        (eqTerms, isleEquations)

    }

  lazy val finalProbVars: Map[RandomVar[Any], Set[Expression]] = finalVars
    .collect {
      case p @ GeneratorVariables.Elem(_, randomVar) => randomVar -> p
    }
    .groupBy(_._1)
    .mapValues(v => v.map { case (_, x) => FinalVal(x) : Expression }).toMap

  lazy val finalProbTotals: Set[Expression] =
    finalProbVars.mapValues(_.reduce(_ + _)).values.toSet

  lazy val totalProbEquations: Set[Equation] =
    finalProbTotals.map(t => Equation(t, Literal(1)))

  def eventTotal[X, Y](ev: Event[X, Y]): Expression = {
    val elemProbs: Set[Expression] = finalProbs(ev.base).collect {
      case (x, p) if ev.sort.pred(x) => p
    }
    if (elemProbs.nonEmpty) elemProbs.reduce(_ + _) else FinalVal(ev)
  }

  def eventValue[X, Y](ev: Event[X, Y]): Double = ev.sort match {
    case Sort.All() => 1
    case Sort.Filter(pred) =>
      val b: FD[X] = sd.value(finalState)(ev.base).filter(pred)
      b.total
    case Sort.Restrict(optMap) =>
      val b: FD[Y] = sd.value(finalState)(ev.base).mapOpt(optMap)
      b.total
  }

  def pairEventTotal[X1, X2, Y](ev: PairEvent[X1, X2, Y]): Expression = {
    val elemProbs: Set[Expression] = for {
      (x1, p1) <- finalProbs(ev.base1)
      (x2, p2) <- finalProbs(ev.base2)
      if (ev.sort.pred(x1, x2))
    } yield p1 * p2
    if (elemProbs.nonEmpty) elemProbs.reduce(_ + _) else FinalVal(ev)
  }

  def pairEventValue[X1, X2, Y](ev: PairEvent[X1, X2, Y]): Double = {
    val bp: FD[(X1, X2)] =
      sd.value(finalState)(ev.base1).zip(sd.value(finalState)(ev.base2))
    ev.sort match {
      case Sort.All()            => 1
      case Sort.Filter(pred)     => bp.filter(pred).total
      case Sort.Restrict(optMap) => bp.mapOpt(optMap).total
    }
  }

  lazy val eventEquations: Set[Equation] = finalVars.collect {
    case ev: Event[x, y] => Equation(eventTotal(ev), FinalVal(ev))
  }

  lazy val pairEventEquations: Set[Equation] = finalVars.collect {
    case ev: PairEvent[x1, x2, y] => Equation(pairEventTotal(ev), FinalVal(ev))
  }

  lazy val eventValues: Map[VarVal[_], Double] = finalVars.collect {
    case ev: Event[x, y] => (FinalVal(ev): VarVal[_]) -> eventValue(ev)
  }.toMap

  lazy val pairEventValues: Map[VarVal[_], Double] = finalVars.collect {
    case ev: PairEvent[x1, x2, y] =>
      (FinalVal(ev): VarVal[_]) -> pairEventValue(ev)
  }.toMap

  lazy val initElemValues: Map[VarVal[_], Double] =
    elemInitVars.values.flatten.map {
      case v: Elem[u] =>
        (InitialVal(v): VarVal[_]) -> sd.value(initState)(v.randomVar)(
          v.element)
    }.toMap

  lazy val finalElemValues: Map[VarVal[_], Double] =
    elemInitVars.values.flatten.map {
      case v: Elem[u] =>
        (FinalVal(v): VarVal[_]) -> sd.value(initState)(v.randomVar)(v.element)
    }.toMap

  lazy val varValues
    : Map[VarVal[_], Double] = initElemValues ++ finalElemValues ++ eventValues ++ pairEventValues

}
