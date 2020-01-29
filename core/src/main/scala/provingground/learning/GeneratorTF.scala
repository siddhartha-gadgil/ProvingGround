package provingground.learning
import provingground.{HoTT, FiniteDistribution => FD}
import shapeless._
import HList._

import scala.language.higherKinds
import GeneratorVariables._, Expression._
import scala.collection.mutable.{Map => mMap}

import scala.util.Try

import monix.eval._

case class TFData(
    vars: Map[VarVal[_], Double],
    varSets: Set[Set[VarVal[_]]],
    inFinalEvent: Map[Variable[_], Set[Variable[_]]], // InIsle events are not events
    inFinalPairEvent: Map[Variable[_], Set[(Variable[_], Variable[_])]],
    equations: Set[Equation]) {

  def map(f: VariableMap): TFData = {
    TFData(
      vars.map {
        case (FinalVal(variable), p)   => FinalVal(f(variable))    -> p
        case (InitialVal(variable), p) => (InitialVal(f(variable)) -> p)
      },
      varSets.map(
        s =>
          s.map(
            (x) =>
              (x match {
                case FinalVal(variable)   => FinalVal(f(variable))
                case InitialVal(variable) => InitialVal(f(variable))
              }): VarVal[_]
        )),
      inFinalEvent.map {
        case (e, s) => (f(e) -> s.map(f))
      },
      inFinalPairEvent.map {
        case (e, s) => f(e) -> s.map { case (a, b) => (f(a), f(b)) }
      },
      equations.map(_.mapVars(f))
    )
  }

  def ++(that: TFData): TFData =
    TFData(
      vars ++ that.vars,
      varSets union that.varSets,
      inFinalEvent ++ that.inFinalEvent,
      inFinalPairEvent ++ inFinalPairEvent,
      equations union that.equations
    )
}

object TFData {
  val empty = TFData(Map(), Set(), Map(), Map(), Set())
}

object GeneratorTF {
  def fromEvolved(ev: EvolvedState, varWeight : Double = 0.3): GeneratorTF[TermState] =
    GeneratorTF(ev.params.nodeCoeffSeq, varWeight, ev.init, ev.result)

  val gset: collection.mutable.Set[GeneratorTF[_]] = collection.mutable.Set()
}

case class GeneratorTF[State](
    nodeCoeffSeq: NodeCoeffSeq[State, Double],
    varWeight: Double,
    initState: State,
    finalState: State)(implicit sd: StateDistribution[State, FD]) {
  // pprint.log(initState)
  // pprint.log(finalState)

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

  def eventSupport[X, Y](ev: Event[X, Y]): Set[Elem[_]] =
    elemFinalVars
      .getOrElse(ev.base, Set.empty[Elem[_]])
      .filter((el) => ev.sort.pred(el.element.asInstanceOf[X]))

  def pairEventSupport[X1, X2, Y](
      ev: PairEvent[X1, X2, Y]): Set[(Elem[_], Elem[_])] = {
    for {
      x1 <- elemFinalVars.getOrElse(ev.base1, Set.empty[Elem[_]])
      x2 <- elemFinalVars.getOrElse(ev.base2, Set.empty[Elem[_]])
      if (ev.sort.pred(
        (x1.element.asInstanceOf[X1], x2.element.asInstanceOf[X2])))
    } yield (x1, x2)

  }

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
    }).groupBy(_._1).mapValues(s => s.map(_._2))

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

  lazy val (recurrenceEquations: Set[Equation], recData) =
    nodeCoeffSeqEquations(nodeCoeffSeq)

  lazy val equations
    : Set[Equation] = recurrenceEquations union recData.equations

  lazy val tfData: TFData = recData.copy(equations = equations)

  def nodeCoeffSeqEquations(
      ncs: NodeCoeffSeq[State, Double]): (Set[Equation], TFData) =
    ncs match {
      case NodeCoeffSeq.Empty()          => Set() -> TFData.empty
      case NodeCoeffSeq.Cons(head, tail) =>
        // pprint.log(head.output)
        (
          nodeCoeffsEquations(head)._1 union nodeCoeffSeqEquations(tail)._1,
          nodeCoeffsEquations(head)._2 ++ nodeCoeffSeqEquations(tail)._2
        )
    }

  def nodeCoeffsEquations[Dom <: HList, Y](
      nodeCoeffs: NodeCoeffs[State, Double, Dom, Y])
    : (Set[Equation], TFData) =
    nodeCoeffs.output match {
      case _: RandomVar[Y] =>
        // pprint.log(nodeCoeffs.output)
        val (terms, data) = nodeCoeffsEquationTerms(nodeCoeffs, HNil)
        Equation.group(terms) -> data
      case fmly =>
        val eqns = finalElemIndices(nodeCoeffs.output).flatMap { x =>
          // pprint.log(x)
          val (terms, _) = nodeCoeffsEquationTerms(nodeCoeffs, x)
          Equation.group(terms)
        }
        val dataSeq = finalElemIndices(nodeCoeffs.output).map { x =>
          nodeCoeffsEquationTerms(nodeCoeffs, x)._2
        }
        eqns -> dataSeq.foldLeft(baseData)(_ ++ _)

    }

  def nodeCoeffsEquationTerms[Dom <: HList, Y](
      nodeCoeffs: NodeCoeffs[State, Double, Dom, Y],
      x: Dom): (Set[EquationNode], TFData) =
    nodeCoeffs match {
      case NodeCoeffs.Target(output) => (Set(), baseData)
      case bc: NodeCoeffs.Cons[State, Double, Dom, Y] =>
        val (hts, hes) = bc.headGen match {
          case gen: GeneratorNode[Y] =>
            // pprint.log(gen)
            nodeEquationTerms(gen)
          case pf: GeneratorNodeFamily.Pi[Dom, Y] =>
            nodeEquationTerms(pf.nodes(x))
          case pf: GeneratorNodeFamily.PiOpt[Dom, Y] =>
            pf.nodesOpt(x)
              .map(nodeEquationTerms)
              .getOrElse(Set.empty[EquationNode] -> baseData)
          case _ =>
            throw new Exception(
              s"node family ${bc.headGen} with output ${bc.output} in $nodeCoeffs not a family")
        }
        val (tts, tes) = nodeCoeffsEquationTerms(bc.tail, x)
        (hts.map(_ * bc.headCoeff) union tts, hes ++ tes)
    }

  lazy val baseData: TFData =
    TFData(
      initElemValues ++ finalElemValues,
      elemInitVars.values.toSet
        .map((s: Set[Elem[_]]) => s.map(x => InitialVal(x): VarVal[_])) union elemFinalVars.values.toSet
        .map((s: Set[Elem[_]]) => s.map(x => FinalVal(x): VarVal[_])),
      finalVars.collect {
        case ev: Event[x, y] =>
          ((ev: Variable[_]), eventSupport(ev).map((x) => x: Variable[_]))
      }.toMap,
      finalVars.collect {
        case ev: PairEvent[x1, x2, y] =>
          ((ev: Variable[_]), pairEventSupport(ev).map {
            case (a, b) => (a: Variable[_]) -> (b: Variable[_])
          })
      }.toMap,
      Set()
    )

  val nodeMap: mMap[GeneratorNode[_], (Set[EquationNode], TFData)] = mMap()

  def nodeEquationTerms[Y](
      node: GeneratorNode[Y]): (Set[EquationNode], TFData) =
    nodeMap.getOrElse(
      node, {
//        pprint.log(node)
        val result = node match {
          case Init(input) =>
            val eqTerms =
              initProbs(input).map {
                case (x, _) =>
                  EquationNode(FinalVal(Elem(x, input)),
                               InitialVal(Elem(x, input)))
              }
            (eqTerms, baseData)
          case Atom(x, input) =>
            val eqTerm =
              EquationNode(FinalVal(Elem(x, input)), FinalVal(Elem(x, input)))
            (Set(eqTerm), baseData)
          case GeneratorNode.Map(f, input, output) =>
            val eqTerms =
              finalProbs(input).flatMap {
                case (x, p) =>
                  finalElemValOpt(f(x), output).map(EquationNode(_, p))
              }
            (eqTerms, baseData)
          case MapOpt(f, input, output) =>
            val eqTerms =
              finalProbs(input).flatMap {
                case (x, p) =>
                  f(x).flatMap { y =>
                    val condition = FinalVal(Event(input, Sort.Restrict(f)))
                    finalElemValOpt(y, output).map(
                      EquationNode(_, p / condition))
                  }
              }
            (eqTerms, baseData)
          case ZipMap(f, input1, input2, output) =>
            val eqTerms =
              for {
                (x1, p1) <- finalProbs(input1)
                (x2, p2) <- finalProbs(input2)
                fve      <- finalElemValOpt(f(x1, x2), output)
              } yield EquationNode(fve, p1 * p2)
            (eqTerms, baseData)
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
            (eqTerms, baseData)
          case FlatMap(baseInput, fiberNode, output) =>
            val eqTerms: Set[EquationNode] = for {
              (x, p) <- finalProbs(baseInput)
              // _ = pprint.log(s"$fiberNode($x) = ${fiberNode(x)}")
              eqT <- nodeEquationTerms(fiberNode(x))._1
            } yield eqT * p
            val allData: Set[TFData] = for {
              (x, p) <- finalProbs(baseInput)
              eqn = nodeEquationTerms(fiberNode(x))._2
            } yield eqn
            (eqTerms, allData.foldLeft(baseData)(_ ++ _))
          case FlatMapOpt(baseInput, fiberNodeOpt, output) =>
            val eqTerms: Set[EquationNode] = for {
              (x, p)                   <- finalProbs(baseInput)
              (node: GeneratorNode[Y]) <- fiberNodeOpt(x).toSet
              // _ = pprint.log(s"$fiberNodeOpt($x) = Some($node)")
              eqT <- nodeEquationTerms(node)._1
            } yield eqT * p
            val allData: Set[TFData] = for {
              (x, p)                   <- finalProbs(baseInput)
              (node: GeneratorNode[Y]) <- fiberNodeOpt(x).toSet
              eqn = nodeEquationTerms(node)._2
            } yield eqn
            (eqTerms, allData.foldLeft(baseData)(_ ++ _))
          case ZipFlatMap(baseInput, fiberVar, f, output) =>
            val eqTerms =
              for {
                (x1, p1) <- finalProbs(baseInput)
                (x2, p2) <- finalProbs(fiberVar(x1))
                fve      <- finalElemValOpt(f(x1, x2), output)
              } yield EquationNode(fve, p1 * p2)
            (eqTerms, baseData)
          case FiberProductMap(quot, fiberVar, f, baseInput, output) =>
            val d1     = finalProbs(baseInput)
            val byBase = d1.groupBy { case (x, _) => quot(x) } // final probs grouped by terms in quotient
            val baseWeights = byBase.mapValues(v =>
              v.map(_._2)
                .reduce[Expression](_ + _)) // weights of terms in the quotient
            val eqT =
              for {
                (z, pmf1) <- byBase // `z` is in the base, `pmf1` is all terms above `z`
                d2 = finalProbs(fiberVar(z)) // distribution of the fiber at `z`
//            _  = pprint.log(z)
//            _  = pprint.log(fiberVar(z))
                d = pmf1.zip(d2).map {
                  case ((x1, p1), (x2, p2)) =>
                    Try((f(x1, x2), p1 * p2)).fold(fa => {
                      // pprint.log(x1)
                      // pprint.log(x2)
                      // pprint.log(f)
                      // pprint.log(node)
                      throw fa
                    }, fb => fb)
                }
                (y, p) <- d
                fve    <- finalElemValOpt(y, output)
              } yield EquationNode(fve, p)
            (eqT.toSet, baseData)
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
            (eqT, baseData)
          case isle: Island[_, State, o, b] =>
            val (isleInit, boat) = isle.initMap(initState)(varWeight)
            val fs               = isle.finalMap(boat, finalState)
            if (sd.isEmpty(fs)) (Set.empty[EquationNode], TFData.empty)
            else {
              val isleEqNew =
                GeneratorTF(nodeCoeffSeq, varWeight, isleInit, fs)
              val isleEq =
                GeneratorTF.gset.find(_ == isleEqNew).getOrElse(isleEqNew)
              GeneratorTF.gset += isleEq
              val isleData: TFData =
                isleEq.tfData.map((x) => InIsle(x, boat, isle))
              val isleFinalProb = isleEq.finalProbs(isle.islandOutput(boat))
              val eqTerms =
                for {
                  (x, FinalVal(p)) <- isleFinalProb
                  y = isle.export(boat, x)
                  fve <- finalElemValOpt(y, isle.output)
                } yield
                  EquationNode(
                    fve,
                    FinalVal(GeneratorVariables.InIsle(p, boat, isle)))
              (eqTerms, baseData ++ isleData)
            }

        }
//        pprint.log(node)
//        pprint.log(result)
        nodeMap += node -> result
        // pprint.log(nodeMap.size)
        result
      }
    )

  def nodeEquationTermsTask[Y](
      node: GeneratorNode[Y]): Task[(Set[EquationNode], TFData)] =
    if (nodeMap.keySet.contains(node)) Task.now(nodeMap(node))
    else {
      //        pprint.log(node)
      val resultTask = node match {
        case Init(input) =>
          Task {
            val eqTerms =
              initProbs(input).map {
                case (x, _) =>
                  EquationNode(FinalVal(Elem(x, input)),
                               InitialVal(Elem(x, input)))
              }
            (eqTerms, baseData)
          }
        case Atom(x, input) =>
          Task {
            val eqTerm =
              EquationNode(FinalVal(Elem(x, input)), FinalVal(Elem(x, input)))
            (Set(eqTerm), baseData)
          }
        case GeneratorNode.Map(f, input, output) =>
          Task {
            val eqTerms =
              finalProbs(input).flatMap {
                case (x, p) =>
                  finalElemValOpt(f(x), output).map(EquationNode(_, p))
              }
            (eqTerms, baseData)
          }
        case MapOpt(f, input, output) =>
          Task {
            val eqTerms =
              finalProbs(input).flatMap {
                case (x, p) =>
                  f(x).flatMap { y =>
                    val condition = FinalVal(Event(input, Sort.Restrict(f)))
                    finalElemValOpt(y, output).map(
                      EquationNode(_, p / condition))
                  }
              }
            (eqTerms, baseData)
          }
        case ZipMap(f, input1, input2, output) =>
          Task {
            val eqTerms =
              for {
                (x1, p1) <- finalProbs(input1)
                (x2, p2) <- finalProbs(input2)
                fve      <- finalElemValOpt(f(x1, x2), output)
              } yield EquationNode(fve, p1 * p2)
            (eqTerms, baseData)
          }
        case zm: ZipMapOpt[o1, o2, Y] =>
          Task {
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
            (eqTerms, baseData)
          }
        case FlatMap(baseInput, fiberNode, output) =>
          val weightedTasks
            : Set[(Task[(Set[EquationNode], TFData)], FinalVal[_])] =
            for {
              (x, p) <- finalProbs(baseInput)
            } yield nodeEquationTermsTask(fiberNode(x)).memoize -> p
          val eqTermTask: Task[Set[EquationNode]] =
            Task
              .gather(weightedTasks.map {
                case (tsk, p) => tsk.map { case (eqTS, _) => eqTS.map(_ * p) }
              })
              .map(_.flatten)
          val dataTask =
            Task
              .gather(weightedTasks.map { case (tsk, _) => tsk.map(_._2) })
              .map(s => s.foldLeft(baseData)(_ ++ _))
          for {
            eqTerms <- eqTermTask
            data    <- dataTask
          } yield (eqTerms, data)
        case FlatMapOpt(baseInput, fiberNodeOpt, output) =>
          val weightedTasks
            : Set[(Task[(Set[EquationNode], TFData)], FinalVal[_])] =
            for {
              (x, p) <- finalProbs(baseInput)
              node   <- fiberNodeOpt(x)
            } yield nodeEquationTermsTask(node).memoize -> p
          val eqTermTask: Task[Set[EquationNode]] =
            Task
              .gather(weightedTasks.map {
                case (tsk, p) => tsk.map { case (eqTS, _) => eqTS.map(_ * p) }
              })
              .map(_.flatten)
          val dataTask =
            Task
              .gather(weightedTasks.map { case (tsk, _) => tsk.map(_._2) })
              .map(s => s.foldLeft(baseData)(_ ++ _))
          for {
            eqTerms <- eqTermTask
            data    <- dataTask
          } yield (eqTerms, data)
        case ZipFlatMap(baseInput, fiberVar, f, output) =>
          Task {
            val eqTerms =
              for {
                (x1, p1) <- finalProbs(baseInput)
                (x2, p2) <- finalProbs(fiberVar(x1))
                fve      <- finalElemValOpt(f(x1, x2), output)
              } yield EquationNode(fve, p1 * p2)
            (eqTerms, baseData)
          }
        case FiberProductMap(quot, fiberVar, f, baseInput, output) =>
          Task {
            val d1     = finalProbs(baseInput)
            val byBase = d1.groupBy { case (x, _) => quot(x) } // final probs grouped by terms in quotient
            val baseWeights = byBase.mapValues(v =>
              v.map(_._2)
                .reduce[Expression](_ + _)) // weights of terms in the quotient
            val eqT =
              for {
                (z, pmf1) <- byBase // `z` is in the base, `pmf1` is all terms above `z`
                d2 = finalProbs(fiberVar(z)) // distribution of the fiber at `z`
                //            _  = pprint.log(z)
                //            _  = pprint.log(fiberVar(z))
                d = pmf1.zip(d2).map {
                  case ((x1, p1), (x2, p2)) =>
                    Try((f(x1, x2), p1 * p2)).fold(fa => {
                      // pprint.log(x1)
                      // pprint.log(x2)
                      // pprint.log(f)
                      // pprint.log(node)
                      throw fa
                    }, fb => fb)
                }
                (y, p) <- d
                fve    <- finalElemValOpt(y, output)
              } yield EquationNode(fve, p)
            (eqT.toSet, baseData)
          }
        case tc: ThenCondition[o, Y] =>
          Task {
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
            (eqT, baseData)
          }
        case isle: Island[_, State, o, b] =>
          Task {
            val (isleInit, boat) = isle.initMap(initState)(varWeight)
            val fs               = isle.finalMap(boat, finalState)
            if (sd.isEmpty(fs)) (Set.empty[EquationNode], TFData.empty)
            else {
              val isleEqNew =
                GeneratorTF(nodeCoeffSeq, varWeight, isleInit, fs)
              val isleEq =
                GeneratorTF.gset.find(_ == isleEqNew).getOrElse(isleEqNew)
              GeneratorTF.gset += isleEq
              val isleData: TFData =
                isleEq.tfData.map((x) => InIsle(x, boat, isle))
              val isleFinalProb = isleEq.finalProbs(isle.islandOutput(boat))
              val eqTerms =
                for {
                  (x, FinalVal(p)) <- isleFinalProb
                  y = isle.export(boat, x)
                  fve <- finalElemValOpt(y, isle.output)
                } yield
                  EquationNode(
                    fve,
                    FinalVal(GeneratorVariables.InIsle(p, boat, isle)))
              (eqTerms, baseData ++ isleData)
            }
          }
      }

      resultTask.map { result =>
        nodeMap += node -> result
        result
      }
    }

  def nodeCoeffsEquationTermsTask[Dom <: HList, Y](
      nodeCoeffs: NodeCoeffs[State, Double, Dom, Y],
      x: Dom): Task[(Set[EquationNode], TFData)] =
    nodeCoeffs match {
      case NodeCoeffs.Target(output) => Task.now((Set(), baseData))
      case bc: NodeCoeffs.Cons[State, Double, Dom, Y] =>
        val tsk = bc.headGen match {
          case gen: GeneratorNode[Y] =>
            nodeEquationTermsTask(gen)
          case pf: GeneratorNodeFamily.Pi[Dom, Y] =>
            nodeEquationTermsTask(pf.nodes(x))
          case pf: GeneratorNodeFamily.PiOpt[Dom, Y] =>
            pf.nodesOpt(x)
              .map(nodeEquationTermsTask)
              .getOrElse(Task.now(Set.empty[EquationNode] -> baseData))
          case _ =>
            throw new Exception(
              s"node family ${bc.headGen} with output ${bc.output} in $nodeCoeffs not a family")
        }
        for {
          pair <- tsk
          (hts, hes) = pair
          pair2 <- nodeCoeffsEquationTermsTask(bc.tail, x)
          (tts, tes) = pair2
        } yield (hts.map(_ * bc.headCoeff) union tts, hes ++ tes)
    }

  def nodeCoeffSeqEquationsTask(
      ncs: NodeCoeffSeq[State, Double]): Task[(Set[Equation], TFData)] =
    ncs match {
      case NodeCoeffSeq.Empty() => Task.now(Set() -> TFData.empty)
      case NodeCoeffSeq.Cons(head, tail) =>
        for {
          hp <- nodeCoeffsEquationsTask(head)
          tp <- nodeCoeffSeqEquationsTask(tail)
        } yield (hp._1 union (tp._1), hp._2 ++ tp._2)
    }

  def nodeCoeffsEquationsTask[Dom <: HList, Y](
      nodeCoeffs: NodeCoeffs[State, Double, Dom, Y])
    : Task[(Set[Equation], TFData)] =
    nodeCoeffs.output match {
      case _: RandomVar[Y] =>
        // pprint.log(nodeCoeffs.output)
        nodeCoeffsEquationTermsTask(nodeCoeffs, HNil).map {
          case (terms, data) =>
            Equation.group(terms) -> data
        }
      case fmly =>
        val tskSet: Set[Task[(Set[EquationNode], TFData)]] =
          finalElemIndices(nodeCoeffs.output).map { x =>
            nodeCoeffsEquationTermsTask(nodeCoeffs, x).memoize
          }
        val eqnTask: Task[Set[Equation]] = Task
          .gather(tskSet.map(s => s.map(_._1)))
          .map(_.flatten)
          .map(Equation.group)
        val dataTask: Task[TFData] = Task
          .gather(tskSet.map(s => s.map(_._2)))
          .map(_.foldLeft(baseData)(_ ++ _))
        for {
          eqs  <- eqnTask
          data <- dataTask
        } yield (eqs, data)

    }

  val recTask: Task[(Set[Equation], TFData)] = nodeCoeffSeqEquationsTask(
    nodeCoeffSeq).memoize

  val eqnTask: Task[Set[Equation]] = recTask.map {
    case (re, rd) => re union (rd.equations)
  }.memoize

  lazy val finalProbVars: Map[RandomVar[Any], Set[Expression]] = finalVars
    .collect {
      case p @ GeneratorVariables.Elem(_, randomVar) => randomVar -> p
    }
    .groupBy(_._1)
    .mapValues(v => v.map { case (_, x) => FinalVal(x) })

  lazy val finalProbTotals: Set[Expression] =
    finalProbVars.mapValues(_.reduce(_ + _)).values.toSet

}
