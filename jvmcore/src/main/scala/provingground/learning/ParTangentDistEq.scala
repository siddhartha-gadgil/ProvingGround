package provingground.learning

import provingground._, HoTT._

import GeneratorVariables._, Expression._

import scala.collection.parallel._
import shapeless._
import TermRandomVars._
import ParMapState._
import DE.{finalProb, initProb}
import provingground.learning.GeneratorNode._

import scala.collection.parallel.immutable.ParVector
import scala.math.Ordering.Double.TotalOrdering
import spire.syntax.group
import provingground.learning.Sort.All
import provingground.learning.Sort.Filter
import provingground.learning.Sort.Restrict

object ParTangentDistEq {
  def fromParams(
      baseState: ParMapState,
      tg: TermGenParams,
      varWeight: Double = 0.3
  ) =
    new ParTangentDistEq(
      ParMapState.parNodeSeq(tg).nodeCoeffSeq,
      baseState,
      varWeight
    )
}
class ParTangentDistEq(
    nodeCoeffSeqParam: NodeCoeffSeq[ParMapState, Double],
    baseState: ParMapState,
    varWeight: Double = 0.3,
    val memo: ParDistEqMemo = new ParDistEqMemo
) extends RecParDistEq {
  val nodeCoeffSeq: NodeCoeffSeq[ParMapState, Double] = nodeCoeffSeqParam

  def baseDist[Y](rv: RandomVar[Y], cutoff: Double): ParMap[Y, Double] =
    purge(baseState.value(rv), cutoff)

  def tangentDist[Y](
      tangentState: ParMapState,
      rv: RandomVar[Y],
      cutoff: Double
  ): ParMap[Y, Double] = purge(tangentState.value(rv), cutoff)

  def nodeDistCalc[Y](
      tangentState: ParMapState,
      maxDepth: Option[Int],
      halted: => Boolean
  )(
      generatorNode: GeneratorNode[Y],
      epsilon: Double,
      coeff: Expression
  ): (ParMap[Y, Double], ParSet[EquationNode]) =
    if (epsilon > 1 || halted)
      (ParMap.empty[Y, Double], ParSet.empty[EquationNode])
    else
      generatorNode match {
        case Init(input) =>
          val initDist = baseDist(input, epsilon)
          val eqs = initDist.keySet.map { x =>
            EquationNode(finalProb(x, input), coeff * initProb(x, input))
          }
          (initDist, eqs)
        case Atom(value, output) => (ParMap(value -> 1.0), ParSet.empty)
        case provingground.learning.GeneratorNode.Map(f, input, output) =>
          val (dist, eqs) =
            varDist(tangentState, maxDepth, halted)(input, epsilon)
          val meqs = dist.keySet.map { (x) =>
            EquationNode(
              finalProb(f(x), output),
              coeff * finalProb(x, input)
            )
          }
          (mapMap(dist, f), parUnion(eqs, meqs))
        case MapOpt(f, input, output) =>
          val (dist, eqs) =
            varDist(tangentState, maxDepth, halted)(input, epsilon)
          val meqs = dist.keySet.flatMap { (x) =>
            f(x).map(
              y =>
                EquationNode(
                  finalProb(y, output),
                  coeff * finalProb(x, input)
                )
            )
          }
          (mapMapOpt(dist, f), parUnion(eqs, meqs))
        case zm: ZipMap[x1, x2, Y] =>
          import zm._
          val (dist1, eqs1) =
            varDist(tangentState, maxDepth.map(_ - 1), halted)(input1, epsilon)
          val (dist2, eqs2) =
            varDist(tangentState, maxDepth.map(_ - 1), halted)(input2, epsilon)
          val triples1 = (for {
            (x1, p1) <- tangentDist(tangentState, input1, epsilon)
            (x2, p2) <- dist2
            if p1 * p2 >= epsilon
          } yield ((x1, x2, f(x1, x2)), p1 * p2)).to(ParVector)
          val triples2 = (for {
            (x1, p1) <- dist1
            (x2, p2) <- tangentDist(tangentState, input2, epsilon)
            if p1 * p2 >= epsilon
          } yield ((x1, x2, f(x1, x2)), p1 * p2)).to(ParVector)
          val triples = triples1 ++ triples2
          val meqs = triples
            .map {
              case ((x1, x2, y), _) =>
                EquationNode(
                  finalProb(y, output),
                  coeff * finalProb(x1, input1) * finalProb(x2, input2)
                )
            }
            .to(ParSet)
          val tripleMap =
            makeMap(triples.map { case ((_, _, y), p) => (y, p) })
          (tripleMap, parUnion(eqs1, parUnion(eqs2, meqs)))
        case zm: ZipMapOpt[x1, x2, Y] =>
          import zm._
          val (dist1, eqs1) =
            varDist(tangentState, maxDepth.map(_ - 1), halted)(input1, epsilon)
          val (dist2, eqs2) =
            varDist(tangentState, maxDepth.map(_ - 1), halted)(input2, epsilon)
          val triples1 =
            (for {
              (x1, p1) <- tangentDist(tangentState, input1, epsilon)
              (x2, p2) <- dist2
              if p1 * p2 >= epsilon
              y <- f(x1, x2)
            } yield ((x1, x2, y), p1 * p2)).to(ParVector)
          val triples2 = (for {
            (x1, p1) <- dist1
            (x2, p2) <- tangentDist(tangentState, input2, epsilon)
            if p1 * p2 >= epsilon
            y <- f(x1, x2)
          } yield ((x1, x2, y), p1 * p2)).to(ParVector)
          val triples = triples1 ++ triples2
          val meqs = triples
            .map {
              case ((x1, x2, y), _) =>
                EquationNode(
                  finalProb(y, output),
                  coeff * finalProb(x1, input1) * finalProb(x2, input2)
                )
            }
            .to(ParSet)
          val tripleMap =
            makeMap(triples.map { case ((_, _, y), p) => (y, p) })
          (tripleMap, parUnion(eqs1, parUnion(eqs2, meqs)))
        case fpm: FiberProductMap[x1, x2, z, Y] =>
          import fpm._
          val (d1, baseEqs) =
            varDist(tangentState, maxDepth.map(_ - 1), halted)(
              baseInput,
              epsilon
            )
          val byBase1         = d1.groupBy(xp => quot(xp._1))
          val baseMaxWeights1 = byBase1.mapValues(_.values.max)
          val groups1 = baseMaxWeights1.map {
            case (z, p) =>
              val result = tangentDist(tangentState, fiberVar(z), epsilon / p)
              z -> result
          }
          val triples1 = d1
            .flatMap {
              case (x1, p1) =>
                val cutoff = epsilon / p1
                groups1(quot(x1)).flatMap {
                  case (x2, p2) if p2 > cutoff =>
                    Some((x1, x2, f(x1, x2)), p1 * p2)
                  case _ => None
                }
            }
            .to(ParVector)
          val d2 =
            tangentDist(tangentState, baseInput, epsilon)
          val byBase2         = d2.groupBy(xp => quot(xp._1))
          val baseMaxWeights2 = byBase2.mapValues(_.values.max)
          val groups2 = baseMaxWeights2.map {
            case (z, p) =>
              val result = varDist(tangentState, maxDepth.map(_ - 1), halted)(
                fiberVar(z),
                epsilon / p
              )
              z -> result
          }
          val fiberEqs = groups2.flatMap(_._2._2).to(ParSet)
          val triples2 = d2
            .flatMap {
              case (x1, p1) =>
                val cutoff = epsilon / p1
                groups2(quot(x1))._1.flatMap {
                  case (x2, p2) if p2 > cutoff =>
                    Some((x1, x2, f(x1, x2)), p1 * p2)
                  case _ => None
                }
            }
            .to(ParVector)
          val triples = triples1 ++ triples2
          val fibEqs = triples
            .map {
              case ((x1, x2, y), _) =>
                EquationNode(
                  finalProb(y, output),
                  coeff * finalProb(x1, baseInput) * finalProb(
                    x2,
                    fiberVar(quot(x1))
                  )
                )
            }
            .to(ParSet)
          val tripleMap =
            makeMap(triples.map { case ((_, _, y), p) => (y, p) })
          (tripleMap, parUnion(baseEqs, parUnion(fibEqs, fiberEqs)))
        case zfm: ZipFlatMap[x1, x2, Y] =>
          import zfm._
          val (baseDist1, baseEqs) =
            varDist(tangentState, maxDepth.map(_ - 1), halted)(
              baseInput,
              epsilon
            )
          val fibers1 = baseDist1.to(ParVector).map {
            case (x1, p1) =>
              val dist2 = tangentDist(tangentState, fiberVar(x1), epsilon / p1)
              (
                dist2.map { case (x2, p2) => ((x1, x2, f(x1, x2)), p1 * p2) }
              )
          }
          val triples1 = fibers1.flatten
          val baseDist2 =
            tangentDist(tangentState, baseInput, epsilon)
          val fibers2 = baseDist2.to(ParVector).map {
            case (x1, p1) =>
              val (dist2, eqs2) = varDist(
                tangentState,
                maxDepth.map(_ - 1),
                halted
              )(fiberVar(x1), epsilon / p1)
              (
                dist2.map { case (x2, p2) => ((x1, x2, f(x1, x2)), p1 * p2) },
                eqs2
              )
          }
          val fiberEqs = fibers2.flatMap(_._2).to(ParSet)
          val triples2 = fibers2.flatMap(_._1)
          val triples  = triples1 ++ triples2
          val fibEqs = triples
            .map {
              case ((x1, x2, y), _) =>
                EquationNode(
                  finalProb(y, output),
                  coeff * finalProb(x1, baseInput) * finalProb(x2, fiberVar(x1))
                )
            }
            .to(ParSet)
          val tripleMap =
            makeMap(triples.map { case ((_, _, y), p) => (y, p) })
          (tripleMap, parUnion(baseEqs, parUnion(fibEqs, fiberEqs)))
        case fm: FlatMap[x, Y] =>
          import fm._
          val (baseDist1, baseEqs) =
            varDist(tangentState, maxDepth.map(_ - 1), halted)(
              baseInput,
              epsilon
            )
          val fibers1 = baseDist1.to(ParVector).map {
            case (x1, p1) =>
              val dist2 =
                tangentDist(tangentState, fiberNode(x1).output, epsilon / p1)
              (
                dist2.map { case (x2, p2) => ((x1, x2), p1 * p2) }
              )
          }
          val pairs1 = fibers1.flatten
          val baseDist2 =
            tangentDist(tangentState, baseInput, epsilon)
          val fibers2 = baseDist1.to(ParVector).map {
            case (x1, p1) =>
              val (dist2, eqs2) = nodeDist(
                tangentState,
                maxDepth.map(_ - 1),
                halted
              )(fiberNode(x1), epsilon / p1, coeff)
              (
                dist2.map { case (x2, p2) => ((x1, x2), p1 * p2) },
                eqs2
              )
          }
          val pairs2   = fibers2.flatMap(_._1)
          val pairs    = pairs1 ++ pairs2
          val fiberEqs = fibers2.flatMap(_._2).to(ParSet)
          val fibEqs = pairs
            .map {
              case ((x1, x2), _) =>
                EquationNode(
                  finalProb(x2, output),
                  coeff * finalProb(x1, baseInput) * finalProb(
                    x2,
                    fiberNode(x1).output
                  )
                )
            }
            .to(ParSet)
          val pairMap =
            pairs.map { case ((x1, x2), p) => (x2, p) }.to(ParMap)
          (pairMap, parUnion(baseEqs, parUnion(fibEqs, fiberEqs)))
        case fm: FlatMapOpt[x, Y] =>
          import fm._
          val (baseDist1, baseEqs) =
            varDist(tangentState, maxDepth.map(_ - 1), halted)(
              baseInput,
              epsilon
            )
          val fibers1 = baseDist1.to(ParVector).flatMap {
            case (x1, p1) =>
              fiberNodeOpt(x1).map { node =>
                val dist2 = tangentDist(tangentState, node.output, epsilon / p1)
                (
                  dist2.map { case (x2, p2) => ((x1, x2), p1 * p2, node) }
                )
              }
          }
          val pairs1 = fibers1.flatten
          val baseDist2 =
            tangentDist(tangentState, baseInput, epsilon)
          val fibers2 = baseDist2.to(ParVector).flatMap {
            case (x1, p1) =>
              fiberNodeOpt(x1).map { node =>
                val (dist2, eqs2) = nodeDist(
                  tangentState,
                  maxDepth.map(_ - 1),
                  halted
                )(node, epsilon / p1, coeff)
                (
                  dist2.map { case (x2, p2) => ((x1, x2), p1 * p2, node) },
                  eqs2
                )
              }
          }
          val pairs2   = fibers2.flatMap(_._1)
          val pairs    = pairs1 ++ pairs1
          val fiberEqs = fibers2.flatMap(_._2).to(ParSet)
          val fibEqs = pairs
            .map {
              case ((x1, x2), _, node) =>
                EquationNode(
                  finalProb(x2, output),
                  coeff * finalProb(x1, baseInput) * finalProb(
                    x2,
                    node.output
                  )
                )
            }
            .to(ParSet)
          val pairMap =
            pairs.map { case ((x1, x2), p, _) => (x2, p) }.to(ParMap)
          (pairMap, parUnion(baseEqs, parUnion(fibEqs, fiberEqs)))
        case tc: ThenCondition[o, Y] =>
          import tc._
          val base =
            nodeDist(tangentState, maxDepth, halted)(gen, epsilon, coeff)
          val event = Event(tc.gen.output, tc.condition)
          val finEv = FinalVal(event)
          import Sort._
          condition match {
            case _: All[_] => base
            case c: Filter[_] =>
              val (dist, eqs) = base
              val condDist    = normalize(dist.filter { case (x, _) => c.pred(x) })
              val ceqs = condDist.keySet.map { x =>
                EquationNode(
                  finalProb(x, tc.output),
                  finalProb(x, tc.gen.output) / finEv
                )
              }
              val evEqs = condDist.keySet.map(
                x =>
                  EquationNode(
                    finEv,
                    finalProb(x, tc.output)
                  )
              )
              (condDist, parUnion(eqs, parUnion(ceqs, evEqs)))
            case res: Restrict[u, Y] =>
              val (dist, eqs) = base
              val condDist    = mapMapOpt(dist, res.optMap)
              val ceqs = for {
                x <- dist.keySet
                y <- res.optMap(x)
              } yield
                EquationNode(
                  finalProb(y, tc.output),
                  finalProb(x, tc.gen.output) / finEv
                )
              val evEqs: ParSet[EquationNode] =
                condDist.keySet.map(
                  x =>
                    EquationNode(
                      finEv,
                      finalProb(x, tc.output)
                    )
                )
              (condDist, parUnion(eqs, parUnion(ceqs, evEqs)))
          }
        case isle: Island[y, _, o, b] =>
          val isleCast = isle.asInstanceOf[Island[y, ParMapState, o, b]]
          import isleCast._
          val (isleInit, boat) = initMap(tangentState)(varWeight) // initial condition for island, boat to row back
          val isleOut = varDist(isleInit, maxDepth.map(_ - 1), halted)(
            islandOutput(boat),
            epsilon
          ) //result for the island
          val (dist, eqs) = isleOut
          val isleEqs =
            eqs.map(_.mapVars(InIsle.variableMap(boat, isle)))
          val bridgeEqs = dist.keySet.map { x =>
            EquationNode(
              finalProb(export(boat, x), isle.output),
              coeff * FinalVal(
                InIsle(Elem(x, isle.islandOutput(boat)), boat, isle)
              )
            )
          }
          val initVarElems = eqs
            .flatMap { (eq) =>
              Expression.varVals(eq.rhs)
            }
            .collect {
              case InitialVal(Elem(el, rv)) => Elem(el, rv)
            }
          val isleIn: ParSet[EquationNode] =
            initVarElems.map { el =>
              val rhs =
                if (boat == el.element)
                  (IsleScale(boat) * -1) + Literal(1)
                else IsleScale(boat) * InitialVal(el)
              EquationNode(
                InitialVal(InIsle(el, boat, isle)),
                rhs
              )
            }
          (
            mapMap(dist, export(boat, _)),
            parUnion(isleEqs, parUnion(bridgeEqs, isleIn))
          )
      }
}
