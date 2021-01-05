package provingground.learning

import provingground._, HoTT._

import GeneratorVariables._, Expression._

import scala.collection.parallel._
import shapeless._
import TermRandomVars._, NodeCoeffs._
import ParMapState._
import DE.{finalProb, initProb}
import provingground.learning.GeneratorNode._

import scala.collection.parallel.immutable.ParVector
import scala.math.Ordering.Double.TotalOrdering
import spire.syntax.group
import provingground.learning.Sort.All
import provingground.learning.Sort.Filter
import provingground.learning.Sort.Restrict

trait RecParDistEq {
  val nodeCoeffSeq: NodeCoeffSeq[ParMapState, Double]

  def nodeDist[Y](
      initState: ParMapState,
      maxDepth: Option[Int],
      halted: => Boolean
  )(
      generatorNode: GeneratorNode[Y],
      epsilon: Double,
      coeff: Expression
  ): (ParMap[Y, Double], ParSet[EquationNode])

  def nodeFamilyDistFunc[Dom <: HList, Y](
      initState: ParMapState,
      maxDepth: Option[Int],
      halted: => Boolean
  )(
      generatorNodeFamily: GeneratorNodeFamily[Dom, Y],
      epsilon: Double
  )(arg: Dom): (ParMap[Y, Double], ParSet[EquationNode]) =
    generatorNodeFamily match {
      case node: GeneratorNode[Y] =>
        assert(
          arg == HNil,
          s"looking for coordinate $arg in $node which is not a family"
        )
        val coeff = Coeff.get(node)
        nodeDist(initState, maxDepth, halted)(node, epsilon, coeff)
      case f: GeneratorNodeFamily.Pi[Dom, Y] =>
        val coeff =
          Coeff.get(f.nodes(arg))
        // pprint.log(arg)
        // pprint.log(f.nodes(arg))
        // pprint.log(epsilon)
        nodeDist(initState, maxDepth, halted)(f.nodes(arg), epsilon, coeff)
      case f: GeneratorNodeFamily.PiOpt[Dom, Y] =>
        f.nodesOpt(arg)
          .map { (node) =>
            val coeff = Coeff.get(node)
            nodeDist(initState, maxDepth, halted)(node, epsilon, coeff)

          }
          .getOrElse((ParMap.empty[Y, Double], ParSet.empty[EquationNode]))
    }

  def nodeCoeffFamilyDist[Dom <: HList, Y](
      initState: ParMapState,
      maxDepth: Option[Int],
      halted: => Boolean
  )(
      nodeCoeffs: NodeCoeffs[ParMapState, Double, Dom, Y],
      epsilon: Double
  )(arg: Dom): (ParMap[Y, Double], ParSet[EquationNode]) =
    if (epsilon > 1 || halted)
      (ParMap.empty[Y, Double], ParSet.empty[EquationNode])
    else
      nodeCoeffs match {
        case Target(_) => (ParMap.empty[Y, Double], ParSet.empty[EquationNode])
        case bc: Cons[ParMapState, Double, Dom, Y] =>
          val p = bc.headCoeff

          val pa = nodeFamilyDistFunc(initState, maxDepth, halted)(
            bc.headGen,
            epsilon / p
          )(
            arg
          )
          val pb =
            nodeCoeffFamilyDist(initState, maxDepth, halted)(bc.tail, epsilon)(
              arg
            )
          // pprint.log(pa._1)
          // pprint.log(pb._1)
          (add(pa._1.mapValues(_ * p).to(ParMap), pb._1), pa._2 union pb._2)
      }

  def varFamilyDistFunc[RDom <: HList, Y](
      initState: ParMapState,
      maxDepth: Option[Int],
      halted: => Boolean
  )(
      randomVarFmly: RandomVarFamily[RDom, Y],
      epsilon: Double
  )(arg: RDom): (ParMap[Y, Double], ParSet[EquationNode]) =
    if (epsilon > 1 || halted)
      (ParMap.empty[Y, Double], ParSet.empty[EquationNode])
    else
      nodeCoeffSeq
        .find(randomVarFmly)
        .map { nc =>
          // pprint.log(nc)
          nodeCoeffFamilyDist(initState, maxDepth, halted)(nc, epsilon)(
            arg
          )
        }
        .getOrElse((ParMap.empty[Y, Double], ParSet.empty[EquationNode]))

  def nodeCoeffDist[Y](
      initState: ParMapState,
      maxDepth: Option[Int],
      halted: => Boolean
  )(
      nodeCoeffs: NodeCoeffs[ParMapState, Double, HNil, Y],
      epsilon: Double,
      rv: RandomVar[Y]
  ): (ParMap[Y, Double], ParSet[EquationNode]) =
    if (epsilon > 1 || halted)
      (ParMap.empty[Y, Double], ParSet.empty[EquationNode])
    else
      nodeCoeffs match {
        case Target(_) => (ParMap.empty[Y, Double], ParSet.empty[EquationNode])
        case bc: Cons[ParMapState, Double, HNil, Y] =>
          val p: Double = bc.headCoeff
          val (d: (ParMap[Y, Double], ParSet[EquationNode]), nc) =
            bc.headGen match {
              case gen: GeneratorNode[Y] =>
                val coeff = Coeff.get(gen) // FIXME : should be outer coefficient
                val (dist, eqs) = nodeDist(initState, maxDepth, halted)(
                  gen,
                  epsilon / p,
                  coeff
                )
                (dist.mapValues(_ * p), eqs) -> Coeff.get(gen)

              case _ =>
                throw new IllegalArgumentException(
                  "found node family for generating a simple random variable"
                )
            }
          val (a, eqa) = d
          val pb = nodeCoeffDist(initState, maxDepth, halted)(
            bc.tail,
            epsilon,
            rv
          )
          val (b, eqb) = pb
          (add(a, b), eqa union eqb)
      }

  def varDist[Y](
      initState: ParMapState,
      maxDepth: Option[Int],
      halted: => Boolean
  )(
      randomVar: RandomVar[Y],
      epsilon: Double
  ): (ParMap[Y, Double], ParSet[EquationNode]) =
    if (epsilon > 1 || halted)
      (ParMap.empty[Y, Double], ParSet.empty[EquationNode])
    else if (maxDepth.map(_ < 1).getOrElse(false))
      (initState.value(randomVar), ParSet())
    else {

      val result =
        randomVar match {
          case RandomVar.AtCoord(randomVarFmly, fullArg) =>
            val (dist, eqs) = varFamilyDistFunc(initState, maxDepth, halted)(
              randomVarFmly,
              epsilon
            )(fullArg)
            (normalize(dist), eqs)
          case _ =>
            nodeCoeffSeq
              .find(randomVar)
              .map { nc =>
                val (fd, eqs) = nodeCoeffDist(initState, maxDepth, halted)(
                  nc,
                  epsilon,
                  randomVar
                )
                (normalize(fd), eqs)

              }
              .getOrElse((ParMap.empty[Y, Double], ParSet.empty[EquationNode]))
        }
      result
    }

  def nextStateEqs(initState: ParMapState, cutoff: Double, maxDepth: Option[Int] = None, halted : => Boolean = false) : (ParMapState, ParSet[EquationNode]) = 
    {   val (newTerms, teqs) = varDist(initState, maxDepth, halted)(Terms, cutoff)
        val (newTyps, tpeqs) = varDist(initState, maxDepth, halted)(Typs, cutoff)
        (ParMapState(
        newTerms,
        newTyps,
        initState.vars,
        initState.inds,
        initState.goalDist,
        initState.context
    ), teqs union(tpeqs))
    }
}

class ParDistEq(
    nodeCoeffSeqParam: NodeCoeffSeq[ParMapState, Double],
    varWeight: Double = 0.3
) extends RecParDistEq {
  val nodeCoeffSeq: NodeCoeffSeq[ParMapState, Double] = nodeCoeffSeqParam

  def nodeDist[Y](
      initState: ParMapState,
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
          val initDist = initState.value(input)
          val eqs = initDist.keySet.map { x =>
            EquationNode(finalProb(x, input), coeff * initProb(x, input))
          }
          // pprint.log(input)
          // pprint.log(initDist)
          (initDist, eqs)
        case Atom(value, output) => (ParMap(value -> 1.0), ParSet.empty)
        case provingground.learning.GeneratorNode.Map(f, input, output) =>
          val (dist, eqs) = varDist(initState, maxDepth, halted)(input, epsilon)
          val meqs = dist.keySet.map { (x) =>
            EquationNode(
              finalProb(f(x), output),
              coeff * finalProb(x, input)
            )
          }
          (mapMap(dist, f), eqs union (meqs))
        case MapOpt(f, input, output) =>
          val (dist, eqs) = varDist(initState, maxDepth, halted)(input, epsilon)
          val meqs = dist.keySet.flatMap { (x) =>
            f(x).map(
              y =>
                EquationNode(
                  finalProb(y, output),
                  coeff * finalProb(x, input)
                )
            )
          }
          (mapMapOpt(dist, f), eqs union (meqs))
        case zm: ZipMap[x1, x2, Y] =>
          import zm._
          val (dist1, eqs1) =
            varDist(initState, maxDepth.map(_ - 1), halted)(input1, epsilon)
          // pprint.log(dist1)
          val (dist2, eqs2) =
            varDist(initState, maxDepth.map(_ - 1), halted)(input2, epsilon)
          // pprint.log(dist2)
          val triples = dist1
            .zip(dist2)
            .map { case ((x1, p1), (x2, p2)) => ((x1, x2, f(x1, x2)), p1 * p2) }
            .filter(_._2 > epsilon).to(ParVector)
          // pprint.log(triples)
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
            // pprint.log(tripleMap)
          (tripleMap, eqs1 union eqs2 union meqs)
        case zm: ZipMapOpt[x1, x2, Y] =>
          import zm._
          val (dist1, eqs1) =
            varDist(initState, maxDepth.map(_ - 1), halted)(input1, epsilon)
          val (dist2, eqs2) =
            varDist(initState, maxDepth.map(_ - 1), halted)(input2, epsilon)
          val triples = dist1
            .zip(dist2)
            .flatMap {
              case ((x1, p1), (x2, p2)) =>
                f(x1, x2).map(y => ((x1, x2, y), p1 * p2))
            }
            .filter(_._2 > epsilon).to(ParVector)
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
          (tripleMap, eqs1 union eqs2 union meqs)
        case fpm: FiberProductMap[x1, x2, z, Y] =>
          import fpm._
          val (d1, baseEqs) =
            varDist(initState, maxDepth.map(_ - 1), halted)(baseInput, epsilon)
          val byBase         = d1.groupBy(xp => quot(xp._1))
          val baseMaxWeights = byBase.mapValues(_.values.max)
          val groups = baseMaxWeights.map {
            case (z, p) =>
              // pprint.log(fiberVar(z))
              // pprint.log(epsilon / p)
              val result = varDist(initState, maxDepth.map(_ - 1), halted)(
                fiberVar(z),
                epsilon / p
              )
              // pprint.log(result)
              z -> result
          }
          // pprint.log(groups.mapValues(_._1))
          val fiberEqs = groups.flatMap(_._2._2).to(ParSet)
          val triples = d1
            .flatMap {
              case (x1, p1) =>
                // pprint.log(x1)
                val cutoff = epsilon / p1
                // pprint.log(cutoff)
                groups(quot(x1))._1.map {
                  case (x2, p2) if p2 > cutoff =>
                    // pprint.log(x2)
                    // pprint.log(f(x1, x2))
                   ((x1, x2, f(x1, x2)), p1 * p2)
                }
            }.to(ParVector)
          val fibEqs = triples
            .map {
              case ((x1, x2, y), _) =>
                EquationNode(
                  finalProb(y, output),
                  coeff * finalProb(x1, baseInput) * finalProb(x2, fiberVar(quot(x1)))
                )
            }
            .to(ParSet)
          val tripleMap =
            makeMap(triples.map { case ((_, _, y), p) => (y, p) })
          (tripleMap, baseEqs union fibEqs union fiberEqs)
        case zfm: ZipFlatMap[x1, x2, Y] =>
          import zfm._
          val (baseDist, baseEqs) =
            varDist(initState, maxDepth.map(_ - 1), halted)(baseInput, epsilon)
          val fibers = baseDist.to(ParVector).map {
            case (x1, p1) =>
              val (dist2, eqs2) = varDist(
                initState,
                maxDepth.map(_ - 1),
                halted
              )(fiberVar(x1), epsilon / p1)
              (
                dist2.map { case (x2, p2) => ((x1, x2, f(x1, x2)), p1 * p2) },
                eqs2
              )
          }
          val triples  = fibers.flatMap(_._1)
          val fiberEqs = fibers.flatMap(_._2).to(ParSet)
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
          (tripleMap, baseEqs union fibEqs union fiberEqs)
        case fm: FlatMap[x, Y] =>
          import fm._
          val (baseDist, baseEqs) =
            varDist(initState, maxDepth.map(_ - 1), halted)(baseInput, epsilon)
          val fibers = baseDist.to(ParVector).map {
            case (x1, p1) =>
              val (dist2, eqs2) = nodeDist(
                initState,
                maxDepth.map(_ - 1),
                halted
              )(fiberNode(x1), epsilon / p1, coeff)
              (
                dist2.map { case (x2, p2) => ((x1, x2), p1 * p2) },
                eqs2
              )
          }
          val pairs    = fibers.flatMap(_._1)
          val fiberEqs = fibers.flatMap(_._2).to(ParSet)
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
          (pairMap, baseEqs union fibEqs union fiberEqs)
        case fm: FlatMapOpt[x, Y] =>
          import fm._
          val (baseDist, baseEqs) =
            varDist(initState, maxDepth.map(_ - 1), halted)(baseInput, epsilon)
          val fibers = baseDist.to(ParVector).flatMap {
            case (x1, p1) =>
              fiberNodeOpt(x1).map { node =>
                val (dist2, eqs2) = nodeDist(
                  initState,
                  maxDepth.map(_ - 1),
                  halted
                )(node, epsilon / p1, coeff)
                (
                  dist2.map { case (x2, p2) => ((x1, x2), p1 * p2, node) },
                  eqs2
                )
              }
          }
          val pairs    = fibers.flatMap(_._1)
          val fiberEqs = fibers.flatMap(_._2).to(ParSet)
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
          (pairMap, baseEqs union fibEqs union fiberEqs)
        case tc: ThenCondition[o, Y]                                 => 
            import tc._
            val base  = nodeDist(initState, maxDepth, halted)(gen, epsilon, coeff)
            val event = Event(tc.gen.output, tc.condition)
            val finEv = FinalVal(event)
            import Sort._
            condition match {
                case _ : All[_] => base
                case c : Filter[_] => 
                    val (dist, eqs) = base
                    val condDist = normalize(dist.filter{case (x, _) => c.pred(x)})
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
                    (condDist, eqs union(ceqs union(evEqs)))
                case res : Restrict[u, Y] => 
                    val (dist, eqs) = base
                    val condDist = mapMapOpt(dist, res.optMap)
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
                    (condDist, eqs union(ceqs union(evEqs)))
            }
        case isle: Island[Y, ParMapState, o, b] =>
            import isle._
            val (isleInit, boat) = initMap(initState)(varWeight)                                   // initial condition for island, boat to row back
            val isleOut          = varDist(isleInit, maxDepth.map(_ - 1),halted)(islandOutput(boat), epsilon) //result for the island
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
            (mapMap(dist, export(boat, _)), isleEqs union bridgeEqs union isleIn)
      }
}
