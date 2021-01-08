import scala.collection.parallel.ParMap
import provingground._ , interface._, HoTT._, learning._ 
import library._, MonoidSimple._
import scala.collection.parallel._

val tg = TermGenParams.zero.copy(appW = 0.2, unAppW = 0.2, goalWeight = 0)
val state = ParMapState(dist1.toParMap, dist1.map(_.typ).toParMap)
val ns = ParMapState.parNodeSeq(tg)
val pde = new ParDistEq(ns.nodeCoeffSeq)
val (nextState, eqs) = pde.nextStateEqs(state, math.pow(10, -5))
val thms = nextState.thmsWithProofs
println(thms.map(_._1).toVector.mkString("\n"))
println(thms.size)
val lem = eqM(l)(op(l)(r))
val lemPf = thms.find(_._1 == lem).map(_._3).get._1
println(lemPf)
val tpde = new ParTangentDistEq(ns.nodeCoeffSeq, nextState) 
val tangentState = ParMapState(ParMap(lemPf -> 1.0), ParMap())
val (tangNextState, teqs) = tpde.nextStateEqs(tangentState, math.pow(10, -4))
val tThms = tangNextState.termDist.keySet.map(_.typ).intersect(nextState.typDist.keySet)
tThms.contains(eqM(l)(r))
