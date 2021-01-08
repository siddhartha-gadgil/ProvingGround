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
// val (tangNextState, teqs) = tpde.nextStateEqs(tangent, math.pow(10, -5))
// val tThms = tangNextState.termDist.keySet.map(_.typ).intersect(nextState.typDist.keySet)
// tThms.size
// import TermRandomVars._
// println(tpde.baseDist(Funcs, 0.01))
// val node = ns.Gen.unifApplnNode
// node.input1
// node.input2
// val (unD, _) = tpde.nodeDist(tangent, None, false)(node, math.pow(10, -4), Expression.Coeff(node))
// val (ttD, _) = tpde.varDist(tangent, None, false)(TermRandomVars.Terms, math.pow(10, -4))
// println(unD)
// val (d2, _) =
//             tpde.varDist(tangent, None, false)(node.input2, math.pow(10, -4))
// val bd = tpde.baseDist(node.input1, math.pow(10, -4))
// val triples1 = bd
//             .zip(d2)
//             .flatMap {
//               case ((x1, p1), (x2, p2)) =>
//                 node.f(x1, x2).map(y => ((x1, x2, y), p1 * p2))
//             }
// println(dist1.toParMap.keySet.map(_.typ).to(Vector))
// val sym = dist1.support.find(_.typ == a ~>: (b ~>: (eqM(a)(b) ->: eqM(b)(a)))).get
// println(node.f(ExstFunc.opt(sym).get, lem1Pf))
// val symm = ExstFunc.opt(sym).get
// bd(ExstFunc.opt(sym).get)
// val fatTriples = bd
//             .zip(d2)
//             .map {
//               case ((x1, p1), (x2, p2)) =>
//                 ((x1, x2, node.f(x1, x2)), p1 * p2)
//             }
// fatTriples.filter(_._1._1 == symm)
// bd.filter(_._1 == symm)
// bd.zip(d2).filter(_._1._1 == symm)
// bd.zip(d2).map(_._1)