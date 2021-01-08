// Modus Ponens proved using parallel generation
import provingground._ , learning._, interface._, translation._, HoTT._, induction._
val A = Type.sym
val B = Type.sym
val ns = ParMapState.parNodeSeq(TermGenParams())
import scala.collection.parallel._, immutable.ParVector
val state0 = ParMapState(ParMap(), ParMap(Type -> 1.0))
val pde = new ParDistEq(ns.nodeCoeffSeq)
val MP = A ~>: (B ~>: (A ->: (A ->: B) ->: B))
import GeneratorVariables._, Expression._, TermRandomVars._
lazy val (mpts1, mpeq1) = pde.varDist(state0, None, false)(termsWithTyp(MP), math.pow(10, -4))
val pfs1 = mpts1.keys.to(Set)
println(pfs1.head)
val state1 = ParMapState(ParMap(), ParMap(Type -> 1.0), goalDist = ParMap(MP -> 1.0))
val pde1  = new ParDistEq(ns.nodeCoeffSeq)
val (nextState, eqs) = pde1.nextStateEqs(state1, math.pow(10, -4))
val mpProof = nextState.termDist.filter(_._1.typ == MP)
println(mpProof)
