import provingground.learning.Expression.Coeff
import provingground.learning.ParMapState.ParGenNodes
import provingground._, learning._, interface._, HoTT._
import collection.parallel._, immutable.ParVector
val A = Type.sym
val a = A.sym
val f = (A ->: A).sym 
val g = (A ->: A).sym 

val tg = TermGenParams.zero.copy(appW = 0.2)
val state = ParMapState(ParMap(a -> 0.4, f -> 0.3, g -> 0.3), ParMap())
val pde = ParDistEq.fromParams(tg)
val (s1, _) = pde.nextStateEqs(state, 0.001)
println(s1.termDist.keySet.to(Vector))
val (s2, _) = pde.nextStateEqs(state, 0.001, maxDepth = Some(1))
println(s2.termDist.keySet.to(Vector))
val (s3, _) = pde.nextStateEqs(state, 0.001, maxDepth = Some(2))
println(s3.termDist.keySet.to(Vector))
val tpde = ParTangentDistEq.fromParams(state, tg)
val (ts1, _) = tpde.nextStateEqs(ParMapState(ParMap(f -> 1.0), ParMap()), 0.0001)
println(ts1.termDist.keys.to(Vector))
ts1.termDist.keySet.contains(g(a))
ts1.termDist.keySet.contains(f(g(a)))
ts1.termDist.keySet.contains(g(g(f(a))))
val (ts2, teq) = tpde.nextStateEqs(ParMapState(ParMap(f -> 1.0), ParMap()), 0.0001, maxDepth = Some(3))
println(ts2.termDist.keys.to(Vector))
ts2.termDist.size
val (ts3, _) = tpde.nextStateEqs(ParMapState(ParMap(f -> 0.5, g -> 0.5), ParMap()), 0.0001)
println(ts3.termDist.keys.to(Vector))
val coeffs = teq.flatMap(e => Expression.atoms(e.rhs)).collect{case cf : Coeff[_] => cf}.toVector
val cvs = coeffs.map(ParMapState.coeffVal(tg))
