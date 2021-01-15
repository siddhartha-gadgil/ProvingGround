import provingground.learning.Expression.FinalVal
import provingground._, HoTT._, learning._ 

val A = Type.sym
val B = Type.sym

import collection.parallel._

val state = ParMapState(ParMap(), ParMap(A -> 0.5, B -> 0.5))
val pde = ParDistEq.fromParams(TermGenParams())
val (ns, eqs) = pde.nextStateEqs(state, math.pow(10, -5))
eqs.size

val exported = EquationExporter.export(eqs.to(Set), Set(A, B), Vector(A, B))
import GeneratorVariables._
val termsLHS = exported.flatMap(eqn => Expression.varVals(eqn.lhs)).collect{case FinalVal(Elem(t: Term, _)) => t}
termsLHS.size
println(termsLHS.find(_.dependsOn(B)))
def hasEscaped(exp: Expression) = Expression.varVals(exp).collect{
    case FinalVal(Elem(t: Term, _)) => t
    case FinalVal(Elem(fn: ExstFunc, _)) => (fn.func : Term)
}.nonEmpty
val badAll = exported.filter(eqn => hasEscaped(eqn.rhs))
println(badAll.mkString("\n"))
exported.size