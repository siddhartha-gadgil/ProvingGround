import provingground._ , interface._, HoTT._, learning._
repl.pprinter.bind(translation.FansiShow.fansiPrint)
val A = Type.sym
val B = Type.sym
val a = A.sym
val f = "f" :: A ->: B
val mp = a :-> (f :-> f(a))
val de = new DerivedEquations
val mp = a :-> (f :-> f(a))

val tg = TermGenParams()

val ts = TermState(FiniteDistribution(), FiniteDistribution.unif(A, B))
val eqNodes = de.formalEquations(mp) union de.termStateInit(ts)
val equations = Equation.group(eqNodes)
val ev = ExpressionEval.fromInitEqs(ts, Equation.group(eqNodes), tg)
ev.finalTyps
ev.finalTerms
val mpU = A :~> (B :~> mp)
val tsU = TermState(FiniteDistribution(), FiniteDistribution.unif(A, B, Type))
val eqNodesU = de.formalEquations(mpU) union de.termStateInit(tsU)
val evU = ExpressionEval.fromInitEqs(tsU, Equation.group(eqNodesU), tg)
evU.finalTerms
evU.finalTyps
val tsU = TermState(FiniteDistribution(), FiniteDistribution.unif(Type))
val eqNodesU = de.formalEquations(mpU) union de.termStateInit(tsU)
val evU = ExpressionEval.fromInitEqs(tsU, Equation.group(eqNodesU), tg)
evU.finalTerms
evU.finalTyps
