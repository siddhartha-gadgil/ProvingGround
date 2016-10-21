import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD}
import Sampler._
val d = new BasicDeducer(varWeight = 0.5, piWeight = 0.4, lambdaWeight = 0.4)
val UD = FD.unif[Term](Type)
val initAB = d.hFunc(0.3)(FD.unif[Term](A, B))
val initL = d.hFunc(0.3)(UD)
val sampL = sample(initL, 100000000)
val TS = new TermSampler(d)
val bufLs = TS.loggedBuffer(toFD(sampL).flatten, 25000, 50000, 0.5, 0.5, 0.9)
val A = "A" ::  Type
// bufLs map (_.p(A:-> A))
val a = "a" :: A
val id = lambda(A)(lmbda(a)(a))
// bufLs map (_.p(id))
