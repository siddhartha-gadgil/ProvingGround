import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD, _}
import HoTT._
import Sampler._
val A = "A" :: Type
val B = "B" :: Type

val d = new BasicDeducer(varWeight = 0.5, piWeight = 0.4, lambdaWeight = 0.4)
val UD = FD.unif[Term](Type)
val initL = d.hFunc(0.3)(UD)
val sampL = sample(initL, 100000000)
val TS = new TermSampler(d)
val bufL = TS.loggedBuffer(toFD(sampL).flatten, 25000, 50000, 0.5, 0.5, 0.9)
println("Logic buffer started")
// bufLs map (_.p(A:-> A))
val aa = "a" :: A
val id = lambda(A)(lmbda(aa)(aa))
// bufLs map (_.p(id))



val dAB = new BasicDeducer(varWeight = 0.5, piWeight = 0.4, lambdaWeight = 0.4, vars = Vector(A, B))
val TSAB = new TermSampler(dAB)
val initAB = dAB.hFunc(0.3)(FD.unif[Term](A, B))
val sampAB = sample(initAB, 100000000)
val bufAB = TSAB.loggedBuffer(toFD(sampAB).flatten, 25000, 50000, 0.5, 0.5, 0.9)
println("A, B buffer started")


import library.Monoid._
val dist = smallDist*0.8 ++ (FD.unif[Term](M, refl) * 0.2)
val dMon = new BasicDeducer(vars = Vector(a, b, c))
val initM = dMon.hFunc(0.3)(dist)
val sampM = sample(initM, 1000000)
val TSMon = new TermSampler(dMon)
val bufMon = TSMon.loggedBuffer(toFD(sampM).flatten, 25000, 50000, 0.5, 0.5, 0.9)
println("Monoid buffer started")
