import provingground.{FiniteDistribution => FD, TruncatedDistribution => TD, ProbabilityDistribution => PD}
import scala.collection.mutable.ArrayBuffer, scala.concurrent._ , Sampler._
val A = "A" ::  Type
val B = "B" ::  Type
val dAB = new BasicDeducer(varWeight = 0.5, piWeight = 0.4, lambdaWeight = 0.4, vars = Vector(A, B))
val TSAB = new TermSampler(dAB)
val initAB = dAB.hFunc(0.3)(FD.unif[Term](A, B))
val sampAB = sample(initAB, 100000000)
val bufABs = TSAB.loggedBuffer(toFD(sampAB).flatten, 25000, 50000, 0.5, 0.5, 0.9)
println(bufABs.size)
val a = "a" :: A
val b = "b" :: B
val cnst = a :-> (b :-> a)
