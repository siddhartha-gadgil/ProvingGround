import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD, _}
import library._, MonoidSimple._
import learning._
val tv = new TermEvolver(lambdaWeight = 0.0, piWeight = 0.0)
import provingground.{FiniteDistribution => FD, ProbabilityDistribution => PD, _}
import scala.concurrent.duration._
val fdT = Truncate.task(tv.baseEvolveTyps(dist1), math.pow(0.1, 8), 2.minutes).memoize
val fd = Truncate.task(tv.baseEvolve(dist1), math.pow(0.1, 5), 2.minutes).memoize
import monix.execution.Scheduler.Implicits.global
import math.log

val thms = for{
     terms <- fd;
     typs <- fdT
     termTypsRaw = terms.map(_.typ).filter(typs(_) > 0)
     termTyps = termTypsRaw.normalized()
     statements = termTyps.supp.filter((typ) => typs(typ) > 0)
  } yield statements.map{(thm) => (thm, termTypsRaw(thm), termTyps(thm))}

val thmsEntp = thms.map{(triples) => triples.map {case (t, p, q) => (t, q, h(p,q))}.sortBy(_._3)}.memoize
