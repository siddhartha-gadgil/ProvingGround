import scala.collection.parallel.ParMap
import provingground._ , interface._, HoTT._, learning._ 
import library._, MonoidSimple._
import scala.collection.parallel._

val tg = TermGenParams.zero.copy(appW = 0.1, unAppW = 0.1)
val ts = ParMapState(dist1.toParMap, dist1.map(_.typ).toParMap, goalDist = ParMap(eqM(l)(r) -> 1.0))

