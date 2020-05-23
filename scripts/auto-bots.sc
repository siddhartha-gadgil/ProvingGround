import provingground._ , learning._, interface._, translation._, HoTT._
import scribe._, writer._, Utils._
Utils.logger = {
    import scribe._, writer._, Utils._
    logger.withHandler(writer = FileWriter().path(file.LogPath.daily())).replace()
}
import library._, MonoidSimple._
val tg = TermGenParams.zero.copy(appW = 0.1, unAppW = 0.1)
val ts = TermState(dist1, dist1.map(_.typ), goals = FiniteDistribution.unif(eqM(l)(r)))
val lp = LocalProver(ts, tg)
import HoTTBot._
repl.pprinter.bind(translation.FansiShow.fansiPrint)
val bs = Vector(lpLemmas, scaleSplitLemmas(1), lemmaTangents(), lptToTermResult, termResultToFinalState, reportSuccesses)
val sess = new HoTTWebSession(bots = bs)
sess.post(lp, Set())
