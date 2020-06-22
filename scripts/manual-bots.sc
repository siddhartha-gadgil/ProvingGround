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
val web = new HoTTPostWeb()
val ws = WebState[HoTTPostWeb, HoTTPostWeb.ID](web)
val ws1 = ws.post(lp, Set())
import monix.execution.Scheduler.Implicits.global
val ws2 = ws1.flatMap(w => w.act(HoTTBot.lpLemmas))
val ws3 = ws2.flatMap(w => w.act(HoTTBot.scaleSplitLemmas(1)))
val ws4 = ws3.flatMap(w => w.act(HoTTBot.lemmaTangents()))
val ws5 = ws4.flatMap(w => w.act(HoTTBot.lptToTermResult ))
val ws6 = ws5.flatMap(w => w.act(HoTTBot.termResultToFinalState ))
val ws7 = ws6.flatMap(w => w.act(HoTTBot.reportSuccesses ))
