import provingground._ , learning._, interface._, translation._, HoTT._
Utils.logger = {
    import scribe._, writer._, Utils._
    logger.withHandler(writer = FileWriter().path(file.LogPath.daily())).
    withHandler(writer = FileWriter().path(file.LogPath.daily("errors")), minimumLevel = Some(Level.Error)).
    withHandler(writer = FileWriter().path(file.LogPath.daily("debug")), minimumLevel = Some(Level.Debug)).replace()
}
import monix.execution.Scheduler.Implicits.global
import library._, CzSlOly._
import HoTTBot._
import HoTTMessages._
val ws1 =  ws.post(TautologyInitState(tautGen), Set())
val ws2 = ws1.flatMap(ws => ws.postLast(mulInit))
val ws3 = ws2.flatMap(ws => ws.postLast(localProver))
val ws4 = ws3.flatMap(ws => ws.act(lpToEnhancedExpEv ))
val ws5 = ws4.flatMap(ws => ws.act(expnEqnUpdate ))
val ws6 = ws5.flatMap(w => w.act(expFS ))
val ws7 = ws6.flatMap(w => w.act(finalStateFilteredLemmas()  ))
val ws8 = ws7.flatMap(w => w.act(lemRefine) )
val ws9 = ws8.flatMap(w => w.act(cappedSpecialBaseState ) )
val ws10 = ws9.flatMap(w => w.act(unAppEquations(math.pow(10, -4) ) ))
val ws11 = ws10.flatMap(ws => ws.act(eqnUpdate))
val ws12 = ws11.flatMap(ws => ws.act(eqnsToExpEv))
val ws13 = ws12.flatMap(ws => ws.act(expEvToFinalState))
val ws14 = ws13.flatMap(ws => ws.act(reportProofs(results)))
