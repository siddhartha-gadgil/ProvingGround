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
val web = new HoTTPostWeb()
val ws = WebState[HoTTPostWeb, HoTTPostWeb.ID](web)
val ws1 = ws.post(localProver, Set())
val ws2 = ws1.flatMap(w => w.act(lpToEnhancedExpEv))
val ws3 = ws2.flatMap(w => w.act(expnEqnUpdate))
val sessF = ws3.map(w => HoTTWebSession.launch(w, bots))
