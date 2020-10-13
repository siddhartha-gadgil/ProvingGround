package provingground.interface

import provingground._, interface._, HoTT._, learning._
import provingground.learning.HoTTMessages._
import HoTTBot._
import scala.util._, Properties.envOrNone
import Utils._

object ChompSession {
  Utils.logger = {
    import scribe._, writer._, Utils._
    logger
      .withHandler(writer = FileWriter().path(file.LogPath.daily()))
      .withHandler(
        writer = FileWriter().path(file.LogPath.daily("errors")),
        minimumLevel = Some(Level.Error)
      )
      .withHandler(
        writer = FileWriter().path(file.LogPath.daily("debug")),
        minimumLevel = Some(Level.Debug)
      )
      .replace()
  }

  logger.info(
    """|This is a Bot based run for goal chomping. For now this is the first attempt
                 |""".stripMargin
  )

  logger.info(s"Number of threads: $threadNum")

  envOrNone("JAVA_OPTS").foreach(w => logger.info(s"JAVA_OPTS: $w"))

  envOrNone("JAVA_HOME").foreach(w => logger.info(s"JAVA_HOME: $w"))

  val web = new HoTTPostWeb()
  val ws  = WebState[HoTTPostWeb, HoTTPostWeb.ID](web)

  val terms = FiniteDistribution.unif[Term](Unit, Zero, Star)
  val typs  = FiniteDistribution.unif[Typ[Term]](Type, Unit, Zero)
  val ts    = TermState(terms, typs)
  val ts0   = TermState(FiniteDistribution(), FiniteDistribution.unif(Type))
  val tg    = TermGenParams(solverW = 0.05)

  val lp  = LocalProver(ts, tg).sharpen(10)
  val lp0 = LocalProver(ts0).sharpen(10)

  val expFS =
    expEvToFinalState.andThen(updateTerms)

  lazy val wsF = 
    for {
        ws1 <- ws.post(lp0, Set())
        ws2  <- ws1.act(lpToEnhancedExpEv)
        ws3  <- ws2.act(expnEqnUpdate)
        ws4  <- ws3.act(expFS)
        ws5 <- ws4.postLast(lp)
        ws6 <- ws5.act(finalStateToChomp().triggerWith[LocalProver])
        ws7 <- ws6.act(chompReport())
    } yield ws7
}
