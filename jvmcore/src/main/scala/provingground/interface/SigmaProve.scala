package provingground.interface

import provingground._, interface._, HoTT._, learning._
import provingground.learning.HoTTMessages._
import HoTTBot._
import scala.util._, Properties.envOrNone
import Utils._
import scala.concurrent._

object SigmaProve {
  Utils.logger = {
    import scribe._, writer._, Utils._
    logger
      .withHandler(
        writer = FileWriter().path(file.LogPath.daily()),
        minimumLevel = Some(Level.Info)
      )
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
    """|
       |This is a Bot based run for testing backward reasoning by instantiation.
       |During chomps sigma-types were proved directly, so here instead we act using sigma-backward 
       |before launching. 
       |This worked the first time, but we are running again with better logging, mainly named bots.
       |""".stripMargin
  )

  logger.info(s"Number of threads: $threadNum")

  envOrNone("JAVA_OPTS").foreach(w => logger.info(s"JAVA_OPTS: $w"))

  envOrNone("JAVA_HOME").foreach(w => logger.info(s"JAVA_HOME: $w"))

  val web = new HoTTPostWeb()
  val ws  = WebState[HoTTPostWeb, HoTTPostWeb.ID](web)

  val terms = FiniteDistribution.unif[Term](
    Unit,
    Zero,
    Star
  )
  val typs = FiniteDistribution.unif[Typ[Term]](Type, Unit, Zero)
  val ts   = TermState(terms, typs)
  val tg   = TermGenParams(solverW = 0.05)

  val lp = LocalProver(ts, tg).sharpen(10)

  val A = "A" :: Type

  val goal = A &: (A ->: Zero)

  val bots: Vector[HoTTBot] = Vector(
    goalAttempt(0.3),
    negateGoal.triggerMap[FailedToProve](_.seek),
    skolemBot.triggerMap[FailedToProve](_.seek),
    viaZeroBot.triggerMap[FailedToProve](_.seek),
    productBackward.triggerMap[FailedToProve](_.seek),
    coproductBackward.triggerMap[FailedToProve](_.seek),
    goalInContext.triggerMap[FailedToProve](_.seek),
    instanceToGoal,
    instancesFromLP,
    deducedResults,
    sigmaBackward.triggerMap[FailedToProve](_.seek),
    resolveFromAll,
    resolveFromAny,
    reportProved,
    reportContradicted,
    topLevelRelevantGoalsBot[FailedToProve](true),
    topLevelRelevantGoalsBot[Proved](true)
  )

  lazy val wsF =
    for {
      ws1 <- ws.post(lp, Set())
      ws2 <- ws1.postLast(SeekGoal(goal, Context.Empty))
      ws3 <- ws2.act(sigmaBackward)
    } yield ws3

  lazy val sessF: Future[HoTTWebSession] =
    wsF.map(ws => HoTTWebSession.launch(ws, bots))
}
