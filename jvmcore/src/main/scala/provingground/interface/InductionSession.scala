package provingground.interface

import provingground._, interface._, HoTT._, learning._
import provingground.learning.HoTTMessages._
import HoTTBot._
import scala.util._, Properties.envOrNone
import Utils._
import scala.concurrent._
import provingground.scalahott.NatRing

object InductionSession {
  Utils.logger = {
    import scribe._, writer._, Utils._, scribe.output.format.ASCIIOutputFormat
    logger
      .withHandler(
        writer = FileWriter().path(file.LogPath.daily("debug")),
        minimumLevel = Some(Level.Debug)
      )
      .withHandler(
        writer = FileWriter().path(file.LogPath.daily()),
        minimumLevel = Some(Level.Info),
        outputFormat = ASCIIOutputFormat
      )
      .withHandler(
        writer = FileWriter().path(file.LogPath.daily("errors")),
        minimumLevel = Some(Level.Error)
      )
      .replace()
  }

  logger.info(
    """|
       |This is a Bot based run for testing a simple induction problem:
       |given f(n) = f(n + 1) for all n, we conclude that f(0) = f(n).
       |We are using a simple local prover, so that all the work should be done by Bots.
       |""".stripMargin
  )

  

  envOrNone("JAVA_OPTS").foreach(w => logger.info(s"JAVA_OPTS: $w"))

  envOrNone("JAVA_HOME").foreach(w => logger.info(s"JAVA_HOME: $w"))

  val web = new HoTTPostWeb()
  val ws  = WebState[HoTTPostWeb, HoTTPostWeb.ID](web)

  import IdentityTyp._

  val A = "A" :: Type

  import provingground.scalahott.NatRing._

  val n = "n" :: NatTyp

  val f = "f" :: NatTyp ->: A

  val conclusion = n ~>: { f(zero) =:= f(n) }

  val terms = FiniteDistribution.unif[Term](
    reflTerm(A),
    transTerm(A),
    f(zero),
    NatRing.zero,
    Star
  )
  val typs = FiniteDistribution.unif[Typ[Term]](NatTyp, A)
  val ts   = TermState(terms, typs)
  val tg   = TermGenParams.zero.copy(appW = 0.2, unAppW = 0.2)

  val lp = LocalProver(ts, tg)

  val hypothesis = n ~>: (f(n) =:= f(succ(n)))

  val goal = hypothesis ->: conclusion

  val bots: Vector[HoTTBot] = Vector(
    parGoalAttemptEqs(0.3),
    negateGoal.triggerMap[FailedToProve](_.seek),
    skolemBot.triggerMap[FailedToProve](_.seek),
    viaZeroBot.triggerMap[FailedToProve](_.seek),
    productBackward.triggerMap[FailedToProve](_.seek),
    coproductBackward.triggerMap[FailedToProve](_.seek),
    goalInContext.triggerMap[FailedToProve](_.seek),
    exportProof,
    inductionBackward(Some(1)).triggerMap[FailedToProve](_.seek),
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
      ws2 <- ws1.postLast(exstInducDefn)
      ws3 <- ws2.postLast(SeekGoal(goal, Context.Empty))
    } yield ws3

  lazy val sessF: Future[HoTTWebSession] =
    wsF.map(ws => HoTTWebSession.launch(ws, bots))
}
