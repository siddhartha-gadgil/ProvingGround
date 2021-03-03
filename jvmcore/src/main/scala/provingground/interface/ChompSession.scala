package provingground.interface

import provingground._, HoTT._, learning._
import provingground.learning.HoTTMessages._
import HoTTBot._
import scala.util._, Properties.envOrNone
import Utils._, JvmUtils._
import scala.concurrent._

object ChompSession {
  logger.info(
    """|
       |This is a Bot based run for goal chomping. This has been refined a few times.
       |Biggest change after first runs was having a session with bots.
       |Also parallel chomping, proving via zero, using inclusions and projections,
       |better reporting of status. 
       |
       |""".stripMargin
  )

  

  envOrNone("JAVA_OPTS").foreach(w => logger.info(s"JAVA_OPTS: $w"))

  envOrNone("JAVA_HOME").foreach(w => logger.info(s"JAVA_HOME: $w"))

  val web = new HoTTPostWeb()
  val ws  = WebState[HoTTPostWeb, HoTTPostWeb.ID](web)

  val terms1 = FiniteDistribution.unif[Term](
    Unit,
    Zero,
    Star,
    ProdTyp.proj1,
    ProdTyp.proj2,
    SigmaTyp.proj1,
    SigmaTyp.proj2,
    PlusTyp.incl1,
    PlusTyp.incl2
  )
  val terms = FiniteDistribution.unif[Term](
    Unit,
    Zero,
    Star
  )
  val typs = FiniteDistribution.unif[Typ[Term]](Type, Unit, Zero)
  val ts   = TermState(terms, typs)
  val ts0  = TermState(FiniteDistribution(), FiniteDistribution.unif(Type))
  val tg   = TermGenParams(solverW = 0.05)

  val lp  = LocalProver(ts, tg).sharpen(10)
  val lp0 = LocalProver(ts0).sharpen(50)

  val expFS =
    expEvToFinalState.andThen(updateTerms)

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
      ws1 <- ws.post(lp0, Set())
      ws2 <- ws1.act(lpToEnhancedExpEv)
      ws3 <- ws2.act(expnEqnUpdate)
      ws4 <- ws3.act(expFS)
      ws5 <- ws4.postLast(lp)
      ws6 <- ws5.act(
        finalStateToConcurrentChomp(concurrency = 8).triggerWith[LocalProver]
      )
      ws7 <- ws6.act(chompReport())
      ws8 <- ws7.act(goalsAfterChomp)
    } yield ws8

  lazy val sessF: Future[HoTTWebSession] =
    wsF.map(ws => HoTTWebSession.launch(ws, bots))
}

object ChompSessionEq {
  JvmUtils.logger = {
    import scribe._, writer._
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
    """|
       |This is a Bot based run for goal chomping. This has been refined a few times.
       |Biggest change after first runs was having a session with bots.
       |Also parallel chomping, proving via zero, using inclusions and projections,
       |better reporting of status. 
       |After debugging, everything generated was proved but there was an error in reporting.
       |""".stripMargin
  )

  

  envOrNone("JAVA_OPTS").foreach(w => logger.info(s"JAVA_OPTS: $w"))

  envOrNone("JAVA_HOME").foreach(w => logger.info(s"JAVA_HOME: $w"))

  val web = new HoTTPostWeb()
  val ws  = WebState[HoTTPostWeb, HoTTPostWeb.ID](web)

  val terms1 = FiniteDistribution.unif[Term](
    Unit,
    Zero,
    Star,
    ProdTyp.proj1,
    ProdTyp.proj2,
    SigmaTyp.proj1,
    SigmaTyp.proj2,
    PlusTyp.incl1,
    PlusTyp.incl2
  )
  val terms = FiniteDistribution.unif[Term](
    Unit,
    Zero,
    Star
  )
  val typs = FiniteDistribution.unif[Typ[Term]](Type, Unit, Zero)
  val ts   = TermState(terms, typs)
  val ts0  = TermState(FiniteDistribution(), FiniteDistribution.unif(Type))
  val tg   = TermGenParams(solverW = 0.05)

  val lp  = LocalProver(ts, tg).sharpen(10)
  val lp0 = LocalProver(ts0).sharpen(50)

  val expFS =
    expEvToFinalState.andThen(updateTerms)

  val bots: Vector[HoTTBot] = Vector(
    goalAttemptEqs(0.3),
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
    eqnUpdate.triggerWith[FailedToProve],
    eqnUpdate.triggerWith[Proved],
    resolveFromAll,
    resolveFromAny,
    reportProved,
    reportContradicted,
    topLevelRelevantGoalsBot[FailedToProve](true),
    topLevelRelevantGoalsBot[Proved](true),
    repostGoals(LocalProver(ts, tg).sharpen(10)),
    goalsAfterChomp,
    exportProof
  )

  lazy val wsF =
    for {
      ws1 <- ws.post(lp0, Set())
      ws2 <- ws1.act(lpToEnhancedExpEv)
      ws3 <- ws2.act(expnEqnUpdate)
      ws4 <- ws3.act(expFS)
      ws5 <- ws4.postLast(lp)
      ws6 <- ws5.act(
        finalStateToConcurrentChomp(concurrency = 8).triggerWith[LocalProver]
      )
      ws7 <- ws6.act(chompReport())
      ws8 <- ws7.act(chompEqnUpdate)
      ws9 <- ws8.act(goalsAfterChomp)
    } yield ws9

  lazy val sessF: Future[HoTTWebSession] =
    wsF.map(
      ws =>
        HoTTWebSession
          .launch(ws, bots, Some(PostResponse.capResponse(HoTTMessages.Cap)))
    )
}

object ParChompSessionEq {
  JvmUtils.logger = {
    import scribe._, writer._, scribe.output.format.ASCIIOutputFormat
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
       |This is a Bot based run for parallel generation goal chomping. This has been refined a few times.
       |Biggest change after first runs was having a session with bots.
       |Also parallel chomping, proving via zero, using inclusions and projections,
       |better reporting of status. 
       |After debugging, everything generated was proved but there was an error in reporting.
       |""".stripMargin
  )

  

  envOrNone("JAVA_OPTS").foreach(w => logger.info(s"JAVA_OPTS: $w"))

  envOrNone("JAVA_HOME").foreach(w => logger.info(s"JAVA_HOME: $w"))

  val web = new HoTTPostWeb()
  val ws  = WebState[HoTTPostWeb, HoTTPostWeb.ID](web)

  val terms1 = FiniteDistribution.unif[Term](
    Unit,
    Zero,
    Star,
    ProdTyp.proj1,
    ProdTyp.proj2,
    SigmaTyp.proj1,
    SigmaTyp.proj2,
    PlusTyp.incl1,
    PlusTyp.incl2
  )
  val terms = FiniteDistribution.unif[Term](
    Unit,
    Zero,
    Star
  )
  val typs = FiniteDistribution.unif[Typ[Term]](Type, Unit, Zero)
  val ts   = TermState(terms, typs)
  val ts0  = TermState(FiniteDistribution(), FiniteDistribution.unif(Type))
  val tg   = TermGenParams(solverW = 0.05)

  val lp  = LocalProver(ts, tg).sharpen(10).copy(maxTime = Some(600000L))
  val lp0 = LocalProver(ts0).sharpen(50).copy(maxTime = Some(600000L))

  val expFS =
    expEvToFinalState.andThen(updateTerms)

  val bots: Vector[HoTTBot] = Vector(
    parGoalAttemptEqs(0.3),
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
    eqnUpdate.triggerWith[FailedToProve],
    eqnUpdate.triggerWith[Proved],
    resolveFromAll,
    resolveFromAny,
    reportProved,
    reportContradicted,
    topLevelRelevantGoalsBot[FailedToProve](true),
    topLevelRelevantGoalsBot[Proved](true),
    // repostGoals(LocalProver(ts, tg).sharpen(10)),
    goalsAfterChomp,
    exportProof
  )

  lazy val wsF =
    for {
      ws1 <- ws.post(lp0, Set())
      ws2 <- ws1.act(parLpEqns)
      ws3 <- ws2.act(eqnUpdate)
      ws4 <- ws3.act(eqnsToExpEv(Some(ParMapState.coeffVal(lp0.tg))))
      ws5 <- ws4.act(expFS)
      ws6 <- ws5.postLast(lp)
      ws7 <- ws6.act(
        finalStateToParallelChomp(cutoffScales = List(10, 1))
          .triggerWith[LocalProver]
      )
      ws8 <- ws7.act(chompReport())
      ws9 <- ws8.act(chompEqnUpdate)
      ws10 <- ws9.act(goalsAfterChomp)
    } yield ws10

  lazy val sessF: Future[HoTTWebSession] =
    wsF.map(
      ws =>
        HoTTWebSession
          .launch(ws, bots, 
          // Some(PostResponse.capResponse(HoTTMessages.Cap))
          )
    )
}
