package provingground.library

import provingground._, interface._, HoTT._, learning._
import provingground.learning.HoTTMessages._
import monix.execution.Scheduler.Implicits.global
import scala.concurrent._, duration._

object CzSlOly {
  val M = "M" :: Type

  val eqM = "eqM" :: M ->: M ->: Type

  val a = "a" :: M
  val b = "b" :: M
  val c = "c" :: M

  val m = "m" :: M

  val n = "n" :: M

  val mul = "mul" :: M ->: M ->: M

  val mn = mul(m)(n)

  val results = Vector(
    eqM(mul(m)(n))(mul(n)(m)),
    eqM(mul(mul(m)(n))(n))(m),
    eqM(mul(mul(m)(n))(mul(mul(m)(n))(n)))(n),
    eqM(n)(mul(mul(m)(n))(mul(mul(m)(n))(n))),
    eqM(mul(mul(m)(n))(mul(mul(m)(n))(n)))(mul(mul(m)(n))(m)),
    eqM(mul(mul(mul(m)(n))(m))(m))(mul(m)(n)),
    eqM(n)(mul(mul(m)(n))(m)),
    eqM(mul(n)(m))(mul(mul(mul(m)(n))(m))(m)),
    eqM(mul(n)(m))(mul(m)(n))
  )

  val refl = "refl" :: a ~>: (eqM(a)(a))

  val sym = "sym" :: a ~>: (b ~>: (eqM(a)(b) ->: eqM(b)(a)))

  val trans =
    "trans" :: a ~>:
      (b ~>: (c ~>: ((eqM(a)(b)) ->: (eqM(b)(c)) ->: (eqM(a)(c)))))

  val leftMul = "left-multiply" :: a ~>: (b ~>: (c ~>: (eqM(b)(c) ->: eqM(
    mul(a)(b)
  )(mul(a)(c)))))
  val rightMul = "right-multiply" :: a ~>: (b ~>: (c ~>: (eqM(b)(c) ->: eqM(
    mul(b)(a)
  )(mul(c)(a)))))

  val ass1 = "ass1" :: a ~>: (b ~>: eqM(mul(mul(a)(b))(b))(a))
  val ass2 = "ass2" :: a ~>: (b ~>: eqM(mul(a)(mul(a)(b)))(b))

  val steps: Vector[Typ[Term]] = Vector(
    trans.typ,
    Unify.appln(trans, "lemma" :: results(3)).get.typ,
    leftMul(mn).typ,
    rightMul(m).typ
  )

  val inferTriples = Vector(
    (trans.typ, results(3), steps(1)),
    (steps(1), results(4), results(6)),
    (steps(2), results(1), results(4)),
    (steps(3), results(6), results(7))
  )

  val termState: TermState = TermState(
    FiniteDistribution
      .unif(
        mul,
        eqM,
        m,
        n,
        mn,
        ass1,
        ass2,
        sym,
        leftMul,
        rightMul,
        trans,
        refl
      ),
    FiniteDistribution.unif(M),
    goals = FiniteDistribution.unif(eqM(mul(m)(n))(mul(n)(m)))
  )

  val tautGen: TermState = TermState(
    FiniteDistribution
      .unif(mul, m, n, mn, sym, trans, refl),
    FiniteDistribution.unif(M)
  )

  val transitivtyInit = SpecialInitState(
    TermState(
      FiniteDistribution(trans -> 0.9, sym -> 0.1),
      FiniteDistribution.unif(M)
    ),
    baseCutoff = math.pow(10, -3),
    cutoffScale = 0.05, // temporarily disable
    tgOpt = Some(TermGenParams.zero.copy(unAppW = 0.4)),
    depthOpt = Some(2)
  )

  val mulInit = SpecialInitState(
    TermState(
      FiniteDistribution
        .unif(
          m,
          n,
          mn,
          leftMul,
          rightMul
        ),
      FiniteDistribution.unif(M)
    ),
    tgOpt = Some(TermGenParams.zero.copy(appW = 0.4)),
    baseCutoff = math.pow(10, -3),
    depthOpt = Some(2),
    cutoffScale = 0.5
  )

  val localProver: LocalProver = LocalProver(
    termState,
    TermGenParams.zero.copy(appW = 0.1, unAppW = 0.1),
    maxTime = Some(500000L)
  ).noIsles

  import HoTTBot._

  // Old style, kept for debugging
  val bots0: Vector[HoTTBot] = Vector(
    expEvToFinalState,
    finalStateFilteredLemmas(), // needed argument tautGen
    eqnsToExpEv(),
    lemmasBigTangentEquations(scale = 0.1, power = 0.4, lemmaMix = 0.3),
    eqnUpdate,
    updateTerms,
    reportProofs(results),
    expnEqnUpdate
  )

  val lemRefine = tangentLemmas(scale = 0.1, cutoff = 0.007, power = 0.7) :+ baseMixinLemmas(
    0.3
  )
  val expFS =
    expEvToFinalState.andThen(updateTerms).andThen(reportProofs(results))
  val tangEq = tangentEquations(results, steps)

  val bots: Vector[HoTTBot] = Vector(
    expEvToFinalState,
    finalStateFilteredLemmas(
      tg = TermGenParams.zero
        .copy(appW = 0.1, unAppW = 0.1, piW = 0.05, lmW = 0.05)
    ),
    lemRefine,
    cappedSpecialBaseState,
    timedUnAppEquations(math.pow(10, -6), 2.minutes, 4),
    // unAppEquations(math.pow(10, -6)),
    // cappedForkedTangentEquations,
    eqnsToExpEv(
      Some(
        TermGenParams.zero
          .copy(appW = 0.1, unAppW = 0.1, piW = 0.05, lmW = 0.05)
      )
    ),
    eqnUpdate,
    updateTerms,
    expnEqnUpdate,
    reportProofs(results),
    reportProofs(steps, "Steps (in final state)"),
    reportBaseTangents(results, steps, inferTriples)
  )

  val web = new HoTTPostWeb()
  val ws  = WebState[HoTTPostWeb, HoTTPostWeb.ID](web)

  lazy val wsF =
    for {
      ws1  <- ws.post(TautologyInitState(tautGen), Set())
      ws2  <- ws1.postLast(transitivtyInit)
      ws3  <- ws2.postLast(mulInit)
      ws4  <- ws3.postLast(localProver)
      ws5  <- ws4.act(lpToEnhancedExpEv)
      ws6  <- ws5.act(expnEqnUpdate)
      ws7  <- ws6.act(expFS)
      ws8  <- ws7.act(finalStateFilteredLemmas())
      ws9  <- ws8.act(lemRefine)
      ws10 <- ws9.act(cappedBaseState(0.3))
    } yield ws10

  lazy val sessF = wsF.map(ws => HoTTWebSession.launch(ws, bots))
}
