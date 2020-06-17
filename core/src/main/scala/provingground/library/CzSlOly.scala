package provingground.library

import provingground._, interface._, HoTT._, learning._
import provingground.learning.HoTTMessages._
import monix.execution.Scheduler.Implicits.global

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

  val steps = Vector(
    Unify.appln(trans, "lemma" :: results(3)).get.typ,
    Unify.appln(trans, "lemma" :: results(4)).get.typ
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
      FiniteDistribution
        .unif(trans),
      FiniteDistribution.unif()
    ),
    cutoffScale = 5,
    tgOpt = Some(TermGenParams.zero.copy(unAppW = 0.2)),
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
    tgOpt = Some(TermGenParams.zero.copy(appW = 0.2)),
    cutoffScale = 1
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
    eqnsToExpEv,
    lemmasBigTangentEquations(scale = 0.1, power = 0.4, lemmaMix = 0.3),
    eqnUpdate,
    updateTerms,
    reportProofs(results),
    expnEqnUpdate
  )

  val bots: Vector[HoTTBot] = Vector(
    expEvToFinalState,
    finalStateFilteredLemmas(),
    baseMixinLemmas(0.3) :: tangentLemmas(power = 0.7),
    cappedBaseState(0.3),
    cappedTangentEquations,
    eqnsToExpEv.triggerWith[EquationsCompleted.type],
    eqnUpdate,
    updateTerms,
    expnEqnUpdate,
    reportProofs(results),
    reportMixinLemmas(results),
    reportTangentLemmas(results),
    reportTangentBaseTerms(steps)
  )

  val web = new HoTTPostWeb()
  val ws  = WebState[HoTTPostWeb, HoTTPostWeb.ID](web)

  lazy val sessF =
    for {
      ws1 <- ws.post(TautologyInitState(tautGen), Set())
      ws2 <- ws1.postLast(transitivtyInit)
      ws3 <- ws2.postLast(mulInit)
      ws4 <- ws3.postLast(localProver)
      ws5 <- ws4.act(lpToEnhancedExpEv)
      ws6 <- ws5.act(expnEqnUpdate)
    } yield HoTTWebSession.launch(ws5, bots)
}
