package provingground.library

import provingground._, interface._, HoTT._, learning._

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

  val localProver: LocalProver = LocalProver(
    termState,
    TermGenParams.zero.copy(appW = 0.1, unAppW = 0.1),
    maxTime = Some(500000L)
  ).noIsles

  import HoTTBot._

  val bots: Vector[HoTTBot] = Vector(
    expEvToFinalState,
    finalStateFilteredLemmas(tautGen),
    eqnsToExpEv,
    lemmasBigTangentEquations(scale = 0.1, power = 0.4, lemmaMix = 0.3),
    eqnUpdate,
    updateTerms,
    reportProofs(results),
    expnEqnUpdate
  )
}
