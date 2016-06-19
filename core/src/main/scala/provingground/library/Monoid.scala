package provingground.library

import provingground._

import HoTT._

object Monoid {
  val M = "M" :: Type

  val sym = IdentityTyp.symm(M)

  val trans = IdentityTyp.trans(M)

  val a = "a" :: M
  val b = "b" :: M
  val c = "c" :: M

  val l = "e_l" :: M

  val r = "e_r" :: M

  val op = "_*_" :: M ->: M ->: M

  val leftId = (a ~>: (op(l)(a) =:= a)).Var
  val rightId = (a ~>: (op(a)(r) =:= a)).Var

  val refl = lambda(a)(Refl(M, a))

  val idUnique =
    b ~>: (
        (a ~>: (op(l)(a) =:= a)) ~>: (l =:= b)
    )

  val assoc = (a ~>: (b ~>: (c ~>: (
                  op(a)(op(b)(c)) =:= op(op(a)(b))(c)
              )))).Var

  val MVars = Vector[Term](a, b, c) map (Weighted(_, 1.0 / 3.0))

  val X = "X" :: Type

  val Y = "Y" :: Type

  val f = "f" :: X ->: Y

  val g = "g" :: X ->: Y

  val x = "x" :: X

  val extensionality = lambda(X)(
      lambda(Y)(
          lambda(f)(IdentityTyp.extnslty(f))
      ))

  val transfer = (X ~>: (
          Y ~>: (
              f ~>: (
                  g ~>: (
                      x ~>: ((f =:= g) ->: (f(x) =:= g(x)))
                  )
              )
          )
      )).Var

  val names = Vector(refl -> "id.reflexivity",
                     sym -> "id.symmetry",
                     trans -> "id.transitivity",
                     extensionality -> "id.extendsionality",
                     transfer -> "id.transfer")

  val dist = FiniteDistribution.unif(M,
                                     a,
                                     b,
                                     c,
                                     l,
                                     r,
                                     op,
                                     leftId,
                                     rightId,
                                     refl,
                                     sym,
                                     trans,
                                     assoc,
                                     extensionality,
                                     transfer)

  val ded = Deducer(vars = MVars, cutoff = 0.01)

  def smooth(fd: FiniteDistribution[Term]) =
    (fd ++ (fd map (sigma)) ++ fd map ((t) => sigma(sigma(t)))).normalized()

  def sigma(t: Term) = {
    val aa = "aa" :: M
    val bb = "bb" :: M
    val cc = "cc" :: M
    t.replace(a, aa)
      .replace(b, bb)
      .replace(c, cc)
      .replace(aa, b)
      .replace(bb, c)
      .replace(cc, a)
  }
}

object Group {
  import Monoid.{M => G, l => e, _}

  val i = "inv" :: G ->: G

  val lInv = a ~>: (op(a)(i(a)) =:= a)

  val rInv = a ~>: (op(i(a))(a) =:= a)
}
