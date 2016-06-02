package provingground.scratch

import provingground.{FiniteDistribution => FD, TruncatedDistribution => TD, _}

import HoTT._

object DedGrad {
  val A = "A" :: Type

  val B = "B" :: Type

  val a = "a" :: A

  val f = "f" :: (A ->: A)

  val idA = lmbda(a)(a)

  object SimpleGrad {
    val deduc = new DeducerFunc(0.2, 0.2, 0.2, 0.3)

    val ev = deduc.memFunc(FD.unif(A, a, f))

    lazy val samp = ev sample 100000

    lazy val sampLambda = deduc.lambdaFD(samp)(x)

    val idProp = (fd: FD[Term]) => (td: TD[Term]) => td

    import deduc._

    lazy val props: List[Prop] = List(funcProp _,
                                      lambdaPropVar _,
                                      lambdaPropValues _,
                                      piPropVar _,
                                      piPropValues _)

    val x = A.Var

    val terms = List(
        f, a, f(a), f(x), A ->: A, lmbda(a)(a), lmbda(x)(a), lmbda(a)(f(a)))

    type Prop =
      (=> FD[Term] => TD[Term] => TD[Term]) => FD[Term] => TD[Term] => TD[Term]

    lazy val backEg =
      deduc.backProp(0.5, deduc.applnInvImage)(samp)(TD.atom(lmbda(x)(f(x))))

    def grad(p: Prop) =
      (for (t <- terms) yield
        (
            p,
            t,
            p(idProp)(samp)(TD.atom(t)).getFD(0.001)
        )) groupBy (_._1)
  }

  object ABU {
    val deduc = new DeducerFunc(0.2, 0.2, 0.2, 0.3, List(A, B))

    val ev = deduc.memFunc(FD.unif(A, B, Type))

    lazy val samp = ev sample 100000

    def invIdA = deduc.applnInvImage(idA)

    def invProdIdA = {
      for ((f, x) <- invIdA) yield (Unify.appln(f, x), f, x)
    }

    def unifInvIdA =
      for (result <- deduc.invImageMap.keys;
           (f, x) <- deduc.invImageMap(result);
           unif <- Unify.unify(result, idA, TermToExpr.isVar)) yield
        UnifInv(idA, result, unif, f, x)
  }
}

case class UnifInv(target: Term,
                   result: Term,
                   unif: Map[Term, Term],
                   func: Term,
                   arg: Term) {
  import Unify.{multisub}
  val newResult = multisub(result, unif)
  val newArg = multisub(arg, unif)
  val newFunc = multisub(func, unif)

  def origProd = {
    Unify.appln(func, arg) == Some(result)
  }

  def unified = newResult == target

  def prodOpt = {
    Unify.appln(newFunc, newArg)
  }

  def bug = prodOpt != Some(target)
}
