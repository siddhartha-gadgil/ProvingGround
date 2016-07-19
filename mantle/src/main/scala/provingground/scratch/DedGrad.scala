package provingground.scratch

import provingground.{FiniteDistribution => FD, TruncatedDistribution => TD, _}

import HoTT._

import ammonite.ops._

//import upickle.default._

import scala.io.StdIn

object TempMain /*extends App*/ {
  DedGrad.LongRun.buf.run
  println(s"Long running process (about 10 hours)\nPress RETURN to stop...")
  StdIn.readLine() // for the future transformation
}

object DedGrad {
  val A = "A" :: Type

  val B = "B" :: Type

  val a = "a" :: A

  val b = "b" :: B

  val distABU = FD.unif[Term](A, B, Type)

  val distAB = FD.unif[Term](A, B)

  val cnst = lmbda(a)(lmbda(b)(a))

  val f = "f" :: (A ->: A)

  val idA = lmbda(a)(a)

  object LongRun {
    val file = cwd / 'data / "ABrun.dist"

    write.over(file, "# 10 hour run with A, B\n")

    val hfile = cwd / 'data / "ABrunHour.dist"

    write.over(hfile, "# 1 hour run with A, B\n")

    def save(fd: FD[Term]) =
      write.append(file, FreeExprLang.writeDist(fd) + "\n")

    def hsave(fd: FD[Term]) =
      write.append(hfile, FreeExprLang.writeDist(fd) + "\n")

    val ded = new Deducer(vars = Vector(Weighted(A, 0.3), Weighted(B, 0.3)))

    val dedh = new Deducer(vars = Vector(Weighted(A, 0.3), Weighted(B, 0.3)))

    val longtime = 1000.toLong * 3600 * 10

    val hour = 1000.toLong * 3600

    val hbuf = new dedh.BufferedRun(
        distAB, 100000, 5000, _.getElapsedTime > hour, hsave)

    val buf = new ded.BufferedRun(
        distAB, 10000000, 10000, _.getElapsedTime > longtime, save)
  }

  object SimpleGrad {
    type Prob = Term => Double

    val deduc = new Deducer(0.2, 0.2, 0.2, 0.3)

//    val ev = deduc.memFunc(FD.unif(A, a, f))

    // lazy val samp = (x: Term) => (ev sample 100000)(x)
    //
    // lazy val sampLambda = deduc.lambdaFD(samp)(x)
    //
    // val idProp = (fd: Prob) => (td: TD[Term]) => td
    //
    // import deduc._
    //
    // lazy val props: Vector[Prop] = Vector(funcProp _,
    //                                       lambdaPropVar _,
    //                                       lambdaPropValues _,
    //                                       piPropVar _,
    //                                       piPropValues _)

    val x = A.Var

    val terms = Vector(
        f, a, f(a), f(x), A ->: A, lmbda(a)(a), lmbda(x)(a), lmbda(a)(f(a)))

    type Prop =
      (=> Prob => TD[Term] => TD[Term]) => Prob => TD[Term] => TD[Term]

    // lazy val backEg = deduc.backProp(0.5, deduc.applnInvImage)((x: Term) =>
    //       samp(x))(TD.atom(lmbda(x)(f(x))))
    //
    // def grad(p: Prop) =
    //   (for (t <- terms) yield
    //     (
    //         p,
    //         t,
    //         p(idProp)(samp)(TD.atom(t)).getFD(0.001)
    //     )) groupBy (_._1)
  }

  object AB {
    val ded = new Deducer(0.2,
                          0.2,
                          0.2,
                          0.3,
                          Vector(Weighted(A, 0.4), Weighted(B, 0.4)),
                          0.5,
                          0.01,
                          0.5,
                          0,
                          0.5)
  }

  object ABU {
    val deduc = new Deducer(
        0.2, 0.2, 0.2, 0.3, Vector(Weighted(A, 0.4), Weighted(B, 0.4)))

    // val ev = deduc.memFunc(FD.unif(A, B, Type))
    //
    // lazy val samp = ev sample 100000
    //
    // def invIdA = deduc.applnInvImage(idA)
    //
    // def invProdIdA = {
    //   for ((f, x) <- invIdA) yield (Unify.appln(f, x), f, x)
    // }
    //
    // def unifInvIdA =
    //   for (result <- deduc.invImageMap.keys;
    //        (f, x) <- deduc.invImageMap(result);
    //        unif <- Unify.unify(result, idA, HoTT.isVar)) yield
    //     UnifInv(idA, result, unif, f, x)
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
