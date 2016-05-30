package provingground.scratch

import provingground.{FiniteDistribution => FD, _}

import HoTT._

object ABDeduc{
  val A ="A" :: Type

  val B = "B" :: Type



  val a = "a" :: A

  val idA = lmbda(a)(a)

  object ABU{
    val deduc = new DeducerFunc(0.2, 0.2, 0.2, 0.3)

    val ev = deduc.memFunc(FD.unif(A, B, Type))

    lazy val samp = ev sample 100000

    def invIdA = deduc.applnInvImage(idA)

    def fullInvIdA = ???

    def invProdIdA = {
      for ((f, x) <- invIdA) yield (Unify.appln(f, x), f, x)
    }

    def unifInvIdA =
       for (
         result <- deduc.invImageMap.keys;
         (f, x) <- deduc.invImageMap(result);
         unif <- Unify.unify(result, idA, TermToExpr.isVar)
       ) yield UnifInv(idA, result, unif, f, x)
  }

}

case class UnifInv(
  target: Term,
  result: Term,
  unif: Map[Term, Term],
  func: Term,
  arg: Term){
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
