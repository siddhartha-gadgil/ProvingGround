package provingground

import provingground.HoTT._
import ScalaRep._
import scala.reflect.runtime.universe.{
  Try => UnivTry,
  Function => FunctionUniv,
  _
}

/**
  * Recursion and induction for (dependent) pairs.
  */
object InducPairs {
  val A = "A" :: Type

  val B = "B" :: Type

  val C = "C" :: Type

  val f = "f" :: (A ->: B ->: C)

  val a = "a" :: A

  val b = "b" :: B

  val ab = pair(a, b)

  val recPair = lambda(A)(
    lambda(B)(
      lambda(C)(
        lambda(f)(
          lambda(ab)(f(a)(b))
        ))))

  val Bs = "B" :: A ->: Type

  val Btype = PiDefn(Bs)

  val bs = "b" :: Btype

  val toC = a ~>: (Bs(a) ->: C)

  val g = "g" :: toC

  val abDep = DepPair(a, bs(a), Bs)

  val recSigma = lambda(A)(
    lambda(B)(
      lambda(C)(
        lambda(g)(
          lambda(abDep)(g(a)(bs(a)))
        ))))

  val Cs = "C" :: A ->: B ->: Type

//  val toCs = PiTyp(lmbda(a)(PiTyp(lmbda(b)(Cs(a)(b)))))

  val toCs =
    (a !: A) ~>: ((b !: B) ~>: Cs(a)(b)) // the !: checks types and is mainly for documentation.

  val h = "h" :: toCs

  val inducPair = lambda(A)(
    lambda(B)(
      lambda(Cs)(
        lambda(h)(
          lambda(ab)(h(a)(b))
        ))))

  val toCsDep = (a !: A) ~>: ((bs(a) !: Bs(a)) ~>: Cs(a)(bs(a)))

  val hDep = "h" :: toCsDep

  val InducSigma = lambda(A)(
    lambda(Bs)(
      lambda(Cs)(
        lambda(hDep)(
          lambda(abDep)(h(a)(bs(a)))
        ))))
}
