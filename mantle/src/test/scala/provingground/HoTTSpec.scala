package provingground

import HoTT._
import org.scalatest.FlatSpec


class HoTTSpec extends FlatSpec{
  val A = "A" :: __
  val a = "a" :: A

  val B = "B" :: __
  val b = "b" :: B
  val f = "f" :: (A ->: B)

  val g = "g" :: (A ->: B)
  val hA = "h_A" :: (A ->: A)
  val hB = "h_B" :: (B ->: B)

  val id = lambda(A)(lambda(a)(a))

  val Bs = "B(a)" :: (A ->: __)

  val fdep = "f" :: PiTyp(Bs)

  val gdep = "g" :: PiTyp(Bs)

  val AimplB = "_:A->B" :: (A ->: B)

  val mp =
    lambda(A)(
      lambda(B)(
        lambda(a !: A)(
          lambda(AimplB !: (A ->: B))(
            AimplB(a) !: B
          )
        )
      )
    )

  val fmly = (a !: A) ~>: (Bs(a) ->: A)

  val switch =
    lambda(pair(a, b))(
      pair(b, a)
    )

  "A symbolic object" should "have the correct type" in{
    assert(a.typ == A)
    assert(A.typ == __)
    assert(f.typ == (A ->: B))
  }

  it should "substitute correctly" in {
    val x ="x" :: A
    val y = "y" :: A
    assert(a.replace(a, x) == x)
    assert(a.replace(x, y) == a)
  }

  it should "be determined by symbol and type" in {
    assert(a == "a" :: A)
    assert(!(a == "a":: B))
    assert(!(a == "b" :: A))
    assert(A == "A" :: __)
    assert(!(A == "B" :: __))
    assert(!(A == "A" :: A))
  }

  "A function (result)" should "evaluate to the correct type" in {
    assert(f(a).typ == B)
  }

  it should "throw IllegalArgument exception if argument type is wrong" in {
    intercept[IllegalArgumentException]{
      f("a" :: B)
    }
  }

  it should "be substituted on changing either argument or function, even when nested" in {
    val x = "x" :: A

    assert(f(a).replace(a, x) == f(x))
    assert(f(a).replace(f, g) == g(a))
    assert(f(hA(a)).replace(f, g) == g(hA(a)))
    assert(hB(f(hA(a))).replace(f, g) == hB(g(hA(a))))
  }

  "lambda defintions" should "have correct type" in {
    val fn = lambda(a)(f(hA(a)))
    assert(fn.typ == (A ->: B))
  }

  it should "evaluate parametrized Identity function correctly, including for function types" in {
    val x = "x" :: A
    assert(id(A)(x) == x)
    assert(id(B)(b) == b)
    assert(id(A ->: B).typ == (A ->: B) ->: (A ->: B))
    assert(id(A ->: B)(f) == f)
    assert(id(B).typ == B ->: B)
    assert(id(A ->: B).typ == (A ->: B) ->: (A ->: B))
  }

  "A dependent function (result)" should "evaluate to the correct type" in {
    assert(fdep(a).typ == Bs(a))
  }

  it should "throw IllegalArgument exception if argument type is wrong" in {
    intercept[IllegalArgumentException]{
      fdep("a" :: B)
    }
  }

  it should "be substituted on changing either argument or function, even when nested" in {
    val x = "x" :: A

    assert(fdep(a).replace(a, x) == fdep(x))
    assert(fdep(a).replace(fdep, gdep) == gdep(a))
    assert(fdep(hA(a)).replace(fdep, gdep) == gdep(hA(a)))
    assert((fdep(hA(a))).replace(fdep, gdep) == (gdep(hA(a))))
    val h = "h" :: (fdep(hA(a)).typ ->: fdep(hA(a)).typ)
    assert(h(fdep(hA(a))).replace(fdep, gdep) == h(gdep(hA(a))))
  }

  "Modus ponens" should "have type X -> Y when applied to types X and Y" in {
    assert(mp(A)(B).typ == A ->: ((A ->: B) ->: B))
    assert(mp(B)(A).typ == B ->: ((B ->: A) ->: A))
  }

  "Type family defined using lambdas" should "have terms appropriate dependent functions" in {
    val fn = "f" :: fmly
    val x ="x" :: A
    assert(fn(x).typ  == Bs(x) ->: A)
  }

  "A Pair type" should "have variables substituted separately in lambdas" in {
    val x = "x" :: A
    val y = "y" :: B
    assert(switch(pair(x, y))== pair(y, x))
  }

  it should "be a sigma type or pair type depending on whether the second component depends on the first" in {
    val bdep = "b" :: Bs(a)
    val pdep = mkPair(a, bdep)
    val p = mkPair(a, b)
    assert(p.typ == pair(A, B))
    assert(pdep.typ == Sgma(a !: A, Bs(a)))
  }

}
