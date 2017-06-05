package provingground

import translation._, Translator._

import cats._, cats.implicits._

import Functors._

import org.scalatest.FlatSpec

class TranslatorSpec extends FlatSpec {
  class A

  case class B(x: A, y: A) extends A
  case class C(x: A, y: A) extends A

  case object D extends A
  case object E extends A

  case class Big(v: Vector[A], x: A, y: A, w: Vector[A]) extends A

  val Bpat = Pattern.partial[A, II]{case B(x, y) => (x, y)}
  val Dpat = Pattern.partial[A, Un]{case D => ()}

  val bigPat = Pattern.partial[A, VIIV]{
    case Big(v, x, y, w) => (v, (x, (y, w)))
  }

  val trans = Translator.Empty[A, A] ||
                 Bpat >>> {case (x, y) => C(x, y)} ||
                 Dpat >>> {case _ => E} ||
                 bigPat >>> {case (v, (x, (y, w))) => Big(v, x, y, w)}

  val big =
    Big(
      Vector(D, B(D, D)),
      B(B(D, D), B(B(D, D), D)),
      D,
      Vector(D, D, B(D, D))
    )

  val bigIm =
    Big(
      Vector(E, C(E, E)),
      C(C(E, E), C(C(E, E), E)),
      E,
      Vector(E, E, C(E, E))
    )

  import shapeless._

  val Cpat = Pattern.partial[A, IdIdHN]{case C(x, y) => x :: y :: HNil}

  val htrans = trans || Cpat >>> {case x :: y :: HNil => B(x, y)}

  "Composite translation" should "translate trees recursively" in {
    assert(trans(B(B(D, D), B(B(D, D), D))) == Some(C(C(E,E),C(C(E,E),E))))
    assert(trans(big) == Some(bigIm))
  }

  it should "fail gracefully" in {
    assert(trans(B(B(D, E), B(B(D, D), D))) == None)
  }

  it should "mix in and recurse correctly" in {
    assert(htrans(C(B(D, D), D)) == Some(B(C(E, E), E)))
  }

  it should "behave correctly when defaults are mixed in" in {
    val idTrans = Simple((a: A) => Some(a))

    assert(
      (htrans || idTrans)(B(C(D, E), B(D, E))) ==
        Some(C(B(E, E), C(E, E)))
    )
  }

  "Functors" should "be solved as implicits and map correctly" in {
  // these are mainly tests at compile times
  val ll = implicitly[Functor[LL]]

    val li = implicitly[Functor[IL]]

    val ii = implicitly[Functor[II]]

    val iii = implicitly[Functor[III]]

    val nn = implicitly[Functor[N]]

    val ss = implicitly[Functor[S]]

    val xx: IL[Int] = (1, List(1, 2))

    val a = liftMap(List(1, 2, 3), (n: Int) => n + 1)

    val b = liftMap[Int, Int, LL]((List(1), List(2)), (n: Int) => n + 1)

    val c = liftMap[Int, Int, IL]((3, List(1, 2)), (n: Int) => n + 1)

    assert(a == List(2, 3, 4))

    assert(b == (List(2), List(3)))

    assert(c == (4, List(2, 3)))
  }


}
