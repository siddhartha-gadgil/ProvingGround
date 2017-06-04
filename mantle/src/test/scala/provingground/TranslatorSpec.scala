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

  "Composite translation" should "translate trees recursively" in {
    assert(trans(B(B(D, D), B(B(D, D), D))) == Some(C(C(E,E),C(C(E,E),E))))
    assert(trans(big) == Some(bigIm))
  }

  it should "fail gracefully" in {
    assert(trans(B(B(D, E), B(B(D, D), D))) == None)
  }
}
