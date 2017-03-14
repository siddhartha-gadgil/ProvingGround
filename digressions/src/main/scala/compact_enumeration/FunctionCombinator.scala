package compact_enumeration
import spire.math._
import scala.util._

/**
  * @author gadgil
  */
class FunctionCombinator[F](add: (F, F) => F,
                            sum: F => Option[(F, F)],
                            mult: (F, F) => F,
                            prod: F => Option[(F, F)],
                            div: (F, F) => F,
                            quot: F => Option[(F, F)],
                            compose: (F, F) => F,
                            composition: F => Option[(F, F)],
                            const: Double => F) {
  implicit class FnOp(fn: F) {
    def +(that: F) = add(fn, that)

    def *(that: F) = mult(fn, that)

    def /(that: F) = div(fn, that)

    def -(that: F) = this + (const(0.0) * that)

    def unary_- = const(0.0) - fn

    def apply(that: F) = compose(fn, that)

    def circ(that: F) = compose(fn, that)
  }

  def derivative(table: F => Option[F]): F => Option[F] = {
    def sumrule(g: F) =
      for ((x, y) <- sum(g); a <- derivative(table)(x);
      b <- derivative(table)(y)) yield (a + b)

    def leibnitz(g: F) =
      for ((x, y) <- prod(g); a <- derivative(table)(x);
      b <- derivative(table)(y)) yield (a * y + b * x)

    def quotient(g: F) =
      for ((x, y) <- quot(g); a <- derivative(table)(x);
      b <- derivative(table)(y)) yield (a * y - b * x) / (y * y)

    def chain(g: F) =
      for ((x, y) <- composition(g); a <- derivative(table)(x);
      b <- derivative(table)(y)) yield (b * (a circ y))

    (f: F) =>
      table(f) orElse sumrule(f) orElse leibnitz(f) orElse quotient(f) orElse chain(
          f)
  }

  def rationalApprox(
      table: F => Interval[Rational] => Option[Interval[Rational]])
    : F => Interval[Rational] => Option[Interval[Rational]] = {
    def sumIntvl(g: F)(j: Interval[Rational]) =
      for ((x, y) <- sum(g);
      a <- rationalApprox(table)(x)(j); b <- rationalApprox(table)(y)(j)) yield
      (a + b)

    def prodIntvl(g: F)(j: Interval[Rational]) =
      for ((x, y) <- prod(g);
      a <- rationalApprox(table)(x)(j); b <- rationalApprox(table)(y)(j)) yield
      (a * b)

    def quotIntvl(g: F)(j: Interval[Rational]) =
      for ((x, y) <- quot(g);
      a <- rationalApprox(table)(x)(j); b <- rationalApprox(table)(y)(j);
      bound <- Try(a / b).toOption) yield (bound)

    def compositionIntvl(g: F)(j: Interval[Rational]) =
      for ((x, y) <- sum(g);
      a <- rationalApprox(table)(x)(j);
      b <- rationalApprox(table)(y)(a)) yield b

    (f: F) => (j: Interval[Rational]) =>
      table(f)(j) orElse sumIntvl(f)(j) orElse prodIntvl(f)(j) orElse quotIntvl(
          f)(j) orElse compositionIntvl(f)(j)
  }
}
