package compact_enumeration

import spire.algebra._
import spire.math._
import spire.implicits._
import scala.util._
import scala.language.implicitConversions

/**
  * @author gadgil
  *
  * Warning: this creates field operations but does not give actual fields
  *
  * the operations of a field on the type A, but in general without the properties.
  */
trait FieldOps[A] extends Any {
  def negate(x: A): A

  def zero: A

  def one: A

  def plus(x: A, y: A): A

  def times(x: A, y: A): A

  def div(x: A, y: A): A
}

object FieldOps {

  object FieldOpsSyms {

    /**
      * for a type A with FieldOps, define operations +, -, *, /, unary_-
      */
    implicit class FieldOpElem[A: FieldOps](x: A) {
      val fl = implicitly[FieldOps[A]]
      import fl._
      def +(y: A) = plus(x, y)

      def -(y: A) = plus(x, negate(y))

      def unary_- = negate(x)

      def *(y: A) = times(x, y)

      def /(y: A) = div(x, y)
    }

    /**
      * Map the natural numbers into a type with field operations.
      */
    implicit def natField[A: FieldOps](n: Int): A = {
      val fl = implicitly[FieldOps[A]]
      n match {
        case 0          => fl.zero
        case k if k < 0 => -natField[A](-k)
        case _          => natField[A](n - 1) + fl.one
      }
    }
  }

  /**
    * Field operations a la FieldOps on a spire field.
    */
  implicit def fieldAsFieldOps[A: Field]: FieldOps[A] = new FieldOps[A] {
    val f = implicitly[Field[A]]

    def negate(x: A): A = f.negate(x)

    def zero: A = f.zero

    def one: A = f.one

    def plus(x: A, y: A): A = f.plus(x, y)

    def times(x: A, y: A): A = f.times(x, y)

    def div(x: A, y: A): A = f.div(x, y)
  }


  /**
    * Field operations on Intervals with endpoints in a field.
    */
  implicit def intervalFieldOps[F: Field: Order] =
    new FieldOps[Interval[F]] {
      def negate(x: spire.math.Interval[F]): spire.math.Interval[F] = -x

      def zero: spire.math.Interval[F] = Interval.point(Field[F].zero)

      def plus(x: spire.math.Interval[F],
               y: spire.math.Interval[F]): spire.math.Interval[F] = x + y

      def div(x: spire.math.Interval[F],
              y: spire.math.Interval[F]): spire.math.Interval[F] = x / y

      def one: spire.math.Interval[F] = Interval.point(Field[F].one)

      def times(x: spire.math.Interval[F],
                y: spire.math.Interval[F]): spire.math.Interval[F] = x * y
    }

  /**
    * Field operations on functions A => F
    */
  implicit def funcFieldOps[A, F: FieldOps]: FieldOps[A => F] =
    new FieldOps[A => F] {
      val fl = implicitly[FieldOps[F]]
      import FieldOpsSyms._

      def negate(x: A => F): A => F = (a) => -x(a)

      def zero: A => F = (a) => fl.zero

      def plus(x: A => F, y: A => F): A => F = (a) => x(a) + y(a)

      def div(x: A => F, y: A => F): A => F = (a) => x(a) / y(a)

      def one: A => F = (a) => fl.one

      def times(x: A => F, y: A => F): A => F = (a) => x(a) * y(a)
    }

  /**
    * Field operations on optional functions A => Option[F]
    */
  implicit def OptFieldStruct[A, F: FieldOps] = new FieldOps[A => Option[F]] {
    val fl = implicitly[FieldOps[F]]
    import FieldOpsSyms._

    def negate(x: A => Option[F]): A => Option[F] = (a) => x(a) map ((b) => -b)

    def zero: A => Option[F] = (a) => Some(fl.zero)

    def plus(x: A => Option[F], y: A => Option[F]): A => Option[F] =
      (a) => for (p <- x(a); q <- y(a)) yield p + q

    def div(x: A => Option[F], y: A => Option[F]): A => Option[F] =
      (a) => for (p <- x(a); q <- y(a); r <- Try(p / q).toOption) yield r

    def one: A => Option[F] = (a) => Some(fl.one)

    def times(x: A => Option[F], y: A => Option[F]): A => Option[F] =
      (a) => for (p <- x(a); q <- y(a)) yield p * q
  }
}
