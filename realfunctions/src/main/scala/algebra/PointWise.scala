package algebra

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
 */
object PointWise {

  trait FieldOps[A] extends Any{
    def negate(x: A): A

    def zero : A

    def one : A

    def plus(x: A, y: A): A

    def times(x: A, y: A): A

    def div(x: A, y: A) : A
  }

  trait Circ[A] extends Any{
    def circ(x: A, y: A) : A
  }

  object Circ{
    implicit class applyCirc[A: Circ](x : A){
      def apply(y: A) = implicitly[Circ[A]].circ(x, y)

      def circ(y: A) = apply(y)

      def andThen(y: A) = implicitly[Circ[A]].circ(y, x)
    }
  }

  object FieldOpsSyms{
  implicit class FieldOpElem[A: FieldOps](x: A){
    val fl = implicitly[FieldOps[A]]
    import fl._
    def +(y: A) = plus(x,y)

    def -(y: A) = plus(x, negate(y))

    def unary_- = negate(x)

    def *(y: A) = times(x, y)

    def /(y: A) = div(x, y)
  }

  implicit def natField[A : FieldOps](n: Int) : A = {
    val fl = implicitly[FieldOps[A]]
    n match {
      case 0 => fl.zero
      case k if k <0 => -natField[A](-k)
      case _ => natField[A](n - 1) + fl.one
    }
  }

  }

  implicit def fieldAsFiledOps[A: Field] = new FieldOps[A]{
    val f = implicitly[Field[A]]

    def negate(x: A): A = f.negate(x)

    def zero : A = f.zero

    def one : A = f.one

    def plus(x: A, y: A): A = f.plus(x, y)

    def times(x: A, y: A): A = f.times(x, y)

    def div(x: A, y: A) : A = f.div(x, y)
  }

  import Interval._




  implicit def intervalFieldOps[F : Field : Order] = new FieldOps[Interval[F]]{
    def negate(x: spire.math.Interval[F]): spire.math.Interval[F] = -x
    // Members declared in spire.algebra.AdditiveMonoid
    def zero: spire.math.Interval[F] = Interval.point(Field[F].zero)
    // Members declared in spire.algebra.AdditiveSemigroup
    def plus(x: spire.math.Interval[F],y: spire.math.Interval[F]): spire.math.Interval[F] = x + y
    // Members declared in spire.algebra.EuclideanRing
    def div(x: spire.math.Interval[F],y: spire.math.Interval[F]): spire.math.Interval[F] = x/y
    // Members declared in spire.algebra.MultiplicativeMonoid
    def one: spire.math.Interval[F] = ???
    // Members declared in spire.algebra.MultiplicativeSemigroup
    def times(x: spire.math.Interval[F],y: spire.math.Interval[F]): spire.math.Interval[F] = x * y
  }



  implicit def fieldStruct[A, F: FieldOps] : FieldOps[A => F] = new FieldOps[A => F]{
    val fl = implicitly[FieldOps[F]]
    import FieldOpsSyms._
    // Members declared in spire.algebra.AdditiveGroup
    def negate(x: A => F): A => F = (a) => -x(a)
    // Members declared in spire.algebra.AdditiveMonoid
    def zero: A => F = (a) => fl.zero
    // Members declared in spire.algebra.AdditiveSemigroup
    def plus(x: A => F,y: A => F): A => F = (a) => x(a) + y(a)

    // Members declared in spire.algebra.MultiplicativeGroup
    def div(x: A => F,y: A => F): A => F = (a) => x(a) / y(a)
    // Members declared in spire.algebra.MultiplicativeMonoid
    def one: A => F = (a) => fl.one
    // Members declared in spire.algebra.MultiplicativeSemigroup
    def times(x: A => F,y: A => F): A => F = (a) => x(a) * y(a)
  }

  implicit def OptFieldStruct[A, F: FieldOps] = new FieldOps[A => Option[F]]{
    val fl = implicitly[FieldOps[F]]
    import FieldOpsSyms._
    // Members declared in spire.algebra.AdditiveGroup
    def negate(x: A => Option[F]): A => Option[F] = (a) => x(a) map ((b) => -b)
    // Members declared in spire.algebra.AdditiveMonoid
    def zero: A => Option[F] = (a) => Some(fl.zero)
    // Members declared in spire.algebra.AdditiveSemigroup
    def plus(x: A => Option[F],y: A => Option[F]): A => Option[F] =
      (a) => for (p <- x(a); q <- y(a)) yield p + q
    // Members declared in spire.algebra.MultiplicativeGroup
    def div(x: A => Option[F],y: A => Option[F]): A => Option[F] =
      (a) => for (p <- x(a); q <- y(a); r <- Try(p/q).toOption) yield r
    // Members declared in spire.algebra.MultiplicativeMonoid
    def one: A => Option[F] = (a) => Some(fl.one)
    // Members declared in spire.algebra.MultiplicativeSemigroup
    def times(x: A => Option[F],y: A => Option[F]): A => Option[F] =
      (a) => for (p <- x(a); q <- y(a)) yield p * q

  }
}
