package provingground.translation

import cats._

import cats.implicits._

import cats.syntax._

import scala.language.higherKinds

import Translator.{Pattern, Builder}

import Functors._

import shapeless.{Id => Ids, _}

import HList._

import Coproduct._

/**
  * inclusion of type `Y` in `F(X)`
  */
abstract class QuasiInclusion[X, Y, F[_]: Traverse] {

  def incl(y: Y): F[X]

  def proj(fa: F[X]): Option[Y]
}

/**
  * inclusion of type `Y` in `F(X)` with `F(_)` an `HList`
  */
abstract class QuasiInclHList[X, Y, F[_] <: HList: Traverse]
    extends QuasiInclusion[X, Y, F] {

  def incl(y: Y): F[X]

  def proj(fa: F[X]): Option[Y]
}

object QuasiInclusion {
  implicit def componentIncl[X, Y](
      implicit qp: QuasiProjection[X, Y]): QuasiInclusion[X, Y, Id] =
    new QuasiInclusion[X, Y, Id] {
      def incl(y: Y) = qp.incl(y): Id[X]

      def proj(fa: Id[X]) = qp.proj(fa)
    }

  // implicit def pair[X, Y1, Y2, F1[_] : Traverse, F2[_]: Traverse](
  //     implicit qi1: QuasiInclusion[X, Y1, F1],
  //     qi2: QuasiInclusion[X, Y2, F2]): QuasiInclusion[X, (Y1, Y2), ({type F[A] = (F1[A], F2[A])})#F] =
  //   new QuasiInclusion[X, (Y1, Y2),  ({type F[A] = (F1[A], F2[A])})#F] {
  //
  //     def incl(p: (Y1, Y2)) =
  //       (qi1.incl(p._1), qi2.incl(p._2))
  //
  //   }

  def constQI[X, Cnst]: QuasiInclusion[X, Cnst, ({ type F[A] = Cnst })#F] =
    new QuasiInclusion[X, Cnst, ({ type F[A] = Cnst })#F] {

      def incl(c: Cnst) = c

      def proj(fa: Cnst) = None
    }

  implicit def stringQI[X] = constQI[X, String]

  implicit def numQI[X, NT: Numeric] = constQI[X, NT]

  implicit def hnilIncl[X]: QuasiInclHList[X, HNil, HN] =
    new QuasiInclHList[X, HNil, HN] {
      type F[A] = HNil

      val traverse = implicitly[Traverse[F]]

      def incl(c: HNil) = c

      def proj(fa: HNil) = None
    }

  implicit def hConsIncl[X,
                         Y1,
                         Y2 <: HList,
                         F1[_]: Traverse,
                         F2[_] <: HList: Traverse](
      implicit qi1: Lazy[QuasiInclusion[X, Y1, F1]],
      qi2: QuasiInclHList[X, Y2, F2])
    : QuasiInclHList[X, Y1 :: Y2, ({ type F[A] = F1[A] :: F2[A] })#F] =
    new QuasiInclHList[X, Y1 :: Y2, ({ type F[A] = F1[A] :: F2[A] })#F] {

      def incl(p: Y1 :: Y2) =
        qi1.value.incl(p.head) :: qi2.incl(p.tail)

      def proj(fa: F1[X] :: F2[X]) =
        for (x <- qi1.value.proj(fa.head); y <- qi2.proj(fa.tail)) yield x :: y
    }

  implicit def genericIncl[X, Y, R, F1[_] <: HList: Traverse](
      implicit gen: Lazy[Generic.Aux[Y, R]],
      qi: QuasiInclHList[X, R, F1]): QuasiInclHList[X, Y, F1] =
    new QuasiInclHList[X, Y, F1] {

      def incl(y: Y) = qi.incl(gen.value.to(y))

      def proj(fa: F1[X]) = qi.proj(fa) map ((yr) => gen.value.from(yr))
    }

  implicit def travQI[X, Y, F[_]: Traverse, G[_]: Traverse](
      implicit qi: QuasiInclusion[X, Y, F])
    : QuasiInclusion[X, G[Y], ({ type Z[A] = G[F[A]] })#Z] =
    new QuasiInclusion[X, G[Y], ({ type Z[A] = G[F[A]] })#Z] {

      def incl(gy: G[Y]) = gy map ((x) => qi.incl(x))

      def proj(fa: G[F[X]]) =
        implicitly[Traverse[G]].traverse(fa)(qi.proj)
    }
}

/**
  * projection of `X` onto `Y`
  */
trait QuasiProjection[X, Y] {
  def proj(x: X): Option[Y]

  def incl(y: Y): X
}

object QuasiProjection {
  def apply[X, Y](p: X => Option[Y], i: Y => X) = new QuasiProjection[X, Y] {
    def proj(x: X) = p(x)

    def incl(y: Y) = i(y)
  }

  // def constQuasiprojection[Cnst, Y] = QuasiProjection((c: Cnst) => None : Option[Y])

  implicit def idProj[X]: QuasiProjection[X, X] =
    QuasiProjection[X, X]((x) => Some(x), (x) => x)

  implicit def leftProjection[X1, X2 <: Coproduct, Y](
      implicit qp: QuasiProjection[X1, Y]): QuasiProjection[X1 :+: X2, Y] =
    QuasiProjection[X1 :+: X2, Y]({
      case Inl(x1) => qp.proj(x1)
      case _       => None
    }, (y: Y) => Inl(qp.incl(y)))

  implicit def rightProjection[X1, X2 <: Coproduct, Y](
      implicit qp: QuasiProjection[X2, Y]): QuasiProjection[X1 :+: X2, Y] =
    QuasiProjection[X1 :+: X2, Y]({
      case Inr(x2) => qp.proj(x2)
      case _       => None
    }, (y: Y) => Inr(qp.incl(y)))

  implicit def genericProjection[X, Y, R](
      implicit gen: Generic.Aux[X, R],
      qp: QuasiProjection[R, Y]
  ): QuasiProjection[X, Y] =
    QuasiProjection((x: X) => qp.proj(gen.to(x)),
                    (y: Y) => gen.from(qp.incl(y)))
}

/**
  * Pattern and builder for matching whether, for a term `x: X`, `F(x)` is in the ``subtype`` `Y`.
  * Here `Y` is not just a scala subtype, but we have inclusions and projections making it a direct summand.
  */
class SubTypePattern[X, Y, F[_]: Traverse](
    implicit val qi: QuasiInclusion[X, Y, F],
    val qp: QuasiProjection[X, Y]) {
  val split: X => Option[F[X]] = (x) => qp.proj(x) map (qi.incl)

  val build: F[X] => Option[X] = (x) => qi.proj(x) map (qp.incl)

  val pattern = new Pattern(split)

  val builder = new Builder(build)

  def >>[O](build: F[O] => Option[O]) = pattern >> build

  def >>>[O](build: F[O] => O) = pattern >>> build

  def on[I](split: PartialFunction[I, F[I]]) = builder on split
}

object TestTrait {
  sealed trait A

  case object C extends A

  case class B(x: A, y: A) extends A
}

object SubTypePattern {
  def pattern[X, Y, F[_] <: HList: Traverse](
      implicit qi: QuasiInclHList[X, Y, F],
      qp: QuasiProjection[X, Y]) =
    new SubTypePattern[X, Y, F]

  object Test {
    import TestTrait._

    val qi1 = implicitly[QuasiInclusion[A, A, Id]]

    val qi2 = implicitly[QuasiInclHList[A, HNil, HN]]

    import QuasiInclusion._

    val qi3 =
      hConsIncl(implicitly[Traverse[Id]], implicitly[Traverse[HN]], qi1, qi2)

    val qi4 = implicitly[QuasiInclusion[A, A :: HNil, IdHN]]

    val qi41 = implicitly[QuasiInclHList[A, A :: HNil, IdHN]]

    val qi42 = hConsIncl(implicitly[Traverse[Id]],
                         implicitly[Traverse[IdHN]],
                         qi1,
                         qi41)

    val qi44 = hConsIncl[A, A, A :: HNil, Id, IdHN]

    implicitly[Traverse[StHN]]

    // implicit val qi43: QuasiInclusion[A, A :: A :: HNil, IdIdHN] = qi44

    val qi5 = implicitly[QuasiInclusion[A, A :: A :: HNil, IdIdHN]]

    val qii = genericIncl[A, B, A :: A :: HNil, IdIdHN]

    val qi = implicitly[QuasiInclusion[A, B, IdIdHN]]

    val qp2 = implicitly[QuasiProjection[B, B]]

    val qp3 = implicitly[QuasiProjection[B :+: CNil, B]]

    val qp = implicitly[QuasiProjection[A, B]]

    val pat = pattern[A, B, IdIdHN]

    pat >>> ((xy: Int :: Int :: HNil) => xy.head + xy.tail.head)

    //dot notation required as a workaround for https://github.com/scoverage/scalac-scoverage-plugin/issues/175
    pat.>>>[Int]({ case x :: y :: HNil => x.+(y) })
  }
}
