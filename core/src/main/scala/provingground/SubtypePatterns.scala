package provingground

import cats._

import cats.implicits._

import scala.language.higherKinds

import Translator.Pattern

import Functors._

trait QuasiInclusion[X, Y]{
  type F[_]

  implicit val traverse : Traverse[F]

  val incl: Y => F[X]
}

object QuasiInclusion{
  implicit def subtype[X, Y <: X] : QuasiInclusion[X, Y] = new QuasiInclusion[X, Y]{
    type F[A] = Id[A]

    val traverse = implicitly[Traverse[F]]

    val incl = (y: Y) => y : Id[X]
  }

  implicit def pair[X, Y1, Y2](implicit qi1: QuasiInclusion[X, Y1], qi2: QuasiInclusion[X, Y2]) :
    QuasiInclusion[X, (Y1, Y2)] = new QuasiInclusion[X, (Y1, Y2)]{
      type F[A] = (qi1.F[A], qi2.F[A])

      val traverse = traversePair(qi1.traverse, qi2.traverse)

      val incl = {p : (Y1, Y2) => (qi1.incl(p._1), qi2.incl(p._2))}
    }

    def constQI[X, Const] : QuasiInclusion[X, Const] = new QuasiInclusion[X, Const]{
      type F[A] = Const

      val traverse = implicitly[Traverse[F]]

      val incl = (c: Const) => c
    }

    implicit def stringQI[X] = constQI[X, String]

    implicit def numQI[X, NT : Numeric] = constQI[X, NT]

    implicit def travQI[X, Y, G[_]: Traverse](implicit qi: QuasiInclusion[X, Y]) : QuasiInclusion[X, G[Y]] = new QuasiInclusion[X, G[Y]]{
      type F[A] = G[qi.F[A]]

      val traverse =  traverseCompose[G, qi.F](implicitly[Traverse[G]], qi.traverse)

      val incl = (gy: G[Y]) => gy map (qi.incl)
    }
}

trait QuasiProjection[X, Y]{
  val proj: X => Option[Y]
}


class SubTypePattern[X, Y](implicit val qi: QuasiInclusion[X, Y], val qp: QuasiProjection[X, Y]){
  val split: X => Option[qi.F[X]] = (x) => qp.proj(x) map (qi.incl)

  val pattern = new Pattern(split)(qi.traverse)

  def >>[O](build: qi.F[O] => Option[O]) = pattern >> build

  def >>>[O](build: qi.F[O] => O) = pattern >>> build
}

object SubTypePattern{
  def pattern[X, Y](implicit qi: QuasiInclusion[X, Y], qp: QuasiProjection[X, Y]) = new SubTypePattern[X, Y]
}
