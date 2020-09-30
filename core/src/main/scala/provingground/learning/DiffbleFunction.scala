package provingground.learning

import provingground._

import scala.annotation._
// import scala.util._

// import scala.language.implicitConversions

import Collections._

import LinearStructure._

import spire.algebra._

import spire.implicits._

case class TangVec[+A](point: A, vec: A) {
  def &&[B](that: TangVec[B]) = TangVec((point, that.point), (vec, that.vec))
}

object TangVec {
  def liftLinear[A, B](func: A => B): TangVec[A] => TangVec[B] =
    (pv) => TangVec(func(pv.point), func(pv.vec))

  def liftBilinear[A, B, C](bilinear: (A, B) => C)(
      implicit vs: VectorSpace[C, Double]) =
    (pv: TangVec[(A, B)]) =>
      TangVec(
        bilinear(pv.point._1, pv.point._2),
        bilinear(pv.point._1, pv.vec._2) + bilinear(pv.vec._1, pv.point._2))

  def tangProd[A, B](av: TangVec[A], bw: TangVec[B]) =
    TangVec((av.point, bw.point), (av.vec, bw.vec))

  def tProd[A, B](pair: (TangVec[A], TangVec[B])) =
    TangVec((pair._1.point, pair._2.point), (pair._1.vec, pair._2.vec))

  implicit def vs[T](
      implicit bvs: VectorSpace[T, Double]): VectorSpace[TangVec[T], Double] =
    new VectorSpace[TangVec[T], Double] {
      def negate(x: TangVec[T]) =
        TangVec(-x.point, -x.vec)

      val zero = TangVec(bvs.zero, bvs.zero)

      def plus(x: TangVec[T], y: TangVec[T]) =
        TangVec(x.point + y.point, x.vec + y.vec)

      def timesl(r: Double, x: TangVec[T]) = TangVec(r *: x.point, r *: x.vec)

      implicit def scalar: Field[Double] = Field[Double]

    }

  implicit class OnPoints[A, B](func: TangVec[A] => TangVec[B])(
      implicit vsA: VectorSpace[A, Double],
      vsB: VectorSpace[B, Double]) {
    def apply(a: A): B = func(TangVec(a, vsA.zero)).point
  }
}

trait DiffbleFunction[A, B] extends (A => B) { self =>

  def derv(a: A, t: A): B

  def apply(a: A): B

  def total(a: A, t: A): (B, B) = (self(a), derv(a, t))

  def apply(av: TangVec[A]): TangVec[B] =
    TangVec(self(av.point), derv(av.point, av.vec))
}

object DiffbleFunction {
  case class Linear[A, B](func: A => B) extends DiffbleFunction[A, B] {
    def derv(a: A, t: A) = func(t)

    def apply(a: A) = func(a)
  }

  case class Quadratic[A, B](bilinear: (A, A) => B)(
      implicit vs: VectorSpace[B, Double]) {
    def apply(a: A) = bilinear(a, a)

    def derv(a: A, t: A) = bilinear(a, t) + bilinear(t, a)
  }

  import cats._

  // import cats.implicits._

  class LoopyFunc[A, B](recdef: (A, B) => B) extends (A => Eval[B]) { self =>

    def apply(a: A) = Eval.defer(self(a)).map(recdef(a, _))
  }

  class LoopyDiffFunc[A, B](recdef: DiffbleFunction[(A, B), B])
      extends LoopyFunc[A, B]((a, b) => recdef((a, b)))
      with DiffbleFunction[A, Eval[B]] { self =>
    def derv(a: A, t: A) = {
      val d: Eval[B] = Eval.defer(derv(a, t));
      for (x <- self(a); y <- d) yield recdef.derv((a, x), (t, y))
    }
  }
}

trait AdjDiffbleFunction[A, B] { self =>
  val func: A => B

  val adjDer: A => B => A

  def apply(a: A): B = func(a)

  /**
    * Composition f *: g is f(g(_))
    */
  def *:[C](that: => AdjDiffbleFunction[B, C]) = andthen(that)

  def andthen[C](that: => AdjDiffbleFunction[B, C]): AdjDiffbleFunction[A, C] =
    AdjDiffbleFunction.Composition(this, that)

  /**
    * Conjugate that by this.
    */
  def ^:(that: B => B) = (a: A) => adjDer(a)(that(func(a)))

  /**
    * post-compose by the gradient of this, for instance for a feedback.
    */
  def **:(that: A => B) = (a: A) => adjDer(a)(that(a))

  def oplus[C, D](that: AdjDiffbleFunction[C, D]) =
    AdjDiffbleFunction.Oplus(this, that)
}

object AdjDiffbleFunction {
  def apply[A, B](f: => A => B)(grd: => A => (B => A)) =
    new AdjDiffbleFunction[A, B] {
      lazy val func = (a: A) => f(a)

      lazy val adjDer = grd
    }

  case class Composition[A, B, C](f: AdjDiffbleFunction[A, B],
                                  g: AdjDiffbleFunction[B, C])
      extends AdjDiffbleFunction[A, C] {
    val func = (a: A) => g.func(f.func(a))

    val adjDer = (a: A) => (c: C) => f.adjDer(a)(g.adjDer(f.func(a))(c))
  }

  case class Oplus[A, B, C, D](first: AdjDiffbleFunction[A, B],
                               second: AdjDiffbleFunction[C, D])
      extends AdjDiffbleFunction[(A, C), (B, D)] {
    val func = (ac: (A, C)) => (first.func(ac._1), second.func(ac._2))

    val adjDer = (ac: (A, C)) =>
      (bd: (B, D)) => (first.adjDer(ac._1)(bd._1), second.adjDer(ac._2)(bd._2))
  }

  case class SelfAdj[A](func: A => A) extends AdjDiffbleFunction[A, A] {
    val adjDer = (x: A) => (v: A) => func(v)
  }

  //      def selfadj[A](f: => A => A) = SelfAdj(f)

  case class Id[A]() extends AdjDiffbleFunction[A, A] {
    val func   = (a: A) => a
    val adjDer = (a: A) => (b: A) => b
  }

  def id[A]: AdjDiffbleFunction[A, A] = Id[A]()

  case class Incl1[A, B: LinearStructure]()
      extends AdjDiffbleFunction[A, (A, B)] {
    val lsB = implicitly[LinearStructure[B]]

    val func   = (a: A) => (a, lsB.zero)
    val adjDer = (a: A) => (x: (A, B)) => x._1
  }

  case class Incl2[A: LinearStructure, B]()
      extends AdjDiffbleFunction[B, (A, B)] {
    val lsA = implicitly[LinearStructure[A]]

    val func   = (b: B) => (lsA.zero, b)
    val adjDer = (b: B) => (x: (A, B)) => x._2
  }

  case class Proj1[A, B: LinearStructure]()
      extends AdjDiffbleFunction[(A, B), A] {
    val lsB = implicitly[LinearStructure[B]]

    val func   = (x: (A, B)) => x._1
    val adjDer = (x: (A, B)) => (a: A) => (a, lsB.zero)
  }

  case class Proj2[A: LinearStructure, B]()
      extends AdjDiffbleFunction[(A, B), B] {
    val lsA = implicitly[LinearStructure[A]]

    val func   = (x: (A, B)) => x._2
    val adjDer = (x: (A, B)) => (b: B) => (lsA.zero, b)
  }

  //      def Proj2[A, B](implicit lsA: LinearStructure[A]) = apply((x: (A, B)) => x._2)((x) => (b) => (lsA.zero, b))

  def block[A: LinearStructure,
            B: LinearStructure,
            C: LinearStructure,
            D: LinearStructure](f: AdjDiffbleFunction[A, C],
                                g: AdjDiffbleFunction[B, D]) = {
    val add = vsum[AdjDiffbleFunction[(A, B), (C, D)]]

    val p1 = Proj1[A, B]()
    val p2 = Proj2[A, B]()

    val i1 = Incl1[C, D]()
    val i2 = Incl2[C, D]()

    add(p1 andthen f andthen i1, p2 andthen g andthen i2)
  }

  case class ScProd[V: LinearStructure: InnerProduct]()
      extends AdjDiffbleFunction[(Double, V), V] {
    val ls = implicitly[LinearStructure[V]]
    val ip = implicitly[InnerProduct[V]]

    val func = (av: (Double, V)) => ls.mult(av._1, av._2)

    val adjDer = (av: (Double, V)) =>
      (w: V) => (ip.dot(av._2, w), ls.mult(av._1, w))
  }

  /**
    * raise a function to 2^(n -1) wrt composition, so for n = 0 we get identity and n = 1 gives f.
    */
  def repsquare[A: LinearStructure](
      f: AdjDiffbleFunction[A, A]): Int => AdjDiffbleFunction[A, A] = {
    case 0 => id[A]
    case 1 => f
    case n if n < 0 =>
      vzero[AdjDiffbleFunction[A, A]]
    case n => {
      val rs = repsquare(f)
      rs(n - 1) andthen (rs(n - 1))
    }
  }

  /**
    * Iterate a differentiable function.
    */
  @tailrec
  def recIterateDiffble[X](
      fn: AdjDiffbleFunction[X, X],
      n: Int,
      accum: AdjDiffbleFunction[X, X] = id[X]): AdjDiffbleFunction[X, X] = {
    if (n < 1) accum
    else
      recIterateDiffble(fn, n - 1, accum andthen (fn: AdjDiffbleFunction[X, X]))
  }

  case class IteratedDiffble[X](fn: AdjDiffbleFunction[X, X], n: Int)
      extends AdjDiffbleFunction[X, X] {
    require(n > 1, s"should not use case class for $n iterations")
    lazy val g = consIterateDiffble(fn, n - 1)

    val func: X => X = (a: X) => g.func(fn.func(a))

    val adjDer: X => X => X = (a: X) =>
      (c: X) => fn.adjDer(a)(g.adjDer(fn.func(a))(c))
  }

  def consIterateDiffble[X](fn: AdjDiffbleFunction[X, X], n: Int) = n match {
    case 0          => Id[X]()
    case 1          => fn
    case k if k > 1 => IteratedDiffble(fn, k)
  }

  def iterateDiffble[X](fn: AdjDiffbleFunction[X, X], n: Int) =
    consIterateDiffble(fn, n)

  def iterate[A](f: AdjDiffbleFunction[A, A]): Int => AdjDiffbleFunction[A, A] =
    (n) => iterateDiffble(f, n)

  def mixinIsle[A: LinearStructure](
      f: AdjDiffbleFunction[A, A],
      isle: AdjDiffbleFunction[A, A] => AdjDiffbleFunction[A, A],
      normalize: AdjDiffbleFunction[A, A] = id[A]) = {
    val g = iterate(f)
    def h(m: Int): AdjDiffbleFunction[A, A] = m match {
      case 0 => id[A]
      case n if n < 0 =>
        vzero[AdjDiffbleFunction[A, A]]
      case n if n > 0 =>
        val dsum = vsum[AdjDiffbleFunction[A, A]]
        dsum(g(n), isle(h(n - 1))) andthen normalize
    }
    h _
  }

  /**
    * Big sum, with terms (via support) in general depending on the argument.
    */
  case class BigSum[A: LinearStructure, B: LinearStructure](
      fns: A => Iterable[AdjDiffbleFunction[A, B]])
      extends AdjDiffbleFunction[A, B] {
    val func = (a: A) => {
      val terms = for (f <- fns(a)) yield f.func(a)

      terms.foldRight(zeroB)(sumB)
    }

    private val zeroB = vzero[B]
    private val sumB  = vsum[B]

    private val zeroA = vzero[A]
    private val sumA  = vsum[A]

    val adjDer = (a: A) =>
      (b: B) => {
        val terms = for (f <- fns(a)) yield f.adjDer(a)(b)
        terms.foldRight(zeroA)(sumA)
    }
  }

  case class Diagonal[A: LinearStructure]()
      extends AdjDiffbleFunction[A, (A, A)] {
    val lsA = implicitly[LinearStructure[A]]

    val func = (a: A) => (a, a)

    val adjDer = (a: A) => (v: (A, A)) => lsA.sum(v._1, v._2)
  }

  case class DotProd[A: LinearStructure, B: LinearStructure](
      sc: Double,
      vect: AdjDiffbleFunction[A, B])
      extends AdjDiffbleFunction[A, B] {
    val prodB = vprod[B]

    val prodA = vprod[A]

    val func = (a: A) => prodB(sc, vect.func(a))

    val adjDer = (a: A) => (b: B) => prodA(sc, vect.adjDer(a)(b))
  }

  case class Sum[A: LinearStructure, B: LinearStructure](
      first: AdjDiffbleFunction[A, B],
      second: AdjDiffbleFunction[A, B])
      extends AdjDiffbleFunction[A, B] {
    val sumB = vsum[B]

    val sumA = vsum[A]

    val func = (a: A) => sumB(first.func(a), second.func(a))

    val adjDer = (a: A) =>
      (b: B) => sumA(first.adjDer(a)(b), second.adjDer(a)(b))
  }

  case class Zero[A: LinearStructure, B: LinearStructure]()
      extends AdjDiffbleFunction[A, B] {
    val zeroA = vzero[A]

    val zeroB = vzero[B]

    val func = (a: A) => zeroB

    val adjDer = (a: A) => (b: B) => zeroA
  }

  implicit def diffFnLS[A: LinearStructure, B: LinearStructure]
    : LinearStructure[AdjDiffbleFunction[A, B]] = {

    val sum = (f: AdjDiffbleFunction[A, B], g: AdjDiffbleFunction[A, B]) =>
      Sum[A, B](f, g)

    val zero = Zero[A, B]()

    val mult = (c: Double, f: AdjDiffbleFunction[A, B]) => DotProd(c, f)

    LinearStructure(zero, sum, mult)
  }

  implicit def vecSpaceDiffFn[A, B](implicit vsA: VectorSpace[A, Double],
                                    vsB: VectorSpace[B, Double])
    : _root_.spire.algebra.VectorSpace[AdjDiffbleFunction[A, B],
                                       _root_.scala.Double] {} =
    new VectorSpace[AdjDiffbleFunction[A, B], Double] {

      // Members declared in algebra.ring.AdditiveGroup
      def negate(x: AdjDiffbleFunction[A, B]): AdjDiffbleFunction[A, B] =
        AdjDiffbleFunction((a: A) => -x(a))((a: A) => (b: B) => -x.adjDer(a)(b))

      // Members declared in algebra.ring.AdditiveMonoid
      def zero: AdjDiffbleFunction[A, B] =
        AdjDiffbleFunction((a: A) => vsB.zero)((a: A) => (b: B) => vsA.zero)

      // Members declared in algebra.ring.AdditiveSemigroup
      def plus(x: AdjDiffbleFunction[A, B],
               y: AdjDiffbleFunction[A, B]): AdjDiffbleFunction[A, B] =
        AdjDiffbleFunction((a: A) => x(a) + y(a))((a: A) =>
          (b: B) => x.adjDer(a)(b) + y.adjDer(a)(b))

      // Members declared in spire.algebra.Module
      def timesl(r: Double,
                 v: AdjDiffbleFunction[A, B]): AdjDiffbleFunction[A, B] =
        AdjDiffbleFunction((a: A) => vsB.timesl(r, v(a)))((a: A) =>
          (b: B) => vsA.timesl(r, v.adjDer(a)(b)))

      // Members declared in spire.algebra.VectorSpace
      implicit def scalar: Field[Double] = Field[Double]
    }

  trait FormalExtension[A] extends AdjDiffbleFunction[A, A] {
    val func: A => A

    val adjDer = (a: A) => (b: A) => b
  }
}
