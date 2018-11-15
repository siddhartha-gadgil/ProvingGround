package provingground.learning

import provingground._

import scala.language.implicitConversions

//import provingground.{FiniteDistribution => FD}

// import FiniteDistribution.FiniteDistVec
// import LinearStructure._

import cats._

case class TruncDistVal[A](getFD: Double => Option[FiniteDistribution[A]])
    extends AnyVal {

  def <*>(scale: Double) = TruncDistVal.scaled(this, scale)

  def <+>(that: => TruncDistVal[A]) = TruncDistVal.sum(this, that)

  def filter(p: A => Boolean) = {
    def newFD(c: Double) = getFD(c: Double) map ((fd) => fd filter (p))
    TruncDistVal(newFD)
  }

  def flatMap[B](f: A => TruncDistVal[B]): TruncDistVal[B] = {
    def flatGetFD(cutoff: Double) =
      getFD(cutoff) flatMap { (fd) =>
        val dists = fd.supp map (f)

        val empty: TruncDistVal[B] = TruncDistVal.Empty[B]

        val trunc = (dists :\ empty)(TruncDistVal.sum[B](_, _))
        trunc.getFD(cutoff)
      }
    TruncDistVal(flatGetFD)
  }
}

object TruncDistVal {
  def scaled[A](td: TruncDistVal[A], scale: Double) =
    TruncDistVal((c: Double) => td.getFD(c / scale) map ((fd) => fd * scale))

  def sum[A](first: => TruncDistVal[A], second: => TruncDistVal[A]) = {
    def getFD =
      (c: Double) =>
        for (f1 <- first.getFD(c); f2 <- second.getFD(c)) yield (f1 ++ f2)
    TruncDistVal(getFD)
  }

  def FD[A](fd: FiniteDistribution[A]) = {
    def getFD(cutoff: Double) =
      if (cutoff > 1.0) None
      else TruncatedDistribution.pruneFD(fd, cutoff)
    TruncDistVal(getFD)
  }

  def Empty[A] = TruncDistVal[A]((c: Double) => None)

  def atom[A](a: A) = {
    def getFD(cutoff: Double) =
      if (cutoff > 1.0) None
      else Some(FiniteDistribution(Vector(Weighted(a, 1))))
    TruncDistVal(getFD)
  }

  def bigSum[A](tds: => Vector[TruncDistVal[A]]) = {
    def getFD(cutoff: Double) = {
      val fds = (tds map (_.getFD(cutoff))).flatten
      if (fds.isEmpty) None
      else
        Some(fds.reduce(_ ++ _))
    }
    TruncDistVal(getFD)
  }
}

sealed trait TruncatedDistribution[A] {
  //  import TruncatedDistribution.{pruneFD, sum}

  def getFD(cutoff: Double): Option[FiniteDistribution[A]]

  def <*>(scale: Double): TruncatedDistribution[A] =
    if (scale != 0.0) new TruncatedDistribution.Scaled(this, scale)
    else TruncatedDistribution.Empty[A]

  def <*>:(scale: Double): TruncatedDistribution[A] =
    if (scale != 0.0) new TruncatedDistribution.Scaled(this, scale)
    else TruncatedDistribution.Empty[A]

  def <+>(that: => TruncatedDistribution[A]) =
    TruncatedDistribution.sum(this, that)

  def |+|(that: => FiniteDistribution[A]) =
    TruncatedDistribution.sum(this, TruncatedDistribution.FD(that))

  def <++>(that: => List[Weighted[TruncatedDistribution[A]]])
    : TruncatedDistribution[A] =
    that match {
      case List() => this
      case Weighted(a, p) :: ys =>
        <+>(a <*> p) <++> ys
    }

  def map[B](f: A => B): TruncatedDistribution[B] =
    new TruncatedDistribution.Map(this, f)

  def flatMap[B](
      f: => (A => TruncatedDistribution[B])): TruncatedDistribution[B] =
    new TruncatedDistribution.FlatMap(this, f)

  def mapFD[B](f: FiniteDistribution[A] => FiniteDistribution[B])
    : TruncatedDistribution[B] =
    new TruncatedDistribution.MapFD(this, f)

  def mapOpt[B](f: A => Option[B]): TruncatedDistribution[B] =
    new TruncatedDistribution.MapOpt(this, f)

  def filter(f: A => Boolean) = new TruncatedDistribution.Filter(this, f)

  def getOpt: Option[TruncatedDistribution[A]] = Some(this)
}

object TruncatedDistribution extends Functor[TruncatedDistribution] {
  case class Empty[A]() extends TruncatedDistribution[A] {
    def getFD(cutoff: Double) = None

    override def getOpt = None

    override def map[B](f: A => B): TruncatedDistribution[B] =
      TruncatedDistribution.Empty[B]

    override def flatMap[B](
        f: => (A => TruncatedDistribution[B])): TruncatedDistribution[B] =
      TruncatedDistribution.Empty[B]

    override def mapFD[B](f: FiniteDistribution[A] => FiniteDistribution[B]) =
      TruncatedDistribution.Empty[B]

    override def mapOpt[B](f: A => Option[B]): TruncatedDistribution[B] =
      TruncatedDistribution.Empty[B]

    override def <*>(scale: Double) = this
  }

  def pruneFD[A](fd: => FiniteDistribution[A], cutoff: Double) =
    if (cutoff > 1.0) None
    else {
      val dist = fd.flatten.pmf filter ((x) => math.abs(x.weight) > cutoff)
      if (dist.isEmpty) None else Some(FiniteDistribution(dist))
    }

  def prunePosFD[A](fd: => FiniteDistribution[A], cutoff: Double) =
    if (cutoff > 1.0) None
    else {
      val dist = fd.flatten.pmf filter ((x) => x.weight > cutoff)
      if (dist.isEmpty) None else Some(FiniteDistribution(dist))
    }

  case class OptAtom[A](opt: Option[A]) extends TruncatedDistribution[A] {
    def getFD(cutoff: Double) =
      opt flatMap
        ((value) =>
          pruneFD(FiniteDistribution(Vector(Weighted(value, 1.0))), cutoff))
  }

  def atom[A](a: A) = OptAtom(Some(a))

  def apply[A](fd: FiniteDistribution[A]) = FD(fd)

  def apply[A](ws: Seq[Weighted[A]]) = FD(FiniteDistribution(ws.toVector))

  implicit def td[A](fd: FiniteDistribution[A])
    : TruncatedDistribution.FD[A] = FD(fd)

  case class FD[A](fd: FiniteDistribution[A]) extends TruncatedDistribution[A] {
    def getFD(cutoff: Double) = pruneFD(fd, cutoff)
  }

  case class PosFD[A](fd: FiniteDistribution[A])
      extends TruncatedDistribution[A] {
    def getFD(cutoff: Double) = prunePosFD(fd, cutoff)
  }

  class Scaled[A](base: => TruncatedDistribution[A], scale: Double)
      extends TruncatedDistribution[A] {
    def getFD(cutoff: Double) =
      base.getFD(cutoff / scale) map (_ * scale)
  }

  class Sum[A](first: => TruncatedDistribution[A],
               second: => TruncatedDistribution[A])
      extends TruncatedDistribution[A] {

    def getFD(cutoff: Double) =
      if (cutoff > 1.0) None
      else {
        val fd1 = first.getFD(cutoff).getOrElse(FiniteDistribution(Vector()))
        val fd2 = second.getFD(cutoff).getOrElse(FiniteDistribution(Vector()))
        pruneFD(fd1 ++ fd2, cutoff)
      }
  }

  class Map[A, B](base: => TruncatedDistribution[A], f: A => B)
      extends TruncatedDistribution[B] {
    def getFD(cutoff: Double) = base.getFD(cutoff).map((d) => d map f)
  }

  class MapOpt[A, B](base: => TruncatedDistribution[A], f: A => Option[B])
      extends TruncatedDistribution[B] {
    def getFD(cutoff: Double) = base.getFD(cutoff).map((d) => d mapOpt f)
  }

  class Filter[A](base: => TruncatedDistribution[A], f: A => Boolean)
      extends TruncatedDistribution[A] {
    def getFD(cutoff: Double) = base.getFD(cutoff) map (_.filter(f))
  }

  class FlatMap[A, B](base: => TruncatedDistribution[A],
                      f: => (A => TruncatedDistribution[B]))
      extends TruncatedDistribution[B] {
    def getFD(cutoff: Double) =
      base.getFD(cutoff) flatMap
        ((fd) => {
          val dists = fd.supp map (f)

          val empty: TruncatedDistribution[B] =
            TruncatedDistribution.Empty[B]

          val trunc = (dists :\ empty)(sum[B](_, _))
          trunc.getFD(cutoff)
        })
  }

  class MapFD[A, B](base: => TruncatedDistribution[A],
                    f: FiniteDistribution[A] => FiniteDistribution[B])
      extends TruncatedDistribution[B] {
    def getFD(cutoff: Double) = base.getFD(cutoff) map ((fd) => f(fd))
  }

  class FlattenOpt[A](base: => TruncatedDistribution[Option[A]])
      extends TruncatedDistribution[A] {
    def getFD(cutoff: Double) = {
      base
        .getFD(cutoff)
        .map(
          (fd) =>
            fd.filter((oa: Option[A]) => !(oa.isEmpty))
              .map((oa: Option[A]) => oa.get))
    }
  }

  class Flatten[A](base: => TruncatedDistribution[TruncatedDistribution[A]])
      extends TruncatedDistribution[A] {
    def getFD(cutoff: Double) =
      base
        .getFD(cutoff)
        .flatMap((fdtd) => {
          val fdfd = fdtd mapOpt ((td) => td.getFD(cutoff))
          val pmf = for (Weighted(fd, p) <- fdfd.pmf;
                         Weighted(a, q) <- fd.pmf) yield Weighted(a, p * q)
          val fd = FiniteDistribution(pmf)
          pruneFD(fd, cutoff)
        })
  }

  class BigSum[A](tds: => Vector[TruncatedDistribution[A]])
      extends TruncatedDistribution[A] {
    def getFD(cutoff: Double) =
      TruncatedDistribution.bigSum(tds).getFD(cutoff)
  }

  def bigSum[A](tds: => Vector[TruncatedDistribution[A]]) =
    new BigSum(tds)

  case class Coeffs[A](support: Vector[A],
                       coeffs: Double => A => Option[Double])
      extends TruncatedDistribution[A] {
    def getFD(cutoff: Double) = {
      val pmf =
        (support map ((a) => coeffs(cutoff)(a) map (Weighted(a, _)))).flatten
      if (pmf.isEmpty) None else Some(FiniteDistribution(pmf))
    }
  }

  case class FromFDs[A](fds: Double => Option[FiniteDistribution[A]])
      extends TruncatedDistribution[A] {
    def getFD(cutoff: Double) = fds(cutoff)
  }

  def flatten[A](base: TruncatedDistribution[TruncatedDistribution[A]]) =
    new Flatten(base)

  def optF[A](
      fo: TruncatedDistribution[Option[A]]): Option[TruncatedDistribution[A]] =
    fo.getOpt map (new FlattenOpt(_))

  def map[A, B](base: TruncatedDistribution[A])(f: A => B) = (base map (f))

  def mapOpt[A, B](base: TruncatedDistribution[A])(f: A => Option[B]) =
    (base mapOpt (f))

  def mapOp[A, B, C](xd: TruncatedDistribution[A],
                     yd: TruncatedDistribution[B])(op: (A, B) => C) = {
    (for (x <- xd; y <- yd) yield op(x, y))
  }

  def liftOpOpt[A, B, C](op: (A, B) => Option[C]) = {
    def lop(xd: TruncatedDistribution[A], yd: TruncatedDistribution[B]) =
      (for (x <- xd; y <- yd) yield op(x, y))
    lop _
  }

  def liftOpFlatten[A, B, C](op: (A, B) => Option[C]) = {
    def lop(xd: TruncatedDistribution[A], yd: TruncatedDistribution[B]) = {
      val tdOpt = (for (x <- xd; y <- yd)
        yield op(x, y)): TruncatedDistribution[Option[C]]
      new FlattenOpt(tdOpt): TruncatedDistribution[C]
    }
    lop _
  }

  def sum[A](first: => TruncatedDistribution[A],
             second: => TruncatedDistribution[A]) =
    new Sum[A](first, second)
}
