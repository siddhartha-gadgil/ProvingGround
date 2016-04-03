package provingground


sealed trait TruncatedDistribution[A] {
//  import TruncatedDistribution.{pruneFD, sum}

  def getFD(cutoff: Double) : Option[FiniteDistribution[A]]


  def <*>(scale: Double) = TruncatedDistribution.Scaled(this, scale)

  def <+>(that: => TruncatedDistribution[A]) = TruncatedDistribution.sum(this, that)

  def <++>(that: => List[Weighted[TruncatedDistribution[A]]]): TruncatedDistribution[A] =
    that match {
      case List() => this
      case Weighted(a, p) :: ys =>
        <+> (a <*> p) <++> ys
    }

  def map[B](f: A => B) =
    TruncatedDistribution.Map(this, f)

  def flatMap[B](f: A => TruncatedDistribution[B]) =
    TruncatedDistribution.FlatMap(this, f)
}

object TruncatedDistribution{
  case class Empty[A]() extends TruncatedDistribution[A]{
    def getFD(cutoff: Double) = None
  }


  def pruneFD[A](fd:  => FiniteDistribution[A], cutoff: Double) =
    if (cutoff > 1.0) None
      else {
        val dist = fd.flatten.pmf filter (_.weight > cutoff)
        if (dist.isEmpty) None else Some(FiniteDistribution(dist))
      }

  case class Atom[A](opt: Option[A]) extends TruncatedDistribution[A]{
    def getFD(cutoff: Double)
    = opt flatMap ((value) => pruneFD(FiniteDistribution(Vector(Weighted(value, 1.0))), cutoff))
  }


  case class FD[A](fd: FiniteDistribution[A]){
    def getFD(cutoff: Double) = pruneFD(fd, cutoff)
  }

  case class Scaled[A](base: TruncatedDistribution[A], scale: Double) extends TruncatedDistribution[A]{
    def getFD(cutoff: Double) =
      base.getFD(cutoff/scale) map (_ * scale)
  }

  class Sum[A](
      first: => TruncatedDistribution[A],
      second: => TruncatedDistribution[A]) extends TruncatedDistribution[A]{

    def getFD(cutoff: Double) = if (cutoff > 1.0) None else {
      val fd1 = first.getFD(cutoff).getOrElse(FiniteDistribution(Vector()))
      val fd2 = second.getFD(cutoff).getOrElse(FiniteDistribution(Vector()))
      pruneFD(fd1 ++ fd2, cutoff)
    }
  }

  case class Map[A, B](
      base: TruncatedDistribution[A], f: A =>B) extends TruncatedDistribution[B]{
    def getFD(cutoff: Double) = base.getFD(cutoff).map((d) => d map f)
  }

  def lift[A, B](f: A =>B) =
    (base: TruncatedDistribution[A]) => (Map(base, f) : TruncatedDistribution[B])

  def liftOp[A, B, C](op: (A, B) => C) = {
    def lop(xd : TruncatedDistribution[A], yd: TruncatedDistribution[B]) =
        (for (x <- xd; y <- yd) yield op(x, y)) : TruncatedDistribution[C]
    lop _
    }


  case class FlatMap[A, B](
      base: TruncatedDistribution[A], f: A => TruncatedDistribution[B]) extends TruncatedDistribution[B]{
    def getFD(cutoff: Double) = base.getFD(cutoff) flatMap ((fd) =>{
    val dists = fd.supp map (f)

    val empty: TruncatedDistribution[B] = TruncatedDistribution.Empty[B]

    val trunc = (dists :\ empty)(sum[B](_, _))
    trunc.getFD(cutoff)
  })
  }

  def sum[A](first: => TruncatedDistribution[A], second: => TruncatedDistribution[A]) =
    new Sum[A](first, second)
}
