package provingground

import scala.language.implicitConversions

import provingground.{FiniteDistribution => FD}

import FiniteDistribution.FiniteDistVec
import LinearStructure._

sealed trait TruncatedDistribution[A] {
//  import TruncatedDistribution.{pruneFD, sum}

  def getFD(cutoff: Double) : Option[FiniteDistribution[A]]


  def <*>(scale: Double) : TruncatedDistribution[A] = if (scale != 0.0) TruncatedDistribution.Scaled(this, scale) else TruncatedDistribution.Empty[A]

  def <*>:(scale: Double) : TruncatedDistribution[A] = if (scale != 0.0) TruncatedDistribution.Scaled(this, scale) else TruncatedDistribution.Empty[A]
  
  def <+>(that: => TruncatedDistribution[A]) = TruncatedDistribution.sum(this, that)

  def |+|(that : => FiniteDistribution[A]) = 
    TruncatedDistribution.sum(this, TruncatedDistribution.FD(that))
  
  def <++>(that: => List[Weighted[TruncatedDistribution[A]]]): TruncatedDistribution[A] =
    that match {
      case List() => this
      case Weighted(a, p) :: ys =>
        <+> (a <*> p) <++> ys
    }

  def map[B](f: A => B) : TruncatedDistribution[B] =
    TruncatedDistribution.Map(this, f)

  def flatMap[B](f: A => TruncatedDistribution[B]) : TruncatedDistribution[B] =
    TruncatedDistribution.FlatMap(this, f)
    
  def mapFD[B](f: FiniteDistribution[A] => FiniteDistribution[B]) : TruncatedDistribution[B] =
    TruncatedDistribution.MapFD(this, f)
    
  def mapOpt[B](f: A => Option[B]) : TruncatedDistribution[B] =
    TruncatedDistribution.MapOpt(this, f)

  def filter(f: A => Boolean) = TruncatedDistribution.Filter(this, f)
    
  def getOpt: Option[TruncatedDistribution[A]] = Some(this)
}

object TruncatedDistribution extends OptNat[TruncatedDistribution] with Functor[TruncatedDistribution]{
  case class Empty[A]() extends TruncatedDistribution[A]{
    def getFD(cutoff: Double) = None
    
    override def getOpt = None
    
    override def map[B](f: A => B) : TruncatedDistribution[B] =
      TruncatedDistribution.Empty[B]

    override def flatMap[B](f: A => TruncatedDistribution[B]) : TruncatedDistribution[B] =
      TruncatedDistribution.Empty[B]
    
    override def mapFD[B](f: FiniteDistribution[A] => FiniteDistribution[B]) =
      TruncatedDistribution.Empty[B]
    
    override def mapOpt[B](f: A => Option[B]) : TruncatedDistribution[B] =
    TruncatedDistribution.Empty[B]

    override def <*>(scale: Double) = this
  }

  def pruneFD[A](fd:  => FiniteDistribution[A], cutoff: Double) =
    if (cutoff > 1.0) None
      else {
        val dist = fd.flatten.pmf filter ((x) => math.abs(x.weight) > cutoff)
        if (dist.isEmpty) None else Some(FiniteDistribution(dist))
      }

  case class OptAtom[A](opt: Option[A]) extends TruncatedDistribution[A]{
    def getFD(cutoff: Double)
    = opt flatMap ((value) => pruneFD(FiniteDistribution(Vector(Weighted(value, 1.0))), cutoff))
  }

  def atom[A](a: A) = OptAtom(Some(a))

  def apply[A](fd: FiniteDistribution[A]) = FD(fd)
  
  def apply[A](ws : Seq[Weighted[A]]) = FD(FiniteDistribution(ws.toVector))
  
  implicit def td[A](fd: FiniteDistribution[A]) = FD(fd)
  
  case class FD[A](fd: FiniteDistribution[A]) extends TruncatedDistribution[A]{
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
  
  case class MapOpt[A, B](
      base: TruncatedDistribution[A], f: A => Option[B]) extends TruncatedDistribution[B]{
    def getFD(cutoff: Double) = base.getFD(cutoff).map((d) => d mapOpt f)
  }  

  case class Filter[A](
      base: TruncatedDistribution[A], f: A => Boolean) extends TruncatedDistribution[A]{
    def getFD(cutoff: Double) = base.getFD(cutoff) map (_.filter(f))
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

  case class MapFD[A, B](
      base: TruncatedDistribution[A], 
      f: FiniteDistribution[A] => FiniteDistribution[B]) extends TruncatedDistribution[B]{
    def getFD(cutoff: Double) = base.getFD(cutoff) map ((fd) => f(fd))    
  }
  
  case class FlattenOpt[A](
      base: TruncatedDistribution[Option[A]]) extends TruncatedDistribution[A]{
    def getFD(cutoff: Double) = {
      base.getFD(cutoff). 
        map ((fd) => 
        fd.
        filter((oa : Option[A]) => !(oa.isEmpty)).
        map((oa : Option[A]) => oa.get)
        )
    }
  }
  
  case class Flatten[A](
      base: TruncatedDistribution[TruncatedDistribution[A]]) extends TruncatedDistribution[A]{
    def getFD(cutoff: Double) =  
        base.getFD(cutoff).
        flatMap(
            (fdtd) => 
              {
                val fdfd = fdtd mapOpt ((td) => td.getFD(cutoff))
                val pmf = 
                  for (
                      Weighted(fd, p) <- fdfd.pmf;
                      Weighted(a, q) <- fd.pmf) yield Weighted(a, p * q)
                val fd = FiniteDistribution(pmf)
                pruneFD(fd, cutoff)
                }
              )
    
  }
  
  case class BigSum[A](tds: Vector[TruncatedDistribution[A]]) extends TruncatedDistribution[A]{
    def getFD(cutoff: Double) = {
      val fdsOpt = (tds map (_.getFD(cutoff)))
      if (fdsOpt.isEmpty) None else Some(vBigSum(fdsOpt.flatten))
    }
  }
  
  def flatten[A](
      base: TruncatedDistribution[TruncatedDistribution[A]]) = Flatten(base)
  
  def optF[A](fo : TruncatedDistribution[Option[A]]): Option[TruncatedDistribution[A]] = 
    fo.getOpt map (FlattenOpt(_)) 

  
  def map[A, B](base: TruncatedDistribution[A])(f: A =>B) = (base map (f))

  def mapOpt[A, B](base: TruncatedDistribution[A])(f: A => Option[B]) =  
     (base mapOpt (f))
    
  def mapOp[A, B, C](xd : TruncatedDistribution[A], yd: TruncatedDistribution[B])(op: (A, B) => C) = {        
        (for (x <- xd; y <- yd) yield op(x, y)) 
    }
  
  def liftOpOpt[A, B, C](op: (A, B) => Option[C]) = {        
    def lop(xd : TruncatedDistribution[A], yd: TruncatedDistribution[B]) =
        (for (x <- xd; y <- yd) yield op(x, y)) 
    lop _
    }  
    
    
  def liftOpFlatten[A, B, C](op: (A, B) => Option[C]) = {
    def lop(xd : TruncatedDistribution[A], yd: TruncatedDistribution[B]) = {
      val tdOpt =  (for (x <- xd; y <- yd) yield op(x, y)) : TruncatedDistribution[Option[C]]
      FlattenOpt(tdOpt): TruncatedDistribution[C]
    }
    lop _
    }
  
  
  def sum[A](first: => TruncatedDistribution[A], second: => TruncatedDistribution[A]) =
    new Sum[A](first, second)
}
