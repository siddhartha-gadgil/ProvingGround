package provingground

import Collections.Weighted

sealed trait PrunedDistribution[A] {
  import PrunedDistribution.{pruneFD, sum}
  
  val cutoff : Double
  
  val getFD : Option[FiniteDistribution[A]]
  
  def asFD = getFD getOrElse (FiniteDistribution[A](Vector()))
  
  def apply(a: A) = asFD(a)
  
  def supp = asFD.supp
  
  def <*>(scale: Double) = PrunedDistribution.Scaled(this, scale)
  
  def <+>(that: => PrunedDistribution[A]) = PrunedDistribution.Sum(this, that) 
  
  def map[B](f: A => B) = getFD flatMap ((fd) => pruneFD(fd.map(f), cutoff))
  
  def flatMap[B](f: A => PrunedDistribution[B]) = getFD map ((fd) =>{
    val dists = fd.supp map (f)
    
    val empty: PrunedDistribution[B] = PrunedDistribution.Empty[B](cutoff)
    
    (dists :\ empty)(sum[B](_, _))
  }) getOrElse(PrunedDistribution.Empty[B](cutoff))
}

object PrunedDistribution{
  case class Empty[A](cutoff : Double) extends PrunedDistribution[A]{
    val getFD = None
  }

    
  def pruneFD[A](fd:  => FiniteDistribution[A], cutoff: Double) = 
    if (cutoff > 1.0) None 
      else {
        val dist = fd.flatten.pmf filter (_.weight > cutoff)
        if (dist.isEmpty) None else Some(FiniteDistribution(dist))
      }
  
  case class Opt[A](opt: Option[A], cutoff: Double) extends PrunedDistribution[A]{
    val getFD = opt flatMap ((value) => pruneFD(FiniteDistribution(Vector(Weighted(value, 1.0))), cutoff))
  }

  
  case class FD[A](fd: FiniteDistribution[A], cutoff: Double){
    val getFD = pruneFD(fd, cutoff)
  }
  
  case class Scaled[A](base: PrunedDistribution[A], scale: Double) extends PrunedDistribution[A]{
    val cutoff = base.cutoff / scale
    
    val getFD = base.getFD flatMap ((fd) => pruneFD(fd * scale, cutoff))
  }
  
  case class Sum[A](first: PrunedDistribution[A], second: PrunedDistribution[A]) extends PrunedDistribution[A]{
    val cutoff = math.max(first.cutoff, second.cutoff)
    
    val getFD = 
      for (fd1 <- first.getFD; fd2 <- second.getFD; sum <- pruneFD(fd1 ++ fd2, cutoff)) yield sum
  }
  
  def sum[A](first: PrunedDistribution[A], second: PrunedDistribution[A]) = Sum[A](first, second)
}