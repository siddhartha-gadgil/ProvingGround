package provingground
import provingground.{
  FiniteDistribution => FD, 
  TruncatedDistribution => TD, 
  ProbabilityDistribution => PD,
  TermLang => TL}

import HoTT._

object Deducer {
  def isFunc : Term => Boolean = {
    case _ :  FuncLike[_, _] => true
    case _ => false
  }

  
  def appln(rec: => (PD[Term] => PD[Term]))(p: PD[Term]) = 
    rec(p) flatMap ((f) =>
      if (isFunc(f)) rec(p) map (TL.appln(f, _))
      else FD.unif(None : Option[Term])
      )

  def lambda(varweight: Double)(rec: => (PD[Term] => PD[Term]))(p: PD[Term]) : PD[Option[Term]] = 
    rec(p) flatMap ({
      case tp: Typ[u] =>
        val x = tp.Var
        val newp = p <+> (FD.unif(x), varweight)
        (rec (newp)) map ((y: Term) => TL.lambda(x, y))
      case _ => FD.unif(None)
    }
    )
    
  def pi(varweight: Double)(rec: => (PD[Term] => PD[Term]))(p: PD[Term]) : PD[Option[Term]] = 
    rec(p) flatMap ({
      case tp: Typ[u] =>
        val x = tp.Var
        val newp = p <+> (FD.unif(x), varweight)
        (rec (newp)) map ((y: Term) => TL.pi(x, y))
      case _ => FD.unif(None)
    }
    )   
  
    
  def applInvImage(supp: Vector[Term]) =
    {
      val pairs = for (x <- supp; y<- supp if isFunc(x)) yield (x, y)
      val optMap = pairs groupBy {case (f, x) => TL.appln(f, x)}
      for ((yo, fx) <- optMap; y <- yo) yield (y, fx)
    }
}