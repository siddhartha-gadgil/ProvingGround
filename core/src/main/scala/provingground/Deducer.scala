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
  
    
  def applnInvImage(supp: Vector[Term]) =
    {
      val pairs = (supp collect{ 
        case fn: FuncLike[_, _] =>
          supp filter (_.typ == fn.dom) map ((fn : Term, _)) 
      }).flatten
      val optMap = pairs groupBy {case (f, x) => TL.appln(f, x)}
      for ((yo, fx) <- optMap; y <- yo) yield (y, fx)
    }
  
  def invImage(supp: Vector[Term])(fn: Term) : Map[Term, Vector[Term]] = fn match {
    case f: FuncLike[_, _] =>
      val optMap = supp filter (_.typ == f.dom) groupBy (TL.appln(f, _))
      for ((yo, fx) <- optMap; y <- yo) yield (y, fx)
    case _ => Map.empty
  }
  
  def invDstbn(supp: Vector[Term])(fn: Term) = 
    (y: Term) => ((invImage(supp)(fn)) mapValues (FD.uniform(_))).getOrElse(y, FD.empty[Term])
    
  def invTD(supp: Vector[Term])(fn: Term) : Term => TD[Term] = 
    (y: Term) => ((invImage(supp)(fn)) mapValues ((xs) => TD(FD.uniform(xs)))).getOrElse(y, TD.Empty[Term])
    
  def applnAdjoint1(fd: FD[Term])(w : => TD[Term]) : TD[Term] = {
      val supp = fd.supp
      val tds = 
        for (Weighted(f, p) <- fd.pmf if isFunc(f)) yield
          (w flatMap ((y) => invTD(supp)(f)(y))) <*> p 
      TD.BigSum(tds)
    }

  val sym = new AnySym
    
  def lambdaFDMap(fd: FD[Term]) = {    
    val pmf = (fd.pmf collect {
      case Weighted(l: LambdaLike[u, v], p) => 
        (l.variable.typ, l.value.replace(l.variable, l.variable.typ.symbObj(sym)), p)}).
        groupBy(_._1) mapValues ((v) => v map ((z) => Weighted(z._2 : Term, z._3)))
    pmf mapValues (FiniteDistribution(_).flatten)
  }
  
  def lambdaTD(td: TD[Term]) =
    (typ: Typ[Term]) => 
      td mapFD ((fd) => lambdaFDMap(fd).get(typ).getOrElse(FD.empty[Term]))
  
  def lambdaAdjointDist(
      recAdj : => (FD[Term] => TD[Term] => TD[Term]))(
          fd: FD[Term])(
              typ: Typ[Term])(w : => TD[Term]) = {
        val neww = lambdaTD(w)(typ)
        val x = typ.symbObj(sym)
    lambdaFDMap(fd).get(typ) map {
      (newp) =>
        recAdj(newp)(neww)
    } getOrElse(TD.Empty[Term]) <*> (fd(typ)) map ((t) => HoTT.lambda(x)(t))
  }
      
  def lambdaAdjointCoeff(
      recAdj : => (FD[Term] => TD[Term] => TD[Term]))(
          fd: FD[Term])(
              typ: Typ[Term])(w : => TD[Term])(cutoff: Double) = {
        val neww = lambdaTD(w)(typ)
        val x = typ.symbObj(sym)
    lambdaFDMap(fd).get(typ) map {
      (newp) =>
        val wfd = neww.getFD(cutoff).getOrElse(FD.empty[Term])
        (wfd map ((t) => newp(t))).expectation
    } 
  }    
}