package provingground
import provingground.{
  FiniteDistribution => FD, 
  TruncatedDistribution => TD, 
  ProbabilityDistribution => PD,
  TermLang => TL}

import HoTT._

/**
 * Generating terms from given ones using the main HoTT operations, and the adjoint of this generation.
 * This is viewed as deduction.
 * Generation is a map on probability distributions, 
 * but the adjoint regards truncated distributions as the tangent space, and restricts domain to finite distributions.
 * 
 */
object Deducer {
  def isFunc : Term => Boolean = {
    case _ :  FuncLike[_, _] => true
    case _ => false
  }


  /**
   * generating optionally using function application, with function and argument generated recursively;
   * to be mixed in using `<+?>`
   */
  def appln(rec: => (PD[Term] => PD[Term]))(p: PD[Term]) = 
    rec(p) flatMap ((f) =>
      if (isFunc(f)) 
        rec(p) map (TL.appln(f, _))
      else 
        FD.unif(None : Option[Term])
      )

   /**
   * generating optionally as lambdas, with function and argument generated recursively;
   * to be mixed in using `<+?>`
   */
  def lambda(varweight: Double)(rec: => (PD[Term] => PD[Term]))(p: PD[Term]) : PD[Option[Term]] = 
    rec(p) flatMap ({
      case tp: Typ[u] =>
        val x = tp.Var
        val newp = p <+> (FD.unif(x), varweight)
        (rec (newp)) map ((y: Term) => TL.lambda(x, y))
      case _ => FD.unif(None)
    }
    )
    
  /**
   * generating optionally as pi's, with function and argument generated recursively;
   * to be mixed in using `<+?>`
   */
  def pi(varweight: Double)(rec: => (PD[Term] => PD[Term]))(p: PD[Term]) : PD[Option[Term]] = 
    rec(p) flatMap ({
      case tp: Typ[u] =>
        val x = tp.Var
        val newp = p <+> (FD.unif(x), varweight)
        (rec (newp)) map ((y: Term) => TL.pi(x, y))
      case _ => FD.unif(None)
    }
    )   
  
  /**
   * returns map of inverse image under function application, 
   * i.e., for `y` in range, returns vector of pairs `(f, x)` with `f(x) = y`
   */  
  def applnInvImage(supp: Vector[Term]) =
    {
      val pairs = (supp collect{ 
        case fn: FuncLike[_, _] =>
          supp filter (_.typ == fn.dom) map ((fn : Term, _)) 
      }).flatten
      val optMap = pairs groupBy {case (f, x) => TL.appln(f, x)}
      for ((yo, fx) <- optMap; y <- yo) yield (y, fx)
    }
  
  /**
   * returns map of inverse image for a fixed function `f`, 
   * i.e., for `y` in range, returns `x` with `f(x) = y`
   */
  def funcInvImage(supp: Vector[Term])(fn: Term) : Map[Term, Vector[Term]] = fn match {
    case f: FuncLike[_, _] =>
      val optMap = supp filter (_.typ == f.dom) groupBy (TL.appln(f, _))
      for ((yo, fx) <- optMap; y <- yo) yield (y, fx)
    case _ => Map.empty
  }
  
  /**
   * inverse image of a function as a finite distribution;
   * an atom of weight 1 at each inverse image point.
   */
  def invDstbn(supp: Vector[Term])(fn: Term) = 
    (y: Term) => ((funcInvImage(supp)(fn)) mapValues (FD.rawUnif(_))).getOrElse(y, FD.empty[Term])
  /**
   * inverse image of a function as a truncated distribution;
   * an atom of weight 1 at each inverse image point.
   */  
  def invTD(supp: Vector[Term])(fn: Term) : Term => TD[Term] = 
    (y: Term) => ((funcInvImage(supp)(fn)) mapValues ((xs) => TD(FD.rawUnif(xs)))).getOrElse(y, TD.Empty[Term])
    
  /**
   * returns the term of the application adjoint with differentiation holding a function fixed;
   * this is the sum over all functions of the adjoint of that function application, scaled by the function weight. 
   */
  def applnAdjointOnFuncs(fd: FD[Term])(w : => TD[Term]) : TD[Term] = {
      val tds = 
        for (Weighted(f, p) <- fd.pmf if isFunc(f)) yield
          (w flatMap ((y) => invTD(fd.supp)(f)(y))) <*> p 
      TD.BigSum(tds)
    }

  case object variable extends AnySym{
    def apply[U <: Term with Subs[U]](typ: Typ[U]) = typ.symbObj(this)
  }
    
  /**
   * given a finite distribution of terms, 
   * returns a map associating to a type the distribution of `value`s of lambda terms of that type;
   * with variable in values from the above `varaible` object
   */
  def lambdaFDMap(fd: FD[Term]) = {    
    val pmf = (fd.pmf collect {
      case Weighted(l: LambdaLike[u, v], p) => 
        (l.variable.typ, l.value.replace(l.variable, variable(l.variable.typ)), p)}).
        groupBy(_._1).
        mapValues ((v) => v map ((z) => Weighted(z._2 : Term, z._3)))
    pmf mapValues (FiniteDistribution(_).flatten)
  }
  
  /**
   * given a truncated distribution of terms and a type, 
   * returns the truncated distribution of `value`s of lambda terms of that type;
   * with variable in the values from the above `variable` object
   */
  def lambdaTD(td: TD[Term]) =
    (typ: Typ[Term]) => 
      td mapFD ((fd) => lambdaFDMap(fd).get(typ).getOrElse(FD.empty[Term]))
  
  def lambdaAdjointDist(
      recAdj : => (FD[Term] => TD[Term] => TD[Term]))(
          fd: FD[Term])(
              typ: Typ[Term])(w : => TD[Term]) = {
        val neww = lambdaTD(w)(typ)
        val x = variable(typ)
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
        val x = variable(typ)
    lambdaFDMap(fd).get(typ) map {
      (newp) =>
        val wfd = neww.getFD(cutoff).getOrElse(FD.empty[Term])
        (wfd map ((t) => newp(t))).expectation
    } 
  }
  
}

class DeducerFunc(applnWeight: Double, lambdaWeight: Double, piWeight: Double, varWeight: Double){
  import Deducer._ 
  def func(pd: PD[Term]): PD[Term] = 
    pd. 
    <+?> (appln(func)(pd), applnWeight).
    <+?> (lambda(varWeight)(func)(pd), lambdaWeight).
    <+?> (pi(varWeight)(func)(pd), lambdaWeight)
}