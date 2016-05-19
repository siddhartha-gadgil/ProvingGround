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

  def isTyp : Term => Boolean = {
    case _ :  Typ[_] => true
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

  def memAppln(rec: => (PD[Term] => PD[Term]))(p: PD[Term])(save: (Term, Term, Term) => Unit) = {
    rec(p) flatMap ((f) =>
      if (isFunc(f))
        rec(p) map ((x) =>
          TL.appln(f, x) map ((y) => {save(f, x, y); y}))
      else
        FD.unif(None : Option[Term])
      )
  }

  def eqSubs(rec: => (PD[Term] => PD[Term]))(p: PD[Term])(save: (Term, IdentityTyp[Term], Term) => Unit) = {
    rec(p) flatMap {
      case eq @ IdentityTyp(dom, lhs: Term, rhs: Term) =>
        rec(p) map ((x) =>
          if (x.typ == dom)
            Some(x.subs(lhs, rhs)) map ((y) => {save(x, eq.asInstanceOf[IdentityTyp[Term]], y); y})
            else None)
      case _ =>
        FD.unif(None : Option[Term])
    }
  }

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


    // Helpers that may not be used
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

  def argInvImage(supp: Vector[Term])(arg : Term) : Map[Term, Vector[Term]] = {
   val optMap = (supp collect {case fn: FuncLike[u, v] if fn.dom == arg.typ => fn}) groupBy (TL.appln(_, arg))
   for ((yo, fx) <- optMap; y <- yo) yield (y, fx)
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
// End of unused helpers.






  def argInvTD(supp: Vector[Term])(arg: Term) : Term => TD[Term] =
    (y: Term) => ((argInvImage(supp)(arg)) mapValues ((xs) => TD(FD.rawUnif(xs)))).getOrElse(y, TD.Empty[Term])

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

  def applnAdjointOnArgs(fd: FD[Term])(w : => TD[Term]) : TD[Term] = {
      val tds =
        for (Weighted(x, p) <- fd.pmf)  yield
          (w flatMap ((y) => argInvTD(fd.supp)(x)(y))) <*> p
      TD.BigSum(tds)
    }

  def applnAdjoint(recAdj : => (FD[Term] => TD[Term] => TD[Term]))(fd: FD[Term])(w : => TD[Term]) =
    recAdj(fd)(applnAdjointOnArgs(fd)(w) <+> applnAdjointOnFuncs(fd)(w))

  case object variable extends AnySym{
    def apply[U <: Term with Subs[U]](typ: Typ[U]) = typ.symbObj(this)
  }

  /**
   * given a type, returns optionally values of lambda terms with variable of the given type
   * with variable in values from the above `variable` object
   */
  def lambdaValue[U <: Term with Subs[U]](typ: Typ[U]) : Term => Option[Term] = {
    case l: LambdaLike[u, v] =>
      Some(l.value replace (l.variable, variable(l.variable.typ)))
    case _ => None
  }

  def piValue[U <: Term with Subs[U]](typ: Typ[U]) : Term => Option[Term] = {
    case fn: FuncLike[u, v] =>
      val x = variable(fn.dom)
      val codom = fn.depcodom(x)
      Some(codom)
    case _ => None
  }



  /**
   * given a truncated distribution of terms and a type,
   * returns the truncated distribution of `value`s of lambda terms of that type;
   * with variable in the values from the above `variable` object
   */
  def lambdaTD(td: TD[Term]) =
    (typ: Typ[Term]) =>
      td mapOpt (lambdaValue(typ))

  def lambdaFD(fd: FD[Term]) =
    (typ: Typ[Term]) =>
      fd mapOpt (lambdaValue(typ))

  def piTD(td: TD[Term]) =
    (typ: Typ[Term]) =>
      td mapOpt (piValue(typ))

  def piFD(fd: FD[Term]) =
    (typ: Typ[Term]) =>
      fd mapOpt (piValue(typ))

  def lambdaAdjointOnIslands(
      recAdj : => (FD[Term] => TD[Term] => TD[Term]))(
          fd: FD[Term])(w : => TD[Term]) : TD[Term]  = {
        val vec   =
          fd.supp collect {
          case typ: Typ[u] =>
            val innerw = w mapOpt (lambdaValue(typ))
            val x = variable(typ)
            val innerp = fd mapOpt (lambdaValue(typ)) // this already is scaled by weight of type and of lambda-term
            recAdj(innerp)(innerw) map ((t) => HoTT.lambda(x)(t) : Term)
          }
        TD.BigSum(vec)
  }

  def lambdaAdjointOnProbs(
      recAdj : => (FD[Term] => TD[Term] => TD[Term]))(
          fd: FD[Term])(w : => TD[Term]) : TD[Term] = {
        def pmf(cutoff: Double)   =
          fd.supp collect {
            case typ: Typ[u] =>
              val neww = lambdaTD(w)(typ)
              val x = variable(typ)
              val newp = fd mapOpt (lambdaValue(typ))
              val wfd = neww.getFD(cutoff).getOrElse(FD.empty[Term])
              Weighted(typ: Term, (wfd map ((t) => newp(t))).expectation)
    }
        def finDist(cutoff: Double) = Some(FD(pmf(cutoff)))
      recAdj(fd)(TD.FromFDs(finDist))
  }

  def lambdaAdjoint(
      recAdj : => (FD[Term] => TD[Term] => TD[Term]))(
          fd: FD[Term])(w : => TD[Term]) : TD[Term] =
            lambdaAdjointOnProbs(recAdj)(fd)(w) <+> lambdaAdjointOnIslands(recAdj)(fd)(w)

}

class DeducerFunc(applnWeight: Double, lambdaWeight: Double, piWeight: Double, varWeight: Double){
  import Deducer._
  def func(pd: PD[Term]): PD[Term] =
    pd.
    <+?> (appln(func)(pd), applnWeight).
    <+?> (lambda(varWeight)(func)(pd), lambdaWeight).
    <+?> (pi(varWeight)(func)(pd), lambdaWeight)

  var invImageMap : scala.collection.mutable.Map[Term, Set[(Term, Term)]] =
    scala.collection.mutable.Map()

  var subsInvMap : scala.collection.mutable.Map[Term, Set[(IdentityTyp[Term], Term)]] =
    scala.collection.mutable.Map()

  def save(f: Term, x: Term, y: Term) =
    invImageMap(y) = invImageMap.getOrElse(y, Set()) + ((f, x))

  def saveSubs(x: Term, eq: IdentityTyp[Term], result: Term) =
    subsInvMap(result) = subsInvMap.getOrElse(result, Set()) + ((eq, x))

  def memFunc(pd: PD[Term]): PD[Term] =
    pd.
    <+?> (memAppln(func)(pd)(save), applnWeight).
    <+?> (lambda(varWeight)(func)(pd), lambdaWeight).
    <+?> (pi(varWeight)(func)(pd), lambdaWeight)

  def funcPropTerm(backProp: => (FD[Term] => TD[Term] => TD[Term]))(fd: FD[Term]): Term => TD[Term] =
    (result) =>
      invImageMap.get(result) map (
          (v) => {
            val tds = v.toVector map {
              case (f, x) =>
                val scale = applnWeight *fd(f) * fd(x)/fd(result)
                backProp(fd)(TD.FD(FD.unif(f, x)) <*> scale)
            }
            TD.BigSum(tds)
          }
          ) getOrElse (TD.Empty[Term])

   def funcProp(backProp: => (FD[Term] => TD[Term] => TD[Term]))(fd: FD[Term])(td: TD[Term]) =
     td flatMap (funcPropTerm(backProp)(fd))

   def eqSubsPropTerm(backProp: => (FD[Term] => TD[Term] => TD[Term]))(fd: FD[Term]): Term => TD[Term] =
    (result) =>
      subsInvMap.get(result) map (
          (v) => {
            val tds = v.toVector map {
              case (eq, x) =>
                val eqSubsWeight: Double = 0.0
                val scale = eqSubsWeight *fd(eq) * fd(x)/fd(result)
                backProp(fd)(TD.FD(FD.unif(eq, x)) <*> scale)
            }
            TD.BigSum(tds)
          }
          ) getOrElse (TD.Empty[Term])

   def eqSubsProp(backProp: => (FD[Term] => TD[Term] => TD[Term]))(fd: FD[Term])(td: TD[Term]) =
     td flatMap (eqSubsPropTerm(backProp)(fd))

   def lambdaPropVarTerm(backProp: => (FD[Term] => TD[Term] => TD[Term]))(
       fd: FD[Term]): Term => TD[Term] = {
         case l: LambdaLike[_, _] =>
           val atom = TD.atom(l.variable.typ : Term)
           backProp(fd)(atom)
         case _ => TD.Empty[Term]
   }

   def lambdaPropVar(backProp: => (FD[Term] => TD[Term] => TD[Term]))(
       fd: FD[Term])(td: TD[Term]) =
         td flatMap (lambdaPropVarTerm(backProp)(fd))

   def lambdaPropValuesForTyp(backProp: => (FD[Term] => TD[Term] => TD[Term]))(
       fd: FD[Term])(td: TD[Term])(tp: Typ[Term]) : TD[Term] = {
     import Deducer.{lambdaTD, lambdaFD, variable}
     val inner = backProp(lambdaFD(fd)(tp))(lambdaTD(td)(tp))
     inner map ((y) => HoTT.lambda(variable(tp))(y))
   }

   def lambdaPropValues(backProp: => (FD[Term] => TD[Term] => TD[Term]))(
       fd: FD[Term])(td: TD[Term]) : TD[Term] = {
     val v = fd.supp collect {
       case tp: Typ[_] =>
         lambdaPropValuesForTyp(backProp)(fd)(td)(tp)
     }
     TD.BigSum(v)
   }

   def piPropVarTerm(backProp: => (FD[Term] => TD[Term] => TD[Term]))(
       fd: FD[Term]): Term => TD[Term] = {
         case fn: FuncLike[u, v] =>
           val codom = fn.depcodom(variable[u](fn.dom))
           val atom = TD.atom(fn.dom : Term)
           backProp(fd)(atom)
         case _ => TD.Empty[Term]
   }


   def piPropVar(backProp: => (FD[Term] => TD[Term] => TD[Term]))(
       fd: FD[Term])(td: TD[Term]) =
         td flatMap (piPropVarTerm(backProp)(fd))

   def piPropValuesForTyp(backProp: => (FD[Term] => TD[Term] => TD[Term]))(
       fd: FD[Term])(td: TD[Term])(tp: Typ[Term]) : TD[Term] = {
     import Deducer.{piTD, piFD, variable}
     val inner = backProp(piFD(fd)(tp))(piTD(td)(tp))
     inner mapOpt {
       case y : Typ[u] =>
         Some(HoTT.pi(variable(tp))(y.asInstanceOf[Typ[Term]]))
       case _ => None}
   }

   def piPropValues(backProp: => (FD[Term] => TD[Term] => TD[Term]))(
       fd: FD[Term])(td: TD[Term]) : TD[Term] = {
     val v = fd.supp collect {
       case tp: Typ[_] =>
         piPropValuesForTyp(backProp)(fd)(td)(tp)
     }
     TD.BigSum(v)
   }

    def backProp(epsilon: Double)(fd: FD[Term]): TD[Term] => TD[Term] =
      (td) =>
        td <*> (1 - epsilon) <+>
        (funcProp(backProp(epsilon))(fd)(td) <*> epsilon) <+>
        (lambdaPropVar(backProp(epsilon))(fd)(td) <*> epsilon) <+>
        (lambdaPropValues(backProp(epsilon))(fd)(td) <*> epsilon) <+>
        (piPropVar(backProp(epsilon))(fd)(td) <*> epsilon) <+>
        (piPropValues(backProp(epsilon))(fd)(td) <*> epsilon)
}
