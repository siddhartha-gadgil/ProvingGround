package provingGround

import provingGround.HoTT._
import provingGround.Contexts._

//import provingGround.Context.Context.empty

import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}
import annotation._
/*
 * Similar to Recursion, but in families.
 * We import the indexed version of Inductive Types and associated patterns.
 */
class RecursionIndexed(indices : Typ[Term]){
   val indxind =  new IndexedInductiveTypes[Term]
   import indxind._
   
     case class CnstrLHS(cnstr: Constructor, vars: List[AnySym]){
    /*
     * Term of type W
     */
    val arg = foldnames(cnstr.cons, vars)
    
    /*
     * Context for recursion: given rhs gives image of constructor.
     * The type of this is also used for recursion to 
     */
    def recCtx[U <: Term](f: => Term => FuncObj[Term, U], X: Typ[Term]) : Context[Term, Term] = {
      cnstrRecContext[Term](f, cnstr.pattern, vars,(t) => f(t).dom, X)(Context.empty[Term])
    }
    
    def recCtxVar(f: => Term => FuncObj[Term, Term], X: Typ[Term]) : Term  = {
      recCtxTyp(f, X).symbObj(RecInduced(cnstr.cons, f))
    }
    
    def recCtxTyp(f: => Term => FuncObj[Term, Term], X: Typ[Term]) : Typ[Term]  = {
      recCtx(f, X)(X)
    }
    
    def recCtxRHS(f: => Term => FuncObj[Term, Term], X: Typ[Term]) = recCtx(f, X).foldinSym(X)(recCtxVar(f, X))
    
    
    def indCtx(f: => Term => FuncTerm[Term, Term]) : Context[Term, Term] = {
      cnstrIndContext[Term, Term](f, cnstr.pattern, vars,(t) => f(t).dom, (t) => f(t).depcodom)(Context.empty[Term])
    }
    
    
    def indCtxTyp(f: => Term => FuncTerm[Term, Term], tps: Term => Typ[Term]) : Typ[Term]  = {
      indCtx(f)(tps(arg))
    }
    
    
    def indCtxVar(f: => Term => FuncTerm[Term, Term], tps: Term => Typ[Term]) : Term  = {
      indCtxTyp(f, tps).symbObj(IndInduced(cnstr.cons, f))
    }
    
    def indCtxRHS(f: => Term => FuncTerm[Term, Term], tps: Term => Typ[Term]) = indCtx(f).foldinSym(tps(arg))(indCtxVar(f, tps))
    
    
    private def kappaChange(f: => Term => FuncObj[Term, Term], X: Typ[Term]) : Change[Term] = (varname, ptn, ctx) => {
    val x = ptn((t: Term) =>f(t).dom).symbObj(varname)
    ctx  kappa(ptn.induced((t: Term) => f(t).dom, X)(f)(x)) lmbda(x)
    	}
    
    
    def recKappaCtx(f: => Term => FuncObj[Term, Term], X: Typ[Term]) : Context[Term, Term] = {
      cnstrContext[Term](cnstr.pattern, vars, (t: Term) => f(t).dom, kappaChange(f, X))(Context.empty[Term])
    }
    
    private def kappaIndChange(f: => Term => FuncTerm[Term, Term]) : Change[Term] = (varname, ptn, ctx) => {
    val x = ptn((t: Term) =>f(t).dom).symbObj(varname)
    ctx  kappa(ptn.inducedDep((t: Term) => f(t).dom, (t) => f(t).depcodom)(f)(x)) lmbda(x)
    	}
    
    def indKappaCtx(f: => Term => FuncTerm[Term, Term]) : Context[Term, Term] = {
      cnstrContext[Term](cnstr.pattern, vars, (t: Term) => f(t).dom, kappaIndChange(f))(Context.empty[Term])
    }
    
    
    def recIdentity(f: => Term => FuncObj[Term, Term], X: Typ[Term])(rhs: Term) = DefnEqual(f(arg), rhs, recCtx(f, X).symblist(arg.typ)(vars))
    
    def indIdentity(f: => Term => FuncTerm[Term, Term], X: Typ[Term])(rhs: Term) = DefnEqual(f(arg), rhs, indCtx(f).symblist(arg.typ)(vars))
   
  }
  
      /*
   * Change in context for a TypPatn (i.e., simple pattern ending in W).
   * Should allow for dependent types when making a lambda.
   */
  private def recContextChange[V<: Term](f : => Term => (FuncTerm[Term, Term]), 
        W : Term => Typ[Term], X : Typ[V]) : (AnySym, TypPtnLike, Context[Term, V]) => Context[Term, V] = (varname, ptn, ctx) => {
    val x = ptn(W).symbObj(varname)
    ctx  lmbda(ptn.induced(W, X)(f)(x)) lmbda(x)
  }
  
  
    /*
   * Change in context  for Induction for a TypPattern - check
   */
  private def indContextChange[V<: Term](f : => Term => (FuncTerm[Term, Term]), 
      W : Term => Typ[V], 
      Xs : Term => Term => Typ[V]) : (AnySym, TypPtnLike, Context[Term, V]) => Context[Term, V] = (varname, ptn, ctx) =>   {
    val x = ptn(W).symbObj(varname)
    ctx lmbda(ptn.inducedDep(W, Xs)(f)(x)) lmbda(x)
  }
      
  private type Change[V <: Term] = (AnySym, TypPtnLike, Context[Term, V]) => Context[Term, V]
  
  /*
   * Returns the context for a polypattern given change in context for a typepattern.
   * Recursively defined as a change, applied  by default to an empty context.
   */
  private def cnstrContext[V<: Term](
      ptn : PolyPtn[Term], varnames : List[AnySym], 
      W : Term => Typ[V], 
      change: Change[V])(ctx: Context[Term, V] = Context.empty[Term]) : Context[Term, V] = {
    ptn match {
      case IndxW(_) => ctx
 //     case tp: TypPtnLike => change(varnames.head, tp, ctx)
      case FuncPtn(tail, head) => cnstrContext(head, varnames.tail, W, change)(change(varnames.head, tail, ctx))
      case CnstFncPtn(tail : Typ[_], head) => cnstrContext(head, varnames.tail, W, change)( ctx lmbda(tail.symbObj(varnames.head)))
      case DepFuncPtn(tail, headfibre , _) =>
        val x  = tail(W).symbObj(varnames.head).asInstanceOf[Term]
        cnstrContext(headfibre(x), varnames.tail, W, change)( ctx lmbda(x))
      case CnstDepFuncPtn(tail, headfibre , _) =>
        val x = tail.symbObj(varnames.head)
        cnstrContext(headfibre(x), varnames.tail, W, change)( ctx lmbda(x))
    }
  }
  
  def cnstrRecContext[V<: Term](f : => Term => (FuncTerm[Term, Term]), 
      ptn : PolyPtn[Term], varnames : List[AnySym], 
      W : Term => Typ[V], 
      X : Typ[V])(ctx: Context[Term, V] = Context.empty[Term]) : Context[Term, V] = {
    val change = recContextChange[V](f, W, X)
    cnstrContext(ptn, varnames, W, change)(ctx)
  }
  
  private def cnstrIndContext[V<: Term, U <: Term](f :  => Term => (FuncTerm[Term, Term]), 
      ptn : PolyPtn[U], varnames : List[AnySym], 
      W : Term => Typ[V], 
      Xs :  Term => Term => Typ[V])(ctx: Context[Term, V]) : Context[Term, V] = {
    val change = indContextChange[V](f, W, Xs)
    cnstrContext(ptn, varnames, W, change)(ctx)
  }
  
  

  
  
  case class RecInduced(cons : Term, func: Term => Term) extends AnySym
  
  case class IndInduced(cons: Term, func: Term => Term) extends AnySym
  
  case class RecDefinition(f: FuncTerm[Term, FuncObj[Term, Term]], X: Typ[Term], cs: List[CnstrLHS]){
    val types = for (c <- cs) yield c.recCtxTyp(f, X)
    val typ = (types :\ f.typ)(_ ->: _)
    val recfn = typ.symbObj(RecSymbol((t) =>f(t).dom, X))
    
    val consvars = cs map (_.recCtxVar(f, X))
    val freevars = f :: consvars
    
    val identitites = cs map ((c) => {
      val lhs = foldterms(recfn, consvars :+ c.arg)
      val rhs = c.recCtxRHS(f, X)
      DefnEqual(lhs, rhs, freevars)
    })
  }
  
  
  case class IndDefinition(f: FuncTerm[Term, FuncTerm[Term, Term]], tps: Term => Typ[Term], cs: List[CnstrLHS]){
    val types = for (c <- cs) yield c.indCtxTyp(f, tps)
    val typ = (types :\ f.typ)(_ ->: _)
    val recfn = typ.symbObj(IndSymbol((t) => f(t).dom, f.depcodom))
    
    val consvars = cs map (_.indCtxVar(f, tps))
    val freevars = f :: consvars
    
    val identitites = cs map ((c) => {
      val lhs = foldterms(recfn, consvars :+ c.arg)
      val rhs = c.indCtxRHS(f, tps)
      DefnEqual(lhs, rhs, freevars)
    })
  }
  
  
  /*
   * The symbolic object for defining a recursion function from a context.
   */
  case class RecSymbol(W : Term => Typ[Term], X : Typ[Term]) extends AnySym
  
  case class IndSymbol(W : Term => Typ[Term], Xs : Term => Typ[Term]) extends AnySym  
   
  
}