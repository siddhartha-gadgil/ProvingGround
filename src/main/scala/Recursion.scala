package provingGround

import provingGround.HoTT._
import provingGround.Contexts._
//import provingGround.Context.Context.empty

import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}
import annotation._

object Recursion{
  
  case class CnstrLHS(cnstr: Constructor, vars: List[AnySym]){
    /*
     * Term of type W
     */
    val arg = foldnames(cnstr.cons, vars)
    
    /*
     * Context for recursion: given rhs gives image of constructor.
     * The type of this is also used for recursion to 
     */
    def recCtx[U <: Term](f: => FuncObj[Term, U]) : Context[Term, Term] = {
      cnstrRecContext[Term](f, cnstr.pattern, vars,f.dom, f.codom)(Context.empty[Term])
    }
    
    def recCtxVar(f: => FuncObj[Term, Term]) : Term  = {
      recCtxTyp(f).symbObj(RecInduced(cnstr.cons, f))
    }
    
    def recCtxTyp(f: => FuncObj[Term, Term]) : Typ[Term]  = {
      recCtx(f)(f.codom)
    }
    
    def recCtxRHS(f: => FuncObj[Term, Term]) = recCtx(f).foldinSym(f.codom)(recCtxVar(f))
    
    def indCtx[U <: Term](f: => FuncTerm[Term, U]) : Context[Term, Term] = {
      cnstrIndContext[Term, Term](f, cnstr.pattern, vars,f.dom, f.depcodom)(Context.empty[Term])
    }
    
    def indCtxTyp(f: => FuncTerm[Term, Term]) : Typ[Term]  = {
      indCtx(f)(f.depcodom(arg))
    }
    
    def indCtxVar(f: => FuncTerm[Term, Term]) : Term  = {
      indCtxTyp(f).symbObj(IndInduced(cnstr.cons, f))
    }
    
    def indCtxRHS(f: => FuncTerm[Term, Term]) = indCtx(f).foldinSym(f.depcodom(arg))(indCtxVar(f))
    
    private def kappaChange(f: => FuncObj[Term, Term]) : Change[Term] = (varname, ptn, ctx) => {
    val x = ptn(f.dom).symbObj(varname)
    ctx  kappa(ptn.induced(f.dom, f.codom)(f)(x)) lmbda(x)
    	}
    
    def recKappaCtx(f: => FuncObj[Term, Term]) : Context[Term, Term] = {
      cnstrContext[Term](cnstr.pattern, vars,f.dom, kappaChange(f))(Context.empty[Term])
    }
    
    private def kappaIndChange(f: => FuncTerm[Term, Term]) : Change[Term] = (varname, ptn, ctx) => {
    val x = ptn(f.dom).symbObj(varname)
    ctx  kappa(ptn.inducedDep(f.dom, f.depcodom)(f)(x)) lmbda(x)
    	}
    
    def indKappaCtx(f: => FuncTerm[Term, Term]) : Context[Term, Term] = {
      cnstrContext[Term](cnstr.pattern, vars,f.dom, kappaIndChange(f))(Context.empty[Term])
    }
    
    def recIdentity(f: => FuncObj[Term, Term])(rhs: Term) = DefnEqual(f(arg), rhs, recCtx(f).symblist(f.dom)(vars))
    
    def indIdentity(f: => FuncTerm[Term, Term])(rhs: Term) = DefnEqual(f(arg), rhs, indCtx(f).symblist(f.dom)(vars))
  }
  
      /*
   * Change in context for a TypPatn (i.e., simple pattern ending in W).
   * Should allow for dependent types when making a lambda.
   */
  private def recContextChange[V<: Term](f : => (FuncTerm[Term, Term]), 
        W : Typ[Term], X : Typ[V]) : (AnySym, TypPtnLike, Context[Term, V]) => Context[Term, V] = (varname, ptn, ctx) => {
    val x = ptn(W).symbObj(varname)
    ctx  lmbda(ptn.induced(W, X)(f)(x)) lmbda(x)
  }
  
  
    /*
   * Change in context  for Induction for a TypPattern - check
   */
  private def indContextChange[V<: Term](f : => (FuncTerm[Term, Term]), 
      W : Typ[V], Xs : Term => Typ[V]) : (AnySym, TypPtnLike, Context[Term, V]) => Context[Term, V] = (varname, ptn, ctx) =>   {
    val x =  ptn(W).symbObj(varname)
    ctx lmbda(ptn.inducedDep(W, Xs)(f)(x)) lmbda(x)
  }
      
  private type Change[V <: Term] = (AnySym, TypPtnLike, Context[Term, V]) => Context[Term, V]
  
  /*
   * Returns the context for a polypattern given change in context for a typepattern.
   * Recursively defined as a change, applied  by default to an empty context.
   */
  private def cnstrContext[V<: Term](
      ptn : PolyPtn[Term], varnames : List[AnySym], 
      W : Typ[V], 
      change: Change[V])(ctx: Context[Term, V] = Context.empty[Term]) : Context[Term, V] = {
    ptn match {
      case IdW => ctx
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
  
  def cnstrRecContext[V<: Term](f : => (FuncTerm[Term, Term]), 
      ptn : PolyPtn[Term], varnames : List[AnySym], 
      W : Typ[V], 
      X : Typ[V])(ctx: Context[Term, V] = Context.empty[Term]) : Context[Term, V] = {
    val change = recContextChange[V](f, W, X)
    cnstrContext(ptn, varnames, W, change)(ctx)
  }
  
  private def cnstrIndContext[V<: Term, U <: Term](f : => (FuncTerm[Term, Term]), 
      ptn : PolyPtn[U], varnames : List[AnySym], 
      W : Typ[V], 
      Xs :  Term => Typ[V])(ctx: Context[Term, V]) : Context[Term, V] = {
    val change = indContextChange[V](f, W, Xs)
    cnstrContext(ptn, varnames, W, change)(ctx)
  }
  
  

  
  
  case class RecInduced(cons : Term, func: Term => Term) extends AnySym
  
  case class IndInduced(cons: Term, func: Term => Term) extends AnySym
  
  case class RecDefinition(f: FuncObj[Term, Term], cs: List[CnstrLHS]){
    val types = for (c <- cs) yield c.recCtxTyp(f)
    val typ = (types :\ f.typ)(_ ->: _)
    val recfn = typ.symbObj(RecSymbol(f.dom, f.codom))
    
    val consvars = cs map (_.recCtxVar(f))
    val freevars = f :: consvars
    
    val identitites = cs map ((c) => {
      val lhs = foldterms(recfn, consvars :+ c.arg)
      val rhs = c.recCtxRHS(f)
      DefnEqual(lhs, rhs, freevars)
    })
  }
  
  case class IndDefinition(f: FuncTerm[Term, Term], cs: List[CnstrLHS]){
    val types = for (c <- cs) yield c.indCtxTyp(f)
    val typ = (types :\ f.typ)(_ ->: _)
    val recfn = typ.symbObj(IndSymbol(f.dom, f.depcodom))
    
    val consvars = cs map (_.indCtxVar(f))
    val freevars = f :: consvars
    
    val identitites = cs map ((c) => {
      val lhs = foldterms(recfn, consvars :+ c.arg)
      val rhs = c.indCtxRHS(f)
      DefnEqual(lhs, rhs, freevars)
    })
  }
  
  
  /*
   * The symbolic object for defining a recursion function from a context.
   */
  case class RecSymbol(W : Typ[Term], X : Typ[Term]) extends AnySym
  
  case class IndSymbol(W : Typ[Term], Xs : Term => Typ[Term]) extends AnySym  
  
  
}