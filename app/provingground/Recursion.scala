package provingground

import provingground.HoTT._
import provingground.Contexts._
import provingground.InductiveTypes._
//import provingground.Context.Context.empty

import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}
import annotation._

/**
 * Recursion and induction for inductive types.
 * 
 * The type of recursion and induction functions are constructed, as well as contexts which export appropriate values.
 */
object Recursion{
  
  /**
   * argument of the lhs of an equation corresponding to a specific constructor
   * 
   *  @param cnstr constructor
   *  
   *  @param vars variable names to be applied to the constructor. 
   */
  case class CnstrLHS(cnstr: Constructor, vars: List[AnySym]){
    /**
     * term of type W obtained by applying the constructor to terms with given names.
     */
    val arg = foldnames(cnstr.cons, vars)
    
    /**
     * Context for recursion
     * The type of this is also used for recursion 
     */
    def recCtx[U <: Term](f: => FuncObj[Term, U]) : Context[Term, Term] = {
      cnstrRecContext[Term](f, cnstr.pattern, vars,f.dom, f.codom)(Context.empty[Term])
    }
    
    /**
     * A formal object representing the type of the recursion function.
     */
    def recCtxVar(f: => FuncObj[Term, Term]) : Term  = {
      recCtxTyp(f).symbObj(RecInduced(cnstr.cons, f))
    }
    
    /**
     * HoTT type of the recursion function
     */
    def recCtxTyp(f: => FuncObj[Term, Term]) : Typ[Term]  = {
      recCtx(f)(f.codom)
    }
    
    def recCtxRHS(f: => FuncObj[Term, Term]) = recCtx(f).foldinSym(f.codom)(recCtxVar(f))
    
    /**
     * context for inductive definitions.
     */
    def indCtx[U <: Term](f: => FuncTerm[Term, U]) : Context[Term, Term] = {
      cnstrIndContext[Term, Term](f, cnstr.pattern, vars,f.dom, f.depcodom)(Context.empty[Term])
    }
    
    /**
     * HoTT type of the induction function 
     */
    def indCtxTyp(f: => FuncTerm[Term, Term]) : Typ[Term]  = {
      indCtx(f)(f.depcodom(arg))
    }
    
    def indCtxVar(f: => FuncTerm[Term, Term]) : Term  = {
      indCtxTyp(f).symbObj(IndInduced(cnstr.cons, f))
    }
    
    def indCtxRHS(f: => FuncTerm[Term, Term]) = indCtx(f).foldinSym(f.depcodom(arg))(indCtxVar(f))
    
    private def kappaChange(f: => FuncObj[Term, Term]) : Change[Term] = (varname, ptn, ctx) => {
    val x = ptn(f.dom).symbObj(varname)
    val fx = (ptn.induced(f.dom, f.codom)(f))(x)
    x /: (fx |: ctx)
    	}
    
    def recKappaCtx(f: => FuncObj[Term, Term]) : Context[Term, Term] = {
      cnstrContext[Term](cnstr.pattern, vars,f.dom, kappaChange(f))(Context.empty[Term])
    }
    
    private def kappaIndChange(f: => FuncTerm[Term, Term]) : Change[Term] = (varname, ptn, ctx) => {
    val x = ptn(f.dom).symbObj(varname)
    val fx = ptn.inducedDep(f.dom, f.depcodom)(f)(x)
     x /: (fx |: ctx)
    	}
    
    def indKappaCtx(f: => FuncTerm[Term, Term]) : Context[Term, Term] = {
      cnstrContext[Term](cnstr.pattern, vars,f.dom, kappaIndChange(f))(Context.empty[Term])
    }
    
    def recIdentity(f: => FuncObj[Term, Term])(rhs: Term) = DefnEqual(f(arg), rhs, recCtx(f).symblist(f.dom)(vars))
    
    def indIdentity(f: => FuncTerm[Term, Term])(rhs: Term) = DefnEqual(f(arg), rhs, indCtx(f).symblist(f.dom)(vars))
  }
  
   /**
   * Change in context for a TypPattern (i.e., simple pattern ending in W).
   * Should allow for dependent types when making a lambda.
   */
  private def recContextChange[V<: Term](f : => (FuncTerm[Term, Term]), 
        W : Typ[Term], X : Typ[V]) : (AnySym, TypPtnLike, Context[Term, V]) => Context[Term, V] = (varname, ptn, ctx) => {
    val x = ptn(W).symbObj(varname)
    val fx = ptn.induced(W, X)(f)(x)
    x /: fx /: ctx
  }
  
  
    /**
   * Change in context  for Induction for a TypPattern
   */
  private def indContextChange[V<: Term](f : => (FuncTerm[Term, Term]), 
      W : Typ[V], Xs : Term => Typ[V]) : (AnySym, TypPtnLike, Context[Term, V]) => Context[Term, V] = (varname, ptn, ctx) =>   {
    val x =  ptn(W).symbObj(varname)
    val fx = ptn.inducedDep(W, Xs)(f)(x)
    x /: fx /: ctx
  }
  
  /**
   * change in contexts given a type-pattern and a variable name.
   */
  private type Change[V <: Term] = (AnySym, TypPtnLike, Context[Term, V]) => Context[Term, V]
  
  /**
   * Returns the context for a poly-pattern given change in context for a type-pattern.
   * Recursively defined as a change, applied  by default to an empty context.
   * Note that as both contexts and Poly-patterns are built right to left, it is natural to get consistency.
   * 
   * @param ctx accumulator for context.
   * 
   * 
   */
  private def cnstrContext[V<: Term](
      ptn : PolyPtn[Term], varnames : List[AnySym], 
      W : Typ[V], 
      change: Change[V])(ctx: Context[Term, V] = Context.empty[Term]) : Context[Term, V] = {
    ptn match {
      case IdW => ctx // the final co-domain. We have just this for constant constructors.
      case FuncPtn(tail, head) => 
        val headctx = cnstrContext(head, varnames.tail, W, change)(ctx)
        change(varnames.head, tail, headctx)
      case CnstFncPtn(tail , head) =>
        val x = tail.symbObj(varnames.head)
        val headctx = cnstrContext(head, varnames.tail, W, change)(ctx)
        x /: headctx
      case DepFuncPtn(tail, headfibre , _) =>
        val x : Term = tail(W).symbObj(varnames.head)
        val headctx = cnstrContext(headfibre(x), varnames.tail, W, change)(ctx)
        change(varnames.head, tail, headctx)
      case CnstDepFuncPtn(tail, headfibre , _) =>
        val x = tail.symbObj(varnames.head)
        val headctx = cnstrContext(headfibre(x), varnames.tail, W, change)(ctx)
        x /: headctx
    }
  }
  
  /**
   *  context for recursive definition for a constructor.
   */
  def cnstrRecContext[V<: Term](f : => (FuncTerm[Term, Term]), 
      ptn : PolyPtn[Term], varnames : List[AnySym], 
      W : Typ[V], 
      X : Typ[V])(ctx: Context[Term, V] = Context.empty[Term]) : Context[Term, V] = {
    val change = recContextChange[V](f, W, X)
    cnstrContext(ptn, varnames, W, change)(ctx)
  }
  
  /**
   *  context for inductive definition for a constructor.
   */
  def cnstrIndContext[V<: Term, U <: Term](f : => (FuncTerm[Term, Term]), 
      ptn : PolyPtn[U], varnames : List[AnySym], 
      W : Typ[V], 
      Xs :  Term => Typ[V])(ctx: Context[Term, V]) : Context[Term, V] = {
    val change = indContextChange[V](f, W, Xs)
    cnstrContext(ptn, varnames, W, change)(ctx)
  }
  
  

  
  /**
   * formal symbol
   */
  case class RecInduced(cons : Term, func: Term => Term) extends AnySym
  
  /**
   * formal symbol
   */
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