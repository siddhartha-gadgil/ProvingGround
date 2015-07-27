package provingground

import provingground.HoTT._
import provingground.Contexts._

//import provingground.Context.Context.empty

import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}
import annotation._
/**
 * Recursion and induction for inductive type families.
 * A lot of the same code as recursion is used, but with a different import, so constructors etc are indexed.
 *
 * Recursion and induction are given in two ways:
 * * A context for each constructor, from which an identity is generated given an rhs: the kappaCtx maybe the better one for this.
 * * A formal recursion/induction function.
 *
 *
 */
class RecursionIndexed(indices : Typ[Term]){/*
   val indxind =  new IndexedInductiveTypes[Term]
   import indxind._

   /**
   * argument of the lhs of an equation corresponding to a specific constructor.
   *
   *  @param cnstr constructor
   *
   *  @param vars variable names to be applied to the constructor.
   */
  case class CnstrLHS(cnstr: Constructor, vars: List[AnySym]){
    /**
     * the argument of the lhs of identities for this constructor, which is a
     * term of type W obtained by applying the constructor to terms with given names.
     *
     */
    val arg = foldnames(cnstr.cons, vars)

    def varterms(W : Term => Typ[Term]) = symbTerms(cnstr.pattern, vars, W)
    /**
     * Context for recursion
     * The type of this is also used for recursion
     */
    def recCtx[U <: Term](f: => Term => Func[Term, U], X: Typ[Term]) : Context[Term, Term] = {
      cnstrRecContext[Term](f, cnstr.pattern, vars,(t) => f(t).dom, X)(Context.empty[Term])
    }

    def recCtxVar(f: => Term => Func[Term, Term], X: Typ[Term]) : Term  = {
      recCtxTyp(f, X).symbObj(RecInduced(cnstr.cons, f))
    }

    /**
     * HoTT type of the recursion function
     */
	def recCtxTyp(f: => Term => Func[Term, Term], X: Typ[Term]) : Typ[Term]  = {
      recCtx(f, X)(X)
    }

    def recCtxRHS(f: => Term => Func[Term, Term], X: Typ[Term]) = recCtx(f, X).foldinSym(X)(recCtxVar(f, X))

     /**
     * context for inductive definitions.
     */
    def indCtx(f: => Term => FuncLike[Term, Term]) : Context[Term, Term] = {
      cnstrIndContext[Term, Term](f, cnstr.pattern, vars,(t) => f(t).dom, (t) => f(t).depcodom)(Context.empty[Term])
    }


    def indCtxTyp(f: => Term => FuncLike[Term, Term], tps: Term => Typ[Term]) : Typ[Term]  = {
      indCtx(f)(tps(arg))
    }


    def indCtxVar(f: => Term => FuncLike[Term, Term], tps: Term => Typ[Term]) : Term  = {
      indCtxTyp(f, tps).symbObj(IndInduced(cnstr.cons, f))
    }

    def indCtxRHS(f: => Term => FuncLike[Term, Term], tps: Term => Typ[Term]) = indCtx(f).foldinSym(tps(arg))(indCtxVar(f, tps))

     /**
     * change for building a kappa-context for recursion
     */
    private def kappaChange(f: => Term => Func[Term, Term], X: Typ[Term]) : Change[Term] = (varname, ptn, ctx) => {
    val x = ptn((t: Term) =>f(t).dom).symbObj(varname)
    ctx  kappa(ptn.induced((t: Term) => f(t).dom, X)(f)(x)) lmbda(x)
    	}

    /**
     * kappa-context for recursion, with f(n) etc. additional constants for parsing, but not variables.
     */
    def recKappaCtx(f: => Term => Func[Term, Term], X: Typ[Term]) : Context[Term, Term] = {
      cnstrContext[Term](cnstr.pattern, vars, (t: Term) => f(t).dom, kappaChange(f, X))(Context.empty[Term])
    }

	/**
     * change for building a kappa-context for recursion
     */
    private def kappaIndChange(f: => Term => FuncLike[Term, Term]) : Change[Term] = (varname, ptn, ctx) => {
    val x = ptn((t: Term) =>f(t).dom).symbObj(varname)
    ctx  kappa(ptn.inducedDep((t: Term) => f(t).dom, (t) => f(t).depcodom)(f)(x)) lmbda(x)
    	}

    /**
     * kappa-context for induction, with f(n) etc. additional constants for parsing, but not variables.
     */
	def indKappaCtx(f: => Term => FuncLike[Term, Term]) : Context[Term, Term] = {
      cnstrContext[Term](cnstr.pattern, vars, (t: Term) => f(t).dom, kappaIndChange(f))(Context.empty[Term])
    }

    /**
     * identity corresponding to the given constructor for a recursive definition.
     */
    def recIdentity(f: => Term => Func[Term, Term], X: Typ[Term])(rhs: Term) = DefnEqual(f(arg), rhs, recCtx(f, X).symblist(arg.typ)(vars))

    /**
     * identity corresponding to the given constructor for an inductive definition.
     */
    def indIdentity(f: => Term => FuncLike[Term, Term], X: Typ[Term])(rhs: Term) = DefnEqual(f(arg), rhs, indCtx(f).symblist(arg.typ)(vars))

  }

   /**
   * Change in context for a TypPattern (i.e., simple pattern ending in W).
   * Should allow for dependent types when making a lambda.
   */
  private def recContextChange[V<: Term](f : => Term => (FuncLike[Term, Term]),
        W : Term => Typ[Term], X : Typ[V]) : (AnySym, FmlyPtn, Context[Term, V]) => Context[Term, V] = (varname, ptn, ctx) => {
    val x = ptn(W).symbObj(varname)
    val fx = ptn.induced(W, X)(f)(x)
    x /: fx /: ctx
  }


    /*
   * Change in context  for Induction for a TypPattern - check
   */
  private def indContextChange[V<: Term](f : => Term => (FuncLike[Term, Term]),
      W : Term => Typ[V],
      Xs : Term => Term => Typ[V]) : (AnySym, FmlyPtn, Context[Term, V]) => Context[Term, V] = (varname, ptn, ctx) =>   {
    val x = ptn(W).symbObj(varname)
	val fx = ptn.inducedDep(W, Xs)(f)(x)
    x /: fx /: ctx
  }

  /**
   * change in contexts given a type-pattern and a variable name.
   */
  private type Change[V <: Term] = (AnySym, FmlyPtn, Context[Term, V]) => Context[Term, V]

  /**
   * Returns the context for a polypattern given change in context for a typepattern.
   * Recursively defined as a change, applied  by default to an empty context.
   *
   * @param ctx accumulator for context.
   *
   *
   */
  def cnstrContext[V<: Term](
      ptn : ConstructorPattern[Term], varnames : List[AnySym],
      W : Term => Typ[V],
      change: Change[V])(ctx: Context[Term, V] = Context.empty[Term]) : Context[Term, V] = {
    ptn match {
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
   * returns symbolic terms of type according to a poly-pattern
   */
  private def symbTerms(ptn : ConstructorPattern[Term], varnames : List[AnySym],
      W : Term => Typ[Term], accum: List[Term] = List()) : List[Term] = ptn match {
    case IndxW(_) => accum
    case FuncPtn(tail, head) =>
      val x = tail(W).symbObj(varnames.head)
      symbTerms(head, varnames.tail, W, x :: accum)
    case CnstFncPtn(tail , head) =>
      val x = tail.symbObj(varnames.head)
      symbTerms(head, varnames.tail, W, x :: accum)
    case DepFuncPtn(tail, headfibre , _) =>
      val x : Term = tail(W).symbObj(varnames.head)
      symbTerms(headfibre(x), varnames.tail, W, x :: accum)
    case CnstDepFuncPtn(tail, headfibre , _) =>
      val x : Term = tail.symbObj(varnames.head)
      symbTerms(headfibre(x), varnames.tail, W, x :: accum)
  }

  /**
   *  context for recursive definition for a constructor.
   */
  def cnstrRecContext[V<: Term](f : => Term  => (FuncLike[Term, Term]),
      ptn : ConstructorPattern[Term], varnames : List[AnySym],
      W : Term => Typ[V],
      X : Typ[V])(ctx: Context[Term, V] = Context.empty[Term]) : Context[Term, V] = {
    val change = recContextChange[V](f, W, X)
    cnstrContext(ptn, varnames, W, change)(ctx)
  }

  /**
   *  context for inductive definition for a constructor.
   */
  def cnstrIndContext[V<: Term, U <: Term](f : => Term => (FuncLike[Term, Term]),
      ptn : ConstructorPattern[U], varnames : List[AnySym],
      W : Term => Typ[V],
      Xs :  Term => Term => Typ[V])(ctx: Context[Term, V]) : Context[Term, V] = {
    val change = indContextChange[V](f, W, Xs)
    cnstrContext(ptn, varnames, W, change)(ctx)
  }





  case class RecInduced(cons : Term, func: Term => Term) extends AnySym

  case class IndInduced(cons: Term, func: Term => Term) extends AnySym

  case class RecDefinition(f: FuncLike[Term, Func[Term, Term]], X: Typ[Term], cs: List[CnstrLHS]){
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


  case class IndDefinition(f: FuncLike[Term, FuncLike[Term, Term]], tps: Term => Typ[Term], cs: List[CnstrLHS]){
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
   */

}
