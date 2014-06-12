package provingGround

import provingGround.HoTT._
import provingGround.Contexts._
//import provingGround.Context.Context.empty

import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}
import annotation._

object Recursion{
      /*
   * Change in context for a TypPatn (i.e., simple pattern ending in W).
   * Should allow for dependent types when making a lambda.
   */
  private def recContextChange[A, V<: Term](f : => (FuncTerm[Term, Term]), 
        W : Typ[V], X : Typ[V]) : (A, TypPtnLike, Context[A, Term, V]) => Context[A, Term, V] = (varname, ptn, ctx) => {
    val x = ptn(W).symbObj(varname)
    ctx lmbda(x) lmbda(ptn.induced(W, X)(f)(x))
  }
  
  def simpleContextChange[A, V<: Term](W : Typ[V]) : (A, TypPtnLike, Context[A, Term, V]) => Context[A, Term, V] = (varname, ptn, ctx) => {
    val x = ptn(W).symbObj(varname)
    ctx lmbda(x)
  }
  
    /*
   * Change in context  for Induction for a TypPattern - check
   */
  private def indContextChange[A, V<: Term](f : => (FuncTerm[Term, Term]), 
      W : Typ[V], Xs : Term => Typ[V]) : (A, TypPtnLike, Context[A, Term, V]) => Context[A, Term, V] = (varname, ptn, ctx) =>   {
    val x =  ptn(W).symbObj(varname)
    ctx lmbda(x) lmbda(ptn.inducedDep(W, Xs)(f)(x))
  }
      
  private type Change[A, V <: Term] = (A, TypPtnLike, Context[A, Term, V]) => Context[A, Term, V]
  
  /*
   * Returns the context for a polypattern given change in context for a typepattern.
   * Recursively defined as a change, applied  by default to an empty context.
   */
  private def cnstrContext[A, V<: Term](
      ptn : PolyPtn[Term], varnames : List[A], 
      W : Typ[V], 
      change: Change[A, V])(ctx: Context[A, Term, V] = Context.empty[Term]) : Context[A, Term, V] = {
    ptn match {
      case tp: TypPtnLike => change(varnames.head, tp, ctx)
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
  
  def cnstrRecContext[A, V<: Term](f : => (FuncTerm[Term, Term]), 
      ptn : PolyPtn[Term], varnames : List[A], 
      W : Typ[V], 
      X : Typ[V])(ctx: Context[A, Term, V] = Context.empty[Term]) : Context[A, Term, V] = {
    val change = recContextChange[A, V](f, W, X)
    cnstrContext(ptn, varnames, W, change)(ctx)
  }
  
  private def cnstrIndContext[A, V<: Term, U <: Term](f : => (FuncTerm[Term, Term]), 
      ptn : PolyPtn[U], varnames : List[A], 
      W : Typ[V], 
      Xs :  Term => Typ[V])(ctx: Context[A, Term, V]) : Context[A, Term, V] = {
    val change = indContextChange[A, V](f, W, Xs)
    cnstrContext(ptn, varnames, W, change)(ctx)
  }
  
  
  private def cnstrSimpleContext[A, V<: Term, U <: Term]( 
      ptn : PolyPtn[U], varnames : List[A], 
      W : Typ[V])(ctx: Context[A, Term, V] = Context.empty[Term]) : Context[A, Term, V] = {
		  val change = simpleContextChange[A, V](W)
    cnstrContext(ptn, varnames, W, change)(ctx)
  }
  
  
  case class RecInduced(cons : Term, func: Term => Term)
  
  case class IndInduced(cons: Term, func: Term => Term)
  
  private def addConstructor[V <: Term](f : => (FuncTerm[Term, Term]), 
      cnstr : Constructor, varnames : List[Any], 
      W : Typ[Term], 
      X : Typ[Term]) : Context[Any, Term, V] => Context[Any, Term, V] = ctx => {
        val cnstrctx = cnstrRecContext(f, cnstr.pattern, varnames, W, X)()
        val name = cnstrctx.foldinSym(FuncTyp(W, X))(RecInduced(cnstr.cons, f))
      		ctx lmbda (name)
      }
      
  private def addIndConstructor[V <: Term](f : => (FuncTerm[Term, Term]), 
      cnstr : Constructor, varnames : List[Any], 
      W : Typ[Term], 
      Xs : Term => Typ[Term]) : (Context[Any, Term, V]) => (Context[Any, Term, V]) =  ctx => {
        val varctx = cnstrSimpleContext(cnstr.pattern, varnames, W)()
        val variable = varctx.foldin(W)(cnstr.cons.asInstanceOf[varctx.PtnType])
        val target = Context.empty[Term]
        val cnstrctx = cnstrIndContext(f, cnstr.pattern, varnames, W, Xs)(target)
        val name = cnstrctx.foldinSym(Xs(variable))(IndInduced(cnstr.cons, f))
      		 ctx lmbda (name)
      }
      
  def recContext(f : => (FuncTerm[Term, Term]), 
      cnstrvars : List[(Constructor, List[Any])], 
      W : Typ[Term], 
      X : Typ[Term]) ={
    val add : (Context[Any, Term, FuncTerm[Term, Term]], 
        (Constructor, List[Any])) => Context[Any, Term, FuncTerm[Term, Term]] = (ctx, cnvr) =>
      addConstructor(f, cnvr._1, cnvr._2, W, X)(ctx)
      val empty : Context[Any, Term, FuncTerm[Term, Term]] = Context.empty[FuncTerm[Term, Term]]
    (empty /: cnstrvars)(add)
  }
  
  def indContext(f : => (FuncTerm[Term, Term]), 
      cnstrvars : List[(Constructor, List[Any])], 
      W : Typ[Term], 
      Xs : Term => Typ[Term], univ : Typ[Typ[Term]]) ={
    val add : (Context[Any, Term, FuncTerm[Term, Term]], 
        (Constructor, List[Any])) => Context[Any, Term, FuncTerm[Term, Term]] = (ctx, cnvr) =>
      addIndConstructor(f, cnvr._1, cnvr._2, W, Xs)(ctx)
      
      val empty : Context[Any, Term, FuncTerm[Term, Term]] = Context.empty[FuncTerm[Term, Term]]
    (empty /: cnstrvars)(add)
  }
  
  
  def recFunction(f : => (FuncTerm[Term, Term]), 
      cnstrvars : List[(Constructor, List[Any])], 
      W : Typ[Term], 
      X : Typ[Term]) = {

    recContext(f, cnstrvars, W, X).foldinSym(FuncTyp(W, X))(RecSymbol(W, X))}
  
  def indFunction(f : => (FuncTerm[Term, Term]), 
      cnstrvars : List[(Constructor, List[Any])], 
      W : Typ[Term], 
      Xs : Term => Typ[Term], univ : Typ[Typ[Term]]) = {
    val family = typFamilyDefn(W, univ, Xs)
    indContext(f, cnstrvars, W, Xs, univ).foldinSym(PiTyp(family))(IndSymbol(W, Xs))
  }
  
  // Should avoid type coercion by having proper types for constructors and polypatterns.
  def recIdentities(f : => (FuncTerm[Term, Term]), 
      cnstrvars : List[(Constructor, List[Any])], 
      W : Typ[Term], 
      X : Typ[Term]) = {

    val recfn = recContext(f, cnstrvars, W, X).foldinSym(FuncTyp(W, X))(RecSymbol(W, X))
    
    def eqn(cnstr : Constructor, varnames : List[Any]) = {	
    	val ptn = cnstr.pattern
    	val argctx = cnstrSimpleContext(ptn, varnames, W)()
    	val cons = cnstr.cons.asInstanceOf[argctx.PtnType]
    	val arg = argctx.foldin(W)(cons)
    	val lhs = recfn(arg)
    	val rhsctx = cnstrRecContext(f, ptn, varnames, W, X)()
    	val rhs = rhsctx.foldinSym(FuncTyp(W, X))(RecInduced(cons, f))
    	DefnEqual(lhs, rhs, argctx.symblist(W)(varnames))
    }
    
    val eqntail = for ((cnstr, varnames) <- cnstrvars) yield eqn(cnstr, varnames)
	
    val allvars = eqntail flatMap (_.freevars)
    
    DefnEqual(f, recfn, allvars) :: eqntail
  }
  
  def indIdentities(f : => (FuncTerm[Term, Term]), 
      cnstrvars : List[(Constructor, List[Any])], 
      W : Typ[Term], 
      Xs : Term => Typ[Term], univ : Typ[Typ[Term]]) = {
    
    val indfn = indFunction(f, cnstrvars, W, Xs, univ)
    
     def eqn(cnstr : Constructor, varnames : List[Any]) = {	
    	val ptn = cnstr.pattern
    	val argctx = cnstrSimpleContext(ptn, varnames, W)()
    	val cons = cnstr.cons.asInstanceOf[argctx.PtnType]
    	val arg = argctx.foldin(W)(cons)
    	val lhs = indfn(arg)
    	val varctx = cnstrSimpleContext(cnstr.pattern, varnames, W)()
        val variable = varctx.foldin(W)(cnstr.cons.asInstanceOf[varctx.PtnType])
        val target = Context.empty[Term]
    	val rhsctx = cnstrIndContext(f, ptn, varnames, W, Xs)(target)
    	val rhs = rhsctx.foldinSym(Xs(variable))(RecInduced(cons, f))
    	DefnEqual(lhs, rhs, argctx.symblist(W)(varnames))
    }
    
    val eqntail = for ((cnstr, varnames) <- cnstrvars) yield eqn(cnstr, varnames)
	
    val allvars = eqntail flatMap (_.freevars)
    
    DefnEqual(f, indfn, allvars) :: eqntail
    
  }
  
  /*
   * The symbolic object for defining a recursion function from a context.
   */
  case class RecSymbol(W : Typ[Term], X : Typ[Term])
  
  case class IndSymbol(W : Typ[Term], Xs : Term => Typ[Term])
  
}