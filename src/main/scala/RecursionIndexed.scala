package provingGround

import provingGround.HoTT._
import provingGround.Contexts._
//import provingGround.Context.Context.empty

import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}
import annotation._
/*
 * Similar to Recursion, but in families.
 * TypPtnLike is replaced by TypPtnInstance[D]
 */
object RecursionIndexed{
      /*
   * Change in context for a TypPatn (i.e., simple pattern ending in W).
   * Should allow for dependent types when making a lambda.
   */
  private def recContextChange[A, V<: Term, D <: Term](f : => (FuncTerm[Term, Term]), 
        W : D => Typ[V], X : Typ[V]) : (A, TypPtnInstance[D], Context[A, Term, V]) => Context[A, Term, V] = (varname, ptns, ctx) => {
    val x = ptns.ptn(W(ptns.arg)).symbObj(varname)
    ctx lmbda(x) lmbda(ptns.induced(W, X)(f)(x))
  }
  
  private def simpleContextChange[A, V<: Term, D <: Term](W : D=> Typ[V]) : (A, TypPtnInstance[D], Context[A, Term, V]) => Context[A, Term, V] = (varname, ptns, ctx) => {
    val x = ptns.ptn(W(ptns.arg)).symbObj(varname)
    ctx lmbda(x)
  }
  
    /*
   * Change in context  for Induction for a TypPattern - check
   */
  def indContextChange[A, V<: Term, D <: Term](f : => (FuncTerm[Term, Term]), 
      W : D => Typ[V], Xs : Term => Typ[V]) : (A, TypPtnInstance[D], Context[A, Term, V]) => Context[A, Term, V] = (varname, ptns, ctx) =>   {
    val x =  ptns.ptn(W(ptns.arg)).symbObj(varname)
    ctx lmbda(x) lmbda(ptns.inducedDep(W, Xs)(f)(x))
  }
      
  private type Change[A, V <: Term, D <: Term] = (A, TypPtnInstance[D], Context[A, Term, V]) => Context[A, Term, V]
  
  /*
   * The context, recursively defined, for the constructor of a single context.
   * This is also a change, to be applied by default to the empty context of the target type.
   */
  def cnstrContext[A, V<: Term, D <: Term](
      ptn : PolyPtnInstance[D, Term], varnames : List[A], 
      W : D => Typ[V], 
      change: Change[A, V, D])(ctx: Context[A, Term, V] = Context.empty[Term]) : Context[A, Term, V] = {
    ptn match {
      case tp: TypPtnInstance[D] => change(varnames.head, tp, ctx)
      case FuncPtnInstance(tail, head) => cnstrContext(head, varnames.tail, W, change)(change(varnames.head, tail, ctx))
      case CnstFuncPtnInstance(tail : Typ[_], head) => cnstrContext(head, varnames.tail, W, change)( ctx lmbda(tail.symbObj(varnames.head)))
/*
      case DepFuncPtnInstance(tail, headfibre , _) =>
        val x  = tail(W).symbObj(varnames.head).asInstanceOf[Term]
        cnstrContext(headfibre(x), varnames.tail, W, change)( ctx lmbda(x))
      case CnstDepFuncPtn(tail, headfibre , _) =>
        val x = tail.symbObj(varnames.head)
        cnstrContext(headfibre(x), varnames.tail, W, change)( ctx lmbda(x))
        * 
        */
    }
  }
  
  def cnstrRecContext[A, V<: Term, D <: Term](f : => (FuncTerm[Term, Term]), 
      ptns : PolyPtnInstance[D, Term], varnames : List[A], 
      W : D => Typ[V], 
      X : Typ[V])(ctx: Context[A, Term, V] = Context.empty[Term]) : Context[A, Term, V] = {
    val change = recContextChange[A, V, D](f, W, X)
    cnstrContext(ptns, varnames, W, change)(ctx)
  }
  
  def cnstrIndContext[A, V<: Term, U <: Term, D <: Term](f : => (FuncTerm[Term, Term]), 
      ptns : PolyPtnInstance[D, Term], varnames : List[A], 
      W : D => Typ[V], 
      Xs :  Term => Typ[V])(ctx: Context[A, Term, V]) : Context[A, Term, V] = {
    val change = indContextChange[A, V, D](f, W, Xs)
    cnstrContext(ptns, varnames, W, change)(ctx)
  }
  
  
  def cnstrSimpleContext[A, V<: Term, U <: Term, D <: Term]( 
      ptns : PolyPtnInstance[D, Term], varnames : List[A], 
      W : D => Typ[V])(ctx: Context[A, Term, V] = Context.empty[Term]) : Context[A, Term, V] = {
		  val change = simpleContextChange[A, V, D](W)
    cnstrContext(ptns, varnames, W, change)(ctx)
  }
  
  
  case class RecInduced(cons : Term, func: Term => Term)
  
  case class IndInduced(cons: Term, func: Term => Term)
  
  def addConstructor[V <: Term, D <: Term](f : => (FuncTerm[Term, Term]), 
      cnstr : ConstructorFmly[D], varnames : List[Any], 
      W : D => Typ[Term], 
      X : Typ[Term]) : Context[Any, Term, V] => Context[Any, Term, V] = ctx => {
        val cnstrctx = cnstrRecContext(f, cnstr.pattern, varnames, W, X)()
        val name = cnstrctx.foldinSym(FuncTyp(W(cnstr.pattern.arg), X))(RecInduced(cnstr.cons, f))
      		ctx lmbda (name)
      }
      
  def addIndConstructor[V <: Term, D <: Term](f : => (FuncTerm[Term, Term]), 
      cnstr : ConstructorFmly[D], varnames : List[Any], 
      W : D => Typ[Term], 
      Xs : Term => Typ[Term]) : (Context[Any, Term, V]) => (Context[Any, Term, V]) =  ctx => {
        val varctx = cnstrSimpleContext(cnstr.pattern, varnames, W)()
        val variable = varctx.foldin(W(cnstr.pattern.arg))(cnstr.cons.asInstanceOf[varctx.PtnType])
        val target = Context.empty[Term]
        val cnstrctx = cnstrIndContext(f, cnstr.pattern, varnames, W, Xs)(target)
        val name = cnstrctx.foldinSym(Xs(variable))(IndInduced(cnstr.cons, f))
      		 ctx lmbda (name)
      }
      
  def recContext[D <: Term](f : => (FuncTerm[Term, Term]), 
      cnstrvars : List[(ConstructorFmly[D], List[Any])], 
      W : D => Typ[Term], 
      X : Typ[Term]) ={
    val add : (Context[Any, Term, FuncTerm[Term, Term]], 
        (ConstructorFmly[D], List[Any])) => Context[Any, Term, FuncTerm[Term, Term]] = (ctx, cnvr) =>
      addConstructor(f, cnvr._1, cnvr._2, W, X)(ctx)
      val empty : Context[Any, Term, FuncTerm[Term, Term]] = Context.empty[FuncTerm[Term, Term]]
    (empty /: cnstrvars)(add)
  }
  
  def indContext[D <: Term](f : => (FuncTerm[Term, Term]), 
      cnstrvars : List[(ConstructorFmly[D], List[Any])], 
      W : D => Typ[Term], 
      Xs : Term => Typ[Term], univ : Typ[Typ[Term]]) ={
    val add : (Context[Any, Term, FuncTerm[Term, Term]], 
        (ConstructorFmly[D], List[Any])) => Context[Any, Term, FuncTerm[Term, Term]] = (ctx, cnvr) =>
      addIndConstructor(f, cnvr._1, cnvr._2, W, Xs)(ctx)
      
      val empty : Context[Any, Term, FuncTerm[Term, Term]] = Context.empty[FuncTerm[Term, Term]]
    (empty /: cnstrvars)(add)
  }
  
  /*
  def recFunction[D <: Term](f : => (FuncTerm[Term, Term]), 
      cnstrvars : List[(ConstructorFmly[D], List[Any])], 
      W : D => Typ[Term], 
      X : Typ[Term]) = {

    recContext(f, cnstrvars, W, X).foldinSym(FuncTyp(W, X))(RecSymbol(W, X))}
  
  def indFunction(f : => (FuncTerm[Term, Term]), 
      cnstrvars : List[(ConstructorFmly[Term], List[Any])], 
      W : Term => Typ[Term], 
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
  */
  /*
   * The symbolic object for defining a recursion function from a context.
   */
  case class RecSymbol(W : Typ[Term], X : Typ[Term])
  
  case class IndSymbol(W : Typ[Term], Xs : Term => Typ[Term])
  
}