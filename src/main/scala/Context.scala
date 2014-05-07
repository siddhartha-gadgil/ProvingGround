package provingGround

import provingGround.HoTT._
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}

object Context{
  trait DefnEquality{
    val lhs : Term
    val rhs: Term
    
    def map(f: Term => Term): DefnEquality
    
  }
  
  case class DefnEqual(lhs: Term, rhs: Term) extends DefnEquality{
    def map(f : Term => Term) = DefnEqual(f(lhs), f(rhs))
  }
  
  case class Defn[+A](lhsname : A, typ : Typ[Term], rhs: Term) extends DefnEquality{    
    val lhs = typ.symbObj(lhsname)
    
    def map(f : Term => Term, F : Typ[Term]=> Typ[Term]) = Defn(lhsname, F(typ), f(rhs))
    
    def map(f: Term => Term) = Defn(lhsname, f(rhs))
  }
  
  object Defn{
    def infer[A](name: A, rhs : Term) : Defn[A] = Defn(name, rhs.typ, rhs)
    
    def apply[A](name: A, rhs : Term) = infer(name, rhs)
  }
  
  trait Context[+A]{
    val typ: Typ[Term]
    
    type CtxType <: Term
    
    def constants : Seq[Term]
    
    def defns : Seq[Defn[A]]
    
    def defnEqualities : Seq[DefnEquality]
    
    def eliminate(x : Term): Term => Term
    
    val elim : Term => Term
    
    def lmbda(x : Term) = LambdaMixin(x, this)
    
    def cnst(x: Term) = KappaMixin(x, this)
    
    def dfn(x: Term, y: Term, local: Boolean = true) = DefnMixin(Defn(x, y), this)
    
    def dfneql(lhs: Term, rhs: Term, simp: Boolean = false) = DefnEqualityMixin(DefnEqual(lhs, rhs), this)
    
    def globaldfn(x: Term, y: Term, local: Boolean = true) = GlobalDefnMixin(Defn(x, y), this)
    
    def simpeql(lhs: Term, rhs: Term, simp: Boolean = false) = SimpEqualityMixin(DefnEqual(lhs, rhs), this)
  }
  
  object Context{
    case class empty[U <: Term : TypeTag](typ : Typ[U]) extends Context[U]{
    	type CtxType = Term
      
      
    	def constants : Seq[Term] = List.empty
    
    	def defns : Seq[Defn[U]] = List.empty
    
    	def defnEqualities : Seq[DefnEquality] = List.empty
    
    	def eliminate(x : Term): Term => Term = (x) => x
    	
    	val elim = (t: Term) => t
      
    }
    
    def apply[U <: Term : TypeTag](typ: Typ[U]) = empty[U](typ)
  }
  

  
  
  case class LambdaMixin[+A](variable: Term, tail: Context[A], dep: Boolean = false) extends Context[A]{
    type CtxType = FuncTerm[Term, tail.CtxType]
    
    def constants = variable +: tail.constants
    
    def eliminator(y : Term) = Lambda(variable, y)
    
    def defns : Seq[Defn[A]] = tail.defns map ((dfn) => dfn.map(optlambda(variable)))
    
    def defnEqualities : Seq[DefnEquality] = tail.defnEqualities map ((dfn) => dfn.map(optlambda(variable)))
    
    def eliminate(x : Term): Term => Term = (y) => 
      x match {
      case `variable` => Lambda(x,tail.eliminate(x)(y))
      case _ => tail.eliminate(x)(y)
    }
      
    val elim = Lambda(variable, tail.elim(y))
    
    // If tail.typ is a function of variable, we get a Pi-type instead.
    val typ = if (dep) {
      val fibre = (t : Term) => tail.typ subs (x, t)
	    
	    val family = typFamilyDefn[Term, Term](variable.typ, tail.typ.typ, fibre)
	    PiTyp(family)
    }
    else FuncTyp(variable.typ, tail.typ)
  }
  
  case class KappaMixin[+A](const : Term, tail: Context[A]) extends Context[A]{
    type CtxType = tail.CtxType
    
    def constants = const +: tail.constants
    
    def defns : Seq[Defn[A]] = tail.defns
    
    def defnEqualities : Seq[DefnEquality] = tail.defnEqualities
    
    def eliminate(x : Term): Term => Term = tail.eliminate(x)
    
    val elim = tail.elim
    
    val typ = tail.typ
  }
  
  case class DefnMixin[+A](dfn : Defn[A], tail: Context[A], dep: Boolean = false) extends Context[A]{
    type CtxType = FuncTerm[Term, tail.CtxType]
    
    def constants : Seq[Term] = tail.constants
    
    def defns : Seq[Defn[A]] = dfn +: tail.defns
    
    def defnEqualities : Seq[DefnEquality] = dfn +: tail.defnEqualities
    
    def eliminate(x : Term): Term => Term = tail.eliminate(x)
    
    val elim  =  tail.elim andThen ((t : Term) => t.subs(dfn.lhs, dfn.rhs))
    								
    
    val typ = if (dep) {
      val fibre = (t : Term) => tail.typ subs (x, t)
	    
	    val family = typFamilyDefn[Term, Term](dfn.lhs.typ, tail.typ.typ, fibre)
	    PiTyp(family)
    }
    else FuncTyp(dfn.lhs.typ, tail.typ)
  } 
  
  case class GlobalDefnMixin[+A](dfn : Defn[A], tail: Context[A]) extends Context[A]{
    type CtxType = tail.CtxType
    
    def constants : Seq[Term] = tail.constants
    
    def defns : Seq[Defn[A]] = dfn +: tail.defns
    
    def defnEqualities : Seq[DefnEquality] = dfn +: tail.defnEqualities
    
    def eliminate(x : Term): Term => Term = tail.eliminate(x)
    
    val elim  = tail.elim
    
    val typ = tail.typ								
  }
  
  case class DefnEqualityMixin[+A](eqlty : DefnEquality, tail: Context[A]) extends Context[A]{
    type CtxType = tail.CtxType
    
    def constants : Seq[Term] = tail.constants
    
    def defns : Seq[Defn[A]] = tail.defns
    
    def defnEqualities : Seq[DefnEquality] = eqlty +: tail.defnEqualities
    
    def eliminate(x : Term): Term => Term = tail.eliminate(x)
    
    val elim  = tail.elim
    
    val typ = tail.typ								
  }
  
  case class SimpEqualityMixin[+A](eqlty : DefnEquality, tail: Context[A], dep : Boolean = false) extends Context[A]{
    type CtxType = FuncTerm[Term, tail.CtxType]
    
    def constants : Seq[Term] = tail.constants
    
    def defns : Seq[Defn[A]] = tail.defns
    
    def defnEqualities : Seq[DefnEquality] = eqlty +: tail.defnEqualities
    
    def eliminate(x : Term): Term => Term = tail.eliminate(x)
    
    val elim  =  tail.elim andThen ((t : Term) => t.subs(eqlty.lhs, eqlty.rhs))
    								
    val typ = if (dep) {
      val fibre = (t : Term) => tail.typ subs (x, t)
	    
	    val family = typFamilyDefn[Term, Term](eqlty.lhs.typ, tail.typ.typ, fibre)
	    PiTyp(family)
    }
    else FuncTyp(eqlty.lhs.typ, tail.typ) 							
  }

    /*
   * Change in context for a TypPatn (i.e., simple pattern ending in W).
   * Should allow for dependent types when making a lambda.
   */
  def recContextChange[A, U <: Term](f : => (FuncTerm[Term, Term]), ptn : TypPtn[U], varname : A, W : Typ[Term], X : Typ[Term]) : Context[Term] => Context[Term] = {
    val x = ptn(X).symbObj(varname)
    ctx => ctx lmbda(x) lmbda(ptn.induced(W, X)(f)(x))
  }
  
    /*
   * Change in context (as function on W) for Induction - check
   */
  def indContextChange[A, U <: Term](f : => (FuncTerm[Term, Term]), ptn : TypPtn[U], varname : A, W : Typ[Term], Xs : Term => Typ[Term]) : (Term => Context[Term]) => (Term => Context[Term]) = {
    val xs =  (t : Term) => ptn(Xs(t)).symbObj(varname)
    ctxs => (t : Term) => ctxs(t) lmbda(xs(t)) lmbda(ptn.inducedDep(W, Xs)(f)(xs(t)))
  }
  
  /*
   * The context, recursively defined, for the constructor of a single context.
   * This is also a change, to be applied by default to the empty context of the target type.
   */
  @annotation.tailrec def cnstrRecContext[A](f : => (FuncTerm[Term, Term]), 
      ptn : PolyPtn, varnames : List[A], 
      W : Typ[Term], 
      X : Typ[Term])(ctx: Context[Term] = Context.empty[Term](X)) : Context[Term] = {
    ptn match {
      case tp: TypPtn[_] => recContextChange(f, tp, varnames.head, W, X)(ctx)
      case FuncPtn(tail, head) => cnstrRecContext(f, head, varnames.tail, W, X)( recContextChange(f, tail, varnames.head, W, X)(ctx))
      case CnstFncPtn(tail : Typ[_], head) => cnstrRecContext(f, head, varnames.tail, W, X)( ctx lmbda(tail.symbObj(varnames.head)))
      case DepFuncPtn(tail, headfibre , _) =>
        val x = tail(X).symbObj(varnames.head)
        cnstrRecContext(f, headfibre(x), varnames.tail, W, X)( ctx lmbda(x))
      case CnstDepFuncPtn(tail, headfibre , _) =>
        val x = tail.symbObj(varnames.head)
        cnstrRecContext(f, headfibre(x), varnames.tail, W, X)( ctx lmbda(x))
    }
    
    
  }
  
  def addConstructor(f : => (FuncTerm[Term, Term]), 
      cnstr : Constructor, varnames : List[Any], 
      W : Typ[Term], 
      X : Typ[Term]) : Context[Any] => Context[Any] = ctx => {
        val cnstrctx = cnstrRecContext(f, cnstr.pattern, varnames, W, X)()
        val name = cnstrctx.typ.symbObj(cnstr.cons)
      		ctx lmbda (name)
      }
      
  def recContext(f : => (FuncTerm[Term, Term]), 
      cnstrvars : List[(Constructor, List[Any])], 
      W : Typ[Term], 
      X : Typ[Term]) ={
    val add : (Context[Any], (Constructor, List[Any])) => Context[Any] = (ctx, cnvr) =>
      addConstructor(f, cnvr._1, cnvr._2, W, X)(ctx)
      val empty : Context[Any] = Context.empty(FuncTyp(W, X))
    (empty /: cnstrvars)(add)
  }
  
  /*
   * The symbolic object for defining a recursion function from a context.
   */
  case class RecSymbol(W : Typ[Term], X : Typ[Term])
  
}

