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
    def constants : Seq[Term]
    
    def defns : Seq[Defn[A]]
    
    def defnEqualities : Seq[DefnEquality]
    
    def eliminate(x : Term): Term => Term
    
    val elim : Lambda[Term, Term]
    
    def lmbda(x : Term) = LambdaMixin(x, this)
    
    def cnst(x: Term) = KappaMixin(x, this)
    
    def dfn(x: Term, y: Term, local: Boolean = true) = DefnMixin(Defn(x, y), this, local)
    
    def dfneql(lhs: Term, rhs: Term, simp: Boolean = false) = DefnEqualityMixin(DefnEqual(lhs, rhs), this, simp)
  }
  
  object Context{
    case class empty[A]() extends Context[A]{
      
    	def constants : Seq[Term] = List.empty
    
    	def defns : Seq[Defn[A]] = List.empty
    
    	def defnEqualities : Seq[DefnEquality] = List.empty
    
    	def eliminate(x : Term): Term => Term = (x) => x
    	
    	val elim = idFunc
      
    }
    
    def apply[A] = empty[A]
  }
  

  
  case class LambdaMixin[+A](variable: Term, tail: Context[A]) extends Context[A]{
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
  }
  
  case class KappaMixin[+A](const : Term, tail: Context[A]) extends Context[A]{
    def constants = const +: tail.constants
    
    def defns : Seq[Defn[A]] = tail.defns
    
    def defnEqualities : Seq[DefnEquality] = tail.defnEqualities
    
    def eliminate(x : Term): Term => Term = tail.eliminate(x)
    
    val elim = tail.elim
  }
  
  case class DefnMixin[+A](dfn : Defn[A], tail: Context[A], local: Boolean = true) extends Context[A]{
    def constants : Seq[Term] = tail.constants
    
    def defns : Seq[Defn[A]] = dfn +: tail.defns
    
    def defnEqualities : Seq[DefnEquality] = dfn +: tail.defnEqualities
    
    def eliminate(x : Term): Term => Term = tail.eliminate(x)
    
    val elim  =  if (local) tail.elim andThen ((t : Term) => t.subs(dfn.lhs, dfn.rhs))
    								else tail.elim
    
  } 
  
  case class DefnEqualityMixin[+A](eqlty : DefnEquality, tail: Context[A], simp: Boolean = false) extends Context[A]{
    def constants : Seq[Term] = tail.constants
    
    def defns : Seq[Defn[A]] = tail.defns
    
    def defnEqualities : Seq[DefnEquality] = eqlty +: tail.defnEqualities
    
    def eliminate(x : Term): Term => Term = tail.eliminate(x)
    
    val elim  =  if (simp) tail.elim andThen ((t : Term) => t.subs(eqlty.lhs, eqlty.rhs))
    								else tail.elim
    
  }
}

object ContextOldCode{
  	
	trait ConstFmlyTmpl extends Term with AtomicObj{
	  val typ : Typ[Term]
	  
	  
	  type ObjTyp <: Term
	  
	  def map(Q: => Typ[Term]) : ConstFmlyTmpl
	  
	  def dmap(Q: Term => Typ[Term]) : Term => ConstFmlyTmpl
	  
	  def ->:(A : Typ[Term]) = ParamConstTmpl(A, this)
	  
	  def pushforward(f: Term => Term)(arg: ObjTyp) : Term
	}
	
	
	case class ConstTmpl(typ: Typ[Term]) extends ConstFmlyTmpl{
//	  val fullTyp = typ
	  
	  type ObjTyp = typ.Obj
	  
	  def map(Q: => Typ[Term]) = ConstTmpl(Q)
	  
	  def dmap(Q: Term => Typ[Term]) : Term => ConstFmlyTmpl = (obj) => ConstTmpl(Q(obj))
	  
	  def pushforward(f: Term => Term)(arg: ObjTyp) = f(arg)
	}
	
	case class ParamConstTmpl(base: Typ[Term], cod: ConstFmlyTmpl) extends ConstFmlyTmpl{
	  type baseObjTyp = Typ[baseObj]
	  
	  type baseObj = base.Obj
	  
	  val typ = FuncTyp[Term, Term](base.asInstanceOf[Typ[Term]], cod.typ)
	  
	  type ObjTyp = FuncObj[Term, Term]
	  
	  def push(func: ObjTyp)(arg: base.Obj): cod.ObjTyp = func(arg).asInstanceOf[cod.ObjTyp] 
	  
	  def pushforward(f: Term => Term)(func: ObjTyp) = {
	    val g = cod.pushforward(f) _
	    
	    val s : base.Obj => Term = (arg: base.Obj) => g(push(func)(arg))
	    
	    val ss : Term => Term = (arg) => s(arg.asInstanceOf[base.Obj])
	    
	    FuncDefn[Term,  Term](ss, base, cod.typ)
	  }
	  
	  def map(Q: => Typ[Term]): ParamConstTmpl = base ->: cod.map(Q)
	  
	  def dmap(Q: Term => Typ[Term]) : Term => ConstFmlyTmpl = {
	    case f: typ.Obj => 
	      val fibre: Term => ConstFmlyTmpl = (obj) => ConstTmpl(Q(f(obj.asInstanceOf[base.Obj])))
	      DepParamConstTmpl(typ, fibre)
	  } 
	}
	
	case class DepParamConstTmpl(base: Typ[Term], fibre: Term => ConstFmlyTmpl) extends ConstFmlyTmpl{
	  val typ = PiTyp(TypFamilyDefn(base, __, 0, ((obj : Term) => fibre(obj).typ)))
	  
	  type ObjTyp = DepFuncObj[Term,  Term]
	  
	  def push(func: ObjTyp)(arg: base.Obj) = {
	    val cod = fibre(arg)
	    func(arg).asInstanceOf[cod.ObjTyp]
	  }
	  
	  def pushforward(f: Term => Term)(func: ObjTyp) = {	    
	    val s : base.Obj => Term = (arg: base.Obj) => {
	      val cod = fibre(arg)
	      val g = cod.pushforward(f) _
	      g(push(func)(arg).asInstanceOf[cod.ObjTyp])
	    }
	    
	    val ss : Term => Term = (arg) => s(arg.asInstanceOf[base.Obj])
	    
	    def fibretyp(arg: Term) = fibre(arg).typ
	    
	    DepFuncDefn[Term,  Term](ss, base, TypFamilyDefn(base, __, 0, fibretyp _))
	  }
	  
	  def map(Q: => Typ[Term]): DepParamConstTmpl = DepParamConstTmpl(base, (obj) => fibre(obj).map(Q))
	  
	   def dmap(Q: Term => Typ[Term]) : Term => ConstFmlyTmpl = {
	    case f: typ.Obj => 
	      val fibre: Term => ConstFmlyTmpl = (obj) => ConstTmpl(Q(f(obj)))
	      DepParamConstTmpl(typ, fibre)
	  } 
	}
	
	
	// Inductive types can be constructed from a context.
	
	trait ContextElem[+X <: Term]{
	  val constants: List[X]
	  
	  val variables: List[X]
	  
	  val dom: Typ[Term]
	  
	  def exptyp(tp: Typ[Term]) : Typ[Term] 
	  
	  def fulltyp(tp: Typ[Term]) : Typ[Term] 
	  
	  
	  def get(value: Term): Term
	  
	  def subs(x: Term, y: Term): ContextElem[X]
	  

	}
	
	trait Context[+X <: Term] extends ContextElem[X]{
	  /*
	   * The codomain for the multi-function given by the context.
	   */
	  val target: Typ[Term]
	  
	  /*
	   * The type of the object : a multivariate function : that comes from the context.
	   */
	  val typ : Typ[Term]
	  
	  type ObjTyp = typ.Obj
	  
	  
	  def /\:[U <: Term : TypeTag](obj: U) = ContextSeq(LambdaContext(obj), this) 
	  
	  def |:[U <: Term : TypeTag](obj: U) = ContextSeq(KappaContext(obj), this)
	  	  
	  def subs(x: Term, y: Term): Context[X]
	  
	  def recContext(f : Term => Term): Context[X] 
	  
	  def patternMatch(obj: Term) : Option[(Term, List[Term])]
	  
	  object Pattern{
	    def unapply(obj: Term): Option[(Term, List[Term])] = patternMatch(obj)
	  }
	  
	  /*
	   * This can be applied to the ctx being a recursive/inductive one, besides this.
	   */
	  def patternDefn(ctx: Context[Term], fn: Term, obj : Term): PartialFunction[Term, Term] = {
	    case Pattern(`fn`, l) => Context.fold(ctx, l)(obj)
	  }
	  
	}
	
	
	object Context{
	  def instantiate[X <: Term : TypeTag](x: X, y: X): Context[X] => Context[X] = {
	    case ContextSeq(LambdaContext(`x`), tail)  => ContextSeq(KappaContext(y), tail.subs(x, y))
	    case ContextSeq(head, tail) => 
	      val inst = instantiate(x,y)
	      ContextSeq(head.subs(x,y), inst(tail))
	    case ctx => ctx
	  }
	  
	  def instantiateHead[X <: Term : TypeTag](y: Term) : Context[X] => Context[X] = {
	    case ContextSeq(LambdaContext(x), tail) => tail subs (x,y)
	    case ContextSeq(head, tail) => 
	      val inst = instantiateHead(y)
	      ContextSeq(head, inst(tail))
	    case ctx => ctx
	  }
	  
	  def apply(dom: Typ[Term]) = simple(dom)
	  
	  def fold[X <: Term : TypeTag](ctx: Context[X], seq: Seq[Term])(obj : Term) : Term = {
			  if  (seq.isEmpty) ctx.get(obj) 
			  else {val inst = instantiateHead[X](seq.head)
			    fold(inst(ctx), seq.tail)(obj)
			  }
	  }
	  
	  def symbpattern[A, X <: Term](symbs: List[A], ctx: Context[X]) : List[Term] = ctx match {
	    case ContextSeq(head, tail) => head.cnst.typ.symbObj(symbs.head) :: symbpattern(symbs.tail, tail) 
	    case _ => List()
	  }
	  
	  def recsymbpattern(f: Term => Term, Q : Typ[Term], symbs : List[Term], ctx : Context[ConstFmlyTmpl]) : Context[Term] = ctx match {
	    case ContextSeq(LambdaContext(a), tail) => 
	      val b = a map (Q)
	      ContextSeq(LambdaContext(a.typ.symbObj(symbs.head)), 
	          ContextSeq(KappaContext(f(a.typ.symbObj(symbs.head))),recsymbpattern(f, Q, symbs.tail,tail)))
	    case cntx => cntx
	  }
	  
	  case class simple[X <: Term](dom: Typ[Term]) extends Context[X]{
	    val target = dom
	    
	    val typ = dom
	    
	    val constants = List()
	    
	    val variables = List()
	    
	    def exptyp(tp: Typ[Term]) : Typ[Term] = dom
	  
	    def fulltyp(tp: Typ[Term]) : Typ[Term] = dom
	  
	    def get(value: Term): Term = value
	  
	    def subs(x: Term, y: Term): Context[X] = simple(dom)
	    
	    def patternMatch(obj: Term) = if (obj.typ == typ) Some((obj, List())) else None
	    
	    //Should be applied to an appropriate induced map
	    def recContext(f : Term => Term): Context[X] = this
	    
	  }
	}
	
	
	
	trait AtomicContext[+X <: Term] extends ContextElem[X]{
	  val cnst: X
	  
	  val dom = cnst.typ.asInstanceOf[Typ[Term]]
	  
	  val constants = List(cnst)
	 
	  
	  def fulltyp(tp: Typ[Term]) = FuncTyp[Term,  Term](dom , tp)
	  
	  def subs(x: Term, y: Term): AtomicContext[X]
	}
	
	case class ContextSeq[+X <: Term : TypeTag](head: AtomicContext[X], tail: Context[X]) extends Context[X]{
	  val target = tail.target
	  
	  val typ = head.exptyp(tail.typ)
	  
	  lazy val constants = head.cnst :: tail.constants
	  
	  lazy val variables = head.variables ::: tail.variables
	  
	  val dom = head.dom
	  
	  def get(value: Term) = head.get(tail.get(value))
	  
	  def exptyp(tp: Typ[Term]) = head.exptyp(tail.exptyp(tp))
	  
	  def fulltyp(tp: Typ[Term]) = head.exptyp(tail.exptyp(tp))
	  
	  def subs(x: Term, y: Term) = ContextSeq(head.subs(x,y), tail.subs(x, y))
	  
	  /*
	   * The types should be checked
	   */
	  def patternMatch(obj: Term) : Option[(Term, List[Term])] = head match {
	    case l : LambdaContext[_] => 
	      tail.patternMatch(obj) flatMap ((xl) =>  xl._1 match{
	        case applptnterm(func, arg) if (func.dom == dom) => Some((func, arg :: xl._2))
	        case _ => None
	      }	      
	      )
	    case _ => tail.patternMatch(obj)
	  }
	  
	  def recContext(f : Term => Term): Context[X] = head match {
	    case _ : KappaContext[_] => this
	    case l: LambdaContext[_] => ContextSeq(l, ContextSeq(KappaContext(f(l.cnst).asInstanceOf[X]), tail))
	  }
	}
	
	case class LambdaContext[U <: Term  : TypeTag](cnst: U) extends AtomicContext[U]{
	  def export(value: Term) : Term => Term =  (obj) => value.subs(cnst, obj)	  
	  
	  def get(value: Term) = Lambda(cnst, value)
	  
	  def exptyp(tp: Typ[Term]) = FuncTyp[Term,  Term](dom, tp)
	  
	  val variables = List(cnst)
	  
	  def subs(x: Term, y: Term) = LambdaContext(cnst.subs(x, y).asInstanceOf[U])
	}
	
	case class KappaContext[U <: Term : TypeTag](cnst: U) extends AtomicContext[U]{
	  def export(value: Term) : Term => Term = _ => value
	  
	  def get(value: Term) = value
	  
	  def exptyp(tp: Typ[Term]) = tp
	  
	  val variables = List()
	  
	  def subs(x: Term, y: Term) = LambdaContext(cnst.subs(x, y).asInstanceOf[U])
	}
	
	
	trait DefnPattern{
	  /*
	   * This is the type of the object defined by the pattern
	   */
	  val typ : Typ[Term]
	  
	  
	  val fold : Term
	  
	  val contextPrep : Context[Term] => Context[Term]
	  
	  lazy val context = contextPrep(Context.simple(typ))
	  
	   
	  def recContextPrep(f :Term => Option[Term]) : Context[Term] => Context[Term]
	  
	  def recContext(f :Term => Option[Term]) = recContextPrep(f)(Context.simple(typ))
	  
	  
	}
	
	object DefnPattern{
	  
	}
	
	case class Const(head: Term) extends DefnPattern{
	  val typ = head.typ.asInstanceOf[Typ[Term]]
	  
	  val fold = head
	  
	  lazy val contextPrep : Context[Term] => Context[Term] = (ctx) => ContextSeq(LambdaContext(head), ctx) 
	  
	  def recContextPrep(f :Term => Option[Term]) : Context[Term] => Context[Term] = (ctx) =>
	    f(head) match {
	      case Some(fx) => ContextSeq(LambdaContext(head), ContextSeq(KappaContext(fx), ctx))
	      case None => ContextSeq(LambdaContext(head), ctx)
	    }
	  
	}
	
	/*
	 * This also includes the case of dependent functions.
	 */
	case class FuncPattern(head: DefnPattern, tail: DefnPattern) extends DefnPattern{
	  
	  val typ = fold.typ.asInstanceOf[Typ[Term]]
	  
	  val fold  = head.fold match {
	    case f : FuncObj[d,_] if f.dom == tail.typ => f(tail.fold.asInstanceOf[d])
	    case f : FuncTerm[d, _] if f.dom == tail.typ => f(tail.fold.asInstanceOf[d])
	  } 
	  
	  
	  lazy val contextPrep : Context[Term] => Context[Term] = (ctx) => head.contextPrep(tail.contextPrep(ctx)) 
	  
	  def recContextPrep(f :Term => Option[Term]) : Context[Term] => Context[Term] = (ctx) => 
	    head.recContextPrep(f)(tail.recContextPrep(f)(ctx))
	 
	}
	
	case class CasesSymb[U <: Term](cases: List[Term], typ : Typ[U]) extends Term{
	  def subs(x: Term, y: Term) = CasesSymb(cases map (_.subs(x,y)), typ.subs(x,y))
	}
	
	case class UnionPattern(ps: List[DefnPattern]) extends DefnPattern{
	  val typ = ps.head.typ
	  
	  val fold = typ.symbObj(CasesSymb(ps map ((pat) => (pat.fold)), typ))
	  
	  val contextPrep : Context[Term] => Context[Term] = (ctx) => ctx
	  
	  def recContextPrep(f :Term => Option[Term]) : Context[Term] => Context[Term] = (ctx) => ctx
	}
	
	
	// Avoid using type patterns, instead use only defn patterns with blanks where no object is needed.
	
	trait TypPattern{
	  /*
	   * The pattern does not determine the type in the case of dependent types
	   */
//	  val typ : Typ[Term]
	
	}
	
	object TypPattern{
	  val fromConstFmlyTmpl: ConstFmlyTmpl => TypPattern = {
	    case ConstTmpl(tp : Typ[Term]) => ConstTyp(tp)
	    case ParamConstTmpl(head, tail) => FuncTypPattern(ConstTyp(head), fromConstFmlyTmpl(tail))
	    case DepParamConstTmpl(head, tail) => DepFuncTypPattern(ConstTyp(head), (obj) =>  fromConstFmlyTmpl(tail(obj)))
	    
	  }
	}
	
	case class ConstTyp(head : Typ[Term]) extends TypPattern{
	//  val typ = head

	 
	}
	
	case class FuncTypPattern(head: TypPattern, tail: TypPattern) extends TypPattern{
	  
	//  val typ = head.typ match {
	//    case f : FuncTyp[_, _, _] if f.dom == tail.typ => f.codom.asInstanceOf[Typ[Term]]  
	//  } 
	  
	}
	
	case class DepFuncTypPattern(head: TypPattern, tail: Term => TypPattern) extends TypPattern

	
	
	trait InductSeq{
	  val typ: Typ[Term]
	  
	  val pattern: TypPattern
	  
	  def map(Q : => Typ[Term]): InductSeq
	}
	
	/*
	 * This is for constant sequences not ending in the type W being defined
	 */
	case class CnstInductSeq(fm : ConstTmpl) extends InductSeq{
	  val typ = fm.typ
	  
	  val pattern = TypPattern.fromConstFmlyTmpl(fm)
	  
	  def map(Q : => Typ[Term]) = this
	}
	
	/*
	 * These are families ending in the type being defined
	 */
	case class TargetInductSeq(w: ConstFmlyTmpl) extends InductSeq{
	  val typ = w.typ
	  
	  val pattern = TypPattern.fromConstFmlyTmpl(w)
	  
	  def map(Q : => Typ[Term]) = TargetInductSeq(w map Q)
	}
	
	case class InductSeqCons(head: ConstFmlyTmpl, tail: InductSeq) extends InductSeq{
	  val typ = FuncTyp[Term,  Term](head.typ, tail.typ) 
	  
	  val pattern = FuncTypPattern(TypPattern.fromConstFmlyTmpl(head), tail.pattern)
	  
	  def map(Q : => Typ[Term]) = InductSeqCons(head map Q, tail map Q)
	}
	
	case class InductSeqDepCons(head: ConstFmlyTmpl, tail : Term => InductSeq) extends InductSeq{
	  val typ = PiTyp[Term,  Term](TypFamilyDefn(head.typ, __, 0, (obj) => tail(obj).typ))
	  
	  val pattern = DepFuncTypPattern(TypPattern.fromConstFmlyTmpl(head), (obj) => tail(obj).pattern)
	  
	  def map(Q : => Typ[Term]) = InductSeqDepCons(head map Q, (obj) => tail(obj) map Q)
	}
	
	case class InductCons(name: Term, constyp : InductSeq){
	  def map(Q: => Typ[Term]) = InductCons(name, constyp map Q)
	  
	  val obj = constyp.typ.symbObj(name)
	}
	
	class InductiveTyp(consPatterns : List[InductCons]) extends LogicalSTyp{
	  lazy val cons = consPatterns map (_.map(this).obj)
	}
	
	
	
	
	object AlsoOld{
	
	trait ConstructorPattern{
	  val typ : Typ[Term]
	  
	  /*
	   * closed means no free variables
	   */
	  val isClosed : Boolean
	  
	  val dom: Typ[Term]
	  
	}
	
	object ConstructorPattern{
	  case class Overfull(typ : Typ[Term]) extends ConstructorPattern{
	    val isClosed = false
	    
	    val dom = typ
	  }
	}
	
	case class SimpleConstructor(typ: Typ[Term]) extends ConstructorPattern{
	  val isClosed = true
	  
	  val dom = Unit
	}
	
	case class FuncConstructor(dom : Typ[Term], tail: ConstructorPattern) extends ConstructorPattern{
	  val typ = FuncTyp[Term,  Term](dom, tail.typ)
	  
	  val isClosed = false	  	  
	}
	
	class Constructor(pattern: ConstructorPattern) extends RecDefnPattern{
	  val typ = pattern.typ
	  
	  val isClosed = pattern.isClosed
	  
	  val isValid = true
	  
	  val tail   = pattern match {
	    case f : FuncConstructor => f.tail
	    case _ => ConstructorPattern.Overfull(typ)
	  }
	  
	  val argsTotal = true
	  
	  val head = this
	}
	
	/*
	 * A typed pattern for defining objects, given recursively except for its head. Can also have constants in place of the variables
	 */
	trait DefnPattern{
	  val typ : Typ[Term]
	  
	  /*
	   * totality of a definition based on this pattern
	   */
	  val isTotal : Boolean
	  
	  /*
	   * Checks if the types are correct and we have not applied where there is no free vairable
	   */
	  val isValid : Boolean
	}
	
	trait RecDefnPattern extends DefnPattern{
	  val isClosed : Boolean
	  
	  val tail: ConstructorPattern
	  
	  val isTotal = false
	  
	  val argsTotal : Boolean
	  
	  val head: Constructor
	}
	
	
	case class Var(obj: Term) extends DefnPattern{
	  val typ = obj.typ.asInstanceOf[Typ[Term]]
	  
	  val isTotal = true
	  
	  val isValid = true
	}
	
	case class VarFamily(name: Term, tmpl: ConstFmlyTmpl) extends DefnPattern{
	  
	  val typ = tmpl.typ
	  
 
	  
	  val isTotal = true
	  
	  val isValid = true
	}
	
	case class ApplyPattern(func: RecDefnPattern, arg: DefnPattern) extends RecDefnPattern{
	  val typ = func.tail.typ
	  
	  val argsTotal = arg.isTotal && func.argsTotal
	  
	  val tail = func.tail match {
	    case f : FuncConstructor => f.tail
	    case _ => ConstructorPattern.Overfull(typ)
	  }
	  
	  val isClosed = func.tail.isClosed
	  
	  val isValid = arg.isValid && func.isValid && (func match {
	    case f : FuncConstructor => arg.typ == f.dom 
	    case _ => false
	  }
	  )
	  
	  val head = func.head
	}
	
	trait InductiveTypLike extends Typ[Term]{self =>
	  val constrs: Set[Constructor]
	  
	  case class AggregatePattern(ps : Set[RecDefnPattern]) extends DefnPattern{
	    val typ =self
	    
	    val mtch = ps.map(_.head) == constrs
	    
	    val isValid = (mtch /: ps.map(_.isValid))(_ && _)
	    
	    val isTotal = (mtch /: ps.map(_.argsTotal))(_ && _)
	  }
	}
	
	
	

//	def totalPatterns(defs : List[DefnPattern]) : Boolean
	
	
	
	
	class ConstructorDefn(defn : Typ[Term] => Context[ConstFmlyTmpl], target: => Typ[Term]){
	  lazy val context = defn(target)
	}
	
	class InductiveTyp(constructors : List[ConstructorDefn]){
	  lazy val constructorContexts = constructors map (_.context)
	  
	  // Pattern matching functions are defined in terms of maps from Constructors.
	  class Constructor(ctx: Context[ConstFmlyTmpl]){
	    // have simple/recursive/inductive contexts and definitions here
	  }
	  
	  type SimpleDefn = Constructor => Term
	  
	  // Deprecate
	  def namedConstructors[A](syms: List[A]) = for ((n, t) <- syms zip constructorContexts) yield (t.typ.symbObj(n))
	}
	}
	object Old{
	
	class InductiveTyp(cnstrFns: List[Typ[Term] => Context[ConstFmlyTmpl]]) extends LogicalSTyp{self =>
	  lazy val cnstrCtxs = cnstrFns map (_(this))
	  
	  class Constructor(val ctx: Context[ConstFmlyTmpl]){
	    def apply(pattern: List[Term]) = ObjPattern(this, pattern)
	  }
	  
	  
	  val constructors = cnstrCtxs map (new Constructor(_))
	  
	  /*
	   * The patterns in contexts should replace these
	   */
	  case class ObjPattern(cnst: Constructor, pattern: List[Term]) extends Term{
	    val typ = self
	    
	    def subs(x: Term, y: Term) = ObjPattern(cnst, pattern map (_.subs(x,y)))
	  }
	  
	  /* 
	   * There must be a match between the lambda types of the base and ctx. One should be using the patterns in contexts.
	   */
	  def patternMatch(base: Constructor, ctx: Context[ConstFmlyTmpl], value: Term): PartialFunction[Term, Term] = {
	    case ObjPattern(`base`, pattern) => Context.fold(ctx, pattern)(value) 
	  }
	}
	
	
	// May not be needed
	trait InductiveConstructor[+A]{
	  val sym: A
	}
	
	object InductiveConstructor{
	  case class const[A](sym: A)  extends InductiveConstructor[A]
	}
	
	case class ToW[A, B](sym: A, head: Typ[Term] => ConstFmlyTmpl, tail: InductiveConstructor[B]) extends InductiveConstructor[A]
	
	case class IndctParam[A, B](sym: A, head: Typ[Term], tail: InductiveConstructor[B]) extends InductiveConstructor[A]
	
	// Should also add dependent function
	}
}