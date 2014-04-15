package provingGround

import scala.language.implicitConversions 
import scala.util._
import scala.language.existentials
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}


// To Do:
//
// * A cleaner substitution, parametrised by type.
// * Properly abstracted Formal Function Application
// * Inductive type trait, with Sigma type etc. having this.
// * Clean up universes.
//


/** The Homotopy type theory objects, types and utility functions
 *  
 */
object HoTT{
  /** Abstract object */
    trait Term {
      /** 
       * Gives limited information on types when the types are universes, so should not be used in this case.
       * The type `LogicalUniverse' should only be read as some universe.
       * So avoid making this judgement. 
       */
        def typ: Typ[Term]
        
    
    def subs(x: Term, y: Term): Term
    }
    
    /*
     * Objects with simple substitution.
     */
    trait AtomicObj extends Term{
      def subs(x: Term, y: Term) = if (x==this) y else this
    }
   
    
    /*
     * Sub-objects of the given one
     */
    val subObjs: Term => List[Term] ={
      case pair: AbsPair[_,_] => subObjs(pair.first) ::: subObjs(pair.second)
      case applptnterm(func, arg) => subObjs(func) ::: subObjs(arg)
//      case obj: FormalApplication[_,_,_] => subObjs(obj.arg) ::: subObjs(obj.func)
      case eq: IdentityTyp[_] => subObjs(eq.lhs) ::: subObjs(eq.rhs)
      case fnTyp : FuncTyp[_, _, _] => subObjs(fnTyp.dom) ::: subObjs(fnTyp.codom) 
      case Lambda(variable, value) => subObjs(value) map (lambda(variable))
      case PiTyp(fibers) => subObjs(fibers)
      case SigmaTyp(fibers) => subObjs(fibers)
//      case fx: FormalAppl[_,_] => subObjs(fx.func) ::: subObjs(fx.arg)
      
      /* Above case should cover this
      case sym: Symbolic[_] =>
        sym.name match {
          case fnsym : FormalApplication[_, _,_] => subObjs(fnsym.arg) ::: subObjs(fnsym.func)
          case fnsym : FormalDepApplication[_, _,_] => subObjs(fnsym.arg) ::: subObjs(fnsym.func)
          case _ => List(sym)
        }
        * /
        */
      case obj: Term => List(obj)
    } 
    
    /** HoTT Type;
     *  The compiler knows that objects have scala-type extending U.
     *  In particular, we can specify that the objects are types, types of types etc.
     *  We should handle covariance better and move methods to this.
     */
    trait Typ[+U <: Term] extends Term{
    	/** scala type of objects with this HoTT-type */
        type Obj <: U
        
        /** A symbolic object with specified HoTT type, by default 'this', and with scala-type Obj*/
        def symbObj[A, W<: Term](name: A, tp: Typ[W] = this): Obj
        
        /** Make symbolic object */
        def ::[A](name:A) = symbObj(name) 
        
        def subs(x: Term, y: Term) : Typ[U]
        
        // Template for a method allowing covariance. 
        def -->:[W <: Term : TypeTag, V<: Typ[W], UU >: U <: Term : TypeTag](that : V) = FuncTyp[W, V, UU](that, this)
        
        def :::(that: Term) = {require(that.typ == this, "coercion to the wrong type"); that.asInstanceOf[this.Obj]}
    }
    
    
    trait AtomicTyp[U <: Term] extends Typ[U]{
      def subs(x: Term, y: Term) : Typ[U] = if (x == this) y.asInstanceOf[Typ[U]] else this
    }
    


    /** traits that are given by name;
     *  does not include, for instance, pairs each of whose instance is given by a name;
     *  same considerations for functions etc.
     */
    trait Symbolic[A]{
      val name: A
      override def toString = name.toString
    }
    
    def sameName[A](sym: A): Term => Boolean = {
      case obj : Symbolic[_] =>
        obj.name == sym
      case _ => false
    }
    
    /** Constructing symbolic objects that are Term but no more refined*/
    case class SymbObj[A, +U<: Term](name: A, typ: Typ[U]) extends Term with Symbolic[A]{
      override def toString = name.toString+" : "+typ.toString
      
      def subs(x: Term, y: Term) = this match {
        case applptnterm(func, arg) => 
          func.subs(x,y)(arg.subs(x, y))
        case _ => this
      }
    } 
    
    /** Symbolic types, which the compiler knows are types.
     *  The base tells the scala type of objects and gives a factory for symbolic objects of this scala type.
     */
    case class SymbTyp[A, U<:Term, T<: Term](name: A, univ: Typ[T], base: Typ[U]) extends Typ[U] with Symbolic[A]{
      lazy val typ = univ
      
      type Obj = base.Obj
      
      def symbObj[B, W <: Term](name: B, tp: Typ[W] ) = base.symbObj(name, tp)
      
      override def toString = name.toString+" : "+typ.toString
      
      def elem = this
      
      def subs(x: Term, y: Term) = name match {
        case fa: FormalTypAppl[w, U] => 
          fa.func.subs(x, y)(fa.arg.subs(x,y).asInstanceOf[w])
        case _ => this
      }
    }
    
    case class SymbLogicTyp[A](name: A) extends LogicalSTyp with Symbolic[A]{
      override def toString = name.toString+" : "+typ.toString
    }
    
    


    /** HoTT types with underlying scala type Term.
     *  They belong to the LogicalUniv
     */
    trait LogicalTyp extends AtomicTyp[Term]{
      
//      type Obj = Term
      
      lazy val typ = new LogicalUniv
      
//      def symbObj[A, W <: Term](name: A, tp: Typ[W]): Term = SymbObj(name, tp)
      
      /** returns function type A -> B, not used in main code */
      def -->[U <: Term : TypeTag](that: Typ[U]) = FuncTyp[Term, LogicalTyp, U](this, that)
 
      def ~>[U <: Term : TypeTag](fibers: TypFamily[Term, LogicalTyp, U]) = PiTyp(fibers)
      
      def ~~>(fibers : Term => LogicalTyp) = ~>(TypFamilyDefn(this, fibers))
    }
    
    trait SmallTyp extends LogicalTyp{
     
      
    }
    
    class LogicalSTyp extends LogicalTyp{
      type Obj = Term
      
      def symbObj[A, W <: Term](name: A, tp: Typ[W]): Term = SymbObj(name, tp)
    }
    
    /** Universes. Can be just a type so far */
    trait Univ[U<:Term] extends Typ[Typ[U]] 
    
    /** Inductive construction of the next universe */
    case class NextUniv[U<: Term](base: Typ[U]) extends Univ[U] with AtomicTyp[Typ[U]]{
      lazy val typ = NextUniv[Typ[U]](this)
      
      type Obj = Typ[U]
      
      def symbObj[A, W<: Term](name: A, tp: Typ[W])= SymbTyp(name, tp, base)
      
      val __ = symbObj(None)
    }
    
    /** The first universe, consisting of logical types */
    class LogicalUniv extends Univ[Term] with Typ[LogicalTyp]{
      lazy val typ = NextUniv[Term](this)
      
      def -->[U <: Term : TypeTag](that: Typ[U]) = FuncTyp[LogicalTyp, LogicalUniv, U](this, that)
      
      def ~>[U <: Term : TypeTag](fibers: TypFamily[LogicalTyp, LogicalUniv, U]) = PiTyp(fibers)

      // For the below, must generalise the domains of type families beyond logicaltyp
//      def ~~>(fibers : Term => LogicalTyp) = ~>(TypFamilyDefn(this, fibers))
      
      type Obj = LogicalTyp
      
      def symbObj[A, W<: Term](name: A, tp: Typ[W]) = SymbLogicTyp(name)
      
      override def subs(variable: Term, value: Term): LogicalUniv = this
      
      override def toString ="__"
    }
    
    val __ = new LogicalUniv
    
    /** Pair of types (A, B) */
    case class PairTyp[U<: Term, V <: Term](first: Typ[U], second: Typ[V]) extends 
						Typ[AbsPair[U, V]] with AbsPair[Typ[U], Typ[V]]{

    	type Obj = AbsPair[U, V]

		lazy val typ = PairTyp(first.typ, second.typ)
		
		def subs(x: Term, y: Term) = PairTyp(first.subs(x, y), second.subs(x, y))
			
			// The name is lost as `name', but can be recovered using pattern matching.
		def symbObj[A, W<: Term](name: A, tp: Typ[W]): AbsPair[U, V] = PairObj(first.symbObj(name), second.symbObj(name))
			}	
    
    /** Object (a, b) in (A, B) */
    case class PairObj[U <: Term, V <: Term](val first: U, val second: V) extends AbsPair[U, V]{
    	lazy val typ = PairTyp(first.typ, second.typ)
    	
    	def subs(x: Term, y: Term) = PairObj(first.subs(x, y), second.subs(x, y))
					}

    /** Abstract pair, parametrized by scala types of components */
	trait AbsPair[U<: Term, V <: Term] extends Term{
	  val first: U
	  val second: V
	}
	
	def mkPair[U<: Term, V<: Term](f: U, s: V) = (f, s) match{
	  case (fst: Typ[_], scnd: Typ[_]) => PairTyp(fst, scnd)
	  case (fst, scnd) => PairObj(fst, scnd)
	}
    
	/** Function type */
    case class FuncTyp[W<: Term : TypeTag, V <: Typ[W], U<: Term : TypeTag](dom: V, codom: Typ[U]) extends LogicalTyp{
      type Obj = FuncObj[W, V, U]
      
	  override def symbObj[A, T<: Term](name: A, tp: Typ[T]) = FuncSymb[A, W, V, U](name, dom, codom)
	  
	  override def toString = dom.toString + " -> " + codom.toString
	}
    
    /*
     * Includes both functions and dependent functions
     */
    trait FuncTerm[-W <: Term, +U <: Term] extends Term with (W => U){
      val domobjtpe : Type
      
      val codomobjtpe: Type
      
      val dom: Typ[Term]
      
      def apply(arg: W): U
      
      
      def subs(x: Term, y: Term) : FuncTerm[W, U]
    }
    
    /*
     * A symbol representing a formal application
     */
    case class ApplnSym[W <: Term : TypeTag, U <: Term : TypeTag](func : FuncTerm[W, U], arg : W){
      override def toString = func.toString + "("+ arg.toString +")"
    }
    
    
    /*
     * Pattern matching for a formal application.
     */
    case class ApplnPattern[W <: Term : TypeTag, U <: Term : TypeTag](){
      def unapply(term : Term) : Option[(FuncTerm[W, U], U)] = term match {
        case sym : Symbolic[_] => sym.name match {
          case ApplnSym(func : FuncTerm[W, U], arg : U) if typeOf[W] <:< func.domobjtpe && func.codomobjtpe <:< typeOf[U] => Some((func, arg)) 
          case _ => None
        }
        case _ => None
      }
    }
    
    val applptnterm = ApplnPattern[Term, Term]()
    
    /*
     * A formal function term, no non-trivial substitutions. Can be a dependent function.
     */
    trait FormalFuncTerm[-W <: Term, +U <: Term] extends FuncTerm[W, U]{
      def subs(x: Term, y: Term) = if (x==this) y.asInstanceOf[FuncTerm[W, U]] else this
    }
    
	/** a function, i.e.,  an object in a function type, has a codomain and a fixed type for the domain. */
    trait FuncObj[W<: Term  , V<: Typ[W], U<: Term ] extends FuncTerm[W, U]{
      /** domain*/
	  val dom: V
	  /** codomain */
	  val codom: Typ[U]

	  /** Action, i.e., function application */
	  def action(arg:dom.Obj): codom.Obj 
	  
	  /** Function application */
	  def apply(arg: W) = action(arg.asInstanceOf[dom.Obj])
	  
	}

    /** Formal function, i.e., with application purely symbolic. */
//	trait FormalFunc[W<: Term, V<: Typ[W], U<: Term] extends FuncObj[W, V, U] with FormalFuncTerm[W, U]{	  
//	  def action(arg: dom.Obj) = codom.symbObj(FormalApplication[W, V, U](this, arg))
//	}
	
	/** Symbol containing function info */
    case class FuncSymb[A, W<: Term : TypeTag, V<: Typ[W], U<: Term : TypeTag](name: A, dom: V, codom: Typ[U]) extends FuncObj[W, V, U] with FormalFuncTerm[W, U] with Symbolic[A]{
      val domobjtpe = typeOf[W]
	  
	  val codomobjtpe = typeOf[U]
      
      lazy val typ = FuncTyp[W, V, U](dom, codom)
      
      def action(arg: dom.Obj) = codom.symbObj(ApplnSym(this, arg))
    }
	
    trait FormalAppl[W <: Term, U <: Term]{
      val func: FuncTerm[W, U]
      
      val arg: W
    }
    
    trait FormalTypAppl[W <: Term, U <: Term]{
      val func: FuncTerm[W, Typ[U]]
      
      val arg: W
      
    }
    
    case class FuncAppl[W <: Term, V<: Typ[W], U <: Term](){
      def unapply(obj: Term): Option[Term] = None
    }
    
    object FormalAppl{
      def unapply(obj: Term): Option[(FuncObj[Term,Typ[Term], Term], Term)] =obj match{
        case sym: Symbolic[_] =>
        sym.name match {
          case FormalApplication(f, a) => Some((f, a))
          case _ => None
        }
        case _ => None
      }
    }
    
    /** A formal structure representing func(arg) - especially used for pattern matching */
    case class FormalApplication[W<: Term, V<: Typ[W], U<: Term](func: FuncObj[W, V, U], arg: W) extends Term with FormalAppl[W, U]{
      lazy val typ = func.codom
      
      override def toString = func.toString + "("+ arg.toString +")"

      def subs(x: Term, y: Term) = func.subs(x, y)(arg.subs(x, y).asInstanceOf[W])
    }
    
	
    /** A function given by a scala function */
	case class FuncDefn[W<: Term : TypeTag, V<: Typ[W], U<: Term : TypeTag](func: Term => U, dom: V, codom: Typ[U]) extends FuncObj[W, V, U] with FormalFuncTerm[W, U]{
	  val domobjtpe = typeOf[W]
	  
	  val codomobjtpe = typeOf[U]
	  
	  lazy val typ = FuncTyp[W, V, U](dom, codom)
	  
	  def act(arg: Term) = if (arg.typ == dom) Some(func(arg)) else None
	  
	  def action(arg: dom.Obj) = func(arg).asInstanceOf[codom.Obj]
	}
	
	// We need a hybrid of the FuncDefm and FuncSymb, with a function determined by a partial function with formal application outside
	// As formal application acts on substitution by a fresh application, this can be used.
	// This can still extend FormalFuncTerm
	
//	trait PartiallyFormalFunc[W<: Term, V<: Typ[W], U<: Term] extends FuncObj[W, V, U] with FormalFuncTerm[W, U]{
//	  val predef: PartialFunction[dom.Obj, codom.Obj]
//	  
	  // extends FuncObj[W, V, U] with FormalFuncTerm[W, U]
	  
//	  def action(arg: dom.Obj) = predef.applyOrElse(arg, (obj: dom.Obj) => codom.symbObj(FormalApplication[W, V, U](this, arg)))
//	}
	
	/** A lambda-expression.
	 *  variable is mapped to value.
	 *  
	 *  Correction? should allow general variables.
	 *  Should revisit if LogicalTyp variables are too restricitive.
	 *  
	 *  Refine to include a domain and codomain, and extend FuncObj.
	 *  
	 *  Should have a trait with the basic properties and inductive definitions
	 *  The below is a lambda defined function.
	 */
	case class Lambda[X<: Term : TypeTag, Y <: Term : TypeTag](variable: X, value : Y) extends FuncTerm[Term, Term]{
	  val domobjtpe = typeOf[X]
	  
	  val codomobjtpe = typeOf[Y]
	  
	  val dom = variable.typ
	  
	  lazy val typ = FuncTyp[Term, LogicalTyp, Term](variable.typ.asInstanceOf[LogicalTyp] , value.typ)
	  
	  def apply(arg: Term) = value.subs(variable, arg)
	  
	  def subs(x: Term, y: Term) = Lambda(variable.subs(x,y), value.subs(x, y))
	}
	
	
	/** Lambda constructor
	 *  
	 */		
	def lambda[U<: Term : TypeTag, V <: Term  : TypeTag](variable: U)(value : V) = Lambda(variable, value)
	
	
	
	/** Type family, with domain in a subclass of Typ[W] and codomain in Typ[U]*/
	type TypFamily[W<: Term, V<: Typ[W], U<: Term] = FuncObj[W, V, Typ[U]]
	
	trait LogicalTypFamily extends TypFamily[Term, LogicalTyp, Term] with (Term => LogicalTyp){
	  val domobjtep = typeOf[Term]
	  
	  val codomobjtpe = typeOf[Term]
	  
	  /** codomain */
	  val codom = new LogicalUniv
	   
//	  lazy val typ = FuncTyp[Term, LogicalTyp, Term](dom, codom)
	  

	  /** Action, i.e., function application */
	  def action(arg:dom.Obj): LogicalTyp 
	  
	  /** Function application */
	  override def apply(arg: Term) = action(arg.asInstanceOf[dom.Obj])
	  
	  def -->(cod: LogicalTyp) = TypFamilyDefn(dom, (arg : Term) => apply(arg) --> cod)
	  
	}
	
	case class TypFamilyDefn(dom: LogicalTyp, f: Term => LogicalTyp) extends LogicalTypFamily{
	  val domobjtpe = typeOf[Term]
	  
	//  val codomobjtpe = typeOf[LogicalTyp]
	  
	  val typ = FuncTyp[Term, LogicalTyp, LogicalTyp](dom, __)
	  
	  def action(arg: dom.Obj) = f(arg)
	  
	  def subs(x: Term, y: Term) : LogicalTypFamily = this
	}
	
	/** For all/Product for a type family. This is the type of dependent functions */
	case class PiTyp[W<: Term : TypeTag, V<: Typ[W], U<: Term : TypeTag](fibers: TypFamily[W, V, U]) extends LogicalTyp{
	  type Obj = DepFuncObj[W, V, U]
	  
	  override def symbObj[A, T<: Term](name: A, tp: Typ[T]) = DepFuncSymb[A, W, V, U](name, fibers)
	}
	
	/** Exists/Sum for a type family */
	case class SigmaTyp[W<: Term, V<: Typ[W], U<: Term](fibers: TypFamily[W, V, U]) extends LogicalSTyp
	
	/** Object in a dependent function type, i.e.,
	 *  a dependent function. Has a family of codomains 
	 */
	trait DepFuncObj[W<: Term, V<: Typ[W], U<: Term] extends FuncTerm[W, U]{
	  val fibers: TypFamily[W, V, U] 
	   
	  
//	  lazy val typ = PiTyp(fibers)
	  
	  def apply(arg: W) = action(arg.asInstanceOf[fibers.dom.Obj])
	  
	  def action(arg: fibers.dom.Obj): U
	  
	}
	
	/** A formal dependent function, with application symbolic
	 *  
	 */
	trait FormalDepFunc[W<: Term, V<: Typ[W], U<: Term] extends DepFuncObj[W, V, U] with FormalFuncTerm[W, U]{
//	  def act(arg: W) = if (arg.typ == fibers.dom) {
//	    val typ =fibers(arg) 
//	    Some(typ.symbObj(FormalDepApplication[W, V, U](this, arg)).asInstanceOf[U]) 
//	  }
//	  else None
	  
	  
	  def action(arg: fibers.dom.Obj) = fibers(arg).symbObj(FormalDepApplication[W, V, U](this, arg)).asInstanceOf[U]
	}
	
	
	case class DepFuncSymb[A, W<: Term : TypeTag, V<: Typ[W], U<: Term : TypeTag](name: A, fibers: TypFamily[W, V, U]) extends DepFuncObj[W, V, U] with FormalFuncTerm[W, U] with Symbolic[A]{
	  val domobjtpe = typeOf[W]
	  
	  val codomobjtpe = typeOf[U]
	  
	  val dom = fibers.dom
	  
	  lazy val typ = PiTyp(fibers)
	  
	  def action(arg: fibers.dom.Obj) = fibers(arg).symbObj(ApplnSym(this, arg)).asInstanceOf[U]
	}
	
	/** A symbol capturing formal application
	 *  of a dependent function.
	 */ 
	case class FormalDepApplication[W<: Term, V<: Typ[W], U<: Term](func: DepFuncObj[W, V, U], arg: W) extends Term  with FormalAppl[W, U]{
	  val typFamily = func.fibers
      lazy val typ : Typ[U] = (typFamily(arg)) 
      
      override def toString = func.toString + "("+ arg.toString +")"
      
      def subs(x: Term, y: Term) = func.subs(x, y)(arg.subs(x, y).asInstanceOf[W])
    }
	
	/** The identity type. 
	 *  This is the type lhs = rhs
	 */
	case class IdentityTyp[U <: Term](dom: Typ[U], lhs: Term, rhs: Term) extends LogicalSTyp
	
	/** A dependent function given by a scala funcion */
	case class DepFuncDefn[W<: Term : TypeTag, V<: Typ[W], U<: Term : TypeTag](func: W => U, dom: V, fibers: TypFamily[W, V,U]) extends DepFuncObj[W, V, U] with FormalFuncTerm[W, U]{
	  val domobjtpe = typeOf[W]
	  
	  val codomobjtpe = typeOf[U]
	  
	  lazy val typ = PiTyp[W, V, U](fibers)
	  
	  def act(arg: W) = if (arg.typ == dom) Some(func(arg)) else None
	  
	  def action(arg: fibers.dom.Obj): U = func(arg)
	}
	
	/** Companion to dependent functions */
	object DepFuncObj{
	  def apply[W<: Term : TypeTag, V<: Typ[W] : TypeTag, U<: Term : TypeTag](func: Term => U, dom: V, univ: Univ[U]): DepFuncObj[W, V, U] = {
	    def section(arg: Term) = func(arg).typ.asInstanceOf[Typ[U]]
	    val fibers: FuncObj[W, V, Typ[U]] = FuncDefn[W, V, Typ[U]](section, dom, univ)
	    DepFuncDefn(func, dom, fibers)
	  }
	}
	
	case object Unit extends LogicalSTyp
	
	
	trait ConstFmlyTmpl extends Term with AtomicObj{
	  val typ : LogicalTyp
	  
	  
	  type ObjTyp <: Term
	  
	  def map(Q: => LogicalTyp) : ConstFmlyTmpl
	  
	  def dmap(Q: Term => LogicalTyp) : Term => ConstFmlyTmpl
	  
	  def ->:(A : LogicalTyp) = ParamConstTmpl(A, this)
	  
	  def pushforward(f: Term => Term)(arg: ObjTyp) : Term
	}
	
	
	case class ConstTmpl(typ: LogicalTyp) extends ConstFmlyTmpl{
//	  val fullTyp = typ
	  
	  type ObjTyp = typ.Obj
	  
	  def map(Q: => LogicalTyp) = ConstTmpl(Q)
	  
	  def dmap(Q: Term => LogicalTyp) : Term => ConstFmlyTmpl = (obj) => ConstTmpl(Q(obj))
	  
	  def pushforward(f: Term => Term)(arg: ObjTyp) = f(arg)
	}
	
	case class ParamConstTmpl(base: LogicalTyp, cod: ConstFmlyTmpl) extends ConstFmlyTmpl{
	  type baseObjTyp = Typ[baseObj]
	  
	  type baseObj = base.Obj
	  
	  val typ = FuncTyp[Term, LogicalTyp, Term](base.asInstanceOf[LogicalTyp], cod.typ)
	  
	  type ObjTyp = FuncObj[Term, LogicalTyp, Term]
	  
	  def push(func: ObjTyp)(arg: base.Obj): cod.ObjTyp = func(arg).asInstanceOf[cod.ObjTyp] 
	  
	  def pushforward(f: Term => Term)(func: ObjTyp) = {
	    val g = cod.pushforward(f) _
	    
	    val s : base.Obj => Term = (arg: base.Obj) => g(push(func)(arg))
	    
	    val ss : Term => Term = (arg) => s(arg.asInstanceOf[base.Obj])
	    
	    FuncDefn[Term, LogicalTyp, Term](ss, base, cod.typ)
	  }
	  
	  def map(Q: => LogicalTyp): ParamConstTmpl = base ->: cod.map(Q)
	  
	  def dmap(Q: Term => LogicalTyp) : Term => ConstFmlyTmpl = {
	    case f: typ.Obj => 
	      val fibre: Term => ConstFmlyTmpl = (obj) => ConstTmpl(Q(f(obj.asInstanceOf[base.Obj])))
	      DepParamConstTmpl(typ, fibre)
	  } 
	}
	
	case class DepParamConstTmpl(base: LogicalTyp, fibre: Term => ConstFmlyTmpl) extends ConstFmlyTmpl{
	  val typ = PiTyp(TypFamilyDefn(base, ((obj) => fibre(obj).typ)))
	  
	  type ObjTyp = DepFuncObj[Term, LogicalTyp, Term]
	  
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
	    
	    DepFuncDefn[Term, LogicalTyp, Term](ss, base, TypFamilyDefn(base, fibretyp _))
	  }
	  
	  def map(Q: => LogicalTyp): DepParamConstTmpl = DepParamConstTmpl(base, (obj) => fibre(obj).map(Q))
	  
	   def dmap(Q: Term => LogicalTyp) : Term => ConstFmlyTmpl = {
	    case f: typ.Obj => 
	      val fibre: Term => ConstFmlyTmpl = (obj) => ConstTmpl(Q(f(obj)))
	      DepParamConstTmpl(typ, fibre)
	  } 
	}
	
	
	// Inductive types can be constructed from a context.
	
	trait ContextElem[+X <: Term]{
	  val constants: List[X]
	  
	  val variables: List[X]
	  
	  val dom: LogicalTyp
	  
	  def exptyp(tp: LogicalTyp) : LogicalTyp 
	  
	  def fulltyp(tp: LogicalTyp) : LogicalTyp 
	  
	  
	  def get(value: Term): Term
	  
	  def subs(x: Term, y: Term): ContextElem[X]
	  

	}
	
	trait Context[+X <: Term] extends ContextElem[X]{
	  /*
	   * The codomain for the multi-function given by the context.
	   */
	  val target: LogicalTyp
	  
	  /*
	   * The type of the object : a multivariate function : that comes from the context.
	   */
	  val typ : LogicalTyp
	  
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
	  
	  def apply(dom: LogicalTyp) = simple(dom)
	  
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
	  
	  def recsymbpattern(f: Term => Term, Q : LogicalTyp, symbs : List[Term], ctx : Context[ConstFmlyTmpl]) : Context[Term] = ctx match {
	    case ContextSeq(LambdaContext(a), tail) => 
	      val b = a map (Q)
	      ContextSeq(LambdaContext(a.typ.symbObj(symbs.head)), 
	          ContextSeq(KappaContext(f(a.typ.symbObj(symbs.head))),recsymbpattern(f, Q, symbs.tail,tail)))
	    case cntx => cntx
	  }
	  
	  case class simple[X <: Term](dom: LogicalTyp) extends Context[X]{
	    val target = dom
	    
	    val typ = dom
	    
	    val constants = List()
	    
	    val variables = List()
	    
	    def exptyp(tp: LogicalTyp) : LogicalTyp = dom
	  
	    def fulltyp(tp: LogicalTyp) : LogicalTyp = dom
	  
	    def get(value: Term): Term = value
	  
	    def subs(x: Term, y: Term): Context[X] = simple(dom)
	    
	    def patternMatch(obj: Term) = if (obj.typ == typ) Some((obj, List())) else None
	    
	    //Should be applied to an appropriate induced map
	    def recContext(f : Term => Term): Context[X] = this
	    
	  }
	}
	
	
	
	trait AtomicContext[+X <: Term] extends ContextElem[X]{
	  val cnst: X
	  
	  val dom = cnst.typ.asInstanceOf[LogicalTyp]
	  
	  val constants = List(cnst)
	 
	  
	  def fulltyp(tp: LogicalTyp) = FuncTyp[Term, LogicalTyp, Term](dom , tp)
	  
	  def subs(x: Term, y: Term): AtomicContext[X]
	}
	
	case class ContextSeq[+X <: Term : TypeTag](head: AtomicContext[X], tail: Context[X]) extends Context[X]{
	  val target = tail.target
	  
	  val typ = head.exptyp(tail.typ)
	  
	  lazy val constants = head.cnst :: tail.constants
	  
	  lazy val variables = head.variables ::: tail.variables
	  
	  val dom = head.dom
	  
	  def get(value: Term) = head.get(tail.get(value))
	  
	  def exptyp(tp: LogicalTyp) = head.exptyp(tail.exptyp(tp))
	  
	  def fulltyp(tp: LogicalTyp) = head.exptyp(tail.exptyp(tp))
	  
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
	  
	  def exptyp(tp: LogicalTyp) = FuncTyp[Term, LogicalTyp, Term](dom, tp)
	  
	  val variables = List(cnst)
	  
	  def subs(x: Term, y: Term) = LambdaContext(cnst.subs(x, y).asInstanceOf[U])
	}
	
	case class KappaContext[U <: Term : TypeTag](cnst: U) extends AtomicContext[U]{
	  def export(value: Term) : Term => Term = _ => value
	  
	  def get(value: Term) = value
	  
	  def exptyp(tp: LogicalTyp) = tp
	  
	  val variables = List()
	  
	  def subs(x: Term, y: Term) = LambdaContext(cnst.subs(x, y).asInstanceOf[U])
	}
	
	
	trait DefnPattern{
	  /*
	   * This is the type of the object defined by the pattern
	   */
	  val typ : LogicalTyp
	  
	  
	  val fold : Term
	  
	  val contextPrep : Context[Term] => Context[Term]
	  
	  lazy val context = contextPrep(Context.simple(typ))
	  
	   
	  def recContextPrep(f :Term => Option[Term]) : Context[Term] => Context[Term]
	  
	  def recContext(f :Term => Option[Term]) = recContextPrep(f)(Context.simple(typ))
	  
	  
	}
	
	object DefnPattern{
	  
	}
	
	case class Const(head: Term) extends DefnPattern{
	  val typ = head.typ.asInstanceOf[LogicalTyp]
	  
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
	  
	  val typ = fold.typ.asInstanceOf[LogicalTyp]
	  
	  val fold  = head.fold match {
	    case f : FuncObj[d, _,_] if f.dom == tail.typ => f(tail.fold.asInstanceOf[d])
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
//	  val typ : LogicalTyp
	
	}
	
	object TypPattern{
	  val fromConstFmlyTmpl: ConstFmlyTmpl => TypPattern = {
	    case ConstTmpl(tp : LogicalTyp) => ConstTyp(tp)
	    case ParamConstTmpl(head, tail) => FuncTypPattern(ConstTyp(head), fromConstFmlyTmpl(tail))
	    case DepParamConstTmpl(head, tail) => DepFuncTypPattern(ConstTyp(head), (obj) =>  fromConstFmlyTmpl(tail(obj)))
	    
	  }
	}
	
	case class ConstTyp(head : LogicalTyp) extends TypPattern{
	//  val typ = head

	 
	}
	
	case class FuncTypPattern(head: TypPattern, tail: TypPattern) extends TypPattern{
	  
	//  val typ = head.typ match {
	//    case f : FuncTyp[_, _, _] if f.dom == tail.typ => f.codom.asInstanceOf[LogicalTyp]  
	//  } 
	  
	}
	
	case class DepFuncTypPattern(head: TypPattern, tail: Term => TypPattern) extends TypPattern

	
	
	trait InductSeq{
	  val typ: LogicalTyp
	  
	  val pattern: TypPattern
	  
	  def map(Q : => LogicalTyp): InductSeq
	}
	
	/*
	 * This is for constant sequences not ending in the type W being defined
	 */
	case class CnstInductSeq(fm : ConstTmpl) extends InductSeq{
	  val typ = fm.typ
	  
	  val pattern = TypPattern.fromConstFmlyTmpl(fm)
	  
	  def map(Q : => LogicalTyp) = this
	}
	
	/*
	 * These are families ending in the type being defined
	 */
	case class TargetInductSeq(w: ConstFmlyTmpl) extends InductSeq{
	  val typ = w.typ
	  
	  val pattern = TypPattern.fromConstFmlyTmpl(w)
	  
	  def map(Q : => LogicalTyp) = TargetInductSeq(w map Q)
	}
	
	case class InductSeqCons(head: ConstFmlyTmpl, tail: InductSeq) extends InductSeq{
	  val typ = FuncTyp[Term, LogicalTyp, Term](head.typ, tail.typ) 
	  
	  val pattern = FuncTypPattern(TypPattern.fromConstFmlyTmpl(head), tail.pattern)
	  
	  def map(Q : => LogicalTyp) = InductSeqCons(head map Q, tail map Q)
	}
	
	case class InductSeqDepCons(head: ConstFmlyTmpl, tail : Term => InductSeq) extends InductSeq{
	  val typ = PiTyp[Term, LogicalTyp, Term](TypFamilyDefn(head.typ, (obj) => tail(obj).typ))
	  
	  val pattern = DepFuncTypPattern(TypPattern.fromConstFmlyTmpl(head), (obj) => tail(obj).pattern)
	  
	  def map(Q : => LogicalTyp) = InductSeqDepCons(head map Q, (obj) => tail(obj) map Q)
	}
	
	case class InductCons(name: Term, constyp : InductSeq){
	  def map(Q: => LogicalTyp) = InductCons(name, constyp map Q)
	  
	  val obj = constyp.typ.symbObj(name)
	}
	
	class InductiveTyp(consPatterns : List[InductCons]) extends LogicalSTyp{
	  lazy val cons = consPatterns map (_.map(this).obj)
	}
	
	
	
	
	object AlsoOld{
	
	trait ConstructorPattern{
	  val typ : LogicalTyp
	  
	  /*
	   * closed means no free variables
	   */
	  val isClosed : Boolean
	  
	  val dom: LogicalTyp
	  
	}
	
	object ConstructorPattern{
	  case class Overfull(typ : LogicalTyp) extends ConstructorPattern{
	    val isClosed = false
	    
	    val dom = typ
	  }
	}
	
	case class SimpleConstructor(typ: LogicalTyp) extends ConstructorPattern{
	  val isClosed = true
	  
	  val dom = Unit
	}
	
	case class FuncConstructor(dom : LogicalTyp, tail: ConstructorPattern) extends ConstructorPattern{
	  val typ = FuncTyp[Term, LogicalTyp, Term](dom, tail.typ)
	  
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
	  val typ : LogicalTyp
	  
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
	  val typ = obj.typ.asInstanceOf[LogicalTyp]
	  
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
	
	trait InductiveTypLike extends LogicalTyp{self =>
	  val constrs: Set[Constructor]
	  
	  case class AggregatePattern(ps : Set[RecDefnPattern]) extends DefnPattern{
	    val typ =self
	    
	    val mtch = ps.map(_.head) == constrs
	    
	    val isValid = (mtch /: ps.map(_.isValid))(_ && _)
	    
	    val isTotal = (mtch /: ps.map(_.argsTotal))(_ && _)
	  }
	}
	
	
	

//	def totalPatterns(defs : List[DefnPattern]) : Boolean
	
	
	
	
	class ConstructorDefn(defn : LogicalTyp => Context[ConstFmlyTmpl], target: => LogicalTyp){
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
	
	class InductiveTyp(cnstrFns: List[LogicalTyp => Context[ConstFmlyTmpl]]) extends LogicalSTyp{self =>
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
	
	case class ToW[A, B](sym: A, head: LogicalTyp => ConstFmlyTmpl, tail: InductiveConstructor[B]) extends InductiveConstructor[A]
	
	case class IndctParam[A, B](sym: A, head: LogicalTyp, tail: InductiveConstructor[B]) extends InductiveConstructor[A]
	
	// Should also add dependent function
	}
	
	val x = 'x' :: __
	
	val y = "y" :: x
	
	/** Symbol factory */	
	def nextChar(s: Set[Char]) = if (s.isEmpty) 'a' else (s.max + 1).toChar
  
	/** Helper for symbol factory */
	def usedChars(s: Set[Term]): Set[Char] = {
	    def charOpt (obj:Term) : Option[Char] = obj match {
	      case sym: Symbolic[_] => Some(Try(sym.name.asInstanceOf[Char]).toOption).flatten
	      case _ => None
	    }
	    
	    
	    s collect (Function.unlift(charOpt _))
	}
	
}











