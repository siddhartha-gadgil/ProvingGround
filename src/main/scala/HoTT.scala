package provingGround

import scala.language.implicitConversions 
import scala.util._
import scala.language.existentials
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}
import Math._

// To Do:
//
// * A cleaner substitution, parametrised by type.
// * Properly abstracted Formal Function Application - done to some extent
// * Inductive type trait, with Sigma type etc. having this. - done to some extent
// * Clean up universes, LogicalTyp etc.
//


/** The Homotopy type theory objects, types and utility functions
 *  
 */
object HoTT{
  /** Abstract object */
    trait Term extends Subs[Term]{
      /** 
       * Gives limited information on types when the types are universes, so should not be used in this case.
       * The type `LogicalUniverse' should only be read as some universe.
       * So avoid making this judgement. 
       */
        def typ: Typ[Term]
        
    
//    def subs(x: Term, y: Term): Term
    }
    
    trait TmpTerm extends Term{
      lazy val typ = Unit
      
      def subs(x : Term, y: Term) = this
    }
    
    trait Subs[+U <: Term]{
      def subs(x: Term, y: Term) : U
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
      case fnTyp : FuncTyp[_, _] => subObjs(fnTyp.dom) ::: subObjs(fnTyp.codom) 
      case Lambda(variable, value : Term, _) => subObjs(value) map (lambda(variable))
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
    trait Typ[+U <: Term] extends Term with Subs[Typ[U]]{
    	/** scala type of objects with this HoTT-type */
        type Obj <: U with Subs[U]
        
        val typ : Univ
        
        lazy val typlevel : Int = univlevel(typ) 
        
        /** A symbolic object with specified HoTT type, by default 'this', and with scala-type Obj*/
        def symbObj[A](name: A): U with Subs[U]
         
        /** Make symbolic object */
        def ::[A](name:A) = symbObj(name) 
        
//        def subs(x: Term, y: Term) : Typ[U]
        
        // Template for a method allowing covariance. 
        def ->:[W <: Term : TypeTag, UU >: U <: Term : TypeTag](that : Typ[W]) = FuncTyp[W, UU](that, this)
        
        def :::(that: Term) = {require(that.typ == this, "coercion to the wrong type"); that.asInstanceOf[this.Obj]}
    }
    
    
    trait AtomicTyp[U <: Term] extends Typ[U]{
      def subs(x: Term, y: Term) : Typ[U] = if (x == this) y.asInstanceOf[Typ[U]] else this
    }
    


    /** traits that are given by name;
     *  does not include, for instance, pairs each of whose instance is given by a name;
     *  same considerations for functions etc.
     */
    trait Symbolic[+A]{
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
      override def toString = "("+name.toString+" : "+typ.toString+")"
      
      def subs(x: Term, y: Term) = this match {
        case applptnterm(func, arg) => 
          func.subs(x,y)(arg.subs(x, y))
        case _ => this
      }
    } 
    
    /** Symbolic types, which the compiler knows are types.
     *  The base tells the scala type of objects and gives a factory for symbolic objects of this scala type.
     */
    case class SymbTyp[A](name: A) extends Typ[Term] with Symbolic[A]{
      lazy val typ = Universe(0)
      
      type Obj = Term
      
      def symbObj[B](name: B) = SymbObj(name, this)
      
      override def toString = "("+name.toString+" : "+typ.toString+")"
      
      def elem = this
      
      
      val applptntypu = ApplnPattern[Term, Typ[Term]]()
      
      // Change this to remove first case after checking
      def subs(x: Term, y: Term) = (x, y, name) match {
        case (u: Typ[_], v: Typ[_], _) if (u == this) => v
        case (_, _, fa: FormalTypAppl[w, _]) => 
          fa.func.subs(x, y)(fa.arg.subs(x,y).asInstanceOf[w])
        case (_, _,applptntypu(func, arg)) => func.subs(x,y)(arg.subs(x, y))
        case _ => this
      }
    }
    
    case class SymbLogicTyp[A](name: A) extends LogicalSTyp with Symbolic[A]{
      override def toString = name.toString+" : "+typ.toString
    }
    
    
    
    
    

    /** HoTT types with underlying scala type Term.
     *  They belong to the first universe.
     *  Should eventually avoid using this.
     */
    trait LogicalTyp extends Typ[Term] with Subs[LogicalTyp]{
      
//      type Obj = Term
      
      lazy val typ = Universe(0)
      
      def subs(x: Term, y: Term) : LogicalTyp = if (x == this) y.asInstanceOf[LogicalTyp] else this
      
//      def symbObj[A, W <: Term](name: A, tp: Typ[W]): Term = SymbObj(name, tp)
      
      /** returns function type A -> B, not used in main code */
//      def -->[U <: Term : TypeTag](that: Typ[U]) = FuncTyp[Term, LogicalTyp, U](this, that)
 
//      def ~>[U <: Term : TypeTag](fibers: TypFamily[Term, LogicalTyp, U]) = PiTyp(fibers)
      
//      def ~~>(fibers : Term => LogicalTyp) = ~>(TypFamilyDefn(this, fibers))
    }
    
    /*
     * Types with symbolic objects not refined.
     */
    trait SmallTyp extends Typ[Term] with LogicalTyp{
     type Obj = Term
      
      def symbObj[A, W <: Term](name: A, tp: Typ[W]): Term = SymbObj(name, this)      
    }
    
    class LogicalSTyp extends LogicalTyp{
      type Obj = Term
      
      def symbObj[A](name: A): Term = SymbObj(name, this)
    }
    
    /** Universes. Can be just a type so far 
    trait Univ extends Typ[Typ[Term]]{
      val typ : Univ
      
      type Obj = Typ[Term]
      
      val level : Int
      
    } 
    * 
    */
    
    type Univ = Typ[Typ[Term]]
    
    
    /** The universes */
    case class Universe(level : Int) extends Univ{
      require(level >= 0)
      
      type Obj = Typ[Term]
      
      lazy val typ  = Universe(level +1)
      
      
      def symbObj[A](name: A)= SymbTyp(name)
      
      val __ = symbObj(())
      
      def subs(x : Term, y : Term) = this
      
      override def toString = "_"
    }
    
    def univlevel: Typ[Typ[Term]] => Int = {
      case Universe(l) => l
      case _ => 0
    }
    
    case class MiniVerse[U <: Term](sample : Typ[U]) extends Typ[Typ[U]]{
      type Obj = Typ[U]
      
      lazy val typ = MiniVerse[Typ[U]](this)
      
      def symbObj[A](name: A)= sample
      
      def subs(x : Term, y : Term) = this
    }
    
    /** The first universe, consisting of logical types 
    class LogicalUniv extends Univ with Typ[LogicalTyp]{
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
    */
    
    val __ = Universe(0)
    
    /** Pair of types (A, B) */
    case class PairTyp[U<: Term  with Subs[U], V <: Term with Subs[V]](first: Typ[U], second: Typ[V]) extends 
						Typ[AbsPair[U, V]] with AbsPair[Typ[U], Typ[V]]{

    	type Obj = AbsPair[U, V] with Subs[AbsPair[U, V]]

		lazy val typ = Universe(Math.max(first.typlevel, second.typlevel))
		
		def subs(x: Term, y: Term) = PairTyp(first.subs(x, y), second.subs(x, y))
			
			// The name is lost as `name', but can be recovered using pattern matching.
		def symbObj[A](name: A): Obj = PairObj(first.symbObj(name), second.symbObj(name))
			}	
    
    /** Object (a, b) in (A, B) */
    case class PairObj[U <: Term with Subs[U], V <: Term with Subs[V]](val first: U, val second: V) extends AbsPair[U, V] with Subs[AbsPair[U, V]]{
    	lazy val typ = PairTyp(first.typ, second.typ)
    	
    	def subs(x: Term, y: Term) = PairObj[U, V](first.subs(x, y), second.subs(x, y))
					}

    /** Abstract pair, parametrized by scala types of components */
	trait AbsPair[U<: Term, V <: Term] extends Term{
	  val first: U
	  val second: V
	}
	
//	def mkPair(f: Term, s: Term) = (f, s) match{
//	  case (fst: Typ[u], scnd: Typ[v]) => PairTyp[u with Subs[u], v with Subs[v]](fst, scnd)
//	  case (fst, scnd) => PairObj(fst, scnd)
//	}
    
	/** Function type */
    case class FuncTyp[W<: Term : TypeTag, U<: Term : TypeTag](dom: Typ[W], codom: Typ[U]) extends Typ[FuncTerm[W, U]]{
      type Obj = FuncObj[W, U]
      
      lazy val typ = Universe(max(dom.typlevel, codom.typlevel))
      
	  def symbObj[A](name: A) = FuncSymb[A, W, U](name, dom, codom)
	  
	  override def toString = "("+dom.toString + " -> " + codom.toString+")"
	  
	  def subs(x : Term, y: Term) = FuncTyp[W, U](dom.subs(x, y), codom.subs(x,y))
	}
    
    /*
     * Includes both functions and dependent functions
     */
    trait FuncTerm[-W <: Term, +U <: Term] extends Term with (W => U) with Subs[FuncTerm[W, U]]{
      type Obj <: FuncTerm[W, U]
      
      val domobjtpe : Type
      
      val codomobjtpe: Type
      
      val dom: Typ[Term]
      
      def apply(arg: W): U
      
//      def andThen[WW >: U <: Term, UU <: Term](fn: WW => UU): FuncTerm[WW, UU]
      
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
      def unapply(term : Term) : Option[(FuncTerm[W, U], W)] = term match {
        case sym : Symbolic[_] => sym.name match {
          case sm @ ApplnSym(func : FuncTerm[W, U], arg) if typeOf[W] <:< func.domobjtpe & func.codomobjtpe <:< typeOf[U]  => 
            Some((func, arg.asInstanceOf[W])) 
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
    trait FuncObj[W<: Term, +U <: Term] extends FuncTerm[W, U]{
      /** domain*/
	  val dom: Typ[W]
	  /** codomain */
	  val codom: Typ[U]

	  /** Action, i.e., function application */
	  def action(arg:dom.Obj): codom.Obj 
	  
	  /** Function application */
	  def apply(arg: W) = action(arg.asInstanceOf[dom.Obj])
	  
	  def subs(x: Term, y : Term) : FuncObj[W, U]
	  
	}

    /** Formal function, i.e., with application purely symbolic. */
//	trait FormalFunc[W<: Term, V<: Typ[W], U<: Term] extends FuncObj[W, V, U] with FormalFuncTerm[W, U]{	  
//	  def action(arg: dom.Obj) = codom.symbObj(FormalApplication[W, V, U](this, arg))
//	}
	
	/** Symbol containing function info */
    case class FuncSymb[A, W<: Term : TypeTag, U<: Term : TypeTag](name: A, dom: Typ[W], codom: Typ[U]) extends FuncObj[W, U] with Symbolic[A]{
      val domobjtpe = typeOf[W]
	  
	  val codomobjtpe = typeOf[U]
      
      lazy val typ = FuncTyp[W, U](dom, codom)
      
      def action(arg: dom.Obj) = codom.symbObj(ApplnSym(this, arg)).asInstanceOf[codom.Obj]
      
      def subs(x: Term, y: Term) = (x, y, name) match {
        case (u: Typ[_], v: Typ[_], _) => FuncSymb(name, dom.subs(u, v), codom.subs(u, v))
        case (u, v: FuncObj[W, U], _) if (u == this) => v
        case _ => this
      }
      
      override def toString = "("+name.toString+" : "+typ.toString+")"
    }
	
    trait FormalAppl[W <: Term, U <: Term]{
      val func: FuncTerm[W, U]
      
      val arg: W
    }
    
    trait FormalTypAppl[W <: Term, U <: Term]{
      val func: FuncTerm[W, Typ[U]]
      
      val arg: W
      
    }
    
    case class FuncAppl[W <: Term, U <: Term](){
      def unapply(obj: Term): Option[Term] = None
    }
    
    object FormalAppl{
      def unapply(obj: Term): Option[(FuncObj[Term, Term], Term)] =obj match{
        case sym: Symbolic[_] =>
        sym.name match {
          case FormalApplication(f, a : Term) => Some((f.asInstanceOf[FuncObj[Term, Term]], a))
          case _ => None
        }
        case _ => None
      }
    }
    
    /** A formal structure representing func(arg) - especially used for pattern matching */
    case class FormalApplication[W<: Term with Subs[W], U<: Term](func: FuncObj[W, U], arg: W) extends Term with FormalAppl[W, U]{
      lazy val typ = func.codom
      
      override def toString = func.toString + "("+ arg.toString +")"

      def subs(x: Term, y: Term) = func.subs(x, y)(arg.subs(x, y))
    }
    
	
    /** A function given by a scala function */
	case class FuncDefn[W<: Term : TypeTag, U<: Term with Subs[U] : TypeTag](func: W => U, dom: Typ[W], codom: Typ[U]) extends FuncObj[W, U]{
	  val domobjtpe = typeOf[W]
	  
	  val codomobjtpe = typeOf[U]
	  
	  lazy val typ = FuncTyp[W, U](dom, codom)
	  
//	  def act(arg: Term) = if (arg.typ == dom) Some(func(arg)) else None
	  
	  def action(arg: dom.Obj) = func(arg).asInstanceOf[codom.Obj]
	  
	  def subs(x: Term, y: Term) = FuncDefn((w) => func(w).subs(x, y), dom, codom.subs(x, y))
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
	 * 
	 *  
	 *  Refine to include a domain and codomain (maybe not - can have dependent type), and extend FuncObj.
	 *  
	 *  Should have a trait with the basic properties and inductive definitions
	 *  
	 *  Check: Is this the type? It may be a dependent type.
	 *  The below is a lambda defined function.
	 */
	case class Lambda[X<: Term : TypeTag, +Y <: Term with Subs[Y]: TypeTag](variable: X, value : Y, dep : Boolean = false) extends FuncTerm[X, Y]{
	  val domobjtpe = typeOf[X]
	  
	  val codomobjtpe = typeOf[Y]
	  
	  val dom = variable.typ
	  
	  override def toString = "("+variable.toString+") |-> "+value.toString
	  
	  lazy val typ : Typ[FuncTerm[Term, Term]] = if (dep) {
	    val fibre = (t : Term) => value.typ subs (variable, t)
	    
	    val family = typFamilyDefn[Term, Term](variable.typ, value.typ.typ, fibre)
	    PiTyp(family)
	  }
		  else FuncTyp[Term,  Term](variable.typ , value.typ)
	  
	  def apply(arg: X) = value.subs(variable, arg)
	  
	  def subs(x: Term, y: Term) = (x, y) match {
		    case (u : Typ[_], v : Typ[_]) if (typ.subs(u, v) != typ) => 
		      val newvar = changeTyp(variable, typ.subs(u, v))
		      Lambda(newvar , value.subs(x,y))
		    case _ => Lambda(variable.subs(x,y), value.subs(x, y))
		  }
	  
	  object myvar extends TmpTerm
	  
	  def andthen(f : Term => Term) = Lambda(variable.subs(variable, myvar), f(value.subs(variable, myvar)))
	  
	  def andThen[Z<: Term with Subs[Z] : TypeTag](f : Y => Z) = Lambda(variable, f(value))
	}
	
	def changeTyp(term: Term, newtyp : Typ[Term]) : Term = term match {
	  case sym : Symbolic[_] => newtyp.symbObj(sym.name)
	  case _ => newtyp.symbObj(term)
	}
	
	def instantiate(substitutions : Term => Option[Term], target: Typ[Term]) : Term => Option[Term] = {
	  case t: Term if t.typ ==target => Some(t)
	  case Lambda(variable, value : Term, _) => 
	    substitutions(variable) flatMap ((cnst) => {  
	      val reduced = (value.subs(variable, cnst))
	       instantiate(substitutions, target)(reduced)})	   
	  case _ => None
	}
	
	
	val idFunc : Lambda[Term, Term] = {
	  object myvar extends TmpTerm
	  
	  Lambda[Term, Term](myvar, myvar)
	} 
	
	/** Lambda constructor
	 *  
	 */		
	def lambda[U<: Term : TypeTag, V <: Term with Subs[V] : TypeTag](variable: U)(value : V) : FuncTerm[U, V] = Lambda(variable, value)
	
	def optlambda(variable: Term) : Term => Term = value =>
	  {
	  	    if (subObjs(value) contains variable)  lambda(variable)(value) else value
	  }
	
	implicit class TermOps[U <: Term : TypeTag](value: Term){
	  def :->[V <: Term with Subs[V] : TypeTag](that : V) = lambda(this.value)(that)
	}
	
	/** Type family, with domain in a subclass of Typ[W] and codomain in Typ[U]
	 *  Unable to specify that the codomain is a universe, which is needed for extracting the level.
	 *  
	trait TypFamily[W<: Term, +U <: Term] extends FuncObj[W, Typ[U]]{
	  val codom : Typ[Typ[U]] 
	  
	  val codlevel = univlevel(codom)
	  
	  def subs(x: Term, y: Term) : TypFamily[W, U]
	}
*/
	type TypFamily[W<: Term, +U <: Term] = FuncObj[W, Typ[U]]

	/*
	case class TypFamilyDefn[W <: Term : TypeTag, U <: Term](dom: Typ[W], codom: Typ[Typ[U]], f: W => Typ[U]) extends TypFamily[W, U]{
	  val domobjtpe = typeOf[Term]
	  
	  val codomobjtpe = typeOf[Typ[Term]]
	  
//	  val codom = Universe(0)
	  
	  val typ = FuncTyp[W, Term](dom, __ )
	  
	  def action(arg: dom.Obj) = f(arg).asInstanceOf[codom.Obj]
	  
	  def subs(x: Term, y: Term) : TypFamily[W, U] = this
	}
	*/
	
	def typFamilyDefn[W <: Term : TypeTag, U <: Term : TypeTag](dom: Typ[W], codom: Typ[Typ[U]], f: W => Typ[U]) = {
	  FuncDefn[W, Typ[U]](f, dom, codom)
	}
	
	/** For all/Product for a type family. This is the type of dependent functions */
	case class PiTyp[W<: Term : TypeTag, U<: Term : TypeTag](fibers: TypFamily[W, U]) extends Typ[FuncTerm[W, U]]{
	  type Obj = DepFuncObj[W, U]
	  
	  lazy val typ = Universe(max(univlevel(fibers.codom), univlevel(fibers.dom.typ)))
	  
	  override def symbObj[A](name: A) = DepFuncSymb[A, W, U](name, fibers)
	  
	  def subs(x: Term, y: Term) = PiTyp[W, U](fibers.subs(x, y))
	}
	
	/** Exists/Sum for a type family */
	case class SigmaTyp[W<: Term, U<: Term](fibers: TypFamily[W, U]) extends Typ[Term]{
	  lazy val typ = Universe(max(univlevel(fibers.codom), univlevel(fibers.dom.typ)))
	  
	  type Obj = Term
	  
	  def symbObj[A](name : A) = SymbObj(name, this)
	  
	  def subs(x: Term, y: Term) = SigmaTyp[W, U](fibers.subs(x, y))
	}
	
	/** Object in a dependent function type, i.e.,
	 *  a dependent function. Has a family of codomains 
	 */
	trait DepFuncObj[W<: Term, U<: Term] extends FuncTerm[W, U]{
	  val fibers: TypFamily[W, U] 
	   
	  
//	  lazy val typ = PiTyp(fibers)
	  
	  def apply(arg: W) = action(arg.asInstanceOf[fibers.dom.Obj])
	  
	  def action(arg: fibers.dom.Obj): U
	  
	}
	
	/** A formal dependent function, with application symbolic
	 *  
	 */
	trait FormalDepFunc[W<: Term with Subs[W],  U<: Term with Subs[U]] extends DepFuncObj[W, U] with FormalFuncTerm[W, U]{
//	  def act(arg: W) = if (arg.typ == fibers.dom) {
//	    val typ =fibers(arg) 
//	    Some(typ.symbObj(FormalDepApplication[W, V, U](this, arg)).asInstanceOf[U]) 
//	  }
//	  else None
	  
	  
	  def action(arg: fibers.dom.Obj) = fibers(arg).symbObj(FormalDepApplication[W, U](this, arg)).asInstanceOf[U]
	}
	
	
	case class DepFuncSymb[A, W<: Term : TypeTag,  U<: Term : TypeTag](name: A, fibers: TypFamily[W, U]) extends DepFuncObj[W, U] with FormalFuncTerm[W, U] with Symbolic[A]{
	  val domobjtpe = typeOf[W]
	  
	  val codomobjtpe = typeOf[U]
	  
	  val dom = fibers.dom
	  
	  lazy val typ = PiTyp(fibers)
	  
	  def action(arg: fibers.dom.Obj) = fibers(arg).symbObj(ApplnSym(this, arg)).asInstanceOf[U]
	}
	
	/** A symbol capturing formal application
	 *  of a dependent function.
	 */ 
	case class FormalDepApplication[W<: Term with Subs[W],  U<: Term](func: DepFuncObj[W, U], arg: W) extends Term  with FormalAppl[W, U]{
	  val typFamily = func.fibers
      lazy val typ : Typ[U] = (typFamily(arg))
      
      override def toString = func.toString + "("+ arg.toString +")"
      
      def subs(x: Term, y: Term) = func.subs(x, y)(arg.subs(x, y))
    }
	
	/** The identity type. 
	 *  This is the type lhs = rhs
	 */
	case class IdentityTyp[U <: Term](dom: Typ[U], lhs: U, rhs: U) extends Typ[Term]{
	  type Obj = Term
	  
	  lazy val typ = Universe(max(univlevel(lhs.typ.typ), univlevel(rhs.typ.typ)))
	  
	  def subs(x: Term, y: Term) = IdentityTyp(dom.subs(x, y), lhs.subs(x,y), rhs.subs(x,y))
	  
	  def symbObj[A](name: A)= SymbObj(name, this)
	}
	
	/** A dependent function given by a scala funcion */
	case class DepFuncDefn[W<: Term : TypeTag, U<: Term with Subs[U] : TypeTag](func: W => U, dom: Typ[W], fibers: TypFamily[W, U]) extends DepFuncObj[W, U]{
	  val domobjtpe = typeOf[W]
	  
	  val codomobjtpe = typeOf[U]
	  
	  lazy val typ = PiTyp[W, U](fibers)
	  
	  def act(arg: W) = if (arg.typ == dom) Some(func(arg)) else None
	  
	  def action(arg: fibers.dom.Obj): U = func(arg)
	  
	  def subs(x: Term, y: Term) = DepFuncDefn((w : W) => func(w).subs(x, y), dom, fibers.subs(x, y))
	}
	
	/** Companion to dependent functions */
	object DepFuncObj{
	  def apply[W<: Term : TypeTag,  U<: Term with Subs[U] : TypeTag](func: Term => U, dom: Typ[W], univ: Univ with Typ[Typ[U]]): DepFuncObj[W, U] = {
	    def section(arg: Term) = func(arg).typ.asInstanceOf[Typ[U]]
	    val fibers: TypFamily[W, U] = typFamilyDefn[W, U](dom, univ, section)
	    DepFuncDefn(func, dom, fibers)
	  }
	}
	
	case object Unit extends LogicalSTyp 
	
	/*
	 * A common trait for contexts and typpatterns
	 */
	trait TypSeq[+U <: Term, V<: Term]{
	  type PtnType <: U
	  
	  def apply(tp : Typ[V]) : Typ[PtnType]
	}
	
	
	/*
	 * A simple pattern, for inductive type constructors as well as type families.
	 */
	trait TypPtn[U <: Term] extends TypPtnLike with TypSeq[U, Term]{
	  	 type PtnType = U
	  	 
	  	  def induced(W : Typ[Term], X : Typ[Term])(f : Term => Term) : PtnType => PtnType
	  	  
	  	  def inducedDep(W : Typ[Term], Xs : Term => Typ[Term])(f : Term => Term) : PtnType => PtnType
	}
	
	trait TypPtnLike extends PolyPtn[Term]{
	  	 type PtnType <:  Term
	  	 
	  	 def apply(tp : Typ[Term]) : Typ[PtnType]
	  	 
	  	 def induced(W : Typ[Term], X : Typ[Term])(f : Term => Term) : PtnType => PtnType
	  	  
	  	 def inducedDep(W : Typ[Term], Xs : Term => Typ[Term])(f : Term => Term) : PtnType => PtnType
	}
	
	
	
	/*
	 * A composite pattern for inductive types.
	 */
	trait PolyPtn[+U <: Term]{
	  def -->:[V <: Term : TypeTag,  UU >: U <: Term : TypeTag](that : TypPtn[V]) = FuncPtn[UU](that, this)
	  
	  def apply(tp : Typ[Term]) : Typ[PolyPtnType]
	  
	  type PolyPtnType = U
	  
	  def constructor[A](tp: Typ[Term], name: A) : Constructor = {
	    val cons = apply(tp).symbObj(name)
	    ConstructorDefn(this, cons)
	  }
	  
	  val univLevel : Int
	 
	}
	
	object PolyPtn{
	  val W = IdW
	  
	  
	}

	case object IdW extends  TypPtn[Term]{
	  def apply(W : Typ[Term]) = W
	  
	  val univLevel = 0
	  
//	  type PtnType = Term
	  
	  def induced(W : Typ[Term], X : Typ[Term])(f : Term => Term) = f
	  
	  def inducedDep(W : Typ[Term], Xs : Term => Typ[Term])(f : Term => Term) = f
	} 

	/*
	case class OtherPtn(tp : Typ[Term]) extends TypPtn[Term]{
	  
//	  type PtnType = Term
	  
	  def apply(W : Typ[Term]) = tp
	  
	  val univLevel = univlevel(tp.typ)
	}
	*/
	
	case class FuncPtn[U<:Term : TypeTag](tail: TypPtnLike, head : PolyPtn[U]) extends PolyPtn[FuncTerm[Term, U]]{
//	  type PtnType = FuncTerm[Term, head.PtnType]
	  
	  def apply(W : Typ[Term]) = FuncTyp[Term, head.PolyPtnType](tail(W), head(W))
	  
	  val univLevel = max(head.univLevel, tail.univLevel)
	}
	
	case class CnstFncPtn[U <: Term : TypeTag](tail: Typ[Term], head : PolyPtn[U]) extends PolyPtn[FuncTerm[Term, U]]{
//	  type PtnType = FuncTerm[Term, head.PtnType]
	  
	  def apply(W : Typ[Term]) = FuncTyp[Term, head.PolyPtnType](tail, head(W))
	  
	  val univLevel = head.univLevel
	}
	
	case class SimpleFuncPtn[V <: Term with Subs[V]: TypeTag](tail : Typ[Term], head : TypPtn[V]) extends TypPtn[FuncTerm[Term, V]]{
	  def apply(W: Typ[Term]) = FuncTyp[Term, head.PtnType](tail, head(W))
	  
	  val univLevel = max(head.univLevel, univlevel(tail.typ))
	  
	  def induced(W : Typ[Term], X: Typ[Term])(f : Term => Term) : PtnType => PtnType = {
	    (g : PtnType) => 
	      val func =((t : Term) => head.induced(W, X)(f) (g(t)))
	      val codomain = head(X)
	      FuncDefn[Term, head.PtnType](func, tail, codomain)
	  }
	  
	  def inducedDep(W : Typ[Term], Xs: Term => Typ[Term])(f : Term => Term) : PtnType => PtnType = {
	    (g : PtnType) => 
	      val func =((t : Term) => head.induced(W, Xs(t))(f) (g(t)))
	      val section = (t : Term) => head(Xs(t))
	      val fiber = typFamilyDefn[Term, head.PtnType](tail, MiniVerse(head(W)), section)
	      DepFuncDefn[Term, head.PtnType](func, tail, fiber)
	  }
	}
	
	
	case class DepFuncPtn[U <: Term : TypeTag](tail: TypPtnLike, headfibre : Term => PolyPtn[U], headlevel: Int = 0) extends PolyPtn[FuncTerm[Term, U]]{
	  def apply(W : Typ[Term]) : Typ[FuncTerm[Term, U]]   = {
	    val head = headfibre(W.symbObj(""))
	    val fiber = typFamilyDefn[Term, U](tail(W), MiniVerse(head(W)),  (t : Term) => headfibre(t)(W))
	    PiTyp[Term, U](fiber)
	  }
	  
	  
	  
//	  type PtnType = Term
	  
	  val univLevel = max(tail.univLevel, headlevel)
	}
	
	case class CnstDepFuncPtn[U <: Term : TypeTag](tail: Typ[Term], headfibre : Term => PolyPtn[U], headlevel: Int = 0) extends PolyPtn[FuncTerm[Term, U]]{
	  def apply(W : Typ[Term]) : Typ[FuncTerm[Term, U]] = {
	    val head = headfibre(tail.symbObj(""))
	    val fiber = typFamilyDefn[Term, U](tail, MiniVerse(head(W)),  (t : Term) => headfibre(t)(W))
	    PiTyp[Term, U](fiber)
	  }
	  
//	  type PtnType = Term
	  
	  val univLevel = headlevel
	}
	
	/*
	 * Issues: Replace codomain Universe(0) by something reasonable - done.
	 * Correct the induced function
	 */
	case class SimpleDepFuncPtn[V <: Term with Subs[V] : TypeTag](tail: Typ[Term], headfibre : Term => TypPtn[V] with TypPtn[V], headlevel: Int = 0) extends TypPtn[FuncTerm[Term,V]]{
	  def apply(W : Typ[Term]) = {
	    val fiber = typFamilyDefn[Term, head.PtnType](tail, MiniVerse(head(W)),  (t : Term) => headfibre(t)(W))
	    PiTyp[Term, head.PtnType](fiber)
	  }
	  
	  val head = headfibre(tail.symbObj(""))
	  
//	  type PtnType = FuncTerm[Term, head.PtnType]
	  
	   def induced(W : Typ[Term], X: Typ[Term])(f : Term => Term) : PtnType => PtnType = {
	    (g : PtnType) => 
	      val func =((t : Term) => head.induced(W, X)(f) (g(t)))
	      val fiber = typFamilyDefn[Term, head.PtnType](tail, MiniVerse(head(X)),  (t : Term) => headfibre(t)(X))
	      DepFuncDefn[Term, V](func, tail, fiber)
	  }
	   
	  def inducedDep(W : Typ[Term], Xs: Term => Typ[Term])(f : Term => Term) : PtnType => PtnType = {
	    (g : PtnType) => 
	      val func =((t : Term) => head.induced(W, Xs(t))(f) (g(t)))
	      val fiber = typFamilyDefn[Term, head.PtnType](tail, MiniVerse(head(W)),  (t : Term) => headfibre(t)(Xs(t)))
	      DepFuncDefn[Term, V](func, tail, fiber)
	  }
	   
	  val univLevel = max(univlevel(tail.typ), headlevel)
	}

//	case class Constructor(cons: Term, pattern : PolyPtn, typ : Typ[Term]){
//	  require(cons.typ == pattern(typ))
//	}
	
	trait Constructor{
	  type PtnType <: Term
	  
	  val pattern : PolyPtn[PtnType]
	  
//	  val typ: Typ[Term]
	  
	  val cons: PtnType
	}
	
	case class ConstructorDefn[U <: Term](pattern: PolyPtn[U], cons: U) extends Constructor{
	  type PtnType = U
	}
	
	/*
	object Constructor{
	  def apply(pattern : PolyPtn, typ: Typ[Term])(f : pattern.PtnType) = new Constructor(pattern, typ) {
	    
	    val cons = f.asInstanceOf[pattern.PtnType]
	  }
	}
	* 
	*/
	
	
	trait InductiveTyp extends Typ[Term]{
	  val ptns : List[PolyPtn[Term]] = constructors map (_.pattern)
	  
	  val constructorFns : List[Term] = constructors map (_.cons)
	  
	  val constructors : List[Constructor]
	  
	  assert((constructorFns.map(_.typ)) == (ptns map (_(this))), "constructors do not have given patterns")
	}
	
	
	class InductiveTypDefn[A](symptns : List[(A, PolyPtn[Term])]) extends Typ[Term] with InductiveTyp{
	  type Obj = Term
	  
//	  val constructorFns : List[Term] = for ((a, p) <- symptns) yield (p(this).symbObj(a))
	  
//	  val ptns = for ((a, p) <- symptns) yield p
	  
	  lazy val constructors = for ((name, ptn) <- symptns) yield ptn.constructor(this, name) 
	  
	  val univLevel = (ptns map (_.univLevel)).max
	  
	  val typ = Universe(univLevel)
	  
	  def subs(x : Term, y: Term) = this
	  
	  def symbObj[A](name: A): Term = SymbObj(name, this)
	} 
	
	trait PolyPtnInstance[D <: Term, +U <: Term]{
    
		def apply(tps: D => Typ[Term]) : Typ[PolyPtnType]
		
		type PolyPtnType = U
	}
	

  
	case class TypPtnInstance[D <: Term](ptns: D => TypPtn[Term], arg: D) extends PolyPtnInstance[D, Term]{
		val ptn = ptns(arg)
	  
		def apply(tps: D => Typ[Term]) : Typ[Term] = ptns(arg)(tps(arg))
		
		def induced(W : D => Typ[Term], X : Typ[Term])(f : Term => Term) : Term => Term = {
		  ptns(arg).induced(W(arg), X)(f)
		}
	  	  
	  	def inducedDep(W : D => Typ[Term], Xs : Term => Typ[Term])(f : Term => Term) : Term => Term ={
	  	  ptns(arg).inducedDep(W(arg), Xs)(f)
	  	}
		
	}
  
	case class FuncPtnInstance[D <: Term, U <: Term : TypeTag](tail: TypPtnInstance[D], head: PolyPtnInstance[D, U]) extends PolyPtnInstance[D, FuncTerm[Term, U]]{
		def apply(tps: D => Typ[Term]) = FuncTyp[Term, head.PolyPtnType](tail(tps), head(tps))
   
	}
  
	/*
	case class CnstDepFuncPtnInstance[D <: Term, +U <: Term : TypeTag](tail: Typ[Term], headfibre: Term => PolyPtnInstance[D, U]) extends PolyPtnInstance[D, FuncTerm[Term,U]]{
		def apply(Ws : D => Typ[Term]) : Typ[FuncTerm[Term, U]] = {
			val head = headfibre(tail.symbObj(""))
			val fiber = typFamilyDefn[Term, U](tail, MiniVerse(head(Ws)),  (t : Term) => headfibre(t)(Ws))
					PiTyp[Term, U](fiber)
		}
	}
	*/
	
	case class CnstFuncPtnInstance[D <: Term, U <: Term : TypeTag](tail: Typ[Term], head: PolyPtnInstance[D, U]) extends PolyPtnInstance[D, FuncTerm[Term, U]]{
		def apply(tps: D => Typ[Term]) = FuncTyp[Term, head.PolyPtnType](tail, head(tps))
   
	}
  
	
	case class DepFuncPtnInstance[D <: Term, +U <: Term : TypeTag](tail: TypPtnInstance[D], headfibre: Term => PolyPtnInstance[D, U]) extends PolyPtnInstance[D, FuncTerm[Term,U]]{
		def apply(Ws : D => Typ[Term]) : Typ[FuncTerm[Term, U]] = {
			val head = headfibre(tail(Ws).symbObj(""))
			val fiber = typFamilyDefn[Term, U](tail(Ws), MiniVerse(head(Ws)),  (t : Term) => headfibre(t)(Ws))
					PiTyp[Term, U](fiber)
		}
	}

	trait ConstructorFmly[D <: Term]{
	  type PtnType <: Term
	  
	  type DomType = D
	  
//	  val domain : Typ[DomType]
	  
	  val pattern : TypPtnInstance[DomType]
	  
	  
	  val cons: PtnType
	}
	
	case class ConstructorFmlyDefn[D <: Term, U <: Term](domain: Typ[D], pattern: TypPtnInstance[D], cons: U) extends ConstructorFmly[D]{
	  type PtnType = U
	  
//	  type DomType = D
	}
	
	trait InductiveTypFmly[D <: Term] extends (D => Typ[Term]){
//	  val ptns : List[PolyPtn[Term]] = constructors map (_.pattern)
	  
//	  val constructorFns : List[Term] = constructors map (_.cons)
	  
	  val constructors : List[ConstructorFmly[D]]
	  
	  val domain: Typ[D]
	  
	}
	
	
	
//	val x = 'x' :: __
	
//	val y = "y" :: x
	
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











