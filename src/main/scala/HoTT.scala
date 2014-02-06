package provingGround

import scala.language.implicitConversions 
import scala.util._
import scala.language.existentials
//import scala.reflect.runtime.universe._


// To Do:
//
// Dynamics: Most types.
// Substitution: All types except functions and dependent functions.
// 
// Variables in Pi-types (dependent functions) should be Formal dependent functions - done.
//
// Reformulate: All types are (dependent) functions, inductive types or universes.
// In code: Make an inductive type into a class, with the other types extending this.
//


/** The Homotopy type theory objects, types and utility functions
 *  
 */
object HoTT{
  /** Abstract object */
    trait AbsObj {
      /** 
       * Gives limited information on types when the types are universes, so should not be used in this case.
       * The type `LogicalUniverse' should only be read as some universe.
       * So avoid making this judgement. 
       */
        def typ: Typ[AbsObj]
        
    
    def subs(x: AbsObj, y: AbsObj): AbsObj
    }
    
    /*
     * Objects with simple substitution.
     */
    trait AtomicObj extends AbsObj{
      def subs(x: AbsObj, y: AbsObj) = if (x==this) y else this
    }
   
    
    /*
     * Sub-objects of the given one
     */
    val subObjs: AbsObj => List[AbsObj] ={
      case pair: AbsPair[_,_] => subObjs(pair.first) ::: subObjs(pair.second)
      case obj: FormalApplication[_,_,_] => subObjs(obj.arg) ::: subObjs(obj.func)
      case eq: IdentityTyp[_] => subObjs(eq.lhs) ::: subObjs(eq.rhs)
      case fnTyp : FuncTyp[_, _, _] => subObjs(fnTyp.dom) ::: subObjs(fnTyp.codom) 
      case Lambda(variable, value) => subObjs(value) map (lambda(variable))
      case PiTyp(fibers) => subObjs(fibers)
      case SigmaTyp(fibers) => subObjs(fibers)
      case fx: FormalAppl[_,_] => subObjs(fx.func) ::: subObjs(fx.arg)
      
      /* Above case should cover this
      case sym: Symbolic[_] =>
        sym.name match {
          case fnsym : FormalApplication[_, _,_] => subObjs(fnsym.arg) ::: subObjs(fnsym.func)
          case fnsym : FormalDepApplication[_, _,_] => subObjs(fnsym.arg) ::: subObjs(fnsym.func)
          case _ => List(sym)
        }
        * /
        */
      case obj: AbsObj => List(obj)
    } 
    
    /** HoTT Type;
     *  The compiler knows that objects have scala-type extending U.
     *  In particular, we can specify that the objects are types, types of types etc.
     *  We should handle covariance better and move methods to this.
     */
    trait Typ[+U <: AbsObj] extends AbsObj{
    	/** scala type of objects with this HoTT-type */
        type Obj <: U
        
        /** A symbolic object with specified HoTT type, by default this, and with scala-type Obj*/
        def symbObj[A, W<: AbsObj](name: A, tp: Typ[W] = this): Obj
        
        /** Make symbolic object */
        def ::[A](name:A) = symbObj(name) 
        
        def subs(x: AbsObj, y: AbsObj) : Typ[U]
        
        // Template for a method allowing covariance. The problem is FuncTyp is not contravariant
//        def -->[W <: AbsObj](that : Typ[W]) = FuncTyp[Obj, Typ[Obj], W](this, that)
    }
    
    trait AtomicTyp[U <: AbsObj] extends Typ[U]{
      def subs(x: AbsObj, y: AbsObj) : Typ[U] = if (x == this) y.asInstanceOf[Typ[U]] else this
    }
    
    /** Result to be used, not its proof */ 
    class Result[U<: AbsObj](val typ: Typ[U], pf: =>AbsObj) extends AtomicObj{
      def proof = pf
    }

    /** traits that are given by name;
     *  does not include, for instance, pairs each of whose instance is given by a name;
     *  same considerations for functions etc.
     */
    trait Symbolic[A]{
      val name: A
      override def toString = name.toString
    }
    
    def sameName[A](sym: A): AbsObj => Boolean = {
      case obj : Symbolic[_] =>
        obj.name match {
          case `sym` => true
          case _ => false
        }
      case _ => false
    }
    
    /** Constructing symbolic objects that are AbsObj but no more refined*/
    case class SymbObj[A, +U<: AbsObj](name: A, typ: Typ[U]) extends AtomicObj with Symbolic[A]{
      override def toString = name.toString+" : "+typ.toString
    } 
    
    /** Symbolic types, which the compiler knows are types.
     *  The base tells the scala type of objects and gives a factory for symbolic objects of this scala type.
     */
    case class SymbTyp[A, U<:AbsObj, T<: AbsObj](name: A, univ: Typ[T], base: Typ[U]) extends AtomicTyp[U] with Symbolic[A]{
      lazy val typ = univ
      
      type Obj = base.Obj
      
      def symbObj[B, W <: AbsObj](name: B, tp: Typ[W] ) = base.symbObj(name, tp)
      
      override def toString = name.toString+" : "+typ.toString
      
      def elem = this
    }
    
    case class SymbLogicTyp[A](name: A) extends LogicalTyp with Symbolic[A]{
      override def toString = name.toString+" : "+typ.toString
    }

    /** HoTT types with underlying scala type AbsObj.
     *  They belong to the LogicalUniv
     */
    class LogicalTyp extends AtomicTyp[AbsObj]{
      
      type Obj = AbsObj
      
      lazy val typ = LogicalUniv
      
      def symbObj[A, W <: AbsObj](name: A, tp: Typ[W]): AbsObj = SymbObj(name, tp)
      
      /** returns function type A -> B */
      def -->[U <: AbsObj](that: Typ[U]) = FuncTyp[AbsObj, LogicalTyp, U](this, that)
 
      def ~>[U <: AbsObj](fibers: TypFamily[AbsObj, LogicalTyp, U]) = PiTyp(fibers)
    }
    
    /** Universes. Can be just a type so far */
    trait Univ[U<:AbsObj] extends Typ[Typ[U]] with AtomicTyp[Typ[U]]
    
    /** Inductive construction of the next universe */
    case class NextUniv[U<: AbsObj](base: Typ[U]) extends Univ[U]{
      lazy val typ = NextUniv[Typ[U]](this)
      
      type Obj = Typ[U]
      
      def symbObj[A, W<: AbsObj](name: A, tp: Typ[W])= SymbTyp(name, tp, base)
    }
    
    /** The first universe, consisting of logical types */
    object LogicalUniv extends Univ[AbsObj]{
      lazy val typ = NextUniv[AbsObj](this)
      
      type Obj = LogicalTyp
      
      def symbObj[A, W<: AbsObj](name: A, tp: Typ[W]) = SymbLogicTyp(name)
      
      override def toString ="__"
    }
    
    val __ = LogicalUniv
    
    /** Pair of types (A, B) */
    case class PairTyp[U<: AbsObj, V <: AbsObj](first: Typ[U], second: Typ[V]) extends 
						Typ[AbsPair[U, V]] with AbsPair[Typ[U], Typ[V]]{

    	type Obj = AbsPair[U, V]

		lazy val typ = PairTyp(first.typ, second.typ)
		
		def subs(x: AbsObj, y: AbsObj) = PairTyp(first.subs(x, y), second.subs(x, y))
			
			// The name is lost as `name', but can be recovered using pattern matching.
		def symbObj[A, W<: AbsObj](name: A, tp: Typ[W]): AbsPair[U, V] = PairObj(first.symbObj(name), second.symbObj(name))
			}	
    
    /** Object (a, b) in (A, B) */
    case class PairObj[U <: AbsObj, V <: AbsObj](val first: U, val second: V) extends AbsPair[U, V]{
    	lazy val typ = PairTyp(first.typ, second.typ)
    	
    	def subs(x: AbsObj, y: AbsObj) = PairObj(first.subs(x, y), second.subs(x, y))
					}

    /** Abstract pair, parametrized by scala types of components */
	trait AbsPair[U<: AbsObj, V <: AbsObj] extends AbsObj{
	  val first: U
	  val second: V
	}
	
	def mkPair[U<: AbsObj, V<: AbsObj](f: U, s: V) = (f, s) match{
	  case (fst: Typ[_], scnd: Typ[_]) => PairTyp(fst, scnd)
	  case (fst, scnd) => PairObj(fst, scnd)
	}
    
	/** Function type */
    case class FuncTyp[W<: AbsObj, V <: Typ[W], +U<: AbsObj](dom: V, codom: Typ[U]) extends LogicalTyp{
	  override def symbObj[A, T<: AbsObj](name: A, tp: Typ[T]) = FuncSymb[A, W, V, U](name, dom, codom)
	  
	  override def toString = dom.toString + " -> " + codom.toString
	}
    
    trait FuncLikeObj[-W <: AbsObj, +U <: AbsObj] extends AbsObj with (W => U){
      def apply(arg: W): U
      
      def subs(x: AbsObj, y: AbsObj) : FuncLikeObj[W, U]
    }
    
    trait FormalFuncObj[-W <: AbsObj, +U <: AbsObj] extends FuncLikeObj[W, U]{
      def subs(x: AbsObj, y: AbsObj) = if (x==this) y.asInstanceOf[FuncLikeObj[W, U]] else this
    }
    
	/** a function, i.e.,  an object in a function type */
    trait FuncObj[W<: AbsObj, V<: Typ[W], +U<: AbsObj] extends FuncLikeObj[W, U]{
      /** domain*/
	  val dom: V
	  /** codomain */
	  val codom: Typ[U]
	   
	  lazy val typ = FuncTyp[W, V, U](dom, codom)
	  

	  /** Action, i.e., function application */
	  def action(arg:dom.Obj): codom.Obj 
	  
	  /** Function application */
	  def apply(arg: W) = action(arg.asInstanceOf[dom.Obj])
	  
	}

    /** Formal function, i.e., with application purely symbolic. */
	trait FormalFunc[W<: AbsObj, V<: Typ[W], +U<: AbsObj] extends FuncObj[W, V, U] with FormalFuncObj[W, U]{
	  def act(arg: W) = if (arg.typ == dom) Some(codom.symbObj(FormalApplication[W, V, U](this, arg))) else None
	  
	  def action(arg: dom.Obj) = codom.symbObj(FormalApplication[W, V, U](this, arg))
	}
	
	/** Symbol containing function info */
    case class FuncSymb[A, W<: AbsObj, V<: Typ[W], +U<: AbsObj](name: A, dom: V, codom: Typ[U]) extends FormalFunc[W, V, U] with Symbolic[A]
	
    trait FormalAppl[W <: AbsObj, U <: AbsObj]{
      val func: FuncLikeObj[W, U]
      
      val arg: W
    }
    
    object FormalAppl{
      def unapply[W <: AbsObj, U <: AbsObj](obj: U): Option[(FuncLikeObj[W, U], W)] =obj match{
        case sym: Symbolic[_] =>
        sym.name match {
          case fnsym : FormalAppl[W, U] => Some(((fnsym.func : FuncLikeObj[W, U]), (fnsym.arg: W)))
          case _ => None
        }
        case _ => None
      }
    }
    
    /** A formal structure representing func(arg) - especially used for pattern matching */
    case class FormalApplication[W<: AbsObj, V<: Typ[W], U<: AbsObj](func: FuncObj[W, V, U], arg: W) extends AbsObj with FormalAppl[W, U]{
      lazy val typ = func.codom
      
      override def toString = func.toString + "("+ arg.toString +")"

      def subs(x: AbsObj, y: AbsObj) = func.subs(x, y)(arg.subs(x, y).asInstanceOf[W])
    }
	
    /** A function given by a scala function */
	case class FuncDefn[W<: AbsObj, V<: Typ[W], U<: AbsObj](func: AbsObj => U, dom: V, codom: Typ[U]) extends FuncObj[W, V, U] with FormalFuncObj[W, U]{
	  def act(arg: AbsObj) = if (arg.typ == dom) Some(func(arg)) else None
	  
	  def action(arg: dom.Obj) = func(arg).asInstanceOf[codom.Obj]
	}
	
	/** A lambda-expression.
	 *  variable is mapped to value.
	 *  
	 *  Correction? should allow general variables.
	 *  Should revisit if LogicalTyp variables are too restricitive.
	 */
	case class Lambda[X<: AbsObj, Y <: AbsObj](variable: X, value : Y) extends AbsObj{
	  lazy val typ = (variable.typ.asInstanceOf[LogicalTyp]) --> value.typ
	  
	  def apply(arg: AbsObj) = value.subs(variable, arg)
	  
	  def subs(x: AbsObj, y: AbsObj) = Lambda(variable, value.subs(x, y))
	}
	
	
	/** Lambda constructor
	 *  
	 */		
	def lambda[U<: AbsObj, V <: AbsObj](variable: U)(value : V) = Lambda(variable, value)
	
	/** Context
	 *  
	 */
	trait Context{
	  val givens: Stream[AbsObj]
	  
	  def export(obj: AbsObj): AbsObj
	  
	  def apply(obj: AbsObj) = export(obj)	  
	  
	  def typed[A](name: A) = givens find (sameName(name))
	  
	  def allTyped[A](name: A) = givens filter (sameName(name))
	}
	
	
	/** Context given by chain of lambdas. 
	 *  The first object is the latest
	 */
	case class LambdaContext(givens: Stream[AbsObj]) extends Context{
	  def export(obj: AbsObj) = (obj /: givens)((a, x) => lambda(x)(a))
	  
	  def +(variable: AbsObj) = LambdaContext(variable #:: givens)
	  
	  def assume(ass: AbsObj) = this + ass
	}
	
	
	/** Type family, with domain in a subclass of Typ[W] and codomain in Typ[U]*/
	type TypFamily[W<: AbsObj, V<: Typ[W], U<: AbsObj] = FuncObj[W, V, Typ[U]]
	
	trait LogicalTypFamily extends TypFamily[AbsObj, LogicalTyp, AbsObj] with (AbsObj => LogicalTyp){
	  /** codomain */
	  val codom = LogicalUniv
	   
//	  lazy val typ = FuncTyp[AbsObj, LogicalTyp, AbsObj](dom, codom)
	  

	  /** Action, i.e., function application */
	  def action(arg:dom.Obj): LogicalTyp 
	  
	  /** Function application */
	  override def apply(arg: AbsObj) = action(arg.asInstanceOf[dom.Obj])
	  
	}
	
	case class TypFamilyDefn(dom: LogicalTyp, f: AbsObj => LogicalTyp) extends LogicalTypFamily{
	  def action(arg: dom.Obj) = f(arg)
	  
	  def subs(x: AbsObj, y: AbsObj) : LogicalTypFamily = this
	}
	
	/** For all/Product for a type family. This is the type of dependent functions */
	case class PiTyp[W<: AbsObj, V<: Typ[W], U<: AbsObj](fibers: TypFamily[W, V, U]) extends LogicalTyp{
	  override def symbObj[A, T<: AbsObj](name: A, tp: Typ[T]) = DepFuncSymb[A, W, V, U](name, fibers)
	}
	
	/** Exists/Sum for a type family */
	case class SigmaTyp[W<: AbsObj, V<: Typ[W], U<: AbsObj](fibers: TypFamily[W, V, U]) extends LogicalTyp
	
	/** Object in a dependent function type, i.e.,
	 *  a dependent function 
	 */
	trait DepFuncObj[W<: AbsObj, V<: Typ[W], U<: AbsObj] extends FuncLikeObj[W, U]{
	  val fibers: TypFamily[W, V, U] 
	   
	  
	  lazy val typ = PiTyp(fibers)
	  
	  def apply(arg: W) = action(arg.asInstanceOf[fibers.dom.Obj])
	  
	  def action(arg: fibers.dom.Obj): U
	  
	}
	
	/** A formal dependent function, with application symbolic
	 *  
	 */
	trait FormalDepFunc[W<: AbsObj, V<: Typ[W], U<: AbsObj] extends DepFuncObj[W, V, U] with FormalFuncObj[W, U]{
	  def act(arg: W) = if (arg.typ == fibers.dom) {
	    val typ =fibers(arg) 
	    Some(typ.symbObj(FormalDepApplication[W, V, U](this, arg)).asInstanceOf[U]) 
	  }
	  else None
	  
	  
	  def action(arg: fibers.dom.Obj) = fibers(arg).symbObj(FormalDepApplication[W, V, U](this, arg)).asInstanceOf[U]
	}
	
	
	case class DepFuncSymb[A, W<: AbsObj, V<: Typ[W], U<: AbsObj](name: A, fibers: TypFamily[W, V, U]) extends FormalDepFunc[W, V, U] with Symbolic[A]
	
	/** A symbol capturing formal application
	 *  of a dependent function.
	 */ 
	case class FormalDepApplication[W<: AbsObj, V<: Typ[W], U<: AbsObj](func: DepFuncObj[W, V, U], arg: W) extends AbsObj  with FormalAppl[W, U]{
	  val typFamily = func.fibers
      lazy val typ : Typ[U] = (typFamily(arg)) 
      
      override def toString = func.toString + "("+ arg.toString +")"
      
      def subs(x: AbsObj, y: AbsObj) = func.subs(x, y)(arg.subs(x, y).asInstanceOf[W])
    }
	
	/** The identity type. 
	 *  This is the type lhs = rhs
	 */
	case class IdentityTyp[U <: AbsObj](dom: Typ[U], lhs: AbsObj, rhs: AbsObj) extends LogicalTyp
	
	/** A dependent function given by a scala funcion */
	case class DepFuncDefn[W<: AbsObj, V<: Typ[W], U<: AbsObj](func: W => U, dom: V, fibers: TypFamily[W, V,U]) extends DepFuncObj[W, V, U] with FormalFuncObj[W, U]{
	  def act(arg: W) = if (arg.typ == dom) Some(func(arg)) else None
	  
	  def action(arg: fibers.dom.Obj): U = func(arg)
	}
	
	/** Companion to dependent functions */
	object DepFuncObj{
	  def apply[W<: AbsObj, V<: Typ[W], U<: AbsObj](func: AbsObj => U, dom: V, univ: Univ[U]): DepFuncObj[W, V, U] = {
	    def section(arg: AbsObj) = func(arg).typ.asInstanceOf[Typ[U]]
	    val fibers: FuncObj[W, V, Typ[U]] = FuncDefn[W, V, Typ[U]](section, dom, univ)
	    DepFuncDefn(func, dom, fibers)
	  }
	}
	
	case object Unit extends LogicalTyp
	
	/*
	 * Function to LogicalTyp, gives a type associated to an element of W, which is the domain of domain -> W
	 */
	trait ConstructorDomain extends LogicalTyp with (LogicalTyp => LogicalTyp){
	  def recdomtyp(W: LogicalTyp, Q : LogicalTyp) : LogicalTyp
	  
	  def rectyp(W: LogicalTyp, Q : LogicalTyp) = recdomtyp(W, Q) --> (W --> Q)
	}
	
	trait SimpleConstructor extends ConstructorDomain{
	  def ->:(head: LogicalTyp): SimpleConstructor
	  
	  def constyp(tp: => LogicalTyp) = (apply(tp) --> tp)
	  
	  def cons(tp: => LogicalTyp) = constyp(tp).symbObj(tp)
	  

	}
	
	
	
	trait ConstConstructor extends SimpleConstructor{
	  def ->:(head: LogicalTyp) = ToConstantCons(head, this)
	  
	  def recdomtyp(W: LogicalTyp, Q: LogicalTyp) = Q
	}
	
	trait ToW extends SimpleConstructor{
	  def ->:(head: LogicalTyp) = ToToWCons(head, this)
	  
	  /*
	   * Type of the domain of the recursor: an element of this gives a function W --> Q
	   */
	  def recdomtyp(W: LogicalTyp, Q : LogicalTyp) : LogicalTyp = constyp(W) --> constyp(Q)
	  
	  
	
	}
	
	case class SingleConstConstrutor(codom: LogicalTyp) extends ConstConstructor{
	  def apply(tp: LogicalTyp) = codom
	}
	
	/*
	 * The domain of the constructor W -> W
	 */
	case object Succ extends ToW{
	  def apply(tp: LogicalTyp) = tp
	  
//	  def recdomtyp(W: LogicalTyp, Q : LogicalTyp) : LogicalTyp = W --> Q
	  	  
	}
	
	/*
	 * Inductively, we get the constructor domain head -> cons given cons
	 */
	case class ToToWCons(head: LogicalTyp, tail: ToW) extends ToW{
	  def apply(tp: LogicalTyp) = head --> (tail(tp))
	}
	
	case class ToConstantCons(head: LogicalTyp, tail: ConstConstructor) extends ConstConstructor{
	  def apply(tp: LogicalTyp) = head --> (tail(tp))
	}
	
	/*
	 * Suppose A : B -> Univ is a dependent type A(b), then we get a function mapping (W : LogicalTyp) to
	 * (b: B) -> (A(b) -> W), which is a type family. We associate to W the corresponding Pi-type.
	 */
	case class DepToWCons(head: LogicalTyp, tailFamily: LogicalTyp => TypFamily[AbsObj, LogicalTyp, LogicalTyp]) extends ToW{
	  def apply(tp: LogicalTyp) = head ~> tailFamily(tp)
	}
	
	case class DepConstCons(head: LogicalTyp, tailFamily: LogicalTyp => TypFamily[AbsObj, LogicalTyp, LogicalTyp]) extends ConstConstructor{
	  def apply(tp: LogicalTyp) = head ~> tailFamily(tp)
	}
	
	case class CompositeConstructor(headCons: SimpleConstructor, tailCons: ConstructorDomain) extends ConstructorDomain{
	  def apply(tp: LogicalTyp) = headCons(tp) --> tailCons(tp)
	  
	  def recdomtyp(W: LogicalTyp, Q : LogicalTyp) = headCons.recdomtyp(W, Q) --> tailCons.recdomtyp(W, Q)
	}
	
	
	
	val x = 'x' :: __
	
	val y = "y" :: x
	
	/** Symbol factory */	
	def nextChar(s: Set[Char]) = if (s.isEmpty) 'a' else (s.max + 1).toChar
  
	/** Helper for symbol factory */
	def usedChars(s: Set[AbsObj]): Set[Char] = {
	    def charOpt (obj:AbsObj) : Option[Char] = obj match {
	      case sym: Symbolic[_] => Some(Try(sym.name.asInstanceOf[Char]).toOption).flatten
	      case _ => None
	    }
	    
	    
	    s collect (Function.unlift(charOpt _))
	}
	
}











