package provingground

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
// 
//

// TODO: Sigma and equality types as inductive types

/** The Homotopy type theory objects, types and utility functions
 *  
 */
object HoTT{
	trait AnySym
	
	case class Name(name: String) extends AnySym{
	  override def toString = name.toString
	}
	
	implicit def stringSym(name: String) = Name(name)
  
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
     
      //TODO refine types so this is allowed - maybe not, as universes may cause trouble.
      // def typ: Typ[U]
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
      case Lambda(variable, value : Term) => subObjs(value) map (lambda(variable))
      case PiTyp(fibers) => subObjs(fibers)
      case SigmaTyp(fibers) => subObjs(fibers)
//      case fx: FormalAppl[_,_] => subObjs(fx.func) ::: subObjs(fx.arg)
      
      /* Above case should cover this
      case sym: Symbolic =>
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
        
        def obj: U = {
          object newname extends AnySym
          symbObj(newname)
        }
        
        def !:[UU >: U <: Term](term: UU) = {
          assert(term.typ == this," Expected "+toString+"but found "+term.typ.toString)
          term}
        
        val typ : Univ
        
        lazy val typlevel : Int = univlevel(typ) 
        
        /** A symbolic object with specified HoTT type, by default 'this', and with scala-type Obj*/
        def symbObj(name: AnySym): U with Subs[U]
         
        /** Make symbolic object */
        def ::(name:AnySym) = symbObj(name) 
        
        def &&[UU >: U <: Term with Subs[UU], V <: Term with Subs[V]](
            that : Typ[V]) = PairTyp[UU, V](this, that)
            
        def ++[UU >: Typ[U] <: Typ[Term] with Subs[UU] : TypeTag, 
        VV <: Term with Subs[VV], V <: Typ[VV] with Subs[V] : TypeTag](
            those : V) = SigmaTyp[UU, VV](LambdaFixed[UU, V](this,
                those))
        
//        def subs(x: Term, y: Term) : Typ[U]
        
        // Template for a method allowing covariance. 
        def ->:[W <: Term : TypeTag, UU >: U <: Term : TypeTag](that : Typ[W]) = FuncTyp[W, UU](that, this)
        
        def ~>:[UU >: U <: Term :  TypeTag](variable: Term) = {
          val fiber = LambdaFixed[Term, Typ[UU]](variable, this)
          PiTyp(fiber)
        }
        
    }
    

    object SimpleSyms{
    	val Arrow = "->"
    	val MapsTo = "|->"
    	val Pi ="Pi"
    	val Sigma = "S"
    }
    
    object UnicodeSyms{
    	val Arrow = '\u27F6'.toString
    	val MapsTo = "\u27FC"
    	val Pi ="Pi"
    	val Sigma = "S"
    }
    
  //  import SimpleSyms._
    import UnicodeSyms._

    /** traits that are given by name;
     *  does not include, for instance, pairs each of whose instance is given by a name;
     *  same considerations for functions etc.
     */
    trait Symbolic{
      val name: AnySym
      override def toString = name.toString
    }
    
    def sameName[A](sym: A): Term => Boolean = {
      case obj : Symbolic =>
        obj.name == sym
      case _ => false
    }
    
    /** Constructing symbolic objects that are Term but no more refined*/
    case class SymbObj[+U<: Term](name: AnySym, typ: Typ[U]) extends Term with Symbolic{
      override def toString = "("+name.toString+" : "+typ.toString+")"
      
      def subs(x: Term, y: Term) = this match {
        case `x` => y
        case applptnterm(func : FuncTerm[u, Term], arg) => 
          Try(func.subs(x,y)(arg.subs(x, y).asInstanceOf[u])).getOrElse(SymbObj(name, typ.subs(x, y)))
        case _ => SymbObj(name, typ.subs(x, y))
      }
    } 
    
    /** Symbolic types, which the compiler knows are types.
     *  The base tells the scala type of objects and gives a factory for symbolic objects of this scala type.
     */
    case class SymbTyp(name: AnySym) extends Typ[Term] with Symbolic{
      lazy val typ = Universe(0)
      
      type Obj = Term
      
      def symbObj(name: AnySym) = SymbObj(name, this)
      
      override def toString = name.toString
      
      def elem = this
      
      
      val applptntypu = ApplnPattern[Term, Typ[Term]]()
      
      // Change this to remove first case after checking
      def subs(x: Term, y: Term) = (x, y, name) match {
        case (u: Typ[_], v: Typ[_], _) if (u == this) => v
        case (_, _,applptntypu(func, arg)) => func.subs(x,y)(arg.subs(x, y))
        case _ => this
      }
    }
    
    /*
    case class SymbLogicTyp[A](name: A) extends LogicalSTyp with Symbolic{
      override def toString = name.toString+" : "+typ.toString
    }
    */
    
    
    
    type LogicalTyp = Typ[Term]
    
    /*
     * Types with symbolic objects not refined.
     */
    trait SmallTyp extends Typ[Term]{
    	type Obj = Term
    
    	val typ = Universe(0)
    
     
    	def symbObj(name: AnySym): Term = SymbObj(name, this)      
      
    	def subs(x: Term, y: Term) = (x, y) match {
    		case (xt: Typ[_], yt : Typ[_]) if (xt == this) => yt
    		case _ => this
    		}
    }

    type Univ = Typ[Typ[Term]]
    
    
    /** The universes */
    case class Universe(level : Int) extends Univ{
      require(level >= 0)
      
      type Obj = Typ[Term]
      
      lazy val typ  = Universe(level +1)
      
      
      def symbObj(name: AnySym)= SymbTyp(name)
      
      object Underscore extends AnySym
      
      val __ = symbObj(Underscore)
      
      def subs(x : Term, y : Term) = this
      
      override def toString = "_"
    }
    
    def univlevel: Typ[Typ[Term]] => Int = {
      case Universe(l) => l
      case _ => 0
    }
    
    
    //Wrapper for Universe with scala type
    case class ScalaUniv[U <: Term](univ: Typ[Typ[U]])
    
    implicit val baseUniv : ScalaUniv[Term] = ScalaUniv(__)
    
    case class HigherUniv[U <: Typ[Term]](univ: Typ[U]) extends Typ[Typ[U]]{
      type Obj = Typ[U]
      
      lazy val typ = HigherUniv[Typ[U]](this)
      
      def symbObj(name: AnySym)= univ
      
      def subs(x : Term, y : Term) = this
    }
    
    implicit def higherUniv[U <: Term](implicit sc : ScalaUniv[U]) : ScalaUniv[Typ[U]] = {
      ScalaUniv(HigherUniv(sc.univ))
    }
    
    
    case class MiniVerse[U <: Term](sample : Typ[U]) extends Typ[Typ[U]]{
      type Obj = Typ[U]
      
      lazy val typ = MiniVerse[Typ[U]](this)
      
      def symbObj(name: AnySym)= sample
      
      def subs(x : Term, y : Term) = this
    }
    
    
    
    
    
    case class FineVerse[U <: Term with Subs[U]](level: Int = 0, subsymbobj: AnySym => U) extends Typ[Typ[U]]{
      type Obj = Typ[U]
      
      lazy val typ = FineVerse[U](level +1, subsymbobj)
      
      def subs(x: Term, y: Term) = this
      
      def symbObj(name: AnySym) = FineSymbTyp[U](name, subsymbobj)

    }
    

    
    case class FineSymbTyp[U<: Term with Subs[U]](name: AnySym, symbobj: AnySym => U) extends Typ[U] with Symbolic{
      lazy val typ = FineVerse[U](0, symbObj)
      
      
      
      type Obj = U
      
      def symbObj(name: AnySym): U = symbobj(name)
      
      override def toString = name.toString
      
      def elem = this
      
      
      val applptntypu = ApplnPattern[Term, Typ[Term]]()
      
      // Change this to remove first case after checking
      def subs(x: Term, y: Term) = (x, y, name) match {
        case (u: Typ[_], v: Typ[_], _) if (u == this) => v.asInstanceOf[Typ[U]]
        case (_, _,applptntypu(func, arg)) => func.subs(x,y)(arg.subs(x, y)).asInstanceOf[Typ[U]]
        case _ => this
      }
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
    case class PairTyp[U<: Term  with Subs[U], V <: Term with Subs[V]](
        first: Typ[U] with Subs[Typ[U]], second: Typ[V] with Subs[Typ[V]]) extends 
						Typ[PairObj[U, V]] with AbsPair[Typ[U], Typ[V]]{

    	type Obj = PairObj[U, V]

		lazy val typ = Universe(Math.max(first.typlevel, second.typlevel))
		
		def subs(x: Term, y: Term) = PairTyp(first.subs(x, y), second.subs(x, y))
			
			// The name is lost as `name', but can be recovered using pattern matching.
		def symbObj(name: AnySym): Obj = PairObj(first.symbObj(leftSym(name)), second.symbObj(rightSym(name)))
			}	
    
    /** Object (a, b) in (A, B) */
    case class PairObj[U <: Term with Subs[U], V <: Term with Subs[V]](first: U, second: V) extends AbsPair[U, V] with Subs[PairObj[U, V]]{
    	lazy val typ = PairTyp(first.typ, second.typ)
    	
    	def subs(x: Term, y: Term) = PairObj[U, V](first.subs(x, y), second.subs(x, y))
					}

    /** Abstract pair, parametrized by scala types of components */
	trait AbsPair[U<: Term, V <: Term] extends Term{
	  val first: U
	  val second: V
	}
	
	def pair[U <: Term with Subs[U], V <: Term with Subs[V]](
	    first: U, second: V) = PairObj(first, second)
	
	def pair[U<: Term  with Subs[U], V <: Term with Subs[V]](
        first: Typ[U] with Subs[Typ[U]], second: Typ[V] with Subs[Typ[V]]) = PairTyp(first, second)     
	    
//	def mkPair(f: Term, s: Term) = (f, s) match{
//	  case (fst: Typ[u], scnd: Typ[v]) => PairTyp[u with Subs[u], v with Subs[v]](fst, scnd)
//	  case (fst, scnd) => PairObj(fst, scnd)
//	}
    
	/** Function type */
    case class FuncTyp[W<: Term : TypeTag, U<: Term : TypeTag](dom: Typ[W], codom: Typ[U]) extends Typ[FuncObj[W, U]] with 
    Subs[FuncTyp[W, U]]{
      type Obj = FuncObj[W, U]
      
      lazy val typ = Universe(max(dom.typlevel, codom.typlevel))
      
	  def symbObj(name: AnySym) = FuncSymb[W, U](name, dom, codom)
	  
	  override def toString = "("+dom.toString + Arrow + codom.toString+")"
	  
	  def subs(x : Term, y: Term) = FuncTyp[W, U](dom.subs(x, y), codom.subs(x,y))
	}
    
    
    case class domsym(func: AnySym) extends AnySym
    
    case class codomsym(func: AnySym) extends AnySym
    
    case class fnsym(func: AnySym) extends AnySym
    
    case class FuncTypUniv[W<: Term : TypeTag, U<: Term : TypeTag](
        domuniv: Typ[Typ[W]], codomuniv: Typ[Typ[U]]) extends Typ[FuncTyp[W, U]]{
      
      lazy val typ = HigherUniv(this)
      
      def symbObj(name: AnySym) = {
        val dom = domuniv.symbObj(domsym(name))
        val codom = codomuniv.symbObj(codomsym(name))
        FuncTyp(dom, codom)
      }
      
      def subs(x: Term, y: Term) = this
    }
    
    
    implicit def funcUniv[W<: Term : TypeTag, U<: Term : TypeTag](implicit
        domsc: ScalaUniv[W], codomsc: ScalaUniv[U]) : ScalaUniv[FuncObj[W, U]] = {
      ScalaUniv(FuncTypUniv(domsc.univ, codomsc.univ) : Typ[FuncTyp[W, U]])
    }
    
    /**
     * (dependent) function, wraps around FuncTerm but without requiring type parameters.
     * 
     */
    trait FuncTermLike extends Term with Subs[FuncTermLike]{
      type D
      type Cod
      
      type Obj <: FuncTermLike
      
      val domobjtpe : Type
      
      val codomobjtpe: Type
      
      val dom: Typ[Term]
      
//      val depcodom : D => Typ[Cod]
      
      def apply(arg: D): Cod
      
//      def andThen[WW >: U <: Term, UU <: Term](fn: WW => UU): FuncTerm[WW, UU]
      
      def subs(x: Term, y: Term) : FuncTermLike
    }
    
    
    /**
     * Includes both functions and dependent functions
     */
    trait FuncTerm[-W <: Term, +U <: Term] extends Term with (W => U) with Subs[FuncTerm[W, U]]{
      type Obj <: FuncTerm[W, U]
      
      val domobjtpe : Type
      
      val codomobjtpe: Type
      
      val dom: Typ[Term]
      
      val depcodom : W => Typ[U]
      
      def apply(arg: W): U
      
//      def andThen[WW >: U <: Term, UU <: Term](fn: WW => UU): FuncTerm[WW, UU]
      
      def subs(x: Term, y: Term) : FuncTerm[W, U]
    }
    
    /*
     * A symbol representing a formal application
     */
    case class ApplnSym[W <: Term : TypeTag, U <: Term : TypeTag](func : FuncTerm[W, U], arg : W) extends AnySym{
      override def toString = func.toString + "("+ arg.toString +")"
    }
    
    
    /*
     * Pattern matching for a formal application.
     */
    @deprecated("June 2014", "use general version") case class ApplnPattern[W <: Term : TypeTag, U <: Term : TypeTag](){
      def unapply(term : Term) : Option[(FuncTerm[W, U], W)] = term match {
        case sym : Symbolic => sym.name match {
          case sm @ ApplnSym(func : FuncTerm[W, U], arg) if typeOf[W] <:< func.domobjtpe & func.codomobjtpe <:< typeOf[U]  => 
            Some((func, arg.asInstanceOf[W])) 
          case _ => None
        }
        case _ => None
      }
    }
    
    class ApplnPatternAny{
      def unapply(term : Term) : Option[(FuncTermLike, Term)] = term match {
        case sym : Symbolic => sym.name match {
          case sm @ ApplnSym(func : FuncTermLike, arg)  => 
            Some((func, arg)) 
          case _ => None
        }
        case _ => None
      }
    }
    
 //   val applptnterm = ApplnPattern[Term, Term]()
    val applptnterm = new ApplnPatternAny
    
    
	/** 
	 *  a function (not dependent), i.e.,  an object in a function type, has a codomain and a fixed type for the domain. 
	 *  
	 */
    trait FuncObj[W<: Term, +U <: Term] extends FuncTerm[W, U] with Subs[FuncObj[W, U]]{
      /** domain*/
	  val dom: Typ[W]
	  /** codomain */
	  val codom: Typ[U]
	  
	  val depcodom : W => Typ[U] = _ => codom

	  /** Action, i.e., function application */
//	  def action(arg:dom.Obj): codom.Obj 
	  
	  /** Function application */

	
	  def apply(arg: W) : U
	  
	  def subs(x: Term, y : Term) : FuncObj[W, U]
	  
	}

    /** Formal function, i.e., with application purely symbolic. */
//	trait FormalFunc[W<: Term, V<: Typ[W], U<: Term] extends FuncObj[W, V, U] with FormalFuncTerm[W, U]{	  
//	  def action(arg: dom.Obj) = codom.symbObj(FormalApplication[W, V, U](this, arg))
//	}
	
	/** Symbol containing function info */
    case class FuncSymb[W<: Term : TypeTag, U<: Term : TypeTag](name: AnySym, dom: Typ[W], codom: Typ[U]) extends 
              FuncObj[W, U] with Subs[FuncObj[W, U]] with Symbolic with AnySym{
      val domobjtpe = typeOf[W]
      
	  val codomobjtpe = typeOf[U]
      
      lazy val typ = FuncTyp[W, U](dom, codom)
      
      def apply(arg: W) : U = codom.symbObj(ApplnSym(this, arg))
      
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
	  
	  type D = W
	  
	  type Cod = U
	  
	  lazy val typ = FuncTyp[W, U](dom, codom)
	  
	  def apply(arg: W) = func(arg)
	  
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
	 *  This may or may not be a dependent function.
	 *  If it is important to note that it is not dependent, and hence has scala type FuncObj, then use LambdaFixed
	 * 
	 *  
	 *  
	 */
	abstract class LambdaLike[X<: Term : TypeTag, +Y <: Term with Subs[Y]: TypeTag](variable: X, value : Y) extends FuncTerm[X, Y]{
	  val domobjtpe = typeOf[X]
	  
	  val codomobjtpe = typeOf[Y]
	  
	  type D = X
	  
	  type Cod = Y
	  
	  val dom = variable.typ
	  
	  override def toString = "("+variable.toString+ MapsTo +value.toString+")"
	  
	  val dep : Boolean
	  
	  lazy val typ : Typ[FuncTerm[Term, Term]] = if (dep) {
	    val fibre = (t : Term) => value.typ subs (variable, t)
	    
	    val family : FuncObj[Term, Typ[Term]] = LambdaFixed(variable, value.typ)
	    PiTyp(family)
	  }
		  else FuncTyp[Term,  Term](variable.typ , value.typ)
	  
	  def apply(arg: X) = value.subs(variable, arg)
	  
	  def subs(x: Term, y: Term) = (x, y) match {
		    case (u : Typ[_], v : Typ[_]) if (variable.typ.subs(u, v) != variable.typ) => 
		      val newvar = changeTyp(variable, variable.typ.subs(u, v))
		      Lambda(newvar , value.subs(x,y))
		    case _ =>
		      val newvar = variable.typ.obj
		      val newval = value.subs(variable, newvar).subs(x, y).subs(newvar, variable)
		      Lambda(variable, newval)
		  }
	 
	  
	  object myv extends TmpTerm
	  
	  def andthen(f : Term => Term) = Lambda(variable.subs(variable, myv), f(value.subs(variable, myv)))
	  
	  def andThen[Z<: Term with Subs[Z] : TypeTag](f : Y => Z) = Lambda(variable, f(value))
	}
	
	/**
	 * functions given by lambda, which may be dependent - this is checked by making a substitution.
	 */
	case class Lambda[X<: Term : TypeTag, Y <: Term with Subs[Y]: TypeTag](variable: X, value : Y) extends LambdaLike(variable, value){
	  object mysym extends AnySym
	  
	  // type D = X
	  
	  // type Cod = Y
	  
	  val depcodom : X => Typ[Y] = (t : X) => value.typ.subs(variable, t).asInstanceOf[Typ[Y]]
	  
	  val myvar = variable.typ.symbObj(mysym)
	  
	  val dep = value.typ.subs(variable, myvar) != value.typ
	}
	
	case class LambdaFixed[X<: Term : TypeTag, +Y <: Term with Subs[Y]: TypeTag](variable: X, value : Y) 
		extends LambdaLike(variable, value) with FuncObj[X, Y]{
	  override val dom = variable.typ.asInstanceOf[Typ[X]]
	  
	  val codom = value.typ.asInstanceOf[Typ[Y]]
	  
	  val dep = false
	  
//	  override def apply(arg : X) = super.apply(arg)
	  
	  override	def subs(x: Term, y: Term) : FuncObj[X, Y] = (x, y) match {
		    case (u : Typ[_], v : Typ[_]) if (variable.typ.subs(u, v) != variable.typ) => 
		      val newvar = changeTyp(variable, variable.typ.subs(u, v))
		      LambdaFixed(newvar.asInstanceOf[X] , value.subs(x,y))
		    case _ => LambdaFixed(variable.subs(x,y).asInstanceOf[X], value.subs(x, y))
		  }
	  
	}
	
	implicit class TermSymbol(term: Term) extends AnySym
	
	def changeTyp(term: Term, newtyp : Typ[Term]) : Term = term match {
	  case sym : Symbolic => newtyp.symbObj(sym.name)
	  case _ => newtyp.symbObj(term)
	}
	
	def instantiate(substitutions : Term => Option[Term], target: Typ[Term]) : Term => Option[Term] = {
	  case t: Term if t.typ ==target => Some(t)
	  case Lambda(variable, value : Term) => 
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
	
	def lmbda[U<: Term : TypeTag, V <: Term with Subs[V] : TypeTag](variable: U)(value : V) : FuncObj[U, V] = LambdaFixed(variable, value)
	
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
	case class PiTyp[W<: Term : TypeTag, U<: Term : TypeTag](fibers: TypFamily[W, U]) extends 
		Typ[FuncTerm[W, U]] with Subs[PiTyp[W, U]]{
	  type Obj = DepFuncObj[W, U]
	  
	  lazy val typ = Universe(max(univlevel(fibers.codom), univlevel(fibers.dom.typ)))
	  
	  override def symbObj(name: AnySym) = DepFuncSymb[ W, U](name, fibers)
	  
	  def subs(x: Term, y: Term) = PiTyp[W, U](fibers.subs(x, y))
	 
	  override def toString = Pi+"("+fibers.toString+")"
	}
	
	def typFamily[W <: Term : TypeTag, U <: Term : TypeTag](dom: Typ[W], f: W => Typ[U])(
	    implicit su: ScalaUniv[U]) = {
	//  val codom = MiniVerse(f(dom.symbObj("")))
	  val codom = su.univ
	  FuncDefn[W, Typ[U]](f, dom, codom)
	}
	
	case class PiTypUniv[W<: Term : TypeTag, U<: Term : TypeTag](
        domuniv: Typ[Typ[W]], codomuniv: Typ[Typ[U]]) extends Typ[PiTyp[W, U]]{
      
      lazy val typ = HigherUniv(this)
      
      def symbObj(name: AnySym) = {
        val dom = domuniv.symbObj(domsym(name))
        val codom = codomuniv.symbObj(codomsym(name))
        val typFmly = FuncTyp(dom, codomuniv).symbObj(name)
        PiTyp(typFmly)
//        FuncTyp(dom, codom)
      }
      
      def subs(x: Term, y: Term) = this
    }
	
	implicit def piUniv[W<: Term : TypeTag, U<: Term : TypeTag](implicit
        domsc: ScalaUniv[W], codomsc: ScalaUniv[U]) : ScalaUniv[FuncTerm[W, U]] = {
      ScalaUniv(PiTypUniv(domsc.univ, codomsc.univ) : Typ[PiTyp[W, U]])
    }
	
	case class leftSym(name: AnySym) extends AnySym{
	  override def toString = name.toString + "_1"
	}
	
	case class rightSym(name: AnySym) extends AnySym{
	  override def toString = name.toString + "_1"
	}
	
	/** Exists/Sum for a type family */
	case class SigmaTyp[W<: Term with Subs[W], U<: Term with Subs[U]](
	    fibers: TypFamily[W, U]) extends Typ[DepPair[W, U]]{
	  lazy val typ = Universe(max(univlevel(fibers.codom), univlevel(fibers.dom.typ)))
	  
	  type Obj = DepPair[W, U]
	  
//	  def symbObj(name : AnySym) = SymbObj(name, this)
	  
	  def symbObj(name: AnySym) = {
	    val a = fibers.dom.symbObj(leftSym(name))
	    val b = fibers(a).symbObj(rightSym(name))
	    DepPair(a, b, fibers)
	  }
	  
	  def subs(x: Term, y: Term) = SigmaTyp[W, U](fibers.subs(x, y))
	  
	//  override def toString = Sigma+"("+fibers.toString+")"
	}
	
	/**
	 * Dependent pair (a: A, b : B(a)) - element of a sigma type.
	 * 
	 */
	case class DepPair[W<: Term with Subs[W], U<: Term with Subs[U]](a : W, b: U, fibers: TypFamily[W, U]) extends Term with
		Subs[DepPair[W, U]]{
	  val typ = SigmaTyp(fibers)
	  
	  def subs(x: Term, y: Term) = DepPair(a.subs(x,y), b.subs(x,y), fibers.subs(x, y))
	}
	
	/** 
	 *  Object in a dependent function type, i.e.,
	 *  a dependent function. Has a family of codomains 
	 */
	trait DepFuncObj[W<: Term, U<: Term] extends FuncTerm[W, U]{
	  val fibers: TypFamily[W, U] 
	   
	  type D = W
	  
	  type Cod = U
	  
//	  lazy val typ = PiTyp(fibers)
	  
	  def apply(arg: W) = action(arg.asInstanceOf[fibers.dom.Obj])
	  
	  def action(arg: fibers.dom.Obj): U
	  
	}
	
	/** 
	 *  A formal dependent function, with application symbolic
	 *  
	 *  
	 
	trait FormalDepFunc[W<: Term with Subs[W],  U<: Term with Subs[U]] extends DepFuncObj[W, U] with FormalFuncTerm[W, U]{
//	  def act(arg: W) = if (arg.typ == fibers.dom) {
//	    val typ =fibers(arg) 
//	    Some(typ.symbObj(FormalDepApplication[W, V, U](this, arg)).asInstanceOf[U]) 
//	  }
//	  else None
	  
	  
	  def action(arg: fibers.dom.Obj) = fibers(arg).symbObj(FormalDepApplication[W, U](this, arg)).asInstanceOf[U]
	}
	*/
	
	case class DepFuncSymb[W<: Term : TypeTag,  U<: Term : TypeTag](name: AnySym, fibers: TypFamily[W, U]) extends 
	DepFuncObj[W, U] with Symbolic with AnySym{
	  val domobjtpe = typeOf[W]
	  
	  val codomobjtpe = typeOf[U]
	  
	  // type D = W
	  
	  // type Cod = U
	  
	  val dom = fibers.dom
	  
	  val depcodom : W => Typ[U] = (arg : W) => fibers(arg)
	  
	  lazy val typ = PiTyp(fibers)
	  
	  def action(arg: fibers.dom.Obj) = fibers(arg).symbObj(ApplnSym(this, arg)).asInstanceOf[U]
	  
	  def subs(x: Term, y: Term) = (x, y, name) match {
        case (u: Typ[_], v: Typ[_], _) => DepFuncSymb(name, fibers.subs(u, v))
        case (u, v: FuncObj[W, U], _) if (u == this) => v
        case _ => this
      }
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
	  
	  def symbObj(name: AnySym)= SymbObj(name, this)
	}
	
	case class PlusTyp(first: Typ[Term], second: Typ[Term]) extends SmallTyp{
	  def i(value: Term) = PlusTyp.FirstIncl(this, value)
	  
	  def j(value: Term) = PlusTyp.ScndIncl(this, value)
	}
	
	case object PlusTyp{
	  case class FirstIncl(typ: PlusTyp,  value: Term) extends Term{
		def subs(x: Term, y: Term) = FirstIncl(typ, value.subs(x, y))  
	  }
	  
	  case class ScndIncl(typ: PlusTyp,  value: Term) extends Term{
		def subs(x: Term, y: Term) = ScndIncl(typ, value.subs(x, y))  
	  }
	}
	
	
	/** A dependent function given by a scala funcion */
	case class DepFuncDefn[W<: Term : TypeTag, U<: Term with Subs[U] : TypeTag](func: W => U, dom: Typ[W], fibers: TypFamily[W, U]) extends DepFuncObj[W, U]{
	  val domobjtpe = typeOf[W]
	  
	  val codomobjtpe = typeOf[U]
	  
	 // type D = W
	  
	 // type Cod = U
	  
	  val depcodom : W => Typ[U] = (arg: W) => fibers(arg)
	  
	  lazy val typ = PiTyp[W, U](fibers)
	  
	  def act(arg: W) = if (arg.typ == dom) Some(func(arg)) else None
	  
	  def action(arg: fibers.dom.Obj): U = func(arg)
	  
	  def subs(x: Term, y: Term) = DepFuncDefn((w : W) => func(w).subs(x, y), dom, fibers.subs(x, y))
	}
	
	
	def depFunc[W<: Term : TypeTag, U<: Term with Subs[U] : TypeTag](dom: Typ[W], func: W => U)(
	    implicit su: ScalaUniv[U]): FuncTerm[W, U] = {
	  val fibers = typFamily(dom, (w: W) => func(w).typ.asInstanceOf[Typ[U]])
	  DepFuncDefn(func, dom, fibers)
	}
	
	implicit class RichTypFamily[W<: Term : TypeTag, U<: Term with Subs[U] : TypeTag](
	    fibre: FuncObj[W, Typ[U]])(
	    implicit su: ScalaUniv[U]){
//	  val dom = func.dom
	  
	 def pi = PiTyp(fibre)
	}
	
	/** Companion to dependent functions */
	object DepFuncObj{
	  def apply[W<: Term : TypeTag,  U<: Term with Subs[U] : TypeTag](func: Term => U, dom: Typ[W], univ: Univ with Typ[Typ[U]]): DepFuncObj[W, U] = {
	    def section(arg: Term) = func(arg).typ.asInstanceOf[Typ[U]]
	    val fibers: TypFamily[W, U] = typFamilyDefn[W, U](dom, univ, section)
	    DepFuncDefn(func, dom, fibers)
	  }
	}
	
	case object Unit extends SmallTyp
	
	/*
	 * A common trait for contexts and typpatterns
	 */
	trait TypSeq[+U <: Term, V<: Term]{
	  type PtnType <: U
	  
	  def apply(tp : Typ[V]) : Typ[PtnType]
	}
	
	

	
	/**
	 * folds in as many terms of the list as possible, 
	 * applying terms as long as the result is a function and the list is non-empty. 
	 */
	def foldterms: (Term, List[Term]) => Term = {
	  case (f: FuncTerm[u, _], x :: ys) if f.dom == x.typ => 
	    foldterms(f(x.asInstanceOf[u]), ys)
	  case (t, _) => t
	}
	
	/**
	 * folds in as many terms with names given by the list as possible, 
	 * applying terms as long as the result is a function and the list is non-empty. 
	 */
	def foldnames : (Term, List[AnySym]) => Term = {
	  case (f: FuncTerm[u, _], x :: ys)  =>
	    val newvar = f.dom.symbObj(x)
	    foldnames(f(newvar.asInstanceOf[u]), ys)
	  case (t, _) => t
	}
	
	
//	val x = 'x' :: __
	
//	val y = "y" :: x
	
	/** Symbol factory */	
	def nextChar(s: Set[Char]) = if (s.isEmpty) 'a' else (s.max + 1).toChar
  
	/** Helper for symbol factory */
	def usedChars(s: Set[Term]): Set[Char] = {
	    def charOpt (obj:Term) : Option[Char] = obj match {
	      case sym: Symbolic => Some(Try(sym.name.asInstanceOf[Char]).toOption).flatten
	      case _ => None
	    }
	    
	    
	    s collect (Function.unlift(charOpt _))
	}
	
}











