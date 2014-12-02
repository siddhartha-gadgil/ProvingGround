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

//

/** The Homotopy type theory objects, types and utility functions
 *
 */
object HoTT{
  /**
   * Symbol
   */
	trait AnySym

	/**
	 * Strings as symbols
	 */
	case class Name(name: String) extends AnySym{
	  override def toString = name.toString
	}

	/**
	 * use strings as symbols.
	 */
	implicit def stringSym(name: String) = Name(name)

  /** Abstract object */
    trait Term extends Subs[Term]{
      /**
       * Gives limited information on types when the types are universes, so should not be used in this case.
       * So avoid making this judgement.
       */
        def typ: Typ[Term]


        def dependsOn(that: Term) = {
          val newVar = innervar(that)
          replace(that, newVar) != this
        }

        def indepOf(that: Term) = !dependsOn(that)

    }

	trait WithTyp[+U <: Typ[Term]] extends Term with Subs[WithTyp[U]]{
	  def typ : U
	}

	/**
	 * specify result of substitution, typically so a class is closed under substitution.
	 */
    trait Subs[+U <: Term]{
      def subs(x: Term, y: Term) : U with Subs[U]

      def replace(x: Term, y: Term) : U with Subs[U] = {
        assert(x.typ==y.typ, s"cannot replace $x of type ${x.typ} with $y of type ${y.typ}")
        (x, y) match {
          case (ab: AbsPair[u, v], cd: AbsPair[w, x]) if (ab.first indepOf ab.second) && (ab.second indepOf ab.first) =>
            replace(ab.first, cd.first) replace(ab.second, cd.second)
          case _ => subs(x, y)
        }
      }

      def newobj : U with Subs[U]
    }


    /**
     * Objects with simple substitution.
     */
    trait AtomicTerm extends Term{
      def subs(x: Term, y: Term) = if (x==this) y else this

      def newobj = typ.obj
    }




    /** HoTT Type;
     *  The compiler knows that objects have scala-type extending U.
     *  In particular, we can specify that the objects are types, functions, dependent functions etc.
     *  We should handle covariance better and move methods to this.
     */
    trait Typ[+U <: Term] extends Term with Subs[Typ[U]]{
    	/** scala type of objects with this HoTT-type */
        type Obj <: U with Subs[U]

        /**
         * factor for producing objects of the given type.
         */
        def obj: U = {
          object newname extends AnySym
          symbObj(newname)
        }

        /**
         * checks term is of this type and returns it; useful for documentation.
         */
        def !:[UU >: U <: Term](term: UU) = {
          assert(term.typ == this," Expected "+toString+"but found "+term.typ.toString)
          term}

        val typ : Univ

        lazy val typlevel : Int = univlevel(typ)

        /** A symbolic object with this HoTT type, and with scala-type Obj*/
        def symbObj(name: AnySym): U with Subs[U]

        /** Make symbolic object */
        def ::(name:AnySym) = symbObj(name)


        /**
         * function type:  this -> that
         */
        def ->:[W <: Term : TypeTag, UU >: U <: Term : TypeTag](that : Typ[W]) = FuncTyp[W, UU](that, this)

        /**
         * dependent function type (Pi-Type): this depends on a variable, which hence gives a type family.
         */
        def ~>:[UU >: U <: Term :  TypeTag](variable: Term) = {
          val fiber = LambdaFixed[Term, Typ[UU]](variable, this)
          PiTyp(fiber)
        }


        /**
         * returns pair type, mainly to use for "and" for structures
         */
        def &&[UU >: U <: Term with Subs[UU], V <: Term with Subs[V]](
            that : Typ[V]) = PairTyp[UU, V](this, that)

        /**
         * returns Sigma-Type, mainly to use as "such that", for example a group type is this with product etc. dependent on this.
         */
        def ++[UU >: Typ[U] <: Typ[Term] with Subs[UU] : TypeTag,
        VV <: Term with Subs[VV], V <: Typ[VV] with Subs[V] : TypeTag](
            those : V) = SigmaTyp[UU, VV](LambdaFixed[UU, V](this,
                those))
    }

    /**
     * symbols for printing
     */
    trait TermSyms{
      val Arrow : String
      val MapsTo : String
      val Pi : String
      val Sigma : String
    }

    /**
     * simple symbols for maps etc.
     */
    object SimpleSyms extends TermSyms{
    	val Arrow = "->"
    	val MapsTo = "|->"
    	val Pi ="Pi"
    	val Sigma = "S"
    }

    /**
     * unicode symbols for maps etc.
     */
    object UnicodeSyms extends TermSyms{
    	val Arrow = '\u27F6'.toString
    	val MapsTo = "\u27FC"
    	val Pi ="Pi"
    	val Sigma = "S"
    }

  //  import SimpleSyms._
    import SimpleSyms._

    /** traits that are given by name;
     *  does not include, for instance, pairs each of whose instance is given by a name;
     *  most useful for pattern matching, where the name contains information about formal function applications etc.
     */
    trait Symbolic{
      val name: AnySym
      override def toString = name.toString
    }

    /**
     * checks if symbolic object has given name.
     */
    def hasName(sym: AnySym): Term => Boolean = {
      case obj : Symbolic =>
        obj.name == sym
      case _ => false
    }

    /** Constructing symbolic objects that are Terms but no more refined*/
    case class SymbObj[+U<: Term : TypeTag](name: AnySym, typ: Typ[U]) extends Term with Symbolic{
      override def toString = "("+name.toString+" : "+typ.toString+")"

      def newobj = SymbObj(new InnerSym(this), typ)

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

      def newobj = SymbTyp(new InnerSym(this))

      type Obj = Term

      def symbObj(name: AnySym) = SymbObj(name, this)

      override def toString = name.toString

      def elem = this


      val applptntypu = ApplnPattern[Term, Typ[Term]]()

      def subs(x: Term, y: Term) = (x, y, name) match {
        case (u: Typ[_], v: Typ[_], _) if (u == this) => v
        case (_, _,applptntypu(func, arg)) => func.subs(x,y)(arg.subs(x, y))
        case _ => this
      }
    }


    /**
     * Types with symbolic objects not refined.
     */
    trait SmallTyp extends Typ[Term]{
    	type Obj = Term

    	val typ = Universe(0)


    	def symbObj(name: AnySym): Term = SymbObj(name, this)

    	def newobj = typ.obj

    	def subs(x: Term, y: Term) = (x, y) match {
    		case (xt: Typ[_], yt : Typ[_]) if (xt == this) => yt
    		case _ => this
    		}
    }

    /**
     * Empty type
     */
    case object Zero extends SmallTyp

    /**
	 * Unit type.
	 */
	case object Unit extends SmallTyp

	case object Star extends AtomicTerm{
      val typ = Unit
    }

    val One = Unit

	case class fromZero[U<: Term : TypeTag](codom: Typ[U]) extends AtomicTerm{
      lazy val typ = Zero ->: codom
    }


    /**
     * Notation for universes.
     */
    type Univ = Typ[Typ[Term]]


    /** The universes */
    case class Universe(level : Int) extends Univ{
      require(level >= 0)

      type Obj = Typ[Term]

      lazy val typ  = Universe(level +1)


      def symbObj(name: AnySym)= SymbTyp(name)

      def newobj = this

      def subs(x : Term, y : Term) = this

      override def toString = "_"
    }

    def univlevel: Typ[Typ[Term]] => Int = {
      case Universe(l) => l
      case _ => 0
    }


    /**
     * Wrapper for universe with refined scala type for objects (i.e., types) in it.
     * Refined scala types typically recursively built from (dependent) function types and types of already refined types.
     */
    case class ScalaUniv[U <: Term](univ: Typ[Typ[U]])

    /**
     * scala universe with no refinement.
     */
    implicit val baseUniv : ScalaUniv[Term] = ScalaUniv(__)

    /**
     * given a universe with objects of scala type Typ[U], gives one with scala type Typ[Typ[U]]
     */
    case class HigherUniv[U <: Typ[Term]](univ: Typ[U]) extends Typ[Typ[U]]{
      type Obj = Typ[U]

      lazy val typ = HigherUniv[Typ[U]](this)

      def symbObj(name: AnySym)= univ

      def newobj = this

      def subs(x : Term, y : Term) = this
    }

    /**
     * implicitly returns from a scala universe of Typ[U] one of Typ[Typ[U]]
     */
    implicit def higherUniv[U <: Term](implicit sc : ScalaUniv[U]) : ScalaUniv[Typ[U]] = {
      ScalaUniv(HigherUniv(sc.univ))
    }


    /**
     * The first universe
     */
    val __ = Universe(0)



    //FIXME pair substitutions wrong - not checked if x = (a, b).
    /** Pair of types (A, B) */
    case class PairTyp[U<: Term  with Subs[U], V <: Term with Subs[V]](
        first: Typ[U], second: Typ[V]) extends
						Typ[PairObj[U, V]] with AbsPair[Typ[U], Typ[V]]{

    	type Obj = PairObj[U, V]

		lazy val typ = Universe(Math.max(first.typlevel, second.typlevel))

		def newobj = PairTyp(first.newobj, second.newobj)

		def subs(x: Term, y: Term) = if (x == this) Try(
		    y.asInstanceOf[PairTyp[U, V]]).getOrElse(
		        {println(y); println(x); println(y.typ);PairTyp(first.subs(x, y), second.subs(x, y))}) else PairTyp(first.subs(x, y), second.subs(x, y))

			// The name is lost as `name', but can be recovered using pattern matching.
		def symbObj(name: AnySym): Obj = PairObj(first.symbObj(LeftSym(name)), second.symbObj(RightSym(name)))
			}

    /** Object (a, b) in (A, B) */
    case class PairObj[U <: Term with Subs[U], V <: Term with Subs[V]](first: U, second: V) extends AbsPair[U, V] with Subs[PairObj[U, V]]{
    	lazy val typ = PairTyp(first.typ, second.typ)

    	def newobj = PairObj(first.newobj, second.newobj)

    	def subs(x: Term, y: Term) = if (x == this) y.asInstanceOf[PairObj[U, V]] else  PairObj[U, V](first.subs(x, y), second.subs(x, y))
					}


    /** Abstract pair, parametrized by scala types of components, generally a pair object or a pair type. */
	trait AbsPair[+U<: Term, +V <: Term] extends Term{
	  val first: U
	  val second: V
	}

	/**
	 * overloaded method returning a pair object or a pair type.
	 */
	def pair[U <: Term with Subs[U], V <: Term with Subs[V]](
	    first: U, second: V) = PairObj(first, second)

	/**
	 * overloaded method returning a pair object or a pair type.
	 */
	def pair[U<: Term  with Subs[U], V <: Term with Subs[V]](
        first: Typ[U] with Subs[Typ[U]], second: Typ[V] with Subs[Typ[V]]) = PairTyp(first, second)



	/** Function type (not dependent functions)*/
    case class FuncTyp[W<: Term : TypeTag, U<: Term : TypeTag](dom: Typ[W], codom: Typ[U]) extends Typ[FuncObj[W, U]] with
    Subs[FuncTyp[W, U]]{
      type Obj = FuncObj[W, U]

      lazy val typ = Universe(max(dom.typlevel, codom.typlevel))

	  def symbObj(name: AnySym) = FuncSymb[W, U](name, dom, codom)

	  override def toString = "("+dom.toString + Arrow + codom.toString+")"

	  def newobj = FuncTyp(dom.newobj, codom.newobj)

	  def subs(x : Term, y: Term) = FuncTyp[W, U](dom.subs(x, y), codom.subs(x,y))
	}

	/**
	 * Symbol for domain of a symbolic function
	 */
    case class DomSym(func: AnySym) extends AnySym

    /**
	 * Symbol for co-domain of a symbolic function
	 */
    case class CodomSym(func: AnySym) extends AnySym

//    case class FnSym(func: AnySym) extends AnySym

    /**
     * Universe whose elements are FuncTyps
     */
    case class FuncTypUniv[W<: Term : TypeTag, U<: Term : TypeTag](
        domuniv: Typ[Typ[W]], codomuniv: Typ[Typ[U]]) extends Typ[FuncTyp[W, U]]{

      lazy val typ = HigherUniv(this)

      def symbObj(name: AnySym) = {
        val dom = domuniv.symbObj(DomSym(name))
        val codom = codomuniv.symbObj(CodomSym(name))
        FuncTyp(dom, codom)
      }

      def newobj = FuncTypUniv(domuniv.newobj, codomuniv.newobj)

      def subs(x: Term, y: Term) = this
    }


    /**
     * implicitly build universe with elements FuncTyps from universes for domain and codomain.
     */
    implicit def funcUniv[W<: Term : TypeTag, U<: Term : TypeTag](implicit
        domsc: ScalaUniv[W], codomsc: ScalaUniv[U]) : ScalaUniv[FuncObj[W, U]] = {
      ScalaUniv(FuncTypUniv(domsc.univ, codomsc.univ) : Typ[FuncTyp[W, U]])
    }


    /**
     * Includes both functions and dependent functions
     *
     */
    trait FuncTerm[-W <: Term, +U <: Term] extends Term with (W => U) with Subs[FuncTerm[W, U]]{
      type Obj <: FuncTerm[W, U]

      val domobjtpe : Type

      val codomobjtpe: Type

      val dom: Typ[Term]

      val depcodom : W => Typ[U]

      def act(arg: W): U

      def apply(arg: W): U = {
        assert(arg.typ == dom, s"function with domain ${dom} cannot act on term ${arg} with type ${arg.typ}")
        act(arg)
      }
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
     case class ApplnPattern[W <: Term : TypeTag, U <: Term : TypeTag](){
      def unapply(term : Term) : Option[(FuncTerm[W, U], W)] = term match {
        case sym : Symbolic => sym.name match {
          case sm @ ApplnSym(func : FuncTerm[W, U], arg) if typeOf[W] <:< func.domobjtpe & func.codomobjtpe <:< typeOf[U]  =>
            Some((func, arg.asInstanceOf[W]))
          case _ => None
        }
        case _ => None
      }
    }



    val applptnterm = ApplnPattern[Term, Term]()
 //   val applptnterm = new ApplnPatternAny


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


	  def act(arg: W) : U

	  def subs(x: Term, y : Term) : FuncObj[W, U]

	}



	/** Symbol containing function info */
    case class FuncSymb[W<: Term : TypeTag, U<: Term : TypeTag](name: AnySym, dom: Typ[W], codom: Typ[U]) extends
              FuncObj[W, U] with Subs[FuncObj[W, U]] with Symbolic with AnySym{

      val domobjtpe = typeOf[W]

	  val codomobjtpe = typeOf[U]

      lazy val typ = FuncTyp[W, U](dom, codom)

      def act(arg: W) : U = codom.symbObj(ApplnSym(this, arg))

      def newobj = typ.obj

      def subs(x: Term, y: Term) = (x, y) match {
        case (u: Typ[_], v: Typ[_]) => FuncSymb(name, dom.subs(u, v), codom.subs(u, v))
        case (u, v: FuncObj[W, U]) if (u == this) => v
        case _ => this
      }

      override def toString = "("+name.toString+" : "+typ.toString+")"
    }




    /** A function given by a scala function */
	case class FuncDefn[W<: Term : TypeTag, U<: Term with Subs[U] : TypeTag](func: W => U, dom: Typ[W], codom: Typ[U]) extends FuncObj[W, U]{
	  val domobjtpe = typeOf[W]

	  val codomobjtpe = typeOf[U]

	  type D = W

	  type Cod = U

	  lazy val typ = FuncTyp[W, U](dom, codom)

	  def act(arg: W) = func(arg)

	  def newobj = typ.obj

	  def subs(x: Term, y: Term) = FuncDefn((w) => func(w).subs(x, y), dom.subs(x, y), codom.subs(x, y))
	}



	/**
	 *  A lambda-expression.
	 *  variable is mapped to value.
	 *  This may or may not be a dependent function.
	 *  If it is important to note that it is not dependent, and hence has scala type FuncObj, then use LambdaFixed
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

	  lazy val typ : Typ[FuncTerm[X, Y]] = if (dep) {
	    val fibre = (t : X) => value.typ subs (variable, t)

	    val family : FuncObj[X, Typ[Y]] = LambdaFixed(variable, value.typ.asInstanceOf[Typ[Y]])
	    PiTyp(family)
	  }
		  else FuncTyp(variable.typ , value.typ.asInstanceOf[Typ[Y]])

	  def act(arg: X) = value.replace(variable, arg)

	  override def hashCode = {
	    val newvar = variable.typ.symbObj(Name("hash"))
	    val newval = value.subs(variable, newvar)
	    41 * (variable.typ.hashCode + 41) + newval.hashCode
	  }


	  def subs(x: Term, y: Term) = (x, y) match {
		    case (u : Typ[_], v : Typ[_]) if (variable.typ.subs(u, v) != variable.typ) =>
		      val newvar = changeTyp(variable, variable.typ.subs(u, v))
		      Lambda(newvar , value.subs(x,y))
		    case _ =>
		      val newvar = variable.newobj
		      val newval = value.subs(variable, newvar).subs(x, y).subs(newvar, variable) // change variable to avoid name clashes.
		      Lambda(variable, newval)
		  }


	  private lazy val myv = variable.newobj

	  def andThen[Z<: Term with Subs[Z] : TypeTag](f : Y => Z) = Lambda(variable, f(value))
	}

	/**
	 * functions given by lambda, which may be dependent - this is checked by making a substitution.
	 */
	case class Lambda[X<: Term : TypeTag, Y <: Term with Subs[Y]: TypeTag](variable: X, value : Y) extends
		LambdaLike(variable, value){


	  val depcodom : X => Typ[Y] = (t : X) => value.typ.subs(variable, t).asInstanceOf[Typ[Y]]

	  val dep = value dependsOn variable

		def newobj = Lambda(variable.newobj, value.newobj)

	  override def equals(that: Any) = that match {
	    case Lambda(x: Term, y : Term) => y.subs(x, variable) == value
	    case _ => false
	  }
	}

	// TODO replace asInstanceOf with structural bounds  {val typ: Typ[Y]}
	/**
	 * lambda which is known to have fixed codomain.
	 */
	case class LambdaFixed[X<: Term : TypeTag, +Y <: Term with Subs[Y]: TypeTag](variable: X, value : Y)
		extends LambdaLike(variable, value) with FuncObj[X, Y] with Subs[LambdaFixed[X, Y]]{
	  override val dom = variable.typ.asInstanceOf[Typ[X]]

	  val codom = value.typ.asInstanceOf[Typ[Y]]

	  val dep = false

	  override def equals(that: Any) = that match {
	    case LambdaFixed(x: Term, y : Term) => y.subs(x, variable) == value
	    case _ => false
	  }


		def newobj = LambdaFixed(variable.newobj.asInstanceOf[X], value.newobj)

	  override	def subs(x: Term, y: Term) : LambdaFixed[X, Y] = (x, y) match {
		    case (u : Typ[_], v : Typ[_]) if (variable.typ.subs(u, v) != variable.typ) =>
		      val newvar = changeTyp(variable, variable.typ.subs(u, v))
		      LambdaFixed(newvar.asInstanceOf[X] , value.subs(x,y))
		    case _ =>
		      val newvar = variable.newobj
		      val newval = value.subs(variable, newvar).subs(x, y).subs(newvar, variable) // change variable to avoid name clashes.
		      LambdaFixed(variable, newval)
		  }

	}

	/**
	 * term as a symbol
	 */
	case class TermSymbol(term: Term) extends AnySym

	implicit def termSymbol(term: Term) : AnySym = TermSymbol(term)
	
	/**
	 * returns symbolic object with new type.
	 */
	def changeTyp[U <: Term](term: U, newtyp : Typ[U]) : U = term match {
	  case sym : Symbolic => newtyp.symbObj(sym.name)
	  case _ => newtyp.symbObj(term)
	}

	/**
	 * instantiates variable values in a term if it is made from lambdas, to give a term (if possible) of a required HoTT type.
	 * @param target required type after instantiation
	 * @param substitutions substitutions to make.
	 */
	def instantiate(substitutions : Term => Option[Term], target: Typ[Term]) : Term => Option[Term] = {
	  case t: Term if t.typ ==target => Some(t)
	  case Lambda(variable, value : Term) =>
	    substitutions(variable) flatMap ((cnst) => {
	      val reduced = (value.subs(variable, cnst))
	       instantiate(substitutions, target)(reduced)})
	  case _ => None
	}


	class InnerSym[U <: Term : TypeTag](variable: U) extends AnySym{
	  override def toString = variable.toString
	}

	def innervar[U <: Term : TypeTag](variable : U) : U = {
		val typ = variable.typ.asInstanceOf[Typ[U]]
		variable match {
		  case PairObj(a : Term, b : Term) => PairObj(
		      a.typ.symbObj(new InnerSym(variable)),
		      b.typ.symbObj(new InnerSym(variable))).asInstanceOf[U]
		  case PairTyp(a : Term, b : Term) => PairTyp(
		      a.typ.symbObj(new InnerSym(variable)),
		      b.typ.symbObj(new InnerSym(variable))).asInstanceOf[U]
		  case DepPair(a : Term, b : Term, fibre) => DepPair[Term, Term](
		      a.typ.symbObj(new InnerSym(variable)),
		      b.typ.symbObj(new InnerSym(variable)), fibre.asInstanceOf[TypFamily[Term, Term]]).asInstanceOf[U]
		  case _ => typ.symbObj(new InnerSym(variable))
		}

	}

	/** Lambda constructor
	 *
	 */
	def lambda[U<: Term : TypeTag, V <: Term with Subs[V] : TypeTag](variable: U)(value : V) : FuncTerm[U, V] = {
	  val newvar = variable.newobj
	  if (variable.typ dependsOn value) Lambda(newvar, value.replace(variable, newvar)) else LambdaFixed(newvar, value.replace(variable, newvar))
	}

	/**
	 * lambda constructor for fixed codomain
	 */
	def lmbda[U<: Term with Subs[U] : TypeTag, V <: Term with Subs[V] : TypeTag](variable: U)(value : V) : FuncObj[U, V] = {
		val newvar = variable.newobj
	    LambdaFixed(newvar, value.replace(variable, newvar))
	}

	/**
	 * lambda if necessary, otherwise constant.
	 */
	def optlambda(variable: Term) : Term => Term = value =>
	  	    if (value dependsOn variable)  lambda(variable)(value) else value


	def lambdaPair[U<: Term with Subs[U]: TypeTag, V <: Term with Subs[V] : TypeTag](
	    variable: U)(value : V) = {
	  	      val fibre = lmbda(variable)(value.typ.asInstanceOf[Typ[V]])
	  	      DepPair(variable, value, fibre)
	  	    }

	/**
	 * Sigma type based on lambda
	 *
	 */
	 def sigma[U<: Term with Subs[U]: TypeTag, V <: Term with Subs[V]: TypeTag](
	     variable: U)(value : Typ[V]) = {
	  	      val fibre = lmbda(variable)(value)
	  	      SigmaTyp(fibre)
	  	    }


	/**
	 * type family
	 */
	type TypFamily[W<: Term, +U <: Term] = FuncObj[W, Typ[U]]

	/**
	 *  For all/Product for a type family. This is the type of dependent functions
	 *  */
	case class PiTyp[W<: Term : TypeTag, U<: Term : TypeTag](fibers: TypFamily[W, U]) extends
		Typ[FuncTerm[W, U]] with Subs[PiTyp[W, U]]{
	  type Obj = DepFuncObj[W, U]

	  lazy val typ = Universe(max(univlevel(fibers.codom), univlevel(fibers.dom.typ)))

	  override def symbObj(name: AnySym) = DepFuncSymb[ W, U](name, fibers)

	  def newobj = PiTyp(fibers.newobj)

	  def subs(x: Term, y: Term) = PiTyp[W, U](fibers.subs(x, y))

	  override def toString = Pi+"("+fibers.toString+")"
	}

	/**
	 * create type family, implicitly using a scala-universe object to build the codomain.
	 */
	def typFamily[W <: Term : TypeTag, U <: Term : TypeTag](dom: Typ[W], f: W => Typ[U])(
	    implicit su: ScalaUniv[U]) = {
	  val codom = su.univ
	  FuncDefn[W, Typ[U]](f, dom, codom)
	}

	/**
	 * Universe with objects Pi-Types
	 */
	case class PiTypUniv[W<: Term : TypeTag, U<: Term : TypeTag](
        domuniv: Typ[Typ[W]], codomuniv: Typ[Typ[U]]) extends Typ[PiTyp[W, U]]{

      lazy val typ = HigherUniv(this)

      def symbObj(name: AnySym) = {
        val dom = domuniv.symbObj(DomSym(name))
        val codom = codomuniv.symbObj(CodomSym(name))
        val typFmly = FuncTyp(dom, codomuniv).symbObj(name)
        PiTyp(typFmly)
      }

      def newobj = this

      def subs(x: Term, y: Term) = this
    }

	/**
	 * builds scala universe for pi-types given ones for domain and codomain types.
	 */
	implicit def piUniv[W<: Term : TypeTag, U<: Term : TypeTag](implicit
        domsc: ScalaUniv[W], codomsc: ScalaUniv[U]) : ScalaUniv[FuncTerm[W, U]] = {
      ScalaUniv(PiTypUniv(domsc.univ, codomsc.univ) : Typ[PiTyp[W, U]])
    }




	/**
	 *  Object in a dependent function type, i.e.,
	 *  a dependent function. Has a family of codomains
	 */
	trait DepFuncObj[W<: Term, U<: Term] extends FuncTerm[W, U]{
	  val fibers: TypFamily[W, U]

	  type D = W

	  type Cod = U


	  def act(arg: W) : U

	}


	/**
	 * Symbolic dependent function
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

	  def act(arg: W) = fibers(arg).symbObj(ApplnSym(this, arg))

	  def newobj = DepFuncSymb(name, fibers.newobj)

	  def subs(x: Term, y: Term) = (x, y, name) match {
        case (u: Typ[_], v: Typ[_], _) => DepFuncSymb(name, fibers.subs(u, v))
        case (u, v: FuncTerm[W, U], _) if (u == this) => v
        case _ => this
      }
	}


	/** A dependent function given by a scala funcion */
	case class DepFuncDefn[W<: Term : TypeTag, U<: Term with Subs[U] : TypeTag](func: W => U, dom: Typ[W], fibers: TypFamily[W, U]) extends DepFuncObj[W, U]{
	  val domobjtpe = typeOf[W]

	  val codomobjtpe = typeOf[U]

	  val depcodom : W => Typ[U] = (arg: W) => fibers(arg)

	  lazy val typ = PiTyp[W, U](fibers)

//	  def act(arg: W) = if (arg.typ == dom) Some(func(arg)) else None

	  def act(arg: W) = func(arg)

	  def newobj = typ.obj

	  def subs(x: Term, y: Term) = DepFuncDefn((w : W) => func(w).subs(x, y), dom, fibers.subs(x, y))
	}


	/**
	 * returns dependent function inferring type fiber.
	 */
	def depFunc[W<: Term : TypeTag, U<: Term with Subs[U] : TypeTag](dom: Typ[W], func: W => U)(
	    implicit su: ScalaUniv[U]): FuncTerm[W, U] = {
	  val fibers = typFamily(dom, (w: W) => func(w).typ.asInstanceOf[Typ[U]])
	  DepFuncDefn(func, dom, fibers)
	}




	/**
	 *  symbol for left component in pair from a given symbol
	 */
	case class LeftSym(name: AnySym) extends AnySym{
	  override def toString = name.toString + "_1"
	}

	/**
	 *  symbol for right component in pair from a given symbol
	 */
	case class RightSym(name: AnySym) extends AnySym{
	  override def toString = name.toString + "_2"
	}

	/**
	 *  Exists/Sum for a type family
	 *  */
	case class SigmaTyp[W<: Term with Subs[W], U<: Term with Subs[U]](
	    fibers: TypFamily[W, U]) extends Typ[DepPair[W, U]]{
	  lazy val typ = Universe(max(univlevel(fibers.codom), univlevel(fibers.dom.typ)))

	  type Obj = DepPair[W, U]

	  def symbObj(name: AnySym) = {
	    val a = fibers.dom.symbObj(LeftSym(name))
	    val b = fibers(a).symbObj(RightSym(name))
	    DepPair(a, b, fibers)
	  }

	  def newobj = SigmaTyp(fibers.newobj)

	  def subs(x: Term, y: Term) = SigmaTyp[W, U](fibers.subs(x, y))

	  override def toString = Sigma+"("+fibers.toString+")"
	}



	/**
	 * Dependent pair (a: A, b : B(a)) - element of a Sigma type.
	 *
	 */
	case class DepPair[W<: Term with Subs[W], U<: Term with Subs[U]](first : W, second: U, fibers: TypFamily[W, U]) extends Term with
		Subs[DepPair[W, U]] with AbsPair[W, U]{
	  val typ = SigmaTyp(fibers)

	  def newobj = DepPair(first.newobj, second.newobj, fibers)

	  def subs(x: Term, y: Term) = if (x == this) y.asInstanceOf[DepPair[W, U]] else DepPair(first.subs(x,y), second.subs(x,y), fibers.subs(x, y))
	}



	/** The identity type.
	 *  This is the type lhs = rhs
	 */
	case class IdentityTyp[+U <: Term with Subs[U]](dom: Typ[U], lhs: U, rhs: U) extends Typ[Term] with Subs[IdentityTyp[U]]{
	  type Obj = Term

	  lazy val typ = Universe(max(univlevel(lhs.typ.typ), univlevel(rhs.typ.typ)))

	  def newobj = IdentityTyp(dom, lhs.newobj, rhs.newobj)

	  def subs(x: Term, y: Term) = IdentityTyp(dom.subs(x, y), lhs.subs(x,y), rhs.subs(x,y))

	  def symbObj(name: AnySym)= SymbObj(name, this)
	}

	case class Refl[U <: Term with Subs[U]](dom: Typ[U], value: U) extends AtomicTerm{
	  lazy val typ = IdentityTyp(dom, value, value)
	}

	implicit class RichTerm[U <: Term with Subs[U] : TypeTag](term: U){

	  def =:=(rhs: U) =  {
	    assert(term.typ == rhs.typ, "mismatched types for equality "+ term.typ+" and "+rhs.typ)
	    IdentityTyp(term.typ.asInstanceOf[Typ[U]], term, rhs)
	  }

	  def :->[V <: Term with Subs[V] : TypeTag](that: V) = lmbda(term)(that)

	  def :~>[V <: Term with Subs[V] : TypeTag](that: V) = lambda(term)(that)
	}


//	implicit def richTerm(term: Term with Subs[Term]) = RichTerm(term)

//	implicit def richTyp(typ: Typ[Term] with Subs[Typ[Term]]) = RichTerm(typ)

	/**
	 * type A + B
	 */
	case class PlusTyp(first: Typ[Term], second: Typ[Term]) extends SmallTyp{
	  def i(value: Term) = PlusTyp.FirstIncl(this, value)

	  def j(value: Term) = PlusTyp.ScndIncl(this, value)
	}

	object PlusTyp{
	  /**
	   * A -> A + B
	   */
	  case class FirstIncl(typ: PlusTyp,  value: Term) extends Term{

	    def newobj = this

		def subs(x: Term, y: Term) = FirstIncl(typ, value.subs(x, y))
	  }

	  /**
	   * B -> A + B
	   */
	  case class ScndIncl(typ: PlusTyp,  value: Term) extends Term{
	    def newobj = this

		def subs(x: Term, y: Term) = ScndIncl(typ, value.subs(x, y))
	  }
	}



	/**
	 * convenience for Pi-type
	 */
	implicit class RichTypFamily[W<: Term : TypeTag, U<: Term with Subs[U] : TypeTag](
	    fibre: FuncObj[W, Typ[U]])(
	    implicit su: ScalaUniv[U]){
//	  val dom = func.dom

	 def pi = PiTyp(fibre)
	}

	/** Companion to dependent functions
	 *
	 *  */
	object DepFuncObj{

	  def apply[W<: Term : TypeTag,  U<: Term with Subs[U] : TypeTag](func: Term => U, dom: Typ[W])(implicit su: ScalaUniv[U]) = {
	    def section(arg: Term) = func(arg).typ.asInstanceOf[Typ[U]]
	    val fibers: TypFamily[W, U] = typFamily[W, U](dom, section)
	    DepFuncDefn(func, dom, fibers)
	  }
	}


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

	// -----------------------------------------------
	// Deprecated code - old style type families.

	object Deprec{
		/**
	 * returns type family, but needs a universe specified as the codomain.
	 */
	def typFamilyDefn[W <: Term : TypeTag, U <: Term : TypeTag](dom: Typ[W], codom: Typ[Typ[U]], f: W => Typ[U]) = {
	  FuncDefn[W, Typ[U]](f, dom, codom)
	}

	case class MiniVerse[U <: Term](sample : Typ[U]) extends Typ[Typ[U]]{
      type Obj = Typ[U]

      lazy val typ = MiniVerse[Typ[U]](this)

      def symbObj(name: AnySym)= sample

      def newobj = this

      def subs(x : Term, y : Term) = this
    }
	}

}
