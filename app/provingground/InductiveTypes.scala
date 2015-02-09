package provingground

import HoTT._
import scala.language.implicitConversions
import scala.util._
import scala.language.existentials
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}
import Math._

/**
 * Inductively defined types in homotopy type theory
 */
object InductiveTypes{
  	/**
	 * A simple pattern, for inductive type constructors as well as type families.
	 * for instance A -> B -> W, where W is the type to be defined;
	 * ends with the type being defined.
	 * the pattern is a function of the inductive type.
	 *
	 * @typparam U (upper bound on) scala type of an object with the pattern - especially functions.
	 * this is needed to ensure that families have a common scala type that can be used inductively.
	 */
	sealed trait TypPtn[U <: Term] extends TypPtnLike with TypSeq[U, Term]{
		/**
		 * scala type (upper bound)
		 */
	  	 type PtnType = U

	  	 /**
	  	  * function induced by f: W -> X of type (A -> W) -> (A -> X) etc
	  	  */
	  	  def induced(W : Typ[Term], X : Typ[Term])(f : Term => Term) : PtnType => PtnType

	  	  /**
	  	  * dependent function induced by dependent f: W -> X(s) of type (A -> W) -> (A ~> X(s)) etc
	  	  */
	  	  def inducedDep(W : Typ[Term], Xs : Term => Typ[Term])(f : Term => Term) : PtnType => PtnType
	}

	/**
	 * a single trait to hold all type patterns, independent of U.
	 */
	trait TypPtnLike{
	  /**
	   * the universe containing the type
	   */
	  val univLevel : Int

	  /**
	   * scala type (upper bound)
	   */
	  type PtnType <:  Term

	  /**
	   * returns the type corresponding to the pattern, such as A -> W, given the (inductive) type W
	   */
	  def apply(tp : Typ[Term]) : Typ[PtnType]

	  /**
	  * function induced by f: W -> X of type (A -> W) -> (A -> X) etc
	  *
	  * @param f function from which to induce
	  *
	  * @param W the inductive type
	  *
	  * @param X codomain of the given function
	  */
	  def induced(W : Typ[Term], X : Typ[Term])(f : Term => Term) : PtnType => PtnType

	  /**
	  * dependent function induced by dependent f: W -> X(s) of type (A -> W) -> ((a : A) ~> Xs(a)) etc
	  *
	  * @param f dependent function from which to induce
	  *
	  * @param W the inductive type
	  *
	  * @param Xs family of codomains of the given dependent function
	  */
	  def inducedDep(W : Typ[Term], Xs : Term => Typ[Term])(f : Term => Term) : PtnType => PtnType
	}



	/**
	 * A composite pattern for inductive types.
	 * Typically (A -> B -> W)-> C -> W -> (D -> W) -> W as a function of W
	 */
	sealed trait PolyPtn[+U <: Term]{
	  def -->:[V <: Term ,  UU >: U <: Term ](that : TypPtn[V]) = FuncPtn[UU](that, this)

	  def -->:[UU >: U <: Term ](that : Typ[Term])(implicit self : Typ[Term]) : PolyPtn[FuncTerm[Term, UU]] = {
	    if (that == self) FuncPtn[UU](IdW, this) else CnstFncPtn[UU](that, this)
	  }

//	  def :::[A](name : A)(implicit mytyp: Typ[Term]) : Constructor = constructor(mytyp, name)

	  /**
	   * returns typ corresponding to the pattern given the inductive type.
	   */
	  def apply(tp : Typ[Term]) : Typ[PolyPtnType]

	  /**
	   * (upper bound for) scala type of the poly-pattern, especially functions.
	   */
	  type PolyPtnType <: U

	  /**
	   * constructor for this pattern given inductive type and name.
	   */
	  def constructor[A](tp: => Typ[Term], name: AnySym) : Constructor = {
	    val cons = apply(tp).symbObj(name)
	    ConstructorDefn(this, cons)
	  }

	  /**
	   * constructor for this pattern given inductive type, with a name symbol generated.
	   */
	  def newconstructor(tp: Typ[Term]): Constructor = {
	    val cons = apply(tp).obj
	    ConstructorDefn(this, cons)
	  }

	  val univLevel : Int

	}

	object PolyPtn{
	  val W = IdW


	}

	/**
	 * The type pattern W - the only valid head for both type patterns and poly-patterns.
	 */
	case object IdW extends  TypPtn[Term] with PolyPtn[Term]{
	  def apply(W : Typ[Term]) = W

	  val univLevel = 0

	  type PolyPtnType = Term
	  /**
	   * induced function is the given one.
	   */
	  def induced(W : Typ[Term], X : Typ[Term])(f : Term => Term) = f

	  /**
	   * induced function is the given one.
	   */
	  def inducedDep(W : Typ[Term], Xs : Term => Typ[Term])(f : Term => Term) = f
	}

	/* removed since other patterns are directly incorporated in poly-patterns and are not type patterns.
	case class OtherPtn(tp : Typ[Term]) extends TypPtn[Term]{

//	  type PtnType = Term

	  def apply(W : Typ[Term]) = tp

	  val univLevel = univlevel(tp.typ)
	}
	*/

	/**
	 * Extending a poly-pattern by a type pattern.
	 */
	case class FuncPtn[U<:Term ](tail: TypPtnLike, head : PolyPtn[U]) extends PolyPtn[FuncTerm[Term, U]]{
	  type PolyPtnType = FuncTerm[Term, U]

	  def apply(W : Typ[Term]) = FuncTyp[Term, head.PolyPtnType](tail(W), head(W))

	  val univLevel = max(head.univLevel, tail.univLevel)
	}

	/**
	 * Extending a poly-pattern by a constant type, i.e., not depending on W.
	 */
	case class CnstFncPtn[U <: Term ](tail: Typ[Term], head : PolyPtn[U]) extends PolyPtn[FuncTerm[Term, U]]{
	  type PolyPtnType = FuncTerm[Term, U]

	  def apply(W : Typ[Term]) = FuncTyp[Term, head.PolyPtnType](tail, head(W))

	  val univLevel = head.univLevel
	}

	/**
	 * Extending a type pattern by a constant type to get (tail --> head).
	 */
	case class SimpleFuncPtn[V <: Term with Subs[V]](tail : Typ[Term], head : TypPtn[V])(
	      implicit su: ScalaUniv[V]) extends TypPtn[FuncTerm[Term, V]]{
	  def apply(W: Typ[Term]) = FuncTyp[Term, head.PtnType](tail, head(W))

	  val univLevel = max(head.univLevel, univlevel(tail.typ))

	  /**
	   * inductively defining the induced function.
	   * maps (g : tail --> head(W)) to func : tail --> head(X) given (head(W) --> head(X))
	   *
	   */
	  def induced(W : Typ[Term], X: Typ[Term])(f : Term => Term) : PtnType => PtnType = {
	    (g : PtnType) =>
	      val func =((t : Term) => head.induced(W, X)(f) (g(t)))
	      val codomain = head(X)
	      FuncDefn[Term, head.PtnType](func, tail, codomain)
	  }

	  /**
	   * inductively defining the induced function.
	   * maps (g : tail --> head(W)) to func : (t : tail) ~> head(Xs(t)) given (head(W) --> (t: tail) ~> head(Xs(t)))
	   *
	   */
	  def inducedDep(W : Typ[Term], Xs: Term => Typ[Term])(f : Term => Term) : PtnType => PtnType = {
	    (g : PtnType) =>
	      val func =((t : Term) => head.inducedDep(W, Xs)(f) (g(t)))
	      val section = (t : Term) => head(Xs(t))
	      val fiber = typFamily[Term, head.PtnType](tail, section)
	      DepFuncDefn[Term, head.PtnType](func, tail, fiber)
	  }
	}

	/**
	 * Dependent extension of a poly-pattern by a type pattern.
	 * XXX this may never be applicable
	 */
	case class DepFuncPtn[U <: Term ](tail: TypPtnLike,
	    headfibre : Term => PolyPtn[U], headlevel: Int = 0)(implicit su: ScalaUniv[U]) extends PolyPtn[FuncTerm[Term, U]]{
		type PolyPtnType = FuncTerm[Term, U]

	  def apply(W : Typ[Term]) : Typ[FuncTerm[Term, U]]   = {
	    val head = headfibre(W.symbObj(""))
	    val fiber = typFamily[Term, U](tail(W),  (t : Term) => headfibre(t)(W))
	    PiTyp[Term, U](fiber)
	  }



//	  type PtnType = Term

	  val univLevel = max(tail.univLevel, headlevel)
	}

	/**
	 * Dependent extension by a constant type  of a poly-pattern depending on elements of that type.
	 */
	case class CnstDepFuncPtn[U <: Term ](tail: Typ[Term], headfibre : Term => PolyPtn[U], headlevel: Int = 0)(
	    implicit su: ScalaUniv[U]) extends PolyPtn[FuncTerm[Term, U]]{

		type PolyPtnType = FuncTerm[Term, U]
	  def apply(W : Typ[Term]) : Typ[FuncTerm[Term, U]] = {
	    val fiber = typFamily[Term, U](tail,  (t : Term) => headfibre(t)(W))
	    PiTyp[Term, U](fiber)
	  }

//	  type PtnType = Term

	  val univLevel = headlevel
	}

	/**
	 * Extending by a constant type A a family of type patterns depending on (a : A).
	 *
	 */
	case class SimpleDepFuncPtn[V <: Term with Subs[V] ](tail: Typ[Term],
	    headfibre : Term => TypPtn[V], headlevel: Int = 0)(implicit su: ScalaUniv[V]) extends TypPtn[FuncTerm[Term,V]]{
	  def apply(W : Typ[Term]) = {
	    val fiber = typFamily(tail,  (t : Term) => headfibre(t)(W))
	    PiTyp[Term, head.PtnType](fiber)
	  }

	  val head = headfibre(tail.symbObj(""))

//	  type PtnType = FuncTerm[Term, head.PtnType]

	   def induced(W : Typ[Term], X: Typ[Term])(f : Term => Term) : PtnType => PtnType = {
	    (g : PtnType) =>
	      val func =((t : Term) => headfibre(t).induced(W, X)(f) (g(t)))
	      val fiber = typFamily[Term, V](tail,  (t : Term) => headfibre(t)(X))
	      DepFuncDefn[Term, V](func, tail, fiber)
	  }

	  def inducedDep(W : Typ[Term], Xs: Term => Typ[Term])(f : Term => Term) : PtnType => PtnType = {
	    (g : PtnType) =>
	      val func =((t : Term) => headfibre(t).induced(W, Xs(t))(f) (g(t)))
	      val fiber = typFamily[Term, V](tail, (t : Term) => headfibre(t)(Xs(t)))
	      DepFuncDefn[Term, V](func, tail, fiber)
	  }

	  val univLevel = max(univlevel(tail.typ), headlevel)
	}

//	case class Constructor(cons: Term, pattern : PolyPtn, typ : Typ[Term]){
//	  require(cons.typ == pattern(typ))
//	}

	/**
	 * Constructor for an inductive type, with given scala type and poly-pattern of this type.
	 *
	 * abstraction of ConstructorDefn mainly to allow different type parameters.
	 */
	trait Constructor{
	  /**
	   * scala type, especially (nested) functions
	   */
	  type PtnType <: Term

	  /**
	   * poly-pattern for the constructor
	   */
	  val pattern : PolyPtn[PtnType]

//	  val typ: Typ[Term]

	  /**
	   * the constructor function itself.
	   */
	  val cons: PtnType
	}

	/**
	 * a constructor given by its parameters.
	 *
	 * @param pattern poly-pattern for the constructor.
	 *
	 * @param cons constructor function.
	 *
	 * @typparam U scala type of polypattern.
	 */
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

	/**
	 * inductive type, specified by constructors.
	 */
	trait InductiveTyp extends Typ[Term]{
	  /**
	   * just the constructor patterns.
	   */
	  val ptns : List[PolyPtn[Term]] = constructors map (_.pattern)

	  /**
	   * just the constructor functions
	   */
	  val constructorFns : List[Term] = constructors map (_.cons)

	  /**
	   * the constructors, including functions and patterns
	   */
	  val constructors : List[Constructor]

//	  def cnstr[U <: Term](ptn: PolyPtn[U]) = ptn.newconstructor(this)

//	  assert((constructorFns.map(_.typ)) == (ptns map (_(this))), "constructors do not have given patterns")

	  implicit def thisAsPtn(me :this.type): PolyPtn[Term] = IdW


	  implicit val self: Typ[Term] = this
	}

	/**
	 * inductive type constructed from given patterns and names of corresponding functions.
	 */
	class InductiveTypDefn(symptns : List[(AnySym, PolyPtn[Term])]) extends SmallTyp with InductiveTyp{
//	  type Obj = Term

//	  val constructorFns : List[Term] = for ((a, p) <- symptns) yield (p(this).symbObj(a))

//	  val ptns = for ((a, p) <- symptns) yield p

	  lazy val constructors = for ((name, ptn) <- symptns) yield ptn.constructor(this, name)
/*
	  val univLevel = (ptns map (_.univLevel)).max

	  val typ = Universe(univLevel)

	  def subs(x : Term, y: Term) = this

	  def symbObj(name: AnySym): Term = SymbObj(name, this)*/
	}


}
