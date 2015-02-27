package provingground

import HoTT._
import Families._
import scala.language.implicitConversions
import scala.util._
import scala.language.existentials
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}
import Math._
import provingground.ScalaUniverses._

/**
 * Inductively defined types in homotopy type theory
 */
object InductiveTypes{


	/**
	 * A composite pattern for inductive types.
	 * Typically (A -> B -> W)-> C -> W -> (D -> W) -> W as a function of W
	 */
	sealed trait PolyPtn[+U <: Term]{
	  def -->:[V <: Term , T <: Term with Subs[T], D <: Term with Subs[D], UU >: U <: Term](
        that : FmlyPtn[V, T, D, Term]) = FuncPtn[UU](that, this)

	  def -->:[UU >: U <: Term ](that : Typ[Term])(implicit self : Typ[Term]) : PolyPtn[FuncLike[Term, UU]] = {
	    if (that == self) FuncPtn[UU](IdFmlyPtn[Term], this) else CnstFncPtn[UU](that, this)
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
	case object IdW extends PolyPtn[Term]{
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
	case class OtherPtn(tp : Typ[Term]) extends FmlyPtn[Term]{

//	  type PtnType = Term

	  def apply(W : Typ[Term]) = tp

	  val univLevel = univlevel(tp.typ)
	}
	*/

	/**
	 * Extending a poly-pattern by a type pattern.
	 */
	case class FuncPtn[U<:Term ](tail: FmlyPtnLike[Term], head : PolyPtn[U]) extends PolyPtn[FuncLike[Term, U]]{
	  type PolyPtnType = FuncLike[Term, U]

	  def apply(W : Typ[Term]) = FuncTyp[Term, head.PolyPtnType](tail(W), head(W))

	  val univLevel = max(head.univLevel, tail.univLevel)
	}

	/**
	 * Extending a poly-pattern by a constant type, i.e., not depending on W.
	 */
	case class CnstFncPtn[U <: Term ](tail: Typ[Term], head : PolyPtn[U]) extends PolyPtn[FuncLike[Term, U]]{
	  type PolyPtnType = FuncLike[Term, U]

	  def apply(W : Typ[Term]) = FuncTyp[Term, head.PolyPtnType](tail, head(W))

	  val univLevel = head.univLevel
	}

	/**
	 * Extending a type pattern by a constant type to get (tail --> head).
	 */
/**
   * Dependent extension of a poly-pattern by a type pattern.
   * XXX this may never be applicable
   */
  case class DepFuncPtn[U <: Term ](tail: FmlyPtnLike[Term],
      headfibre : Term => PolyPtn[U], headlevel: Int = 0)(implicit su: ScalaUniv[U]) extends PolyPtn[FuncLike[Term, U]]{
    type PolyPtnType = FuncLike[Term, U]

    def apply(W : Typ[Term]) : Typ[FuncLike[Term, U]]   = {
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
	    implicit su: ScalaUniv[U]) extends PolyPtn[FuncLike[Term, U]]{

		type PolyPtnType = FuncLike[Term, U]
	  def apply(W : Typ[Term]) : Typ[FuncLike[Term, U]] = {
	    val fiber = typFamily[Term, U](tail,  (t : Term) => headfibre(t)(W))
	    PiTyp[Term, U](fiber)
	  }

//	  type PtnType = Term

	  val univLevel = headlevel
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
