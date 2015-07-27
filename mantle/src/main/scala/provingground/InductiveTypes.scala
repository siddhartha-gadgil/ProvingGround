package provingground

import HoTT._
import Families._
import ConstructorPattern._
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






//	case class Constructor(cons: Term, pattern : ConstructorPattern, typ : Typ[Term]){
//	  require(cons.typ == pattern(typ))
//	}



	/**
	 * inductive type, specified by constructors.
	 */
	trait InductiveTyp extends Typ[Term]{
	  /**
	   * just the constructor patterns.
	   */
	  val ptns : List[ConstructorPattern[Term, Term]] = constructors map (_.pattern)

	  /**
	   * just the constructor functions
	   */
	  val constructorFns : List[Term] = constructors map (_.cons)

	  /**
	   * the constructors, including functions and patterns
	   */
	  val constructors : List[Constructor[Term, Term]]

//	  def cnstr[U <: Term](ptn: ConstructorPattern[U]) = ptn.newconstructor(this)

//	  assert((constructorFns.map(_.typ)) == (ptns map (_(this))), "constructors do not have given patterns")

	  implicit def thisAsPtn(me :this.type): ConstructorPattern[Term, Term] = IdW


	  implicit val self: Typ[Term] = this
	}

	/**
	 * inductive type constructed from given patterns and names of corresponding functions.
	 */
	class InductiveTypDefn(symptns : List[(AnySym, ConstructorPattern[Term, Term])]) extends SmallTyp with InductiveTyp{
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
