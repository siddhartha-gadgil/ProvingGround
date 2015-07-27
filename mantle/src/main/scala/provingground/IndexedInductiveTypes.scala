package provingground

import provingground.HoTT._

import scala.language.implicitConversions
import scala.util._
import scala.language.existentials
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}
import Math._

	/*
	 * Indexed version of induction
	 */
	class IndexedInductiveTypes[I <: Term]{/*
	  	trait FmlyPtn[U <: Term] extends FmlyPtn{
	  	 type ConstructorType = U

//	  	  def induced(W : Typ[Term], X : Typ[Term])(f : Term => Term) : ConstructorType => ConstructorType

//	  	  def inducedDep(W : Typ[Term], Xs : Term => Typ[Term])(f : Term => Term) : ConstructorType => ConstructorType
	}

	/**
	 * a single trait to hold all type patterns, independent of U.
	 */
	trait FmlyPtn{
		val univLevel : Int

	  	type ConstructorType <:  Term

	  	def apply(tp : I => Typ[Term]) : Typ[ConstructorType]

		def at(X: Typ[Term]) : Typ[ConstructorType]

	  	def induced(W : I => Typ[Term], X : Typ[Term])(f : Term => Term) : ConstructorType => ConstructorType

	  	def inducedDep(W : I => Typ[Term], Xs : Term => Term => Typ[Term])(f : Term => Term) : ConstructorType => ConstructorType
	}



	/*
	 * A composite pattern for inductive types.
	 */
	trait ConstructorPattern[+U <: Term]{
	  def -->:[V <: Term ,  UU >: U <: Term ](that : FmlyPtn[V]) = FuncPtn[UU](that, this)

	  def -->:[UU >: U <: Term ](that : Typ[Term])(implicit self : Typ[Term]) : I => ConstructorPattern[FuncLike[Term, UU]] = (indx) => {
	    if (that == self) FuncPtn[UU](IndxW(indx), this) else CnstFncPtn[UU](that, this)
	  }

//	  def :::[A](name : A)(implicit mytyp: Typ[Term]) : Constructor = constructor(mytyp, name)

	  def apply(tp : I => Typ[Term]) : Typ[ConstructorType]

	  type ConstructorType = U

	  def constructor[A](tp: => (I =>Typ[Term]), name: AnySym) : Constructor = {
	    val cons = apply(tp).symbObj(name)
	    ConstructorDefn(this, cons)
	  }

	  def newconstructor(tp: I => Typ[Term]): Constructor = {
	    val cons = apply(tp).obj
	    ConstructorDefn(this, cons)
	  }

	  val univLevel : Int

	}


	case class IndxW(index: I) extends  FmlyPtn[Term] with ConstructorPattern[Term]{
	  def apply(W : I => Typ[Term]) = W(index)

	  def at(X: Typ[Term]) = X

	  val univLevel = 0

//	  type ConstructorType = Term

	  def induced(W : I => Typ[Term], X : Typ[Term])(f : Term => Term) = f

	  def inducedDep(W : I => Typ[Term], Xs : Term => Term => Typ[Term])(f : Term => Term) = f
	}



	/**
	 * Extending a poly-pattern by a type pattern.
	 */
	case class FuncPtn[U<:Term ](tail: FmlyPtn, head : ConstructorPattern[U]) extends ConstructorPattern[FuncLike[Term, U]]{
//	  type ConstructorType = FuncLike[Term, head.ConstructorType]

	  def apply(W : I=> Typ[Term]) = FuncTyp[Term, head.ConstructorType](tail(W), head(W))

	  val univLevel = max(head.univLevel, tail.univLevel)
	}

	/**
	 * Extending a poly-pattern by a constant type, i.e., not depending on W.
	 */
	case class CnstFncPtn[U <: Term ](tail: Typ[Term], head : ConstructorPattern[U]) extends ConstructorPattern[FuncLike[Term, U]]{
//	  type ConstructorType = FuncLike[Term, head.ConstructorType]

	  def apply(W : I => Typ[Term]) = FuncTyp[Term, head.ConstructorType](tail, head(W))

	  val univLevel = head.univLevel
	}

	case class SimpleFuncPtn[V <: Term with Subs[V]](tail : Typ[Term],
	    head : FmlyPtn[V])(implicit su: ScalaUniv[V]) extends FmlyPtn[FuncLike[Term, V]]{
	  def apply(W: I => Typ[Term]) = FuncTyp[Term, head.ConstructorType](tail, head(W))

	  def at(X: Typ[Term]) = FuncTyp[Term, head.ConstructorType](tail, head.at(X))

	  val univLevel = max(head.univLevel, univlevel(tail.typ))

	  def induced(W : I => Typ[Term], X: Typ[Term])(f : Term => Term) : ConstructorType => ConstructorType = {
	    (g : ConstructorType) =>
	      val func =((t : Term) => head.induced(W, X)(f) (g(t)))
	      val codomain = head.at(X)
	      FuncDefn[Term, head.ConstructorType](func, tail, codomain)
	  }

	  def inducedDep(W : I => Typ[Term], Xs: Term => Term => Typ[Term])(f : Term => Term) : ConstructorType => ConstructorType = {
	    (g : ConstructorType) =>
	      val func =((t : Term) => head.inducedDep(W, Xs)(f) (g(t)))
	      val section = (t : Term) => head(Xs(_)(t))
	      val fiber = typFamily[Term, head.ConstructorType](tail, section)
	      DepFuncDefn[Term, head.ConstructorType](func, tail, fiber)
	  }
	}


	case class DepFuncPtn[U <: Term ](tail: FmlyPtn,
	    headfibre : Term => ConstructorPattern[U], headlevel: Int = 0)(implicit su: ScalaUniv[U]) extends ConstructorPattern[FuncLike[Term, U]]{
	  def apply(W : I => Typ[Term]) : Typ[FuncLike[Term, U]]   = {
	    val head = headfibre(__.symbObj(Star))
	    val fiber = typFamily[Term, U](tail(W), (t : Term) => headfibre(t)(W))
	    PiTyp[Term, U](fiber)
	  }



//	  type ConstructorType = Term

	  val univLevel = max(tail.univLevel, headlevel)
	}

	case class CnstDepFuncPtn[U <: Term ](tail: Typ[Term],
	    headfibre : Term => ConstructorPattern[U], headlevel: Int = 0)(implicit su: ScalaUniv[U]) extends ConstructorPattern[FuncLike[Term, U]]{
	  def apply(W : I => Typ[Term]) : Typ[FuncLike[Term, U]] = {
	    val head = headfibre(tail.symbObj(Star))
	    val fiber = typFamily[Term, U](tail, (t : Term) => headfibre(t)(W))
	    PiTyp[Term, U](fiber)
	  }

//	  type ConstructorType = Term

	  val univLevel = headlevel
	}

	/*
	 * Issues: Replace codomain Universe(0) by something reasonable - done.
	 * Correct the induced function
	 */
	case class SimpleDepFuncPtn[V <: Term with Subs[V] ](tail: Typ[Term],
	    headfibre : Term => FmlyPtn[V] with FmlyPtn[V], headlevel: Int = 0)(implicit su: ScalaUniv[V]) extends FmlyPtn[FuncLike[Term,V]]{
	  def apply(W : I => Typ[Term]) = {
	    val fiber = typFamily[Term, head.ConstructorType](tail, (t : Term) => headfibre(t)(W))
	    PiTyp[Term, head.ConstructorType](fiber)
	  }

	  def at(X: Typ[Term]) = {
	    val fiber = typFamily[Term, head.ConstructorType](tail, (t : Term) => headfibre(t).at(X))
	    PiTyp[Term, head.ConstructorType](fiber)
	  }

	  val head = headfibre(tail.symbObj(Star))

//	  type ConstructorType = FuncLike[Term, head.ConstructorType]

	   def induced(W : I => Typ[Term], X: Typ[Term])(f : Term => Term) : ConstructorType => ConstructorType = {
	    (g : ConstructorType) =>
	      val func =((t : Term) => headfibre(t).induced(W, X)(f) (g(t)))
	      val fiber = typFamily[Term, V](tail,  (t : Term) => headfibre(t).at(X))
	      DepFuncDefn[Term, V](func, tail, fiber)
	  }

	  def inducedDep(W : I => Typ[Term], Xs: Term => Term => Typ[Term])(f : Term => Term) : ConstructorType => ConstructorType = {
	    (g : ConstructorType) =>
	      val func =((t : Term) => headfibre(t).inducedDep(W, Xs)(f) (g(t)))
	      val fiber = typFamily[Term, V](tail, (t : Term) => headfibre(t)(Xs(_)(t)))
	      DepFuncDefn[Term, V](func, tail, fiber)
	  }

	  val univLevel = max(univlevel(tail.typ), headlevel)
	}

	type indCons = Constructor

	trait Constructor{
	  type ConstructorType <: Term

	  val pattern : ConstructorPattern[ConstructorType]

//	  val typ: Typ[Term]

	  val cons: ConstructorType
	}

	case class ConstructorDefn[U <: Term](pattern: ConstructorPattern[U], cons: U) extends Constructor{
	  type ConstructorType = U
	}


	trait InductiveTypFmly extends (I => Typ[Term]){
	  val ptns : List[ConstructorPattern[Term]] = constructors map (_.pattern)

	  val constructorFns : List[Term] = constructors map (_.cons)

	  val constructors : List[Constructor]

	  def cnstr[U <: Term](ptn: ConstructorPattern[U]) = ptn.newconstructor(this)

//	  assert((constructorFns.map(_.typ)) == (ptns map (_(this))), "constructors do not have given patterns")

	  implicit def thisAsPtnFmly(me :this.type): I=>  ConstructorPattern[Term] = IndxW


	  implicit val self: I => Typ[Term] = this
	}


	class InductiveTypDefn(symptns : List[(AnySym, ConstructorPattern[Term])]) extends (I => Typ[Term]) with InductiveTypFmly{
	  case class Tps(indx: I) extends SmallTyp

	  def apply(indx: I) = Tps(indx)

	  type Obj = Term

//	  val constructorFns : List[Term] = for ((a, p) <- symptns) yield (p(this).symbObj(a))

//	  val ptns = for ((a, p) <- symptns) yield p

	  lazy val constructors : List[Constructor]  = for ((name, ptn) <- symptns) yield ptn.constructor(this, name)



	}*/
}
