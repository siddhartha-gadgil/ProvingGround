package provingGround

import provingGround.HoTT._

import scala.language.implicitConversions 
import scala.util._
import scala.language.existentials
import scala.reflect.runtime.universe.{Try => UnivTry, Function => FunctionUniv, _}
import Math._

	/*
	 * Indexed version of induction
	 */
	class IndexedInductiveTypes[I <: Typ[Term]]{
	  	trait TypPtn[U <: Term] extends TypPtnLike{
	  	 type PtnType = U
	  	 
//	  	  def induced(W : Typ[Term], X : Typ[Term])(f : Term => Term) : PtnType => PtnType
	  	  
//	  	  def inducedDep(W : Typ[Term], Xs : Term => Typ[Term])(f : Term => Term) : PtnType => PtnType
	}
	
	trait TypPtnLike{
		val univLevel : Int
	  
	  	type PtnType <:  Term
	  	 
	  	def apply(tp : I => Typ[Term]) : Typ[PtnType]
		
		def at(X: Typ[Term]) : Typ[PtnType]
	  	 
	  	def induced(W : I => Typ[Term], X : Typ[Term])(f : Term => Term) : PtnType => PtnType
	  	  
	  	def inducedDep(W : I => Typ[Term], Xs : Term => Typ[Term])(f : Term => Term) : PtnType => PtnType
	}
	
	
	
	/*
	 * A composite pattern for inductive types.
	 */
	trait PolyPtn[+U <: Term]{
	  def -->:[V <: Term : TypeTag,  UU >: U <: Term : TypeTag](that : TypPtn[V]) = FuncPtn[UU](that, this)
	  
	  def -->:[UU >: U <: Term : TypeTag](that : Typ[Term])(implicit self : Typ[Term]) : I => PolyPtn[FuncTerm[Term, UU]] = (indx) => {
	    if (that == self) FuncPtn[UU](IndxW(indx), this) else CnstFncPtn[UU](that, this) 
	  }
	  
//	  def :::[A](name : A)(implicit mytyp: Typ[Term]) : Constructor = constructor(mytyp, name)
	  
	  def apply(tp : I => Typ[Term]) : Typ[PolyPtnType]
	  
	  type PolyPtnType = U
	  
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
	

	case class IndxW(index: I) extends  TypPtn[Term] with PolyPtn[Term]{
	  def apply(W : I => Typ[Term]) = W(index)
	  
	  def at(X: Typ[Term]) = X
	  
	  val univLevel = 0
	  
//	  type PtnType = Term
	  
	  def induced(W : I => Typ[Term], X : Typ[Term])(f : Term => Term) = f
	  
	  def inducedDep(W : I => Typ[Term], Xs : Term => Typ[Term])(f : Term => Term) = f
	} 


	
	case class FuncPtn[U<:Term : TypeTag](tail: TypPtnLike, head : PolyPtn[U]) extends PolyPtn[FuncTerm[Term, U]]{
//	  type PtnType = FuncTerm[Term, head.PtnType]
	  
	  def apply(W : I=> Typ[Term]) = FuncTyp[Term, head.PolyPtnType](tail(W), head(W))
	  
	  val univLevel = max(head.univLevel, tail.univLevel)
	} 
	
	case class CnstFncPtn[U <: Term : TypeTag](tail: Typ[Term], head : PolyPtn[U]) extends PolyPtn[FuncTerm[Term, U]]{
//	  type PtnType = FuncTerm[Term, head.PtnType]
	  
	  def apply(W : I => Typ[Term]) = FuncTyp[Term, head.PolyPtnType](tail, head(W))
	  
	  val univLevel = head.univLevel
	}
	
	case class SimpleFuncPtn[V <: Term with Subs[V]: TypeTag](tail : Typ[Term], head : TypPtn[V]) extends TypPtn[FuncTerm[Term, V]]{
	  def apply(W: I => Typ[Term]) = FuncTyp[Term, head.PtnType](tail, head(W))
	  
	  def at(X: Typ[Term]) = FuncTyp[Term, head.PtnType](tail, head.at(X))
	  
	  val univLevel = max(head.univLevel, univlevel(tail.typ))
	  
	  def induced(W : I => Typ[Term], X: Typ[Term])(f : Term => Term) : PtnType => PtnType = {
	    (g : PtnType) => 
	      val func =((t : Term) => head.induced(W, X)(f) (g(t)))
	      val codomain = head.at(X)
	      FuncDefn[Term, head.PtnType](func, tail, codomain)
	  }
	  
	  def inducedDep(W : I => Typ[Term], Xs: Term => Typ[Term])(f : Term => Term) : PtnType => PtnType = {
	    (g : PtnType) => 
	      val func =((t : Term) => head.induced(W, Xs(t))(f) (g(t)))
	      val section = (t : Term) => head.at(Xs(t))
	      val fiber = typFamilyDefn[Term, head.PtnType](tail, MiniVerse(head(W)), section)
	      DepFuncDefn[Term, head.PtnType](func, tail, fiber)
	  }
	}
	
	
	case class DepFuncPtn[U <: Term : TypeTag](tail: TypPtnLike, headfibre : Term => PolyPtn[U], headlevel: Int = 0) extends PolyPtn[FuncTerm[Term, U]]{
	  def apply(W : I => Typ[Term]) : Typ[FuncTerm[Term, U]]   = {
	    val head = headfibre(__.symbObj(""))
	    val fiber = typFamilyDefn[Term, U](tail(W), MiniVerse(head(W)),  (t : Term) => headfibre(t)(W))
	    PiTyp[Term, U](fiber)
	  }
	  
	  
	  
//	  type PtnType = Term
	  
	  val univLevel = max(tail.univLevel, headlevel)
	}
	
	case class CnstDepFuncPtn[U <: Term : TypeTag](tail: Typ[Term], headfibre : Term => PolyPtn[U], headlevel: Int = 0) extends PolyPtn[FuncTerm[Term, U]]{
	  def apply(W : I => Typ[Term]) : Typ[FuncTerm[Term, U]] = {
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
	  def apply(W : I => Typ[Term]) = {
	    val fiber = typFamilyDefn[Term, head.PtnType](tail, MiniVerse(head(W)),  (t : Term) => headfibre(t)(W))
	    PiTyp[Term, head.PtnType](fiber)
	  }
	  
	  def at(X: Typ[Term]) = {
	    val fiber = typFamilyDefn[Term, head.PtnType](tail, MiniVerse(head.at(X)),  (t : Term) => headfibre(t).at(X))
	    PiTyp[Term, head.PtnType](fiber)
	  }
	  
	  val head = headfibre(tail.symbObj(""))
	  
//	  type PtnType = FuncTerm[Term, head.PtnType]
	  
	   def induced(W : I => Typ[Term], X: Typ[Term])(f : Term => Term) : PtnType => PtnType = {
	    (g : PtnType) => 
	      val func =((t : Term) => head.induced(W, X)(f) (g(t)))
	      val fiber = typFamilyDefn[Term, head.PtnType](tail, MiniVerse(head.at(X)),  (t : Term) => headfibre(t).at(X))
	      DepFuncDefn[Term, V](func, tail, fiber)
	  }
	   
	  def inducedDep(W : I => Typ[Term], Xs: Term => Typ[Term])(f : Term => Term) : PtnType => PtnType = {
	    (g : PtnType) => 
	      val func =((t : Term) => head.induced(W, Xs(t))(f) (g(t)))
	      val fiber = typFamilyDefn[Term, head.PtnType](tail, MiniVerse(head(W)),  (t : Term) => headfibre(t).at(Xs(t)))
	      DepFuncDefn[Term, V](func, tail, fiber)
	  }
	   
	  val univLevel = max(univlevel(tail.typ), headlevel)
	}
	
	type indCons = Constructor
	
	trait Constructor{
	  type PtnType <: Term
	  
	  val pattern : PolyPtn[PtnType]
	  
//	  val typ: Typ[Term]
	  
	  val cons: PtnType
	}
	
	case class ConstructorDefn[U <: Term](pattern: PolyPtn[U], cons: U) extends Constructor{
	  type PtnType = U
	}
	
		
	trait InductiveTypFmly extends (I => Typ[Term]){
	  val ptns : List[PolyPtn[Term]] = constructors map (_.pattern)
	  
	  val constructorFns : List[Term] = constructors map (_.cons)
	  
	  val constructors : List[Constructor]
	  
	  def cnstr[U <: Term](ptn: PolyPtn[U]) = ptn.newconstructor(this)
	  
//	  assert((constructorFns.map(_.typ)) == (ptns map (_(this))), "constructors do not have given patterns")
	  
	  implicit def thisAsPtnFmly(me :this.type): I=>  PolyPtn[Term] = IndxW
	  
	  
	  implicit val self: I => Typ[Term] = this
	}
	
	
	class InductiveTypDefn(symptns : List[(AnySym, PolyPtn[Term])]) extends (I => Typ[Term]) with InductiveTypFmly{
	  case class Tps(indx: I) extends SmallTyp
	  
	  def apply(indx: I) = Tps(indx)
	  
	  type Obj = Term
	  
//	  val constructorFns : List[Term] = for ((a, p) <- symptns) yield (p(this).symbObj(a))
	  
//	  val ptns = for ((a, p) <- symptns) yield p
	  
	  lazy val constructors : List[Constructor]  = for ((name, ptn) <- symptns) yield ptn.constructor(this, name) 
	 
	  

	}
}