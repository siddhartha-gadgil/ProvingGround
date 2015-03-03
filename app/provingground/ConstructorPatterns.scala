package provingground
import HoTT._
import Families._
import math._
import ScalaUniverses._

/**
 * @author gadgil
 */
object ConstructorPatterns {
    def getArg[U <: Term](func : FuncLike[Term, U], dom: Typ[U])(t: Term): Term => Option[Term] = {
      case fx: ApplnSym[u, w] =>
        if (fx.func == func && fx.arg.typ == dom) Some(fx.arg) else getArg(func, dom)(t)(fx.func)      
      case _ => None
    }
  
    /**
   * A composite pattern for inductive types.
   * Typically (A -> B -> W)-> C -> W -> (D -> W) -> W as a function of W
   */
  sealed trait ConstructorPtn[+U <: Term]{
    def -->:[V <: Term , T <: Term with Subs[T], D <: Term with Subs[D], UU >: U <: Term](
        that : FmlyPtn[V, T, D, Term]) = FuncPtn[UU](that, this)

  //  def -->:[UU >: U <: Term ](that : Typ[Term])(implicit self : Typ[Term]) : ConstructorPtn[FuncLike[Term, UU]] = {
  //    if (that == self) FuncPtn[UU](IdFmlyPtn[Term], this) else CnstFncPtn[UU](that, this)
  //  }

//    def :::[A](name : A)(implicit mytyp: Typ[Term]) : Constructor = constructor(mytyp, name)

    /**
     * returns typ corresponding to the pattern given the inductive type.
     */
    def apply(tp : Typ[Term]) : Typ[ConstructorType]

    /**
     * (upper bound for) scala type of the poly-pattern, especially functions.
     */
    type ConstructorType <: U

    /**
     * (scala) type of data for recursion
     */
    type RecDataType <: Term
    
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

  
  object ConstructorPtn{
    val W = IdW
  }
  
  


  /**
   * The constructor pattern W - the only valid head for constructor-patterns.
   */
  case object IdW extends ConstructorPtn[Term]{
    def apply(W : Typ[Term]) = W

    val univLevel = 0

    type ConstructorType = Term
    
    type RecDataType = Term
    
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

//    type ConstructorType = Term

    def apply(W : Typ[Term]) = tp

    val univLevel = univlevel(tp.typ)
  }
  */

  /**
   * Functional extension of a type pattern
   */
  sealed trait RecursiveConstructorPtn[U <: Term] extends ConstructorPtn[FuncLike[Term, U]]{
    val headfibre: Term => ConstructorPtn[U]
    
        /**
     * constructor for this pattern given inductive type and name.
     */
    override def constructor[A](tp: => Typ[Term], name: AnySym) : Constructor = {
      val cons = apply(tp).symbObj(name)
      RecConstructorDefn(this, cons)
    }

    /**
     * constructor for this pattern given inductive type, with a name symbol generated.
     */
    override def newconstructor(tp: Typ[Term]): Constructor = {
      val cons = apply(tp).obj
      RecConstructorDefn(this, cons)
    }
  }
  
  /**
   * Extending a poly-pattern by a type pattern.
   */
  case class FuncPtn[U<:Term](tail: FmlyPtnLike[Term], head : ConstructorPtn[U]) extends RecursiveConstructorPtn[U]{
    val headfibre = (t: Term) => head
    
    type ConstructorType = FuncLike[Term, U]
    
    type RecDataType = FuncLike[tail.ConstructorType, FuncLike[tail.TargetType, head.RecDataType]]
    
    def apply(W : Typ[Term]) = FuncTyp[Term, head.ConstructorType](tail(W), head(W))

    val univLevel = max(head.univLevel, tail.univLevel)
  }

  /**
   * Extending a poly-pattern by a constant type, i.e., not depending on W.
   */
  case class CnstFncPtn[U <: Term, R <: Term](
      tail: Typ[Term], 
      head : ConstructorPtn[U]
      ) extends RecursiveConstructorPtn[U]{
    val headfibre = (t: Term) => head
    
//    type RecDataType = FuncLike[Term, head.RecDataType]
    
    type ConstructorType = FuncLike[Term, U]

    def apply(W : Typ[Term]) = FuncTyp[Term, head.ConstructorType](tail, head(W))

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
      headfibre : Term => ConstructorPtn[U], headlevel: Int = 0)(implicit su: ScalaUniv[U]) extends RecursiveConstructorPtn[U]{
    type ConstructorType = FuncLike[Term, U]

//    type RecDataType = FuncLike[tail.ConstructorType, FuncLike[tail.TargetType, head.RecDataType]]
   
    
    def apply(W : Typ[Term]) : Typ[FuncLike[Term, U]]   = {
      val head = headfibre(W.symbObj(""))
      val fiber = typFamily[Term, U](tail(W),  (t : Term) => headfibre(t)(W))
      PiTyp[Term, U](fiber)
    }



//    type ConstructorType = Term

    val univLevel = max(tail.univLevel, headlevel)
  }

  /**
   * Dependent extension by a constant type  of a poly-pattern depending on elements of that type.
   */
  case class CnstDepFuncPtn[U <: Term ](tail: Typ[Term], headfibre : Term => ConstructorPtn[U], headlevel: Int = 0)(
      implicit su: ScalaUniv[U]) extends RecursiveConstructorPtn[U]{

    type ConstructorType = FuncLike[Term, U]
    def apply(W : Typ[Term]) : Typ[FuncLike[Term, U]] = {
      val fiber = typFamily[Term, U](tail,  (t : Term) => headfibre(t)(W))
      PiTyp[Term, U](fiber)
    }

//    type ConstructorType = Term

    val univLevel = headlevel
  }
  
    /**
   * Constructor for an inductive type, with given scala type and poly-pattern of this type.
   *
   * abstraction of ConstructorDefn mainly to allow different type parameters.
   */
  trait Constructor{
    /**
     * scala type, especially (nested) functions
     */
    type ConstructorType <: Term

    /**
     * poly-pattern for the constructor
     */
    val pattern : ConstructorPtn[ConstructorType]

//    val typ: Typ[Term]

    /**
     * the constructor function itself.
     */
    val cons: ConstructorType
  }
  
  trait TypedConstructor[U <: Term] extends Constructor{
    type ConstructorType = U
  }

  trait RecursiveConstructor extends Constructor{
    type BaseType <: Term
    
    type ConstructorType = FuncLike[Term, BaseType]
    
    val pattern: RecursiveConstructorPtn[BaseType]
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
  case class ConstructorDefn[U <: Term](pattern: ConstructorPtn[U], cons: U) extends TypedConstructor[U]{
//    type ConstructorType = U
  }

  case class RecConstructorDefn[U <: Term](
      pattern: RecursiveConstructorPtn[U], cons: FuncLike[Term, U]) extends RecursiveConstructor{
    type BaseType = U
  }

  
  /*
  object Constructor{
    def apply(pattern : ConstructorPtn, typ: Typ[Term])(f : pattern.ConstructorType) = new Constructor(pattern, typ) {

      val cons = f.asInstanceOf[pattern.ConstructorType]
    }
  }
  *
  */
}