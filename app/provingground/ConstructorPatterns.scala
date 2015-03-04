package provingground
import HoTT._
import Families._
import math._
import ScalaUniverses._
import scala.util._

/**
 * @author gadgil
 */
object ConstructorPatterns {
    def getArg[D <: Term, U <: Term](func : FuncLike[D, U]): Term => Option[D] = {
      case fx: ApplnSym[u, w] =>
        if (fx.func == func && fx.arg.typ == func.dom) Try(Some(fx.arg.asInstanceOf[D])).getOrElse(None) 
            else getArg(func)(fx.func)      
      case _ => None
    }
  
    /**
   * A composite pattern for inductive types.
   * Typically (A -> B -> W)-> C -> W -> (D -> W) -> W as a function of W
   */
  sealed trait ConstructorPtn{
    def -->:[V <: Term , T <: Term with Subs[T], D <: Term with Subs[D]](
        that : FmlyPtn[V, T, D, Term]) = FuncPtn(that, this)

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
    type ConstructorType <: Term

    /**
     * (scala) type of data for recursion
     */
    type RecDataType <: Term
    
    def recDef(cons: ConstructorType, data: RecDataType, f :  => Func[Term, Term]): Term => Option[Term]
    
    /**
     * constructor for this pattern given inductive type and name.
     */
    def constructor(tp: => Typ[Term], name: AnySym) : Constructor = {
      val cons = apply(tp).symbObj(name)
      ConstructorDefn[ConstructorType](this, cons)
    }
    
    
    
    /**
     * constructor for this pattern given inductive type, with a name symbol generated.
     */
    def newconstructor(tp: Typ[Term]): Constructor = {
      val cons = apply(tp).obj
      ConstructorDefn[ConstructorType](this, cons)
    }

    val univLevel : Int

  }

  
  object ConstructorPtn{
    val W = IdW
  }
  
  


  /**
   * The constructor pattern W - the only valid head for constructor-patterns.
   */
  case object IdW extends ConstructorPtn{
    def apply(W : Typ[Term]) = W

    val univLevel = 0

    type ConstructorType = Term
    
    type RecDataType = Term
    
    def recDef(cons: ConstructorType, data: RecDataType, f :  => Func[Term, Term]): Term => Option[Term] = {
      case (t: Term) if t == cons => Some(data)
      case _ => None
    }
    
    /**
     * induced function is the given one.
     */
    def induced(W : Typ[Term], X : Typ[Term])(f : Term => Term) = f

    /**
     * induced function is the given one.
     */
    def inducedDep(W : Typ[Term], Xs : Term => Typ[Term])(f : Term => Term) = f
  }



  /**
   * Functional extension of a type pattern
   */
  sealed trait RecursiveConstructorPtn extends ConstructorPtn{
    type ArgType <: Term
    
    type HeadType <: Term
    
    type ConstructorType <: FuncLike[ArgType, HeadType]
    
    type RecHeadType <: Term
    
    val headfibre: ArgType => ConstructorPtn{type ConstructorType = HeadType; type RecDataType = RecHeadType}
    
    def recData(data: RecDataType, arg: ArgType, f : => Func[Term, Term]): RecHeadType
    
    def recDef(cons: ConstructorType, data: RecDataType, f :  => Func[Term, Term]): Term => Option[Term] = {
      t => 
        for (arg <- getArg(cons)(t); term <-headfibre(arg).recDef(cons(arg), recData(data, arg, f), f)(t)) yield term
    }
    
        /**
     * constructor for this pattern given inductive type and name.
     */
    /*
    override def constructor(tp: => Typ[Term], name: AnySym) : Constructor = {
      val cons = apply(tp).symbObj(name)
      RecConstructorDefn(this, cons)
    }

    /**
     * constructor for this pattern given inductive type, with a name symbol generated.
     */
    override def newconstructor(tp: Typ[Term]): Constructor = {
      val cons = apply(tp).obj
      RecConstructorDefn[ConstructorType](this, cons)
    }*/
  }
  
  /**
   * Extending a poly-pattern by a type pattern.
   */
  case class FuncPtn(tail: FmlyPtnLike[Term], head : ConstructorPtn) extends RecursiveConstructorPtn{
    type ArgType = tail.FamilyType
    
    type HeadType = head.ConstructorType
    
    val _head : ConstructorPtn{type ConstructorType = HeadType; type RecDataType = RecHeadType} = head
    
    val headfibre = (t: ArgType) => _head
    
    type ConstructorType = FuncLike[Term, head.ConstructorType]
    
    type RecHeadType = head.RecDataType
    
    type RecDataType = FuncLike[tail.FamilyType, FuncLike[tail.TargetType, head.RecDataType]]

    def recData(data: RecDataType, arg: ArgType, f :  => Func[Term, Term]): RecHeadType = {
     val W = f.dom
     val X = f.codom
     val d = tail.induced(W, X)(f)(arg)
    data(arg)(d)
    }
    
    def apply(W : Typ[Term]) = FuncTyp[Term, head.ConstructorType](tail(W), head(W))

    val univLevel = max(head.univLevel, tail.univLevel)
  }

  /**
   * Extending a poly-pattern by a constant type, i.e., not depending on W.
   */
  case class CnstFncPtn(
      tail: Typ[Term], 
      head : ConstructorPtn
      ) extends RecursiveConstructorPtn{
    type ArgType = Term
    
    type HeadType = head.ConstructorType
    
    val _head : ConstructorPtn{type ConstructorType = HeadType; type RecDataType = RecHeadType} = head
    
    val headfibre = (t: ArgType) => _head
    
       
    type RecDataType = FuncLike[Term, head.RecDataType]
    
    type RecHeadType = head.RecDataType
    
    type ConstructorType = FuncLike[Term, head.ConstructorType]

    def recData(data: RecDataType, arg: ArgType, f :  => Func[Term, Term]): RecHeadType = data(arg)
    
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
  case class DepFuncPtn[U <: Term, V <: Term, W <: Term](tail: FmlyPtnLike[Term],
      headfibre : Term => (ConstructorPtn{type ConstructorType = U; type RecDataType = V}),
      headlevel: Int = 0)(implicit su: ScalaUniv[U]) extends RecursiveConstructorPtn{
    type ArgType = tail.FamilyType
    
    type HeadType = U
    
    type ConstructorType = FuncLike[Term, U]

    type RecDataType = FuncLike[tail.FamilyType, FuncLike[tail.TargetType, V]]
   
    type RecHeadType = V
    
    def recData(data: RecDataType, arg: ArgType, f :  => Func[Term, Term]): RecHeadType = {
      val W = f.dom
      val X = f.codom
      val d = tail.induced(W, X)(f)(arg)
      data(arg)(d)
    }
    
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
  case class CnstDepFuncPtn[U <: Term, V <: Term](tail: Typ[Term], 
      headfibre : Term => (ConstructorPtn{type ConstructorType = U; type RecDataType = V}), headlevel: Int = 0)(
      implicit su: ScalaUniv[U]) extends RecursiveConstructorPtn{

    type ArgType = Term
    
    type HeadType = U
    
    type ConstructorType = FuncLike[Term, U]
    
    type RecDataType = FuncLike[Term, V]
    
    type RecHeadType = V
    
    def recData(data: RecDataType, arg: ArgType, f :  => Func[Term, Term]): RecHeadType = {
      data(arg) 
    }
    
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
    val pattern : ConstructorPtn

//    val typ: Typ[Term]

    /**
     * the constructor function itself.
     */
    val cons: pattern.ConstructorType
  }
  
  trait TypedConstructor[U <: Term] extends Constructor{
    type ConstructorType = U
  }

  trait RecursiveConstructor extends Constructor{
    type BaseType <: Term
    
    type ConstructorType = FuncLike[Term, BaseType]
    
    val pattern: RecursiveConstructorPtn
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
  case class ConstructorDefn[U <: Term](
      pattern: ConstructorPtn{type ConstructorType = U}, 
      cons: U) extends Constructor{
    type ConstructorType = U
  }

  /*
  case class RecConstructorDefn[U <: Term, H <: Term](
      pattern: RecursiveConstructorPtn{type HeadType = H; type ArgType = U}, 
      cons: FuncLike[U, H]) extends RecursiveConstructor{
  }
*/
  
  /*
  object Constructor{
    def apply(pattern : ConstructorPtn, typ: Typ[Term])(f : pattern.ConstructorType) = new Constructor(pattern, typ) {

      val cons = f.asInstanceOf[pattern.ConstructorType]
    }
  }
  *
  */
}