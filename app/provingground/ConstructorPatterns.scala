package provingground
import HoTT._
import Families._
import math._
import ScalaUniverses._
import scala.util._
import scala.language.existentials

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
  sealed trait ConstructorPtn{self =>
    type Cod <:  Term
    
    def withCod[CC <: Term with Subs[CC]] : ConstructorPtn{type ConstructorType = self.ConstructorType; type Cod = CC}
    
    def -->:[V <: Term , T <: Term with Subs[T], D <: Term with Subs[D]](
        that : FmlyPtnLike[Term, Cod]) = FuncPtn[Cod](that, this)

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
    
    def recDef(cons: ConstructorType, data: RecDataType, f :  => Func[Term, Cod]): Term => Option[Cod]
    
    def rec[CC <: Term with Subs[CC]] = {
      val newPtn = withCod[CC]
      val fn : (newPtn.ConstructorType, newPtn.RecDataType, Func[Term, CC]) => Term => Option[CC] = { 
        (cons, data, f) => (t) => newPtn.recDef(cons, data, f)(t)
      }
      fn
    }
    
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

    type Cod = Term

    def withCod[CC <: Term with Subs[CC]] = IdTarg[CC]
    
    def recDef(cons: ConstructorType, data: RecDataType, f :  => Func[Term, Term]): Term => Option[Term] = {
      case (t: Term) if t == cons => Some(data)
      case _ => None
    }
    
    case class IdTarg[C<: Term with Subs[C]]() extends ConstructorPtn{
      def apply(W : Typ[Term]) = W

    val univLevel = 0

    type ConstructorType = Term
    
    type RecDataType = C

    type Cod = C
    
    def withCod[CC <: Term with Subs[CC]] = IdTarg[CC]
    
    def recDef(cons: ConstructorType, data: RecDataType, f :  => Func[Term, Cod]): Term => Option[Cod] = {
      case (t: Term) if t == cons => Some(data)
      case _ => None
    }
    }
    
  }



  /**
   * Functional extension of a type pattern
   */
  sealed trait RecursiveConstructorPtn extends ConstructorPtn{self =>
    type ArgType <: Term
    
    // type Cod = Term
    
    type HeadType <: Term
    
    type ConstructorType <: FuncLike[ArgType, HeadType]
    
    type RecHeadType <: Term
    
    val headfibre: ArgType => ConstructorPtn{type ConstructorType = HeadType; 
      type RecDataType = RecHeadType; type Cod = self.Cod}
    
    def recData(data: RecDataType, arg: ArgType, f : => Func[Term, Cod]): RecHeadType
    
    def recDef(cons: ConstructorType, data: RecDataType, f :  => Func[Term, Cod]): Term => Option[Cod] = {
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
  case class FuncPtn[C <: Term](tail: FmlyPtnLike[Term, C], head : ConstructorPtn{type Cod = C}) extends RecursiveConstructorPtn{self =>
    type ArgType = tail.FamilyType
    
    type HeadType = head.ConstructorType
    
    type Cod = C
    
    def withCod[CC <: Term with Subs[CC]] = {
      val _res = FuncPtn[CC](tail.withCod[CC], head.withCod[CC])
      val res  = _res.asInstanceOf[FuncPtn[CC]{type ConstructorType = self.ConstructorType}]
      res
    }
    
    val _head : ConstructorPtn{type ConstructorType = HeadType; type RecDataType = RecHeadType; type Cod = C} = head
    
    val headfibre = (t: ArgType) => _head
    
    type ConstructorType = FuncLike[Term, head.ConstructorType]
    
    type RecHeadType = head.RecDataType
    
    type RecDataType = FuncLike[tail.FamilyType, FuncLike[tail.TargetType, head.RecDataType]]

    def recData(data: RecDataType, arg: ArgType, f :  => Func[Term, C]): RecHeadType = {
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
      ) extends RecursiveConstructorPtn{self =>
    type ArgType = Term
    
    type HeadType = head.ConstructorType
    
    type Cod = head.Cod
    
    def withCod[CC <: Term with Subs[CC]] = {
      val _res = CnstFncPtn(tail, head.withCod[CC])
      val res  = _res.asInstanceOf[CnstFncPtn{type ConstructorType = self.ConstructorType; type Cod = CC}]
      res
    }
    
    val _head : ConstructorPtn{type ConstructorType = HeadType; type RecDataType = RecHeadType; type Cod = self.Cod} = head
    
    val headfibre = (t: ArgType) => _head
    
       
    type RecDataType = FuncLike[Term, head.RecDataType]
    
    type RecHeadType = head.RecDataType
    
    type ConstructorType = FuncLike[Term, head.ConstructorType]

    def recData(data: RecDataType, arg: ArgType, f :  => Func[Term, Cod]): RecHeadType = data(arg)
    
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
  case class DepFuncPtn[U <: Term, V <: Term, W <: Term, C <: Term](tail: FmlyPtnLike[Term, C],
      headfibre : Term => (ConstructorPtn{type ConstructorType = U; type RecDataType = V; type Cod = C}),
      headlevel: Int = 0)(implicit su: ScalaUniv[U]) extends RecursiveConstructorPtn{self =>
    type ArgType = tail.FamilyType
    
    type HeadType = U
    
    type ConstructorType = FuncLike[Term, U]

    type Cod = C
    
    def withCod[CC <: Term with Subs[CC]] = {
      
      val _res = DepFuncPtn(tail.withCod[CC], (t: Term) => headfibre(t).withCod[CC])(su)
      val res  = _res.asInstanceOf[ConstructorPtn{type ConstructorType = self.ConstructorType; type Cod = CC}]
      res
    }
    
    type RecDataType = FuncLike[tail.FamilyType, FuncLike[tail.TargetType, V]]
   
    type RecHeadType = V
    
    def recData(data: RecDataType, arg: ArgType, f :  => Func[Term, C]): RecHeadType = {
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
  case class CnstDepFuncPtn[U <: Term, V <: Term, C <: Term](tail: Typ[Term], 
      headfibre : Term => (ConstructorPtn{type ConstructorType = U; type RecDataType = V; type Cod = C}), headlevel: Int = 0)(
      implicit su: ScalaUniv[U]) extends RecursiveConstructorPtn{self =>

    type ArgType = Term
    
    type HeadType = U
    
    type Cod = C
    
        def withCod[CC <: Term with Subs[CC]] = {
      
      val _res = CnstDepFuncPtn(tail, (t: Term) => headfibre(t).withCod[CC])(su)
      val res  = _res.asInstanceOf[ConstructorPtn{type ConstructorType = self.ConstructorType; type Cod = CC}]
      res
    }
    
    type ConstructorType = FuncLike[Term, U]
    
    type RecDataType = FuncLike[Term, V]
    
    type RecHeadType = V
    
    def recData(data: RecDataType, arg: ArgType, f :  => Func[Term, C]): RecHeadType = {
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