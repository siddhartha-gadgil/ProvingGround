package provingground
import HoTT._
import Families._

import scala.language.implicitConversions
import scala.util._
import scala.language.existentials
import ConstructorPatterns.getArg

/**
 * @author gadgil
 */
class IndexedConstructorPatterns[F <: Term with Subs[F], 
  A <: Term with Subs[A], 
  I <: Term with Subs[I], C <: Term with Subs[C]](
      val typFmlyPtn: FmlyPtn[Term, C]{type FamilyType = F; type ArgType = A; type IterFunc = I}
      ) {outer =>
        type Cod = C
    sealed trait ConstructorPtn{self =>
    /**
     * Type of codomain X
     */
 //   type Cod <:  Term with Subs[Cod]
    
  //  val typFmlyPtn: FmlyPtn[Term, Cod]{type FamilyType = F; type ArgType = A; type IterFunc = I}
    
 //   type IterFunc = typFmlyPtn.IterFunc
    
    /**
     * argument for the final image
     */
    val arg : A
    
    type ConstructorType <: Term with Subs[ConstructorType]
    
    /**
     * type of a constructor for this pattern.
     */
    def apply(tps : F) : Typ[ConstructorType]
    
    /**
     * (scala) type of data for recursion corresponding to the single constructor
     */
    type RecDataType <: Term with Subs[RecDataType]

    /**
     * domain containing the recursion data for the constructor, i.e., the HoTT type of recursion data.
     */
    def recDom(w: F, x: Typ[Cod]) : Typ[RecDataType]
    
    
    /**
     * given a term, matches to see if this is the image of a given (quasi)-constructor.
     * returns simplification (wrapped in Some) if the term matches.
     * @param cons constructor, actually quasi-constructor, with which to match.
     * @param data definition data for the image of the constructor.
     * @param f the function being defined, to be applied recursively.
     */
    def recDef(cons: ConstructorType, data: RecDataType, f :  => I): Term => Option[Cod]
    }
    
    case class iW(arg: A) extends ConstructorPtn{
      type ConstructorType = Term
      
  //    type Cod = C
      
      type RecDataType = Cod
      
      def apply(tps: F) = typFmlyPtn.contractType(tps)(arg)
      
      def recDom(w: F, x: Typ[Cod]) : Typ[RecDataType] = x
      
      def recDef(cons: ConstructorType, data: RecDataType, f :  => I): Term => Option[Cod] = {
        case (t: Term) if t == cons => Some(data)
        case _ => None
    }
    }
    
    /**
     * Eventually remove this, to avoid duplication of constructors
     */
 /*   case class SimpleFuncPtn[F <: Term with Subs[F], 
      A <: Term with Subs[A], 
      I <: Term with Subs[I],
      C<: Term with Subs[C]](
        tailArg: A, head : ConstructorPtn[F, A, I]{type Cod = C}) extends RecursiveConstructorPtn[F, A, I]{
      type Cod = C
      
      type ConstructorType = Func[Term, head.ConstructorType]
      
      val typFmlyPtn = head.typFmlyPtn
      
      val arg = head.arg
      
      type RecDataType = Func[Term, Func[Cod, head.RecDataType]]
      
      def apply(tps: F) = typFmlyPtn.contractType(tps)(tailArg) ->: head(tps)
      
      def recDom(w: F, x: Typ[Cod]) : Typ[RecDataType] =  apply(w) ->: x ->: head.recDom(w, x)
    }
   */ 
    
      /**
   * Functional extension of a type pattern
   */
  sealed trait RecursiveConstructorPtn extends ConstructorPtn{self =>
    /**
     * scala type of argument to constructor A -> ... (or A ~> ...)
     */
    type ArgType <: Term with Subs[ArgType]

    // type Cod = Term

    /**
     * scala type of the head T for constructor A -> T
     * for Pi-Types, the head may have varying HoTT type but must have fixed scala type.
     */
    type HeadType <: Term with Subs[HeadType]

    type ConstructorType <: FuncLike[ArgType, HeadType] with Subs[ConstructorType]

    /**
     * (scala) type of recursive data for head.
     */
    type HeadRecDataType <: Term

    /**
     * The head pattern, constant T for A -> T and T(a) for A ~> T(a)
     */
    val headfibre: ArgType => ConstructorPtn{type ConstructorType = HeadType;
      type RecDataType = HeadRecDataType; type Cod = outer.Cod}

    /**
     * returns data for recursion to be passed on to the head given an argument (when matching with the construtor).
     */
    def headData(data: RecDataType, arg: ArgType, f : => I): HeadRecDataType

    def recDef(cons: ConstructorType, data: RecDataType, f :  => I): Term => Option[Cod] = {
      t =>
        for (arg <- getArg(cons)(t); term <-headfibre(arg).recDef(cons(arg), headData(data, arg, f), f)(t)) yield term
    }

  }
    
    
    
    case class FuncPtn(
        tail: FmlyPtn[Term, C], 
        tailArg: A, 
        head : ConstructorPtn{type Cod = C})extends RecursiveConstructorPtn{
      type ArgType = tail.Family

      type HeadType = head.ConstructorType
      
      type Cod = C
      
      type ConstructorType = Func[ArgType, head.ConstructorType]
      
      type HeadRecDataType = head.RecDataType
      
  //    val typFmlyPtn = head.typFmlyPtn
      
      val arg = head.arg
      
      val _head : ConstructorPtn{type ConstructorType = HeadType; type RecDataType = HeadRecDataType; type Cod = C} = head

      val headfibre = (t: ArgType) => _head
      
            
      type RecDataType = Func[tail.Family, Func[tail.TargetType, head.RecDataType]]
      
      def apply(tps: F) = {
        val w = typFmlyPtn.contractType(tps)(tailArg)
        FuncTyp[ArgType, head.ConstructorType](tail(w), head(tps))
      }
       
     
      
      def recDom(tps: F, x: Typ[Cod]) : Typ[RecDataType] =  {
        val w = typFmlyPtn.contractType(tps)(tailArg)
        tail(w) ->: tail.target(x) ->: head.recDom(tps, x)
      }
      
     def headData(data: RecDataType, arg: ArgType, f :  => I): HeadRecDataType = {
       val g = typFmlyPtn.fill(f)(tailArg)
        data(arg)(tail.induced(g)(arg))
      }
      
    }
}