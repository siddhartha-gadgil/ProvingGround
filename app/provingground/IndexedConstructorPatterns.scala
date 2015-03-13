package provingground
import HoTT._
import Families._

import scala.language.implicitConversions
import scala.util._
import scala.language.existentials

/**
 * @author gadgil
 */
object IndexedConstructorPatterns {
    sealed trait ConstructorPtn[F <: Term with Subs[F], A <: Term with Subs[A]]{self =>
    /**
     * Type of codomain X
     */
    type Cod <:  Term with Subs[Cod]
    
    val typFmlyPtn: FmlyPtn[Typ[Term], Typ[Term]]{type FamilyType = F; type ArgType = A}
    
    val arg : A
    
    type ConstructorType <: Term with Subs[ConstructorType]
    
    def apply(tps : F) : Typ[ConstructorType]
    }
    
    case class iW[F <: Term with Subs[F], A <: Term with Subs[A], C<: Term with Subs[C]](
        typFmlyPtn: FmlyPtn[Typ[Term], Typ[Term]]{type FamilyType = F; type ArgType = A}, arg: A) extends ConstructorPtn[F, A]{
      type ConstructorType = Term
      
      type Cod = C
      
      type RecDataType = C
      
      def apply(tps: F) = (typFmlyPtn.contract(tps))(arg)
    }
    
    case class SimpleFuncPtn[F <: Term with Subs[F], A <: Term with Subs[A], C<: Term with Subs[C]](
        tailArg: A, head : ConstructorPtn[F, A]{type Cod = C}) extends ConstructorPtn[F, A]{
      type Cod = C
      
      type ConstructorType = Func[Term, head.ConstructorType]
      
      val typFmlyPtn = head.typFmlyPtn
      
      val arg = head.arg
      
      def apply(tps: F) = typFmlyPtn.contract(tps)(tailArg) ->: head(tps)
      
    }
    
    case class FuncPtn[F <: Term with Subs[F], A <: Term with Subs[A], C<: Term with Subs[C]](
        tail: FmlyPtn[PairObj[F, A], C], head : ConstructorPtn[F, A]{type Cod = C}){
      type Cod = C
      
      type ConstructorType = Func[Term, head.ConstructorType]
      
      val typFmlyPtn = head.typFmlyPtn
      
      val arg = head.arg
      
    }
}