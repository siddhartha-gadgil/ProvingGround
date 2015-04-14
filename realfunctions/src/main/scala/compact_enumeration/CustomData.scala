package compact_enumeration

import scala.util._
/**
 * @author gadgil
 * Custom data and goals for compact enumeration.
 * To blend with HoTT one instead imports a different object with case classes replaced by objects with apply and unapply.
 */
object CustomData {
  /**
   * local version of Typ
   */
  trait Typ
  
  /**
   * local version of Term
   */
  trait Term{
    val typ : Typ
  }
  
  def andTyp(first: Typ, second: Typ) = (first, second) match{
    case (FuncPositive(f, domleft) , FuncPositive(g, domright)) if f == g => 
      FuncPositive(f, Interval(domleft.lower, domright.upper))
  }
  
  case class Union(first: Term, second: Term) extends Term{
    lazy val typ = andTyp(first.typ, second.typ)
  }
  
  def and = Union.apply _
  
  type RealFunc = Real => Real
  
  type Real = Double
  
  case class Interval(lower: Real, upper: Real) extends Typ{
    lazy val midpoint : Real = (lower + upper) / 2
    
    lazy val width : Real = upper - lower
    
    lazy val firsthalf = Interval(lower, midpoint)
    
    lazy val secondhalf = Interval(midpoint, upper)
    
    assert(width >= 0)
  }
  
  type IntervalTyp = Interval
  
  case class FuncPositive(func : RealFunc, domain: Interval) extends Typ
  
  case class MVTMidPositive(func: RealFunc, domain: Interval, derivativeLower: Real, derivativeUpper: Real) extends Term{
    val mid = domain.midpoint
    val left = mid - ((domain.width / 2) * derivativeUpper)
    val right = mid + ((domain.width / 2) * derivativeLower)
    assert(mid >= 0)
    assert(left >= 0)
    assert(right >= 0)
    
    val typ = FuncPositive(func, domain)
  }
  
  object MVTMidPositive{
    def verify(func: RealFunc, domain: Interval, derivativeLower: Real, derivativeUpper: Real): Option[Term]= {
      Try(MVTMidPositive(func: RealFunc, domain: Interval, derivativeLower: Real, derivativeUpper: Real)).toOption
    }
  }
  
  case class MVTLeftPositive(func: RealFunc, domain: Interval, derivativeLower: Real) extends Term{
    val left = domain.lower
    val right = left + (domain.width * derivativeLower)

    assert(left >= 0)
    assert(right >= 0)
    
    val typ = FuncPositive(func, domain)
  }
  
  object MVTLeftPositive{
    def verify(func: RealFunc, domain: Interval, derivativeLower: Real): Option[Term]= {
      Try(MVTLeftPositive(func: RealFunc, domain: Interval, derivativeLower: Real)).toOption
    }
  }
  
  case class MVTRightPositive(func: RealFunc, domain: Interval, derivativeUpper: Real) extends Term{
    val right = domain.upper
    val left = right - (domain.width * derivativeUpper)

    assert(left >= 0)
    assert(right >= 0)
    
    val typ = FuncPositive(func, domain)
  }
  
    object MVTRightPositive{
    def verify(func: RealFunc, domain: Interval, derivativeUpper: Real): Option[Term]= {
      Try(MVTRightPositive(func: RealFunc, domain: Interval, derivativeUpper: Real)).toOption
    }
  }
}