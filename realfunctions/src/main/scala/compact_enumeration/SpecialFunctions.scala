package compact_enumeration

import breeze.math._
import breeze.numerics
import scala.language.implicitConversions


/**
 * @author gadgil
 */
object SpecialFunctions {
  trait SpecialFunction extends (Double => Double){
    def +(that : Fn) = Add(this, that)
    
    def *(that : Fn) = Mult(this, that)
    
    def circ(that : Fn) = Compose(this, that)
    
    def apply(that: Fn) = Compose(this, that)
    
    def /(that: Fn) = Ratio(this, that)
    
    def -(that : Fn) = this + (-1.0 * that)
    
    def unary_- = 0.0 - (1.0 * this)
  }
  
  
  private type Fn = SpecialFunction
  
  case class Add(first: Fn, second: Fn) extends Fn{
    def apply(x: Double) = first(x) + second(x)
  }
  
  case class Mult(first: Fn, second: Fn) extends Fn{
    def apply(x: Double) = first(x) * second(x)
  }
  
  case class Ratio(first: Fn, second: Fn) extends Fn{
    def apply(x: Double) = first(x) / second(x)
  }
  
  case class Compose(first: Fn, second: Fn) extends Fn{
    def apply(x: Double) = first(second(x))
  }
  
  case class Func(defn: Double => Double) extends SpecialFunction{
    def apply(x: Double) = defn(x)
  }
  
  case class Const(value: Double) extends SpecialFunction{
    def apply(x: Double) = value
  }
  
  implicit def const(value: Double): SpecialFunction = Const(value)
  
  object Const{
    def derivative: PartialFunction[SpecialFunction, SpecialFunction] = {
      case Const(_) => Const(0)
    }
  }
  

  
  val sin = Func((x: Double) => numerics.sin(x))
  
  val cos = Func((x: Double) => numerics.cos(x))
  
  val log = Func((x: Double) => numerics.log(x))
  
  val exp = Func((x: Double) => numerics.exp(x))
  
  case class Pow(a: Double) extends Fn{
    def apply(x: Double) = numerics.pow(x, a)
  }
  
  object Pow{
    def derivative: PartialFunction[SpecialFunction, SpecialFunction] = {
      case Pow(a) => a * Pow(a -1)
    }
  }
  
  val id = Pow(1)

  val sq = Pow(2)
  
  val sqrt = Pow(1.0/ 2)
  
  def derivativeTable = 
    Const.derivative orElse
    Pow.derivative orElse
    Map(sin -> cos, cos ->  -sin, exp -> exp, log -> 1 / id)
}