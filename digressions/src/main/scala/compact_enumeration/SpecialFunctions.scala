package compact_enumeration

import spire.math._

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

  object Ratio{
    def apply(first: Fn, second: Fn) = first *  Reciprocal(second)

    def unapply(fn: Fn) : Option[(Fn, Fn)] = fn match {
      case Mult(num, Reciprocal(den)) => Some((num, den))
      case _ => None
    }
  }


  case class Reciprocal(denominator : Fn) extends Fn{
    def apply(x: Double) = 1.0 / x
  }

  case class Compose(first: Fn, second: Fn) extends Fn{
    def apply(x: Double) = first(second(x))
    override def toString = s"${first} \u2218 ${second}"
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



  val sin = Func((x: Double) => math.sin(x))

  val cos = Func((x: Double) => math.cos(x))

  val log = Func((x: Double) => math.log(x))

  val exp = Func((x: Double) => math.exp(x))

  case class Pow(a: Double) extends Fn{
    def apply(x: Double) = math.pow(x, a)
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
