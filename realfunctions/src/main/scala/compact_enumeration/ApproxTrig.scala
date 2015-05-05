package compact_enumeration

import spire.math._
import spire.algebra._
import spire.implicits._
import spire.syntax.literals._
import annotation.tailrec

import Stream._

/**
 * @author gadgil
 *
 * Interval bounds for exp, log, sin, cos
 */
class ApproxTrig(N: SafeLong) {
  import ApproxTrig.{get, Nat}

  import spire.syntax.literals._

  val width = r"1" / N

  lazy val J = Interval.closed(Rational(0), width)


  lazy val up = J.upperBound
//  import Interval._

  /**
   * stream of bounds on the exponential.
   * At n, this is an interval containing e^(n/ N)
   */
  lazy val expstream : Stream[Interval[Rational]] = Nat map ((n: SafeLong) =>
    if (n ==0) Interval.point(Rational(1))
    else
      {
      val b = get(expstream, n-1)
      val a = b * Interval.closed(r"1/2" , (1 + width) / (2 - (width * width)))
      (a * width * width) + (b * (width + 1))
    })


  def exp(x: Rational) : Interval[Rational] =
    if (x >= 0)
      expstream((x*N).toInt)
      else
        Interval.point(r"1")  / exp(-x) 

   def expDouble(x: Double) = exp(x.toRational) mapBounds(_.toDouble)
   
   def expInterval(x: Rational) : Interval[Rational] = {
        if (x>= 0)
           expstream((x * N).floor.toInt) union expstream((x * N).ceil.toInt)
        else
          Interval.point(r"1")  / expInterval(-x)
      }

  def spreadOpt(stream: Int => Interval[Rational])(xs: Interval[Rational]) = {
    import ApproxTrig._
    val startOpt = getClosed((xs * N).lowerBound map (_.floor.toInt))
    
    val endOpt = getClosed((xs * N).upperBound map (_.ceil.toInt))
    
    val imagesOpt = for (start <- startOpt; end <- endOpt) yield 
      for (j <- start to end) yield stream(j)
    
    imagesOpt map (_.reduce (_ union _))
  }
  
  val expBounds= ApproxTrig.FunctionBounder(spreadOpt(expstream))

}

object ApproxTrig{
  val Nat: Stream[SafeLong] = 0 #:: (Nat map ((n) => n + 1))

  @tailrec def get[A](as: Stream[A], n: SafeLong) : A = {
    if (n ==0) as.head else get(as.tail, n-1)
  }
  
  case class FunctionBounder(bounds : Interval[Rational] => Option[Interval[Rational]]) extends 
    (Interval[Rational] => Option[Interval[Rational]]){
    def apply(j: Interval[Rational]) = bounds(j)
    
    def andThen(that: FunctionBounder) = {
      def composeBound(j: Interval[Rational]) = this(j) flatMap (that(_))
      FunctionBounder(composeBound)
    }
  }

  import spire.math.Interval._

  
  
  
  def getClosed[A](J: Interval.Bound[A]) = J match{
    case Interval.Closed(a) => Some(a)
    case _ => None
  }

}
