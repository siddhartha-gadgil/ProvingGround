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
    val startOpt = ((xs * N) mapBounds (_.floor.toInt)).bottom(0, 0)
      //getClosed((xs * N).lowerBound map (_.floor.toInt))
    
    val endOpt = ((xs * N) mapBounds (_.ceil.toInt)).top(0)
    
    val imagesOpt = for (start <- startOpt; end <- endOpt) yield 
      for (j <- start to end) yield stream(j)
    
    imagesOpt map (_.reduce (_ union _))
  }
  
  val expBounds= ApproxTrig.FunctionBounder(spreadOpt(expstream))

    /**
   * Bounds for solutions to f" + f = 0 (i.e., sin and cos)
   */
  case class TrigBound(width: Rational, b: Interval[Rational], c: Interval[Rational]){
    
    lazy val lftBound = - c / r"2"// bound based on f" + f at left endpoint.
    
    lazy val rghtBound = - (c + b * width) / (2 + width*width) // bound based on f" + f at right endpoint.

    lazy val endsBound = lftBound union rghtBound // bound based on f" + f at both endpoints.
    
    lazy val derImage = b union (b + (endsBound * width * 2))
    
    lazy val derSignChange = derImage.crossesZero
    
    implicit val appr = new ApproximationContext(width)
    
    lazy val b2c2 = (b.pow(2) * 2 + c.pow(2)).sqrt + Interval.closed(-width, width)
    
    lazy val discriminantNonNegative = ((-c - b2c2) union (-c + b2c2))/(Rational(4))
    
    lazy val a = if (derSignChange && (endsBound intersects discriminantNonNegative)) 
      endsBound union discriminantNonNegative else endsBound
    
    lazy val atRightEnd = (a * width * width) + (b * width) + c
    
    lazy val intervalImage = (a * J.pow(2)) + (b * J) + c
  }
 
  lazy val sinStream : Stream[Interval[Rational]] = Nat map ((n: SafeLong) =>
    if (n ==0) Interval.point(Rational(0))
    else
      {
      val c = get(sinStream, n-1)
      val b = get(cosStream, n -1)
      val trigAppr = TrigBound(width, b, c)
      trigAppr.atRightEnd
    })
  
  lazy val cosStream : Stream[Interval[Rational]] = Nat map ((n: SafeLong) =>
    if (n ==0) Interval.point(Rational(1))
    else
      {
      val c = get(cosStream, n-1)
      val b = -get(sinStream, n -1)
      val trigAppr = TrigBound(width, b, c)
      trigAppr.atRightEnd
    })
}

object ApproxTrig{
  import spire.math.interval.{Bound, Closed}
  
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

  
  
  
  def getClosed[A](J: Bound[A]) = J match{
    case Closed(a) => Some(a)
    case _ => None
  }
  


}
