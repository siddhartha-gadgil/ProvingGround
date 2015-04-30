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
  
  lazy val J = Interval.closed(r"0", width)

  
  import Interval._
  
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
  
  
  
}

object ApproxTrig{
  val Nat: Stream[SafeLong] = 0 #:: (Nat map ((n) => n + 1))
  
  @tailrec def get[A](as: Stream[A], n: SafeLong) : A = {
    if (n ==0) as.head else get(as.tail, n-1)
  }
}
