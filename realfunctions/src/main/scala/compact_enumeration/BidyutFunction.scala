package compact_enumeration

import compact_enumeration._
import PointWise._
import FieldOpsSyms._

import FormalElemFunction._
import FormalElemFunc._
import Circ._

import ApproxTrig._

import spire.math.{Interval, Rational, SafeLong}

import spire.math.Interval._

import spire.implicits._

/**
 * @author gadgil
 * Finds interval bounds, for the function given as func. 
 * This can be defined form sin, cos, log, exp and coordinates x,y, z and w
 * Note that sin and sin(x) are not the same, sin is a function of one variable and x is a projection.
 * So tan = sin/cos is correct, giving a function of one variable, 
 * but Tan = sin(x)/cos(x) is a function on the domain (a cube) which is a composition of the projection x with the tan function.
 * In particular, Tan(y) is meaningless.
 */
object BidyutFunction {  
  val I  = Interval.closed(Rational(0), Rational(1))
  
  val cube = Cube(Vector(I, I, I, I))
  
  // Intermediate functions to be defined here
  
  /*
   * the function to check is positive.
   */
  lazy val func: FormalElemFunction = ??? 
  
  def evalRat(N: Int, depth: Int) = cube.splitBound(func, N, depth) 
  
  def eval(N: Int, depth: Int) =
    for (bnd <- evalRat(N, depth)) yield bnd.mapBounds(_.toDouble)
  
  /*
   * returns Some(true) if it is verified that the function is non-negative
   */
  def verify(N: Int, depth: Int) = evalRat(N, depth) map ((bnd) => !bnd.hasBelow(0))
}