package compact_enumeration

import compact_enumeration._
import FieldOps._
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

Formulas:

*/

val t = x

val cosh = (exp + (one/exp )) / 2

val t19 = t * t

val t20 = sqrt(t19-1)

val t21 = sqrt(t19 + one)

val acosh = log( t + t20)

val sinh = (exp(t) - (one/exp(t) ) ) / 2

val asinh = log(t + t21)


/*

The functions a, b, m, n in x,y,z,w.

*/

val n1 = pi * (3-z) * x

val t1 = (one/z)-1

val t2 = ( N(6) * w * t1 ) + N(6)

val a = ( pi/2 ) + ( n1/3 )

val b = y * t2 * pi

val m = ( (N(3) / 2) * w * t1 ) + N(2)

val n = N(3) / z

/*

There are three terms in the expression of the inequality representing the perimeters of three regular hyperbolic polygons.

*/


/*

The first term: Perim(P)

*/


val t3 = a/N(2)

val t4 = pi/( N(2)*n)

val n2 = (cos(t3) * cos(t3)) + cos(t4)

val d2 = sin(t3) * sin(t3)

val frac = n2/d2

val t5 = acosh(frac)

val term1 = n * t5

/*

the second term: Perim(P_1)

*/


val t6 = (b+ (N(2) *pi))/(N(8)* m)

val t7 = pi/(2 * m)

val t8 = sin(t6) * sin(t6)

val t9 = cos(t7)

val n3 = t8 + t9

val d3 = cos(t6) * cos(t6)

val frac3 = n3/d3

val t10 = acosh(frac3)

val term2 = m * t10

/*

the second term: Perim(P_1)

*/

val t11 = (N(4) * n * (pi-a)) - b

val t12 = N(8) * (n + one -m)

val t13 = t11/t12

val t14 = N(2) * (n + one -m)

val t15 = pi/t14

val t16 = sin(t13) * sin(t13)

val t17 = cos(t15)

val n4 = t16+t17

val d4 = cos(t13) * cos(t13)

val frac4 = n4/d4

val t18 = acosh(frac4)

val term3 = (n+ one -m) * t18




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
