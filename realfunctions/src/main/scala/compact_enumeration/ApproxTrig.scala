package compact_enumeration

import spire.math._
import spire.algebra._
import spire.implicits._
import spire.syntax.literals._
import annotation.tailrec
import scala.util._
import Stream._
import algebra._

/**
 * @author gadgil
 *
 * Interval bounds for exp, log, sin, cos
 */
class ApproxTrig(N: SafeLong) {
  import ApproxTrig._

  import algebra.PointWise._
  
  val fieldOps = implicitly[FieldOps[Interval[Rational] => Option[Interval[Rational]]]]
  
  import spire.syntax.literals._

  val width = r"1" / N
  
  val E = Interval.closed(-width, width)
  
  implicit val appr = new ApproximationContext(width)

  val pi = ConstantBounder(Interval.point(Real.pi.toRational) + E)
  
  lazy val J = Interval.closed(Rational(0), width)

    /**
   * returns bound on a positive interval,
   * given a bound at k for the 
   * function in [k/N, (k+1)/N] 
   */
  def spanPositive(stream: Int => Interval[Rational])(
      xs: Interval[Rational]) : Option[Interval[Rational]] = 
      if (xs.isEmpty) Some(Interval.empty)
      else {
        import ApproxTrig._
        val startOpt = getBound((xs * N).lowerBound map (_.floor.toInt))
    
        val endOpt = getBound((xs * N).upperBound map (_.ceil.toInt))
    
        def chop(end: Int, start: Int) = if (start < end) end -1 else end
    
        val imagesOpt = for (start <- startOpt; end <- endOpt) yield 
          for (j <- start to chop(end, start)) yield stream(j)
    
        imagesOpt map (_.reduce (_ union _))
    
  }

  def span(stream: Int => Interval[Rational], 
      inv: Interval[Rational] => Interval[Rational])(xs: Interval[Rational]) =
      { 
    val split = xs.splitAtZero
    for (a <- spanPositive(stream)(split._2); b <- spanPositive(stream)(split._1)) yield
      a union inv(b)
      }
  
  /**
   * stream of bounds on the exponential.
   * At n, this is an interval containing e^(n/ N)
   */
  lazy val expStream : Stream[Interval[Rational]] = Nat map ((n: SafeLong) =>
    if (n ==0) Interval.point(Rational(1))
    else
      {
      val b = get(expStream, n-1)
      val a = b * Interval.closed(r"1/2" , (1 + width) / (2 - (width * width)))
      (a * width * width) + (b * (width + 1))
    })

  
/**
 * exponential approximated on positive and negative sides.
 */
  def expFn(x: Rational) : Interval[Rational] =
    if (x >= 0)
      expStream((x*N).toInt)
      else
        Interval.point(r"1")  / expFn(-x) 

   def expDouble(x: Double) = expFn(x.toRational) mapBounds(_.toDouble)
   
   def expInterval(x: Rational) : Interval[Rational] = {
        if (x>= 0)
           expStream((x * N).floor.toInt) union expStream((x * N).ceil.toInt)
        else
          Interval.point(r"1")  / expInterval(-x)
      }


  def logStream : Stream[Interval[Rational]] =  Nat map ((n: SafeLong) =>
    if (n ==0) Interval.point(r"0")
    else
    {
      val prev= get(logStream, n-1)
      prev + (Interval.closed(r"1"/ (r"1" + (width * n)), r"1" / (r"1" + (width * (n-1)))) * width)     
    }
  )
  
  def logOptBounds(xs: Interval[Rational]) =
    if (xs.hasAtOrBelow(0)) None
    else {
      val spl = xs.split(1)
      val aboveOneOpt = spanPositive(logStream)(spl._2 - 1)
      val belowOneOpt = spanPositive(logStream)(Interval.point(r"1") / spl._1) map ((I) => -I)
      
      for (aboveOne <- aboveOneOpt; belowOne <- belowOneOpt) yield (aboveOne + belowOne)
    }
  


    /**
   * Bounds for solutions to f" + f = 0 (i.e., sin and cos)
   * 
   * @param c  the bound on f at the left end-point
   * @param b the bound on f' at the left end-point
   * @param width width of the interval
   */
   
   
  case class TrigBound(b: Interval[Rational], c: Interval[Rational]){
    
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
 
  lazy val sinStream : Stream[(Interval[Rational], Interval[Rational])] = 
    Nat map ((n: SafeLong) =>
    if (n ==0) (Interval.point(Rational(0)), Interval.point(Rational(0)))
    else
      {
      val c = get(sinStream, n-1)._1
      val b = get(cosStream, n -1)._1
      val trigAppr = TrigBound(b, c)
      (trigAppr.atRightEnd, trigAppr.intervalImage)
    })
  
  lazy val cosStream : Stream[(Interval[Rational], Interval[Rational])] = 
    Nat map ((n: SafeLong) =>
    if (n ==0) (Interval.point(Rational(1)), Interval.point(Rational(1)))
    else
      {
      val c = get(cosStream, n-1)._1
      val b = -get(sinStream, n -1)._1
      val trigAppr = TrigBound(b, c)
      (trigAppr.atRightEnd, trigAppr.intervalImage)
    })
  
  import ApproxTrig._
  
  val exp : Interval[Rational] => Option[Interval[Rational]] = 
    span(expStream, (I) => Interval.point(r"1") /I)
  
  val log : Interval[Rational] => Option[Interval[Rational]] = logOptBounds
  
  val sin : Interval[Rational] => Option[Interval[Rational]] = (
      span((j: Int) =>
        if (j ==0) sinStream(j)._1 else sinStream(j)._2, 
        (I) => -I)
        )
        
  val cos : Interval[Rational] => Option[Interval[Rational]] = (
      span((j: Int) =>
        if (j ==0) cosStream(j)._1 else cosStream(j)._2, 
        (I) => -I)
        )
}

object ApproxTrig{
  import spire.math.interval.{Bound, Closed, ValueBound}
  
  type FuncBound = Interval[Rational] => Option[Interval[Rational]]
  
  import Interval._
  
  val sg = Semiring[Interval[Rational]]
  
  import algebra.PointWise._
  
//  val ma = implicitly[MultiplicativeAbGroup[Interval[Rational]]] 
  
  val goal = implicitly[FieldOps[Interval[Rational] => Option[Interval[Rational]]]]
  
  val Nat: Stream[SafeLong] = 0 #:: (Nat map ((n) => n + 1))

  @tailrec def get[A](as: Stream[A], n: SafeLong) : A = {
    if (n ==0) as.head else get(as.tail, n-1)
  }
  /*
  case class FunctionBounder(bounds : Interval[Rational] => Option[Interval[Rational]]) extends 
    (Interval[Rational] => Option[Interval[Rational]]){
    def apply(j: Interval[Rational]) = bounds(j)
    
    def andThen(that: FunctionBounder) : FunctionBounder = {
      def composeBound(j: Interval[Rational]) = this(j) flatMap (that(_))
      FunctionBounder(composeBound)
    }
    
    def of(that: FunctionBounder) : FunctionBounder = that andThen this
    
    def +(that: FunctionBounder) = {
      def newBounds(I : Interval[Rational]) = 
        for (first <- bounds(I); second <- that.bounds(I)) yield first+ second 
      FunctionBounder(newBounds)
    }
    
    def *(that: FunctionBounder) = {
      def newBounds(I : Interval[Rational]) = 
        for (first <- bounds(I); second <- that.bounds(I)) yield first * second 
      FunctionBounder(newBounds)
    }
    
    def /(that: FunctionBounder) = {
      def newBounds(I : Interval[Rational]) = 
        for (first <- bounds(I); second <- that.bounds(I) if !(second.contains(0))) yield first/ second 
      FunctionBounder(newBounds)
    }
    
    def apply(that: FunctionBounder) = of(that)
  }
  
  */

  def ConstantBounder(r: Interval[Rational]) = 
      ((I: Interval[Rational]) => Some(r))

  
  
//  import FunctionBounder.ConstantBounder
  
  case class Cube(coords: Vector[Interval[Rational]]){

     
      def splitCube : Option[Set[Cube]] = {
        if (coords == Vector()) Some(Set(this))
        else {
          val initCubesOpt = Cube(coords.init).splitCube
          val finalCoordsOpt = split(coords.last)
          for (initCubes <- initCubesOpt; finalCoords <- finalCoordsOpt)
            yield for (cubelet <- initCubes; interval <- finalCoords)
              yield Cube(cubelet.coords :+ interval)  
        }
    }
      
      def recSplit(k: Int) : Option[Set[Cube]] = if (k<1) Some(Set(this)) else this.recSplit(k -1)
      
      def bound(func: Cube => Option[Interval[Rational]]) = func(this)
      
      def recSplitBound(func: Cube => Option[Interval[Rational]], depth: Int) = {
        val boundsOpt = recSplit(depth) map ((cubelets) =>
          for (cube <- cubelets; bnd <- func(cube)) yield bnd)
        for (bounds <- boundsOpt; unionBound <- Try(bounds.reduce(_ union _)).toOption) yield unionBound
      }
      
      def splitBound(fn: FormalElemFunction, N: SafeLong, depth: Int) = 
        recSplitBound(rationalBound(fn, N, _), depth)
  }
    
  import algebra.ElementaryFunctions
  
  import algebra.FormalElemFunction
  
  /**
   * Define functions inside a class extending this,
   * given by a resolution and a cube, and use coordinate functions on it as well as trignometric functions 
   * 
   */  
  class RationalBounds(N: SafeLong, cube: Cube) extends ApproxTrig(N) with 
    ElementaryFunctions[Interval[Rational] => Option[Interval[Rational]]]{
      val proj = (i: Int) => ConstantBounder(cube.coords(i))
        
      }

  implicit val composeApprox = new Circ[Interval[Rational] => Option[Interval[Rational]]]{
    def circ(x: Interval[Rational] => Option[Interval[Rational]], 
        y: Interval[Rational] => Option[Interval[Rational]]) = 
          (I : Interval[Rational]) => y(I) flatMap ((J) => x(J))
  }
  
  def rationalBound(fn: FormalElemFunction, N: SafeLong, cube: Cube) = {
    implicit val local : ElementaryFunctions[Interval[Rational] => Option[Interval[Rational]]] = 
      new RationalBounds(N, cube)
    val bnd = fn.as[Interval[Rational] => Option[Interval[Rational]]]
    bnd(Interval.point(0)) // bound is supposed to be independent of the chosen interval. 
  }


  import spire.math.Interval._

  
  def getBound[A](J: Bound[A]): Option[A] =  J match{
    case ValueBound(a) => Some(a)
    case _ => None
  }
  
  def split[F: Field : Order](J: Interval[F]) = {
    for (lower <- getBound(J.lowerBound); upper <- getBound(J.upperBound))
      yield Set(Interval.closed(lower, (lower + upper)/ 2), Interval.closed((lower + upper)/ 2, upper))
  }

}
