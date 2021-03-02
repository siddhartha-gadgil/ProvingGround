package compact_enumeration

import spire.math._
import spire.algebra._
import spire.implicits._
import annotation.tailrec
import scala.util._
import LazyList._
import compact_enumeration._

/**
  * @author gadgil
  *
  * Interval bounds for exp, log, sin, cos
  */
class ApproxTrig(N: SafeLong) {
  import ApproxTrig._

  import compact_enumeration.FieldOps._

  val fieldOps = implicitly[FieldOps[Approx]]

  import spire.syntax.literals._

  /**
    * width of inductive interval
    */
  val width = r"1" / N

  /**
    * error interval
    */
  val E = Interval.closed(-width, width)

  //  implicit val appr = new ApproximationContext(width)

  import java.math.MathContext

  implicit val mc = MathContext.DECIMAL128

  val pi = ConstantBound(Interval.point(Real.pi.toRational) + E)

  lazy val J = Interval.closed(Rational(0), width)

  /**
    * returns bound on a positive interval,
    * given a bound, with index k, for the
    * function in in [(k -1)/N, k/N] (or [0, 0] if k = 0
    *
    * @param stream bound for function on [k/N, (k+1)/N] is stream(n)
    * @param xs interval whose image is bounded.
    *
    *
    */
  def spanPositive(stream: Int => Interval[Rational])(
      xs: Interval[Rational]): Option[Interval[Rational]] =
    if (xs.isEmpty) Some(Interval.empty)
    else {
      assert(!xs.hasBelow(0),
             s"""spanPositive should have input a positive interval,
              tried to bound on $xs""")
      import ApproxTrig.getBound
      val startOpt = getBound((xs * N).lowerBound map (_.floor.toInt))

      val endOpt = getBound((xs * N).upperBound map (_.ceil.toInt))

      def begin(start: Int, end: Int) = if (start < end) start + 1 else start

      val imagesOpt = for (start <- startOpt; end <- endOpt)
        yield for (j <- begin(start, end) to end) yield stream(j)

      imagesOpt map (_.reduce(_ union _))
    }

  /**
    * for a monotonic function, gives bound on [(k-1)/N, k/N] given bound at the point k/N.
    */
  def monotoneBound(
      endBound: Int => Interval[Rational]): Int => Interval[Rational] =
    (k) => if (k == 0) endBound(k) else endBound(k - 1) + endBound(k)

  /**
    * returns bound on an interval
    *
    * @param stream bound for function on [k/N, (k+1)/N] is stream(n)
    * @param xs interval whose image is bounded.
    * @param inv : inversion, i.e., f(-x) = inv(f(x))
    *
    */
  def span(
      stream: Int => Interval[Rational],
      inv: Interval[Rational] => Interval[Rational])(xs: Interval[Rational]) = {
    val split = xs.splitAtZero
    for (a <- spanPositive(stream)(split._2);
         b <- spanPositive(stream)(-split._1)) yield a union inv(b)
  }

  /**
    * stream of bounds on the exponential.
    * At n, this is an interval containing e^(n/ N)
    */
  lazy val expStream: LazyList[Interval[Rational]] =
    Nat map
      ((n: SafeLong) =>
        if (n == 0) Interval.point(Rational(1))
        else {
          val b = get(expStream, n - 1)
          val a =
            b * Interval.closed(r"1/2", (1 + width) / (2 - (width * width)))
          (a * width * width) + (b * (width + 1))
        })

  /**
    * bound on exponential on the interval [(k-1)/N, k/N]
    */
  val expBound = monotoneBound(expStream)

  /**
    * exponential approximated on positive and negative sides.
    */
  /* Was only used for preliminary testing
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
   */

  /**
    * bound on log(1 + k/N) at index k.
    */
  def logStream: LazyList[Interval[Rational]] =
    Nat map
      ((n: SafeLong) =>
        if (n == 0) Interval.point(r"0")
        else {
          val prev = get(logStream, n - 1)
          prev +
            (Interval.closed(r"1" / (r"1" + (width * n)),
                             r"1" / (r"1" + (width * (n - 1)))) * width)
        })

  /**
    * bound on log(1 + x) for x in [(k-1)/N, k/N] at index k.
    */
  val logBound = monotoneBound(logStream)

  def logOptBounds(xs: Interval[Rational]) =
    if (xs.hasAtOrBelow(0)) None // log(-x) not defined
    else {
      val spl = xs.split(1)
      val aboveOneOpt =
        spanPositive(logBound)(spl._2 - 1) // bound log(1+x), x>1
      val belowOneOpt =
        spanPositive(logBound)((Interval.point(r"1") / spl._1) - 1) map
          ((I) => -I) //for x in (0, 1), use log(x) = -log(1/x) = -log(1 + (1/x-1))

      for (aboveOne <- aboveOneOpt; belowOne <- belowOneOpt)
        yield (aboveOne + belowOne) // union bound
    }

  /**
    * Bounds for solutions to f" + f = 0 (i.e., sin and cos)
    *
    * @param c  the bound on f at the left end-point
    * @param b the bound on f' at the left end-point
    * @param width width of the interval
    */
  case class TrigBound(b: Interval[Rational], c: Interval[Rational]) {

    lazy val lftBound = -c / r"2" // bound based on f" + f at left endpoint.

    lazy val rghtBound =
      -(c + b * width) / (2 + width * width) // bound based on f" + f at right endpoint.

    lazy val endsBound =
      lftBound union rghtBound // bound based on f" + f at both endpoints.

    lazy val derImage =
      b union (b + (endsBound * width * 2)) // bound on image of derivative

    lazy val derSignChange =
      derImage.crossesZero // check if derivative can a priori cross 0

    //    implicit val appr = new ApproximationContext(width)

    import java.math.MathContext

    implicit val mc = MathContext.DECIMAL128

    import spire.math.Numeric._

    lazy val b2c2 =
      (b.pow(2) * 2 + c.pow(2)).sqrt + Interval.closed(-width, width)

    lazy val discriminantNonNegative =
      ((-c - b2c2) union (-c + b2c2)) / (Rational(4)) // interval outside which discriminant is non-negative

    lazy val a =
      if (derSignChange && (endsBound intersects discriminantNonNegative))
        endsBound union discriminantNonNegative
      else endsBound

    lazy val atRightEnd = (a * width * width) + (b * width) + c

    lazy val intervalImage = (a * J.pow(2)) + (b * J) + c
  }

  /**
    * on the interval ((n - 1)/ N, n/N) if n>0 , (0, 0) otherwise,
    * pair giving bounds for sin on image at right end-point and on the whole interval
    */
  lazy val sinStream: LazyList[(Interval[Rational], Interval[Rational])] =
    Nat map
      ((n: SafeLong) =>
        if (n == 0)
          (Interval.point(Rational(0)), Interval.point(Rational(0)))
        else {
          val c        = get(sinStream, n - 1)._1
          val b        = get(cosStream, n - 1)._1
          val trigAppr = TrigBound(b, c)
          (trigAppr.atRightEnd, trigAppr.intervalImage)
        })

  /**
    * on the interval ((n - 1)/ N, n/N) if n>0 , (0, 0) otherwise,
    * pair giving bounds for cos on image at right end-point and on the whole interval
    */
  lazy val cosStream: LazyList[(Interval[Rational], Interval[Rational])] =
    Nat map
      ((n: SafeLong) =>
        if (n == 0)
          (Interval.point(Rational(1)), Interval.point(Rational(1)))
        else {
          val c        = get(cosStream, n - 1)._1
          val b        = -get(sinStream, n - 1)._1
          val trigAppr = TrigBound(b, c)
          (trigAppr.atRightEnd, trigAppr.intervalImage)
        })

  import ApproxTrig._

  val exp: Approx = span(expBound, (I) => Interval.point(r"1") / I)

  val log: Approx = logOptBounds

  val sin: Approx = (span((j) => sinStream(j)._2, (I) => -I))

  val cos: Approx = (span((j: Int) => cosStream(j)._2, (I) => I))

  import spire.math.Numeric._

  val sqrt: Approx = (J) =>
    Some(E + J mapBounds ((x) => spire.math.sqrt(max(x, r"0"))))
}

object ApproxTrig {

  /**
    * rational bounds for functions,
    * specifically, given a rational interval optionally return a rational interval containing its image.
    */
  type Approx = Interval[Rational] => Option[Interval[Rational]]

  import spire.math.interval.{Bound, ValueBound}

  import compact_enumeration.FieldOps._

  /**
    * stream of natural numbers
    */
  val Nat: LazyList[SafeLong] = 0 #:: (Nat map ((n) => n + 1))

  /**
    * lookup in a stream with a SafeLong index.
    */
  @tailrec def get[A](xs: LazyList[A], n: SafeLong): A = {
    if (n == 0) xs.head else get(xs.tail, n - 1)
  }


  /**
    * Get value of a bound, if finite.
    */
  def getBound[A](J: Bound[A]): Option[A] = J match {
    case ValueBound(a) => Some(a)
    case _             => None
  }

  /**
    * (optionally) split an interval.
    */
  def split[F: Field: Order](J: Interval[F]) = {
    for (lower <- getBound(J.lowerBound); upper <- getBound(J.upperBound))
      yield
        Set(Interval.closed(lower, (lower + upper) / 2),
            Interval.closed((lower + upper) / 2, upper))
  }

  /**
    * A constant bound on the image of a function, on a domain,
    * independent of an interval;
    * used for functions of several variables, where an interval as input makes no sense.
    */
  def ConstantBound(r: Interval[Rational]): Approx =
    ((I: Interval[Rational]) => Some(r))

  /**
    * A cube
    * @param coords Intervals for coordinates.
    */
  case class Cube(coords: Vector[Interval[Rational]]) {

    /**
      * (optionally) spit a cube.
      * returns a set of cubes if each coordinate is a bounded interval.
      */
    def splitCube: Option[Set[Cube]] = {
      if (coords == Vector())
        Some(Set(this)) //0-dimensional cube split to itself.
      else {
        val initCubesOpt =
          Cube(coords.init).splitCube // recursively optionally split dropping the last coordinate
        val finalCoordsOpt =
          split(coords.last) // optionally split the last coordinate, which is an interval
        for (initCubes <- initCubesOpt; finalCoords <- finalCoordsOpt) // splits, if Some, of the initial and final coordinates
          yield
            for (cubelet <- initCubes; interval <- finalCoords) //sub-cubes, subintervals in split
              yield
                Cube(cubelet.coords :+ interval) // products of sub-cubes with subintervals
      }
    }

    /**
      * recursively split a cube k times.
      */
    def recSplit(k: Int): Option[Set[Cube]] =
      if (k < 1) Some(Set(this)) // no splitting
      else {
        val prevSplit = recSplit(k - 1)
        prevSplit flatMap
          ((cs) => // sub-cubes in the spltting of depth (k-1), if split successful
          {
            val setopts =
              cs map (_.splitCube) // split each sub-cube if possible.
            if (setopts contains None)
              None // if some sub-cube fails to split, no total split.
            else
              Some(setopts.flatten.flatten) // Extract split cubes (from option type) as Set(Set) and flatten
          })
      }

    /**
      * bound using function giving optional bounds on all cubes
      */
    def bound(func: Cube => Option[Interval[Rational]]) = func(this)

    /**
      * Given bounds on all cubes, split to depth k and bound if psooible.
      */
    def recSplitBound(func: Cube => Option[Interval[Rational]], depth: Int) = {
      val boundsOpt =
        recSplit(depth) flatMap
          ((cubelets) => {
            //sub-cubes in depth k split cube, if splitting successful.
            val bds =
              cubelets map (func) // optional bounds for value of function on cubelets
            if (bds contains None)
              None // if no bound for some sub-cube, then no bound
            else
              Some(bds.flatten) // extract from options a collection of bounds
          }) // collection of bounds if splitting succeeds and we get a bound for each cube.
      for (bounds     <- boundsOpt;                              // collection of bounds if any
           unionBound <- Try(bounds.reduce(_ union _)).toOption) // if union succeeds (not empty collection for example)
        yield unionBound // union of bounds
    }

    /**
      * (optional) bound for a formal elementary function on this cube, assuming it is multivariate
      */
    def splitBound(fn: FormalElemFunction, N: SafeLong, depth: Int) =
      recSplitBound(rationalBound(fn, N, _), depth)
  }

  import compact_enumeration.ElementaryFunctions

  import compact_enumeration.FormalElemFunction

  /**
    * Bounds for elementary functions on a cube;
    * bounds for sin, cos etc are a function on an interval;
    * bounds for coordinates only depend on the cube.
    * Combining the two should be done with care (as is done in rationalBound)
    */
  class RationalBounds(N: SafeLong, cube: Cube)
      extends ApproxTrig(N)
      with ElementaryFunctions[Approx] {
    val proj = (i: Int) => ConstantBound(cube.coords(i))
  }

  /**
    * Composition of rational bounds.
    */
  implicit val composeApprox = new Circ[Approx] {
    def circ(x: Approx, y: Approx) =
      (I: Interval[Rational]) => y(I) flatMap ((J) => x(J))
  }

  import FormalElemFunction.multiVar

  /**
    * rational bound for a formal elementary function on a cube;
    * function should be a multivariate function of coordinates, not involving direct univariates such as sin;
    * this is checked before returing a result.
    */
  def rationalBound(fn: FormalElemFunction, N: SafeLong, cube: Cube) = {
    implicit val local: ElementaryFunctions[Approx] = new RationalBounds(
      N,
      cube) // bounds on the cube for coordinates and on interval for sin etc
    val bnd =
      fn.as[Approx] // rational bound function from the formal function.
    assert(
      multiVar(fn),
      s" cannot bound $fn : bounds are only for functions of coordinates not direct univaraites such as sin")
    bnd(Interval.point(0)) // bound is supposed to be independent of the chosen interval; as verified above.
  }
}
