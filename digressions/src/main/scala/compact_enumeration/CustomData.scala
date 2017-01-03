package compact_enumeration

//import scala.util._
import Stub._

/**
  * @author gadgil
  * Custom data and goals for compact enumeration.
  * To blend with HoTT one instead imports a different object with case classes replaced by objects with apply and unapply.
  */
object CustomData {

  /**
    * combining two statements.
    */
  private def andTyp(first: Typ, second: Typ) = (first, second) match {
    case (FuncPositive(f, domleft), FuncPositive(g, domright)) if f == g =>
      FuncPositive(f, Interval(domleft.lower, domright.upper))
    case _ => ProdTyp(first, second)
  }

  /**
    * pair type
    */
  case class ProdTyp(first: Typ, second: Typ) extends Typ

  /**
    * Local version of pair object.
    */
  case class PairTerm(first: Term, second: Term) extends Term {
    lazy val typ = ProdTyp(first.typ, second.typ)
  }

  private def and = PairTerm.apply _

  /**
    * functions R -> R
    */
  type RealFunc = Real => Real

  /**
    * Real numbers
    */
  type Real = Double

  /**
    * Sign, should be -1 or 1.
    */
  type Sign = Int

  /**
    * the possible signs
    */
  val signs: Set[Sign] = Set(-1, 1)

  /**
    * an interval
    * @param lower lower endpoint
    * @param upper upper endpoint
    */
  case class Interval(lower: Real, upper: Real) extends ConstantTyp {
    lazy val midpoint: Real = (lower + upper) / 2

    lazy val width: Real = upper - lower

    /**
      * the two half-intervals
      */
    def half(s: Sign) =
      if (s == 1) Interval(midpoint, upper) else Interval(lower, midpoint)

    /**
      * boundary point
      * @param s specifies which endpoint
      */
    def bdy(s: Sign) = if (s == 1) upper else lower

    assert(width >= 0)
  }

  /**
    * splits an interval into its pieces
    * public method required for backward reasoning, so needs HoTT analogue.
    */
  def split(I: Interval) = signs map (I.half(_))

  /**
    * Function with domain.
    */
  trait Func {
    def func: RealFunc

    def domain: Interval
  }

  val func = (fn: Func) => fn.func

  def domain(fn: Func) = fn.domain

//  case class FuncPositive(func : RealFunc, domain: Interval) extends ConstantTyp with Func

  /**
    * Proposition that given function is positive on domain
    *
    * @param func the given function
    * @param domain the domain for positivity
    */
  object FuncPositive {
    def apply(func: RealFunc, domain: Interval): ConstantTyp with Func =
      FuncBound(func, domain, 0: Real, -1: Sign)

    def unapply(typ: Typ): Option[(RealFunc, Interval)] = typ match {
      case fb @ FuncBound(func, domain, 0, -1) => Some((func, domain))
      case _                                   => None
    }
  }

  /**
    * Proposition that given function is bounded on the domain
    */
  case class FuncBound(func: RealFunc,
                       domain: Interval,
                       bound: Real,
                       sign: Sign)
      extends ConstantTyp
      with Func
      with Bound

  /**
    * proof of positivity of a function on an interval, given positivity in halves.
    *
    * @param func given function
    * @param domain (full) interval
    *
    * @param pfs given proofs, only those giving positivity on half intervals are considered.
    */
  case class GlueFuncPositive(func: RealFunc, domain: Interval, pfs: Set[Term])
      extends ConstantTerm
      with Func {
    split(domain) map ((j) => // j is a half interval
                       {
                         val thispfs =
                           pfs filter (_.typ == FuncPositive(func, j)) // proofs that func is positive on j.
                         assert(!(thispfs.isEmpty))                    // checks that there is such a proof
                       })

    val typ = FuncPositive(func, domain)
  }

  case class RestrictedFuncBound(domain: Interval, fb: FuncBound)
      extends ConstantTerm
      with Func {
    assert(fb.domain.lower <= domain.lower && domain.upper <= fb.domain.upper)

    lazy val func = fb.func

    lazy val sign = fb.sign

    lazy val typ = FuncBound(func, domain, fb.bound, sign)
  }

  /**
    * (upper or lower) bound
    */
  trait Bound {
    def bound: Real

    def sign: Sign
  }

  def bound(b: Bound) = b.bound

  def sign(b: Bound) = b.sign

  /**
    * bound on the derivative in an interval
    *
    * @param func function whose derivative is bounded.
    * @param domain interval on which there is a bound
    * @param sign -1 for lower bound, 1 for upper bound.
    */
  case class DerBound(func: RealFunc,
                      domain: Interval,
                      bound: Real,
                      sign: Sign)
      extends ConstantTyp
      with Func
      with Bound

  case class IsDerivative(derivative: RealFunc, integral: RealFunc)
      extends ConstantTyp

  case class InferredDerivativeBound(der: IsDerivative, funcBound: FuncBound)
      extends ConstantTerm {
    assert(der.derivative == funcBound.func)
    val typ =
      DerBound(der.integral, funcBound.domain, funcBound.bound, funcBound.sign)
  }

  /**
    * proof using Mean Value theorem and value at midpoint of positivity of function.
    */
  case class MVTMidPositive(func: RealFunc,
                            domain: Interval,
                            derLower: DerBound,
                            derUpper: DerBound)
      extends ConstantTerm
      with Func {
    assert(
      derLower.func == func && derLower.domain == domain &&
        derLower.sign == -1)
    assert(
      derUpper.func == func && derUpper.domain == domain &&
        derUpper.sign == 1)

    val mid = func(domain.midpoint) // value of function at midpoint
    val left =
      mid -
        ((domain.width / 2) * derUpper.bound) // lower bound for value at left endpoint
    val right =
      mid +
        ((domain.width / 2) * derLower.bound) // lower bound for value at right endpoint.
    assert(mid >= 0)
    assert(left >= 0)
    assert(right >= 0)

    val typ = FuncPositive(func, domain)
  }

  /**
    * proof using Mean Value theorem and value at appropriate endpoint of positivity of function.
    */
  case class MVTPositive(func: RealFunc, domain: Interval, derBound: DerBound)
      extends ConstantTerm
      with Func {
    assert(func(domain.bdy(derBound.sign)) >= 0) // check positivity at endpoint for sign
    assert(
      func(domain.bdy(derBound.sign) -
        (derBound.sign * domain.width * derBound.bound)) >= 0) // positivity at other endpoint.
    lazy val typ = FuncPositive(func, domain)
  }

  // Data for cubes

  /**
    * Index set for co-ordinates.
    */
  type Index = Int

  /**
    * Vector in R^n
    */
  type Vec = Vector[Real]

  /**
    * Cube.
    */
  type Cube = Vector[Interval]

  /**
    * split a cube into sub-cubes, recursively by dimension.
    */
  def splitCube(cube: Cube): Set[Cube] = {
    if (cube == Vector()) Set(cube)
    else {
      for (init <- splitCube(cube.init); last <- split(cube.last))
        yield (init :+ last)
    }
  }

  /**
    * Multivariate real functions
    */
  trait MultiFunc {
    def func: RealMultiFunc

    def domain: Cube
  }

  def func(fn: MultiFunc) = fn.func

  def domain(fn: MultiFunc) = fn.domain

  /**
    * proposition that function is positive on a cube.
    */
//  case class FuncPositiveCube(func: RealMultiFunc, domain: Cube) extends ConstantTyp with MultiFunc

  /**
    * Proposition that given function is positive on domain
    *
    * @param func the given function
    * @param domain the domain for positivity
    */
  object FuncPositiveCube {
    def apply(func: RealMultiFunc, domain: Cube): ConstantTyp with MultiFunc =
      MultiFuncBound(func, domain, 0: Real, -1: Sign)

    def unapply(typ: Typ): Option[(RealMultiFunc, Cube)] = typ match {
      case fb @ MultiFuncBound(func, domain, 0, -1) => Some((func, domain))
      case _                                        => None
    }
  }

  /**
    * Proposition that given function is bounded on the domain
    */
  case class MultiFuncBound(func: RealMultiFunc,
                            domain: Cube,
                            bound: Real,
                            sign: Sign)
      extends ConstantTyp
      with MultiFunc
      with Bound

  case class RestrictedMultiFuncBound(domain: Cube, fb: MultiFuncBound)
      extends ConstantTerm
      with MultiFunc {
    for (i <- 0 to domain.size - 1)
      yield
        assert(
          fb.domain(i).lower <= domain(i).lower &&
            domain(i).upper <= fb.domain(i).upper)

    lazy val func = fb.func

    lazy val sign = fb.sign

    lazy val typ = MultiFuncBound(func, domain, fb.bound, sign)
  }

  /**
    * proof of positivity by gluing proofs
    *
    * @param pfs the given proofs
    */
  case class GlueCubeFuncPositive(func: RealMultiFunc,
                                  domain: Cube,
                                  pfs: Set[Term])
      extends ConstantTerm
      with MultiFunc {
    splitCube(domain) map ((j) => // the cubelet
                           {
                             val thispfs =
                               pfs filter (_.typ == FuncPositiveCube(func, j)) // proofs that the function is positive on the cubelet
                             assert(!(thispfs.isEmpty))                        // check there is at least one proof.
                           })

    val typ = FuncPositiveCube(func, domain)
  }

  /**
    * Corners of a cube, i.e., sets of corresponding signs.
    * Given as a map so these can be viewed as 0-dimensional faces.
    */
  private def corners(dim: Index): Set[Map[Index, Sign]] = {
    if (dim == 1) Set(Map(1 -> 1), Map(1 -> -1))
    else
      for (m <- corners(dim - 1); sgn <- Set(-1, 1)) yield (m + (dim -> sgn))
  }

  /**
    * Given lower bound for value on a face, bounds for certain derivatives gives a lower bound at a corner.
    *
    * @param cube the domain
    * @param face signs giving the face, the size of the map is the codimension.
    * @param faceBound lower bound on the function on a face.
    * @param slopes bounds on partial derivative, lower/upper depending on coordinate for the face.
    * @param corner the corner to which extrapolation is done, depends only on coordinates specified for the face.
    *
    * Should be private, but public for tests/debugging.
    */
  def extrapolateLowerBound(cube: Cube,
                            face: Map[Index, Sign],
                            faceBound: Real,
                            slopes: Map[Index, Real],
                            corner: Map[Index, Sign]) = {
    assert(face.keySet == slopes.keySet)
    val diffs = for (i <- face.keySet)
      yield
        (if (face(i) == corner(i)) 0
         else
           cube(i).width * slopes(i) * corner(i)) // difference in the bound for the ith coordinate (which must be a face coordinate.
    (diffs :\ faceBound)(_ + _)
  }

  /**
    * function of several real variables.
    */
  type RealMultiFunc = Vec => Real

  /**
    * proposition for bound on partial derivative of function.
    */
  case class PartialDerBound(func: RealMultiFunc,
                             index: Index,
                             domain: Cube,
                             bound: Real,
                             sign: Sign)
      extends ConstantTyp

  case class IsPartialDerivative(derivative: RealMultiFunc,
                                 integral: RealMultiFunc,
                                 index: Index)
      extends ConstantTyp

  case class InferredPartialDerivativeBound(der: IsPartialDerivative,
                                            funcBound: MultiFuncBound)
      extends ConstantTerm {
    assert(der.derivative == funcBound.func)
    val typ = PartialDerBound(der.integral,
                              der.index,
                              funcBound.domain,
                              funcBound.bound,
                              funcBound.sign)
  }

  /**
    * proposition for bound on value of function on face.
    */
  case class FaceBound(func: RealMultiFunc,
                       domain: Cube,
                       bound: Real,
                       face: Map[Index, Sign],
                       sign: Sign)
      extends ConstantTyp
      with Bound

  /**
    * Computes and certifies bound on bottom corner (as a face)
    */
  case class BottomCorner(func: RealMultiFunc, domain: Cube)
      extends ConstantTerm
      with MultiFunc {
    val face = (for (i <- 0 to domain.size) yield (i, -1)).toMap
    val vec  = (0 to (domain.size - 1)).toVector map (domain(_).lower)
    val typ  = FaceBound(func, domain, func(vec), face, -1)
  }

  /**
    * show function positive using bound on face, partial derivative bounds using MVT
    */
  case class MVTfaceBoundFuncPositive(
      func: RealMultiFunc,
      domain: Cube,
      faceBound: FaceBound,
      partialDerivativeBounds: Traversable[PartialDerBound])
      extends ConstantTerm
      with MultiFunc {
    import faceBound.face
    val derBounds = (for (i <- face.keys;
                          PartialDerBound(`func`, `i`, `domain`, bound, sgn) <- partialDerivativeBounds
                          if sgn == sign(faceBound) * face(i))
      yield (i, bound)).toMap

    for (corner <- corners(domain.size))
      yield
        assert(
          0.0 <= extrapolateLowerBound(domain,
                                       face,
                                       bound(faceBound),
                                       derBounds,
                                       corner))

    val typ = FuncPositiveCube(func, domain)
  }

  /**
    *
    * bound for function on hyper-plane with specified coordinates.
    */
  case class HyperplaneBound(func: RealMultiFunc,
                             coords: Map[Index, Real],
                             bound: Real,
                             sign: Sign)
      extends ConstantTyp
      with Bound

  /**
    * bound on face deduced from bound on hyperplane.
    */
  case class DeducedFaceBoundProof(func: RealMultiFunc,
                                   domain: Cube,
                                   face: Map[Index, Sign],
                                   hypPlnBound: HyperplaneBound)
      extends ConstantTerm
      with MultiFunc {
    for ((i, y) <- hypPlnBound.coords)
      yield assert(domain(i).bdy(face(i)) == y)
    val typ =
      FaceBound(func, domain, bound(hypPlnBound), face, sign(hypPlnBound))
  }
}
