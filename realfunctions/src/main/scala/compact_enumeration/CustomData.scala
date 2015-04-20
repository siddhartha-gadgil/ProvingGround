package compact_enumeration

import scala.util._
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
  private def andTyp(first: Typ, second: Typ) = (first, second) match{
    case (FuncPositive(f, domleft) , FuncPositive(g, domright)) if f == g => 
      FuncPositive(f, Interval(domleft.lower, domright.upper))
    case _ => PairTyp(first, second)     
  }
  
  /**
   * pair type, ie, and.
   */
  case class PairTyp(first: Typ, second: Typ) extends Typ
  
  /**
   * Local version of pair object.
   */
  case class PairObj(first: Term, second: Term) extends Term{
    lazy val typ = PairTyp(first.typ, second.typ)
  }
  
  private def and = PairObj.apply _
  
  type RealFunc = Real => Real
  
  type Real = Double
  
  type Sign = Int
  
  val signs: Set[Sign] = Set(-1, 1)
  
  case class Interval(lower: Real, upper: Real) extends ConstantTyp{
    lazy val midpoint : Real = (lower + upper) / 2
    
    lazy val width : Real = upper - lower
    
    def half(s : Sign) = if (s == 1) Interval(midpoint, upper) else Interval(lower, midpoint)
    
    lazy val firsthalf = Interval(lower, midpoint)
    
    lazy val secondhalf = Interval(midpoint, upper)
    
    def bdy(s: Sign) = if (s == 1) upper else lower
    
    assert(width >= 0)
  }
  
  
  /**
   * splits an interval into its pieces
   * public method required for backward reasoning, so needs HoTT analogue. 
   */
  def split(I: Interval) = signs map (I.half(_))
  
  trait Func{
    def func: RealFunc
    
    def domain: Interval
  }
  
  def func(fn : Func) = fn.func
  
  def domain(fn: Func) = fn.domain
  
  case class FuncPositive(func : RealFunc, domain: Interval) extends ConstantTyp with Func
  
  case class GlueFuncPositive(func: RealFunc, domain : Interval, pfs: Set[Term]) extends ConstantTerm with Func{
    split(domain) map ((j) =>
      {
        val thispfs = pfs filter (_.typ == FuncPositive(func, j))
        assert (!(thispfs.isEmpty))
      })
    
    val typ = FuncPositive(func, domain)
  }
  
  object GlueFuncPositive{
    def verify(func: RealFunc, domain : Interval, pfs: Set[Term]) : Option[Term] = {
      Try(GlueFuncPositive(func, domain, pfs)).toOption
    }
  }
  
  
  trait Bound{
    def bound: Real
    
    def sign: Sign
  }
  
  def bound(b: Bound) = b.bound
  
  def sign(b: Bound) = b.sign
  
  case class DerBound(func : RealFunc, domain: Interval, bound: Real, sign: Sign) extends ConstantTyp with Func with Bound
  
  case class MVTMidPositive(func: RealFunc, domain: Interval, derLower: DerBound, derUpper: DerBound) extends 
    ConstantTerm with Func{
    assert(derLower.func == func && derLower.domain == domain && derLower.sign == -1)
    assert(derUpper.func == func && derUpper.domain == domain && derUpper.sign == 1)
    
    val derivativeLower = derLower.bound
    val derivativeUpper = derUpper.bound
    
    val mid = domain.midpoint
    val left = mid - ((domain.width / 2) * derivativeUpper)
    val right = mid + ((domain.width / 2) * derivativeLower)
    assert(mid >= 0)
    assert(left >= 0)
    assert(right >= 0)
    
    val typ = FuncPositive(func, domain)
  }
  
  object MVTMidPositive{
    def verify(func: RealFunc, domain: Interval, derLower: DerBound, derUpper: DerBound): Option[Term]= {
      Try(MVTMidPositive(func: RealFunc, domain: Interval, derLower , derUpper)).toOption
    }
  }
  
  
  case class MVTPositive(func: RealFunc, domain: Interval, derBound: DerBound) extends ConstantTerm with Func{
    assert(func(domain.bdy(derBound.sign)) >= 0)
    assert(func(domain.bdy(derBound.sign) - (derBound.sign * domain.width * derBound.bound)) >= 0)
    lazy val typ = FuncPositive(func, domain)
  } 
  
  object MVTPositive{
    def verify(func: RealFunc, domain: Interval, derivativeBound: DerBound): Option[Term]= {
      Try(MVTPositive(func, domain, derivativeBound)).toOption
    }
  }
  
  
  // Data for cubes
  
  
  type Index = Int
    
  type Vec = Vector[Real]
    
  type Cube = Vector[Interval]
  
  def splitCube(cube: Cube) : Set[Cube] = {
    if (cube == Vector()) Set(cube)
    else {
      for (init <- splitCube(cube.init); last <- split(cube.last)) yield (init :+ last)
    
    }
    
  }
  
  
  trait MultiFunc{
    def func: RealMultiFunc
    
    def domain: Cube
  }
  
  def func(fn: MultiFunc) = fn.func
  
  def domain(fn: MultiFunc) = fn.domain
  
  case class FuncPositiveCube(func: RealMultiFunc, domain: Cube) extends ConstantTyp with MultiFunc
  
  case class GlueCubeFuncPositive(func: RealMultiFunc, domain : Cube, pfs: Set[Term]) extends ConstantTerm with MultiFunc{
    splitCube(domain) map ((j) =>
      {
        val thispfs = pfs filter (_.typ == FuncPositiveCube(func, j))
        assert (!(thispfs.isEmpty))
      })
    
    val typ = FuncPositiveCube(func, domain)
  }
  
  object GlueCubeFuncPositive{
    def verify(func: RealMultiFunc, domain : Cube, pfs: Set[Term]) : Option[Term] = {
      Try(GlueCubeFuncPositive(func, domain, pfs)).toOption
    }
  }
  
  def corners(dim: Index) : Set[Map[Index, Sign]] = {
    if (dim == 1) Set(Map(1 -> 1), Map (1 -> -1))
    else for (m <- corners(dim -1); sgn <- Set(-1, 1)) yield (m + (dim -> sgn))
  }
  

  
/**
 * Should be private, but public for tests/debugging. 
 */    
  def extrapolateBound(cube: Cube, face : Map[Index, Sign], faceValue: Real, slopes: Map[Index, Real], corner : Map[Index, Sign]) = {
    assert(face.keySet == slopes.keySet)
    val diffs = for (i <- face.keySet) yield(
        if (face(i) == corner(i)) 0 
        else cube(i).width * slopes(i) * corner(i)                  
          )
    (diffs :\ faceValue) (_ + _)
    
  }
  
  type RealMultiFunc = Vec => Real
    
  case class PartialDerBound(func: RealMultiFunc, index: Index, domain: Cube, bound: Real, sign: Sign) extends ConstantTyp
    

  
  case class FaceBound(func: RealMultiFunc,  domain: Cube, bound: Real, face: Map[Index, Sign], sign: Sign) extends 
    ConstantTyp with Bound
 
  case class BottomCorner(func: RealMultiFunc, domain: Cube) extends ConstantTerm with MultiFunc{
    val face = (for (i <- 0 to domain.size) yield (i, -1)).toMap
    val vec = (0 to domain.size).toVector map (domain(_).lower)
    val typ = FaceBound(func, domain, func(vec), face, -1)
  }
  
  case class MVTfaceBound(func: RealMultiFunc, domain: Cube, faceBound : FaceBound, 
      partialDerivativeBounds: Traversable[PartialDerBound]      
      ) extends ConstantTerm with MultiFunc{
    import faceBound.face
    val derBounds = (for (i <- face.keys; pdb <- partialDerivativeBounds 
          if pdb.index == i && pdb.sign == sign(faceBound) * face(i)) yield (i, pdb.bound)).toMap
    
    for (corner <- corners(domain.size)) yield assert (
        0.0 <= extrapolateBound(domain, face, bound(faceBound), derBounds, corner))
    
    val typ = FuncPositiveCube(func, domain)
  }
  
  
  object MVTfaceBound{
    def verify(func: RealMultiFunc, domain: Cube, faceBoundPf : FaceBound, 
      partialDerivativeBounds: Traversable[PartialDerBound]      
      ) : Option[Term] = 
        Try(MVTfaceBound(func, domain, faceBoundPf, partialDerivativeBounds)).toOption
  }
  
  /**
   *
   * bound for function on hyper-plane with specified coordinates.
   */
  case class HyperplaneBound(func: RealMultiFunc, coords: Map[Index, Real], bound: Real, sign: Sign) extends 
    ConstantTyp with Bound
  
  case class DeducedFaceBound(func: RealMultiFunc,  domain: Cube, face: Map[Index, Sign], hypPlnBound: HyperplaneBound) extends 
    ConstantTerm with MultiFunc{
    for ((i, y) <- hypPlnBound.coords) yield assert(domain(i).bdy(face(i)) == y)
    val typ = FaceBound(func, domain, bound(hypPlnBound), face, sign(hypPlnBound))
  }
}