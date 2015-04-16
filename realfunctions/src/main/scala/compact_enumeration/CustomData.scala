package compact_enumeration

import scala.util._
/**
 * @author gadgil
 * Custom data and goals for compact enumeration.
 * To blend with HoTT one instead imports a different object with case classes replaced by objects with apply and unapply.
 */
object CustomData {
  /**
   * local version of Typ
   */
  trait Typ
  
  /**
   * local version of Term
   */
  trait Term{
    val typ : Typ
  }
  
  def andTyp(first: Typ, second: Typ) = (first, second) match{
    case (FuncPositive(f, domleft) , FuncPositive(g, domright)) if f == g => 
      FuncPositive(f, Interval(domleft.lower, domright.upper))
    case _ => PairTyp(first, second)     
  }
  
  case class PairTyp(first: Typ, second: Typ) extends Typ
  
  case class Union(first: Term, second: Term) extends Term{
    lazy val typ = andTyp(first.typ, second.typ)
  }
  
  def and = Union.apply _
  
  type RealFunc = Real => Real
  
  type Real = Double
  
  type Sign = Int
  
  case class Interval(lower: Real, upper: Real) extends Typ{
    lazy val midpoint : Real = (lower + upper) / 2
    
    lazy val width : Real = upper - lower
    
    def half(s : Sign) = if (s == 1) Interval(midpoint, upper) else Interval(lower, midpoint)
    
    lazy val firsthalf = Interval(lower, midpoint)
    
    lazy val secondhalf = Interval(midpoint, upper)
    
    def bdy(s: Sign) = if (s == 1) upper else lower
    
    assert(width >= 0)
  }
  
  type IntervalTyp = Interval
  
  case class FuncPositive(func : RealFunc, domain: Interval) extends Typ
  
  case class MVTMidPositive(func: RealFunc, domain: Interval, derivativeLower: Real, derivativeUpper: Real) extends Term{
    val mid = domain.midpoint
    val left = mid - ((domain.width / 2) * derivativeUpper)
    val right = mid + ((domain.width / 2) * derivativeLower)
    assert(mid >= 0)
    assert(left >= 0)
    assert(right >= 0)
    
    val typ = FuncPositive(func, domain)
  }
  
  object MVTMidPositive{
    def verify(func: RealFunc, domain: Interval, derivativeLower: Real, derivativeUpper: Real): Option[Term]= {
      Try(MVTMidPositive(func: RealFunc, domain: Interval, derivativeLower: Real, derivativeUpper: Real)).toOption
    }
  }
  
  case class DerBound(func : RealFunc, domain: Interval, derBound: Real, sign: Sign) extends Typ
  
  case class MVTPositive(func: RealFunc, domain: Interval, derBound: Real, sign: Sign) extends Term{
    assert(func(domain.bdy(sign)) >= 0)
    assert(func(domain.bdy(sign) - (sign * domain.width * derBound)) >= 0)
    lazy val typ = FuncPositive(func, domain)
  } 
  
  object MVTPositive{
    def verify(func: RealFunc, domain: Interval, derivativeBound: Real, sign: Sign): Option[Term]= {
      Try(MVTPositive(func, domain, derivativeBound, sign)).toOption
    }
  }
  
  type Index = Int
    
  type Vec = Vector[Real]
    
  type Cube = Index => Interval
  
  def corners(dim: Index) : Set[Map[Index, Sign]] = {
    if (dim == 1) Set(Map(1 -> 1), Map (1 -> -1))
    else for (m <- corners(dim -1); sgn <- Set(-1, 1)) yield (m + (dim -> sgn))
  }
  

  

    
  def extrapolateBound(cube: Cube, face : Map[Index, Sign], faceValue: Real, slopes: Map[Index, Real], corner : Map[Index, Sign]) = {
    assert(face.keySet == slopes.keySet)
    val diffs = for (i <- face.keySet) yield(
        if (face(i) == corner(i)) 0 
        else cube(i).width * slopes(i) * corner(i)                  
          )
    (diffs :\ faceValue) (_ + _)
    
  }
  
  type RealMultiFunc = Vec => Real
    
  case class PartialDerBound(func: RealMultiFunc, index: Index, domain: Cube, bound: Real, sign: Sign) extends Typ
    
  case class FuncPositiveCube(func: RealMultiFunc, dim: Index, domain: Cube) extends Typ
  
  case class FaceBound(func: RealMultiFunc, dim: Index, domain: Cube, bound: Real, face: Map[Index, Sign], sign: Sign) extends Typ
 
  case class BottomCorner(func: RealMultiFunc, dim: Index, domain: Cube) extends Term{
    val face = (for (i <- 0 to dim) yield (i, -1)).toMap
    val vec = (0 to dim).toVector map (domain(_).lower)
    val typ = FaceBound(func, dim, domain, func(vec), face, -1)
  }
  
  case class MVTfaceBound(func: RealMultiFunc, dim: Index, domain: Cube, faceBound: Real, 
      face: Map[Index, Sign], faceBoundSign: Sign,
      partialDerivativeBounds: Traversable[PartialDerBound]      
      ) extends Term{
    val derBounds = (for (i <- face.keys; pdb <- partialDerivativeBounds 
          if pdb.index == i && pdb.sign == faceBoundSign * face(i)) yield (i, pdb.bound)).toMap
    
    for (corner <- corners(dim)) yield assert (0.0 <= extrapolateBound(domain, face, faceBound, derBounds, corner))
    
    val typ = FuncPositiveCube(func, dim, domain)
  }
  
  
  object MVTfaceBound{
    def verify(func: RealMultiFunc, dim: Index, domain: Cube, faceBound: Real, 
      face: Map[Index, Sign], sign: Sign,
      partialDerivativeBounds: Traversable[PartialDerBound]      
      ) : Option[Term] = 
        Try(MVTfaceBound(func, dim, domain, faceBound, face, sign, partialDerivativeBounds)).toOption
  }
}