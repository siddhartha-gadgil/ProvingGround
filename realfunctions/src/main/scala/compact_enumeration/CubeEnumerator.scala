package compact_enumeration
import CustomData._

/**
 * @author gadgil
 */
class CubeEnumerator(func: RealMultiFunc, dim: Index, bounds: Cube => Traversable[Typ]) {
  
    def splitCube(cube: Cube, d: Index = dim) : Set[Cube] = {
    if (d == 0) Set(cube)
    else 
      for (cubelet <- splitCube(cube, d -1); sgn <- Set(-1, 1)) 
        yield ((i: Index) => if (i == d - 1) cube(i).half(sgn) else cubelet(i))     
    
  }
    
    def faces(dim: Index) : Set[Map[Index, Sign]] = {
      if (dim == 0) Set(Map())
      else 
        (for (m <- faces(dim -1); sgn <- Set(-1, 1)) yield (m + (dim -> sgn))) union faces(dim)             
  }
    
   def partialDerBound(domain: Cube, face : Map[Index, Sign], sign: Sign) = {
     for (i <- face.keys; pdb @ PartialDerBound(g, index, d,  _, s) <- bounds(domain)
     if g == func && d == domain && index ==i && sign * face(i) == s) yield pdb
  }
  
   def facebounds(domain: Cube) = for (
       fb @ FaceBound(`func`, `dim`, `domain`, bound: Real, face , sign: Sign)
       <- bounds(domain)) yield fb
  
   def mvtProve(domain: Cube) = {
     val cases = facebounds(domain) map ((faceBound) =>
       {
         val pdbs = partialDerBound(domain, faceBound.face, -1)
         MVTfaceBound.verify(func, dim, domain, faceBound.bound, faceBound.face, faceBound.sign, pdbs)
       })
       cases.flatten
   }
   
   def optAnd(first: Option[Term], second: Option[Term]) = {
     for (x <- first; y <- second) yield and(x, y)
   }
   
   def prove(domain: Cube) : Option[Term] = {
     mvtProve(domain).headOption orElse (
       (splitCube(domain) map ((cubelet) => prove(cubelet))) reduce optAnd
     )
   }
   
   def bigOptAnd(s: Seq[Option[Term]]) = s reduce optAnd
}