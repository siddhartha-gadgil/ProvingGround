package compact_enumeration
import CustomData._
import Stub._

/**
 * @author gadgil
 */
class CubeEnumerator(func: RealMultiFunc,  bounds: Cube => Traversable[Typ]) {
  
    
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
       fb @ FaceBound(`func`,  `domain`, bound: Real, face , sign: Sign)
       <- bounds(domain)) yield fb
  
   def mvtProve(domain: Cube) = {
     val cases = facebounds(domain) map ((faceBound) =>
       {
         val pdbs = partialDerBound(domain, faceBound.face, -1)
         MVTfaceBound.verify(func,  domain, faceBound , pdbs)
       })
       cases.flatten
   }
   

   
   def prove(domain: Cube, depth: Int) : Option[Term] = {
     mvtProve(domain).headOption orElse { 
      val pfs = (splitCube(domain) map (prove(_, depth-1))).flatten
      GlueCubeFuncPositive.verify(func, domain, pfs)
    }
   }
   
  
}