package compact_enumeration
import CustomData._
import Stub._
import Verify._

/**
  * @author gadgil
  */
class CubeEnumerator(func: RealMultiFunc, givenBounds: Cube => Set[Typ]) {

  def restrictedBounds(domain: Cube) =
    for (fb @ MultiFuncBound(_, _, _, _) <- givenBounds(domain);
         rfb                             <- optRestrictedMultiFuncBound(domain, fb)) yield rfb

  def bounds(dom: Cube) =
    givenBounds(dom) union (restrictedBounds(dom) map (_.typ))

  def faces(dim: Index): Set[Map[Index, Sign]] = {
    if (dim == 0) Set(Map())
    else
      (for (m <- faces(dim - 1); sgn <- Set(-1, 1))
        yield (m + (dim -> sgn))) union faces(dim)
  }

  def givenPartialDerBound(domain: Cube, face: Map[Index, Sign], sign: Sign) = {
    for (i                                                  <- face.keySet;
         pdb @ PartialDerBound(`func`, `i`, `domain`, _, s) <- bounds(domain)
         if sign * face(i) == s) yield pdb
  }

  def inferredPartialDerBounds(domain: Cube,
                               face: Map[Index, Sign],
                               sign: Sign) =
    for (i                                                  <- face.keySet;
         isPartialDer @ IsPartialDerivative(_, `func`, `i`) <- bounds(domain);
         fb @ MultiFuncBound(_, `domain`, _, s)             <- bounds(domain);
         inferred                                           <- optInferredPartialDerivativeBound(isPartialDer, fb)
         if sign * face(i) == s) yield inferred

  def partialDerBound(domain: Cube, face: Map[Index, Sign], sign: Sign) =
    givenPartialDerBound(domain, face, sign) union
      (inferredPartialDerBounds(domain, face, sign) map (_.typ))

  def facebounds(domain: Cube) =
    for (fb @ FaceBound(`func`, `domain`, bound: Real, _, _) <- bounds(domain))
      yield fb

  def deducedFaceBounds(domain: Cube) =
    for (face                                           <- faces(domain.size);
         hypPlnBound @ HyperplaneBound(`func`, _, _, _) <- bounds(domain);
         pf <- optDeducedFaceBoundProof(func,
                                        domain,
                                        face,
                                        hypPlnBound: HyperplaneBound)) yield pf

  def mvtProve(domain: Cube) = {
    val cases =
      (facebounds(domain) ++ (deducedFaceBounds(domain) map (_.typ))) map
        ((faceBound) => {
          val pdbs = partialDerBound(domain, faceBound.face, -1)
          optMVTfaceBoundFuncPositive(func, domain, faceBound, pdbs)
        })
    cases.flatten
  }

  def prove(domain: Cube, depth: Int): Option[Term] = {
    mvtProve(domain).headOption orElse {
      val pfs = (splitCube(domain) map (prove(_, depth - 1))).flatten
      optGlueCubeFuncPositive(func, domain, pfs)
    }
  }
}
