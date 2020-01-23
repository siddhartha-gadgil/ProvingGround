package compact_enumeration
import CustomData._
import Stub._
import scala.util._

/**
  * @author gadgil
  */
object Verify {

  /**
    * Attempts to prove positivity on domain by gluing
    * @param func given function
    * @param domain (full) interval
    *
    * @param pfs given proofs, only those giving positivity on half intervals are considered.
    */
  def optGlueFuncPositive(func: RealFunc,
                          domain: Interval,
                          pfs: Set[Term]): Option[Term] = {
    Try(GlueFuncPositive(func, domain, pfs)).toOption
  }

  /**
    * Attempts to prove positivity of function based by using Mean Value theorem and the value at the midpoint.
    */
  def optMVTMidPositive(func: RealFunc,
                        domain: Interval,
                        derLower: DerBound,
                        derUpper: DerBound): Option[Term] = {
    Try(MVTMidPositive(func: RealFunc, domain: Interval, derLower, derUpper)).toOption
  }

  /**
    * Attempts to prove positive of function using Mean Value theorem and value at appropriate endpoint
    */
  def optMVTPositive(func: RealFunc,
                     domain: Interval,
                     derivativeBound: DerBound): Option[Term] = {
    Try(MVTPositive(func, domain, derivativeBound)).toOption
  }

  /**
    * Attempts to prove positivity on domain by gluing
    * @param func given function
    * @param domain (full) cube
    *
    * @param pfs given proofs, only those giving positivity on cubelets are considered.
    */
  def optGlueCubeFuncPositive(func: RealMultiFunc,
                              domain: Cube,
                              pfs: Set[Term]): Option[Term] = {
    Try(GlueCubeFuncPositive(func, domain, pfs)).toOption
  }

  def optMVTfaceBoundFuncPositive(
      func: RealMultiFunc,
      domain: Cube,
      faceBoundPf: FaceBound,
      partialDerivativeBounds: Iterable[PartialDerBound]): Option[Term] =
    Try(
      MVTfaceBoundFuncPositive(func,
                               domain,
                               faceBoundPf,
                               partialDerivativeBounds)).toOption

  def optDeducedFaceBoundProof(func: RealMultiFunc,
                               domain: Cube,
                               face: Map[Index, Sign],
                               hypPlnBound: HyperplaneBound) =
    Try(DeducedFaceBoundProof(func, domain, face, hypPlnBound)).toOption

  def optRestricedFuncBound(domain: Interval, fb: FuncBound) =
    Try(RestrictedFuncBound(domain, fb)).toOption

  def optRestrictedMultiFuncBound(domain: Cube, fb: MultiFuncBound) =
    Try(RestrictedMultiFuncBound(domain, fb)).toOption

  def optInferredDerivativeBound(der: IsDerivative, funcBound: FuncBound) =
    Try(InferredDerivativeBound(der: IsDerivative, funcBound: FuncBound)).toOption

  def optInferredPartialDerivativeBound(der: IsPartialDerivative,
                                        funcBound: MultiFuncBound) =
    Try(
      InferredPartialDerivativeBound(der: IsPartialDerivative,
                                     funcBound: MultiFuncBound)).toOption
}
