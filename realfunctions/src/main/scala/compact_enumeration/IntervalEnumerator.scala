package compact_enumeration

import Stub._
import CustomData._ // instead import the corresponding HoTT object
import Verify._ 

/**
 * @author gadgil
 * 
 * Seeks proof that function is positive on a given domain.
 * 
 * @param func function to be proved positive.
 * @param bounds returns proofs concerning given domain.
 * 
 */
class IntervalEnumerator(func: RealFunc,
    bounds: Interval => Set[Typ]) {
  
  /**
   * proofs of derivative bound for domain
   */
  def derBound(domain: Interval) = for (db @ DerBound(`func`, `domain`, b, sign) <- bounds(domain)) yield db

  /**
   * proofs of derivative lower bound for domain
   */
  def derLower(domain: Interval) = for (db @ DerBound(`func`, `domain`, b, -1) <- bounds(domain)) yield db 
  
  /**
   * proofs of derivative upper bound for domain
   */
  def derUpper(domain: Interval) = for (db @ DerBound(`func`, `domain`, b, 1) <- bounds(domain)) yield db 
  
  /**
   * proofs using midpoint and MVT of positivity.
   */
  def midProve(domain: Interval) = for (low <-derLower(domain); 
    high <-derUpper(domain);
    proof <- optMVTMidPositive(func, domain, low , high)) yield proof
  
  /**
   * proofs using end-point. derivative bound and MVT of positivity.
   */
  def mvtProve(domain: Interval) = for (db <-derBound(domain); 
      proof <- optMVTPositive(func, domain, db)) yield proof
    

  /**
   * attempt to prove positivity recursively.
   */
  def prove(domain: Interval, depth: Int) : Option[Term] = {
    if (depth <1) None
    else mvtProve(domain).headOption orElse midProve(domain).headOption  orElse 
    { 
      val pfs = (split(domain) map (prove(_, depth-1))).flatten // proofs for sub-intervals, found with lower depth.
      optGlueFuncPositive(func, domain, pfs)
    }
  }
  
}