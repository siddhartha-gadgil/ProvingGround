package compact_enumeration

import CustomData._ // instead import the corresponding HoTT object
 
/**
 * @author gadgil
 */
class IntervalEnumerator(func: RealFunc,
    bounds: Interval => Set[Typ]) {
  
  def derBound(domain: Interval) = for (db @ DerBound(g, j, b, sign) <- bounds(domain) if g == func && j == domain) yield db
  
  def derLower(domain: Interval) = for (db @ DerBound(g, j, b, -1) <- bounds(domain) if g == func && j == domain) yield db 
  
  def derUpper(domain: Interval) = for (db @ DerBound(g, j, b, 1) <- bounds(domain) if g == func && j == domain) yield db 
  
  def midProve(domain: Interval) = for (low <-derLower(domain); 
    high <-derUpper(domain);
    proof <- MVTMidPositive.verify(func, domain, low , high)) yield proof
      
  def mvtProve(domain: Interval) = for (db <-derBound(domain); 
      proof <- MVTPositive.verify(func, domain, db)) yield proof
    

  
  def prove(domain: Interval, depth: Int) : Option[Term] = {
    if (depth <1) None
    else mvtProve(domain).headOption orElse midProve(domain).headOption  orElse 
    { 
      val pfs = (split(domain) map (prove(_, depth-1))).flatten
      Glue.verify(func, domain, pfs)
    }
  }
  
}