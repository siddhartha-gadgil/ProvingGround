package compact_enumeration

import CustomData._ // instead import the corresponding HoTT object
 
/**
 * @author gadgil
 */
class IntervalEnumerator(func: RealFunc,
    bounds: Interval => Option[Typ]) {
  
  def derivativeBound(domain: Interval) = for (DerBound(g, j, b, sign) <- bounds(domain) if g == func && j == domain) yield (b, sign)
  
  def derivativeLower(domain: Interval) = for (DerBound(g, j, b, -1) <- bounds(domain) if g == func && j == domain) yield b 
  
  def derivativeUpper(domain: Interval) = for (DerBound(g, j, b, 1) <- bounds(domain) if g == func && j == domain) yield b 
  
  def midProve(domain: Interval) = for (low <-derivativeLower(domain); 
    high <-derivativeUpper(domain);
    proof <- MVTMidPositive.verify(func, domain, low , high)) yield proof
      
  def mvtProve(domain: Interval) = for ((bnd, sign) <-derivativeBound(domain); 
      proof <- MVTPositive.verify(func, domain, bnd, sign)) yield proof
    

  
  def prove(domain: Interval, depth: Int) : Option[Term] = {
    if (depth <1) None
    else mvtProve(domain) orElse midProve(domain)  orElse 
    {
      for (fst <- prove(domain.firsthalf, depth -1); 
      scnd <- prove(domain.secondhalf, depth -1)) yield and(fst, scnd)
    }
  }
  
}