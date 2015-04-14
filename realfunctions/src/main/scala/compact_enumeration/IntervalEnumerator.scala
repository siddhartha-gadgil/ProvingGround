package compact_enumeration

import CustomData._ // instead import the corresponding HoTT object
 
/**
 * @author gadgil
 */
class IntervalEnumerator(func: RealFunc,
    derivativeLower: Interval => Option[Real], 
    derivativeUpper: Interval => Option[Real]) {
  
  def midProve(domain: Interval) = for (low <-derivativeLower(domain); 
    high <-derivativeUpper(domain);
    proof <- MVTMidPositive.verify(func, domain, low , high)) yield proof
      
  
  def leftProve(domain: Interval) = for (low <-derivativeLower(domain); 
      proof <- MVTLeftPositive.verify(func, domain, low)) yield proof
  
  def rightProve(domain: Interval) = for (high <-derivativeUpper(domain); 
      proof <- MVTRightPositive.verify(func, domain, high)) yield proof
  
  def prove(domain: Interval, depth: Int) : Option[Term] = {
    if (depth <1) None
    else midProve(domain) orElse leftProve(domain) orElse rightProve(domain) orElse {
      for (fst <- prove(domain.firsthalf, depth -1); 
      scnd <- prove(domain.secondhalf, depth -1)) yield and(fst, scnd)
    }
  }
  
}