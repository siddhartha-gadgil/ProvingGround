package provingground
import HoTT._

import scala.collection.mutable.{Map => mMap}

class TermBucket {
  import TermBucket.{fd, fdMap}
  
  val terms: mMap[Typ[Term], List[Term]] = mMap()
  
  val termTypes : mMap[Typ[Term], Long] =mMap()
  
  val types : mMap[Typ[Term], Long] = mMap()
  
  def append(t: Term) = {
    val typ = t.typ
    
    terms(typ) = t :: (terms.getOrElse(typ, List())) 
    
    termTypes(typ) = termTypes.getOrElse(typ, 0 : Long) + 1
    
    t match {
      case tp : Typ[u] => 
        types(tp) = types.getOrElse(tp, 0 : Long) + 1
      case _ => ()
    }
  }
  
  def termDistMap = fdMap(terms)

  def termTypDist = fd(termTypes)
  
  def typDist = fd(types)
  
}

object TermBucket{
  def fdMap[A](m: mMap[A, List[Term]])  = {
    val tot = m.values.flatten.size
    (m mapValues (
        (l) => 
          FiniteDistribution((l map (Weighted(_, 1.0/tot))).toVector)
          )).toMap  
  }
  
  def fd(m : mMap[Typ[Term], Long]) = {
    val tot = m.values.sum
    val pmf = for ((x, l) <- m) yield Weighted(x, l * 1.0 / tot)
    FiniteDistribution(pmf.toVector)
  }
  
  
}

class WeightedTermBucket {
  import WeightedTermBucket.{fd, fdMap}
  
  val terms: mMap[Typ[Term], List[Weighted[Term]]] = mMap()
  
  val termTypes : mMap[Typ[Term], Double] =mMap()
  
  val types : mMap[Typ[Term], Double] = mMap()
  
  def append(t: Weighted[Term]) = {
    val typ = t.elem.typ
    
    terms(typ) = t :: (terms.getOrElse(typ, List())) 
    
    termTypes(typ) = termTypes.getOrElse(typ, 0.0) + t.weight
    
    t.elem match {
      case tp : Typ[u] => 
        types(tp) = types.getOrElse(tp, 0.0) + t.weight
      case _ => ()
    }
  }
  
  def termDistMap = fdMap(terms)

  def termTypDist = fd(termTypes)
  
  def typDist = fd(types)
  
}

object WeightedTermBucket{
  def fd(m : mMap[Typ[Term], Double]) = {
    val tot = m.values.sum
    val pmf = for ((x, l) <- m) yield Weighted(x, l / tot)
    FiniteDistribution(pmf.toVector)
  }
  
  def fdMap[A](m: mMap[A, List[Weighted[Term]]])  = {
    val tot = (m.values.flatten map (_.weight)).sum
    (m mapValues (
        (l) => 
          FiniteDistribution((l map ((wt) => Weighted(wt.elem, wt.weight/tot))).toVector)
          )).toMap  
  }
}