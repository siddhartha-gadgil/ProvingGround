package provingground

import provingground.{TruncatedDistribution => TD}

class TruncatedDistributionLang[E: ExprLang] extends ExprLang[TruncatedDistribution[E]] {
    val l = implicitly[ExprLang[E]]
  
    def variable[S](name: S, typ: TD[E]): Option[TD[E]] = ???

  /**
   * anonymous variable
   */
  def anonVar(typ: TD[E]): Option[TD[E]] = ???

  /**
   * meta-variable of a given type, i.e., whose value must be inferred 
   * (elaborated in lean's terminology). 
   */
  def metaVar(typ: TD[E]): Option[TD[E]] = ???
  
  def lambda(variable: TD[E], value: TD[E]) : Option[TD[E]] = 
    TD.optF(TD.mapOp(variable, value)(l.lambda))

  def pi(variable: TD[E], typ: TD[E]): Option[TD[E]] = ???

  def appln(func: TD[E], arg: TD[E]): Option[TD[E]] = ???

  def equality(lhs: TD[E], rhs: TD[E]) : Option[TD[E]] = ???
  
  def sigma(variable: TD[E], typFamily: TD[E]) : Option[TD[E]] = ???  
  
  def pair (x: TD[E], y: TD[E]): Option[TD[E]] = ???

  def proj1(xy: TD[E]): Option[TD[E]] = 
    TD.optF(TD.map(xy)(l.proj1))

  def proj2(xy: TD[E]): Option[TD[E]] = ???

  def or(first: TD[E], second: TD[E]):  Option[TD[E]] = ???

  def incl1(typ : TD[E]) : Option[TD[E]] = ???

  def incl2(typ: TD[E]) :  Option[TD[E]] = ???

  /**
   * true type
   */
  def tt : Option[TD[E]] = 
     l.tt map ((e: E) => TD.atom(e))      
    
//    TD.optF(l.tt map ((e: E) => TD.Atom[E](e)))

  /**
   * element of true type
   */
  def qed : Option[TD[E]] = ???

  /**
   * false type
   */
  def ff : Option[TD[E]] = ???

  def numeral(n: Int): Option[TD[E]] = ???
  
  def isPair: TD[E] => Option[(TD[E], TD[E])] = ???
  
  def domain: TD[E] => Option[TD[E]] = ???
}