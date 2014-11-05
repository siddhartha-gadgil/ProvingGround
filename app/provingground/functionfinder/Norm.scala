package provingground.functionfinder

import provingground.HoTT._
import RecEnum._
import IntTypes._
import Math._

object Norm {
	def pairmaxopt(a: Option[Double], b: Option[Double]) = for (x <-a ; y <- b) yield max(x, y)
  
	def maxopt(l: List[Option[Double]]) = ((Some(0.0) : Option[Double]) /: l)(pairmaxopt)
  
	def supnorm(term : Term) : Option[Double] = (term, term.typ) match{
	  case (_, tp : IntTyp) => 
	    term match {
	      case tp.rep(value) => Some(value.toDouble.abs)
	      case _ => None
	    }
	  case p: AbsPair[Term, Term] =>
	    for (a <- supnorm(p.first); b <- supnorm(p.second)) yield max(a, b)
	  case (fn: FuncTerm[_, _]) =>{
	    val domopt = recEnumList(fn.dom)
	    None
	  }
	    
	  case _ => None
	}
}