package provingground

import HoTT._
import scala.util._

object HoTTgen {
	val funcappl: (Term, Term) => Option[Term] = {
	  case (f: FuncTerm[u, _], a : Term) =>
	    Try(f(a.asInstanceOf[u])).toOption
	  case _ => None
	}
	
	val functyp : (Term, Term) => Option[Term] = {
	  case (u: Typ[Term], v: Typ[Term]) => Some(FuncTyp(u, v))
	  case _ => None
	}
	
	val pityp : Term => Option[Term] = {
	  case fmly : FuncObj[u, _] => fmly.codom.typ match {
	    case univ: Typ[w] => Try(PiTyp(fmly.asInstanceOf[FuncObj[Term, Typ[Term]]])).toOption
	    case _ => None
	  }
	  case _ => None
	}
}