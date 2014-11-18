package provingground
import provingground.HoTT._
import scala.util._

object Equality {
	lazy val eqls: (Term, Term) => Boolean = {
	  case (f: FuncTerm[u, _], g : FuncTerm[v, _]) => {
	    val newvar = f.dom.obj
	    Try(eqls(f(newvar.asInstanceOf[u]), g(newvar.asInstanceOf[v]))).getOrElse(false) 
	  }
	  case (x: AbsPair[Term, Term], y: AbsPair[Term, Term]) => eqls(x.first, x.second) && eqls(y.first, y.second)
	  case (PiTyp(f), PiTyp(g)) => eqls(f, g) 
	  case (SigmaTyp(f), SigmaTyp(g)) => eqls(f, g)
	  case (FuncTyp(a, b), FuncTyp(c, d)) => eqls(a, c) && eqls(b, d)
	  case (IdentityTyp(_,a, b), IdentityTyp(_,c, d)) => eqls(a.asInstanceOf[Term], 
	      c.asInstanceOf[Term]) && eqls(b.asInstanceOf[Term], d.asInstanceOf[Term])
	  case (x, y) => x == y
	} 
}