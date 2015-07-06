package provingground

import HoTT._
import FiniteDistributionLearner._
import DiffbleFunction._
import scala.util._
import Collections._

object HoTTgen {
	val funcappl: (Term, Term) => Option[Term] = {
	  case (f: FuncLike[u, _], a : Term) =>
	    Try(f(a.asInstanceOf[u])).toOption
	  case _ => None
	}
	
	val functyp : (Term, Term) => Option[Term] = {
	  case (u: Typ[Term], v: Typ[Term]) => Some(FuncTyp(u, v))
	  case _ => None
	}
	
	val pityp : Term => Option[Term] = {
	  case fmly : Func[u, _] => fmly.codom.typ match {
	    case _ : Typ[w] => Try(PiTyp(fmly.asInstanceOf[Func[u, Typ[w]]])).toOption
	    case _ => None
	  }
	  case _ => None
	}
	
	val sigmatyp : Term => Option[Term] = {
	  case fmly : Func[w, _] => fmly.codom.typ match {
	    case _ : Typ[u] => Try(
	        SigmaTyp(fmly.asInstanceOf[Func[Term, Typ[Term]]])).toOption
	    case _ => None
	  }
	  case _ => None
	}
	
	val pairtyp : (Term, Term) => Option[Term] = {
	  case (a : Typ[_], b: Typ[_]) => Some(PairTyp[Term, Term](a, b))
	  case _ => None
	}
	
	val pairobj : (Term, Term) => Option[Term] = (a, b) => Some(pair(a, b))
	
	val paircons : Term => Option[Term] = {
	  case p : PairTyp[_, _] => Some(p.paircons)
	  case p : SigmaTyp[_, _] => Some(p.paircons)
	  case _ => None
	}
	
	val icons : Term => Option[Term] = {
	  case p : PlusTyp => Some(p.ifn)
	  case _ => None
	}
	
	val jcons : Term => Option[Term] = {
	  case p : PlusTyp => Some(p.jfn)
	  case _ => None
	}
	
	object Move extends Enumeration{
	  lazy val lambda, appl, arrow, pi, sigma, pairt, pair, paircons, icons, jcons  = Move
	}
	
	
	lazy val moves = List((Move.appl, CombinationFn(funcappl)),
	    (Move.arrow, CombinationFn(functyp)),
	    (Move.pi, MoveFn(pityp)),
	    (Move.sigma, MoveFn(sigmatyp)),
	    (Move.pairt, CombinationFn(pairtyp)),
	    (Move.pair, CombinationFn(pairobj)),
	    (Move.paircons, MoveFn(paircons)),
	    (Move.icons, MoveFn(icons)),
	    (Move.jcons, MoveFn(icons))
	    )
	
	val wtdDyn = weightedDyn[Move.type, FiniteDistribution[Term]]
	 
	val wtdMoveList = for (mv <- moves) yield extendM(wtdDyn(mv._1, mv._2))
	
	val wtdMoveSum = vBigSum(wtdMoveList) andthen block(NormalizeFD[Move.type], NormalizeFD[Term])
	
	def lambdaFn[M](l: M, 
	    f: DiffbleFunction[(FiniteDistribution[M], FiniteDistribution[Term]), FiniteDistribution[Term]]
	    )(terms: Set[Term])(typ: Typ[Term]) = {
	  import DiffbleFunction._
	  val x = nextVar(terms)(typ)
	  val incl = (Evaluate(l) oplus id[FiniteDistribution[Term]])
	  val init = NewVertex(x)	  	  
	  val export = MoveFn((t: Term) => 
	    if (t != __) Some(lambda(x)(t) : Term) else None)
	  val head = incl andthen init
	  head andthen export
	}
	
	def lambdaSum[M](l: M)( 
	    f: DiffbleFunction[(FiniteDistribution[M], FiniteDistribution[Term]), FiniteDistribution[Term]]
	    ) = {
			val lambdas = (fd: (FiniteDistribution[M], FiniteDistribution[Term])) => {
				val terms = fd._2.support
				val gettyps : PartialFunction[Term, Typ[Term]] =  {case typ : Typ[_] => typ}
				val typs = terms collect gettyps
				typs map ((typ) => lambdaFn(l, f)(terms)(typ))
				}
			DiffbleFunction.BigSum(lambdas)
			}
	
	def lambdaSumM[M](l : M)( 
	    g: DiffbleFunction[(FiniteDistribution[M], FiniteDistribution[Term]), (FiniteDistribution[M], FiniteDistribution[Term])]
	    ) = {
	  val p = DiffbleFunction.Proj2[FiniteDistribution[M], FiniteDistribution[Term]]
	  val f = g andthen p
	  val withIsle = lambdaSum(l)(f)
	  extendM(withIsle)
	}
	
	val hottDyn = DiffbleFunction.mixinIsle(wtdMoveSum, lambdaSumM(Move.lambda), block(NormalizeFD[Move.type], NormalizeFD[Term]))
	
  val mapTyp = MoveFn[Term, Typ[Term]]((t: Term) =>
    if (t.typ.typ == __) Some(t.typ) else None 
        )
  
  private def ifTyp : Term => Option[Typ[Term]] = {
    case typ: Typ[Term] if typ.typ == __ => Some(typ)
    case _ => None
  }      
        
  def getTyps(d : FiniteDistribution[Term]) = d.mapOpt(ifTyp)
  
  val typFlow = (d: FiniteDistribution[Term]) =>  {
    val shift = 
      mapTyp.func(d) feedback (getTyps(d).getsum(_))
    mapTyp.grad(d)(shift)
  }
  
  def dynTypFlow(dyn : DiffbleFunction[FiniteDistribution[Term], FiniteDistribution[Term]]) = {
    typFlow ^: dyn
  } 
}