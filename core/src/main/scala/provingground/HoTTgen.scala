package provingground

import HoTT._
import FiniteDistributionLearner._
import DiffbleFunction._
import scala.util._
import Collections._
import FiniteDistribution._
import LinearStructure._
import scala.language.existentials

object HoTTgen {

  def isFunc(t: Term) = t.isInstanceOf[FuncLike[_, _]]

  def isTyp(t: Term) = t.isInstanceOf[Typ[_]]

  def inDomain: Term => Term => Boolean = {
    case (func: FuncLike[u, v]) =>
      {
        (arg: Term)  => arg.typ == func.dom
      }
    case _ =>
      (_) => false
  }

	val funcappl: (Term, Term) => Option[Term] = {
	  case (f: FuncLike[u, _], a : Term) =>
	    Try(f(a.asInstanceOf[u])).toOption
	  case _ => None
	}

	val functyp : (Term, Term) => Option[Term] = {
	  case (u: Typ[Term], v: Typ[Term]) if (u.typ == __) => Some(FuncTyp(u, v))
	  case _ => None
	}

	val pityp : Term => Option[Term] = {
	  case fmly : Func[u, _] => fmly.codom.typ match {
	    case _ : Typ[w] => Try {
        val x = fmly.dom.Var
        val y = fmly(x).asInstanceOf[Typ[w with Subs[w]]]
        val fibre = lmbda(x)(y)
        PiTyp[u, w](fibre)
      }.toOption
      case _ => None
	  }
	  case _ => None
	}

	val sigmatyp : Term => Option[Term] = {
	  case fmly : Func[u, _] => fmly.codom.typ match {
      case _ : Typ[w] => Try {
        val x = fmly.dom.Var
        val y = fmly(x).asInstanceOf[Typ[w with Subs[w]]]
        val fibre = lmbda(x)(y)
        SigmaTyp[u, w](fibre)
      }.toOption
	    case _ => None
	  }
	  case _ => None
	}

	val pairtyp : (Term, Term) => Option[Term] = {
	  case (a : Typ[u], b: Typ[v]) if (a.typ == __) && (b.typ == __) => Some(PairTyp(a, b))
	  case _ => None
	}

	val pairobj : (Term, Term) => Option[Term] = {
    case (_ : Universe, _) => None
    case (_, _: Universe) => None
    case (a, b) => Some(pair(a, b))
  }

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

  /*
	object Move extends Enumeration{
	  lazy val lambda, appl, arrow, pi, sigma, pairt, pair, paircons, icons, jcons, id  = Value
	}
*/

	lazy val moves : List[(Move, DiffbleFunction[FiniteDistribution[Term], FiniteDistribution[Term]])] =
    List((Move.appl, CombinationFn(funcappl, isFunc)),
	    (Move.arrow, CombinationFn(functyp, isTyp)),
	    (Move.pi, MoveFn(pityp)),
	    (Move.sigma, MoveFn(sigmatyp)),
	    (Move.pairt, CombinationFn(pairtyp, isTyp)),
	    (Move.pair, CombinationFn(pairobj)),
	    (Move.paircons, MoveFn(paircons)),
	    (Move.icons, MoveFn(icons)),
	    (Move.jcons, MoveFn(icons)),
      (Move.id, Id[FiniteDistribution[Term]])
	    )

  object Move{
    case object lambda extends Move

    case object appl extends Move

    case object arrow extends Move

    case object pi extends Move

    case object sigma extends Move

    case object  pair extends Move

    case object pairt extends Move

    case object paircons extends Move

    case object icons extends Move

    case object jcons extends Move

    case object id extends Move
  }

  sealed trait Move

	val wtdDyn = weightedDyn[Move, FiniteDistribution[Term]]

	val wtdMoveList = for (mv <- moves) yield extendM(wtdDyn(mv._1, mv._2))

	val wtdMoveSum = vBigSum(wtdMoveList) andthen block(NormalizeFD[Move], NormalizeFD[Term])

	def lambdaFn[M](l: M,
	    f: DiffbleFunction[(FiniteDistribution[M], FiniteDistribution[Term]), FiniteDistribution[Term]]
	    )(typ: Typ[Term]) = {
	  import DiffbleFunction._
	  val x = typ.Var
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
				val terms = fd._2.supp
				val gettyps : PartialFunction[Term, Typ[Term]] =  {case typ : Typ[_] => typ}
				val typs = terms collect gettyps
				typs map ((typ) => lambdaFn(l, f)(typ))
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

	val hottDyn = DiffbleFunction.mixinIsle[(FiniteDistribution[Move], FiniteDistribution[Term])](wtdMoveSum, lambdaSumM(Move.lambda), block(NormalizeFD[Move], NormalizeFD[Term]))

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
      mapTyp.func(d) rawfeedback (getTyps(d).getsum(_))
    mapTyp.grad(d)(shift)
  }

  def dynTypFlow(dyn : DiffbleFunction[FiniteDistribution[Term], FiniteDistribution[Term]]) = {
    typFlow ^: dyn
  }
}
