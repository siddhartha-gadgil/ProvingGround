package provingground

import Functors._

import cats.implicits._

import cats._

import HoTT._

import Translator._

object TermPatterns {
  val formalAppln = Pattern[Term, II](FormalAppln.unapply)

  val lambdaAppln = Pattern.partial[Term, II] {
    case l: LambdaLike[u, v] => (l.variable, l.value)
  }

  val lambdaTriple = Pattern.partial[Term, III] {
    case l: LambdaLike[u, v] => ((l.variable, l.variable.typ), l.value)
  }

  val piTriple = Pattern.partial[Term, III] {
    case PiDefn(x: Term, y: Typ[u]) => ((x, x.typ), y)
    case PiTyp(fibre: Func[u, _]) =>
      val x: Term = fibre.dom.Var.asInstanceOf[Term]
      ((x, x.typ), fibre(x))
  }

  val sigmaTriple = Pattern.partial[Term, III] {
    case SigmaTyp(fibre: Func[u, _]) =>
      fibre match {
        case l: LambdaLike[u, v] => ((l.variable, l.variable.typ), l.value)
        case _ =>
          val x: Term = fibre.dom.Var.asInstanceOf[Term]
          ((x, x.typ.asInstanceOf[Typ[Term]]), fibre(x))
      }
  }

  val piTyp = Pattern.partial[Term, Id] {
    case PiDefn(x: Term, y: Typ[v]) => HoTT.lmbda(x)(y)
    case PiTyp(fibre)               => fibre
  }

  val piLam = Pattern.partial[Term, II] {
    case PiDefn(x: Term, y: Typ[u]) => (x, y)
    case PiTyp(fibre: Func[u, _]) =>
      val x: Term = fibre.dom.Var.asInstanceOf[Term]
      (x, fibre(x))
  }

  val sigmaTyp = Pattern.partial[Term, cats.Id] {
    case SigmaTyp(fibre) => fibre
  }

  val sigmaLam = Pattern.partial[Term, II] {
    case SigmaTyp(fibre: Func[u, _]) =>
      val x: Term = fibre.dom.Var.asInstanceOf[Term]
      (x, fibre(x))
  }

  val plusTyp = Pattern.partial[Term, II] {
    case PlusTyp(first: Typ[u], second: Typ[v]) => (first, second)
  }

  val absPair = Pattern.partial[Term, II] {
    case p: AbsPair[u, v] => (p.first, p.second)
  }

  val prodTyp = Pattern.partial[Term, II] {
    case ProdTyp(first: Typ[u], second: Typ[v]) => (first, second)
  }

  val funcTyp = Pattern.partial[Term, II] {
    case FuncTyp(dom: Typ[u], codom: Typ[v]) => (dom, codom)
  }

  val recFunc = Pattern.partial[Term, IIV] {
    case rf : RecFunc[u, v] => (rf.dom, (rf.codom, rf.defnData))
  }

  val indRecFunc = Pattern.partial[Term, VIIV] {
    case rf : IndRecFunc[u, v, w] => (rf.index, (rf.dom, (rf.codom, rf.defnData)))
  }

  val inducFunc = Pattern.partial[Term, IIV] {
    case rf : InducFuncLike[u, v] =>
      val fmly : Term = rf.depcodom match {
        case t: Term => t
        case _ =>
          val x = rf.dom.Var
          x :-> rf.depcodom(x)
      }
      (rf.dom, (fmly, rf.defnData))
  }

  val indInducFunc = Pattern.partial[Term, VIIV] {
    case rf : IndInducFuncLike[u, v, w, z] =>
      val fmly : Term = rf.depcodom match {
        case t: Term => t
        case _ =>
          val x = rf.dom.Var
          x :-> rf.depcodom(x)
      }
      (rf.index, (rf.dom, (fmly, rf.defnData)))
  }

  val identityTyp = Pattern.partial[Term, III] {
    case IdentityTyp(dom: Typ[u], lhs: Term, rhs: Term) => ((dom, lhs), rhs)
  }

  val equation = Pattern.partial[Term, II] {
    case IdentityTyp(dom: Typ[u], lhs: Term, rhs: Term) => (lhs, rhs)
  }

  val firstIncl = Pattern.partial[Term, II] {
    case fi: PlusTyp.FirstIncl[u, v] => (fi.typ, fi.value)
  }

  val secondIncl = Pattern.partial[Term, II] {
    case fi: PlusTyp.ScndIncl[u, v] => (fi.typ, fi.value)
  }

  val star = Pattern.check[Term](_ == Star)

  val unit = Pattern.check[Term](_ == Unit)

  val zero = Pattern.check[Term](_ == Unit)

  val universe = Pattern.partial[Term, N] {
    case Universe(n) => n
  }

  val symbolic = Pattern.partial[Term, Named] {
    case sym: Symbolic with Term =>
      outerSym(sym).name match {
        case Name(name) => (name, sym.typ)
      }
  }

  val symName = Pattern[Term, S] {
    case sym: Symbolic with Term =>
      outerSym(sym).name match {
        case Name(name) => Some(name)
        case _          => None
      }
    case _ => None
  }

  def termToExprRaw[E: ExprLang] = {
    import ExprLang._
    (formalAppln >> appln[E]) || (lambdaAppln >> lambda[E]) ||
    (prodTyp >> pairTyp[E]) || (funcTyp >> func[E]) || (piLam >> pi[E]) ||
    (sigmaLam >> sigma[E]) || (equation >> equality[E]) ||
    (symbolic >> variable[E]) || (plusTyp >> or[E]) || (funcTyp >> func[E]) ||
    (prodTyp >> pairTyp[E]) || (absPair >> pair[E]) ||
    (unit >> { (_) =>
      tt[E]
    }) ||
    (zero >> { (_) =>
      ff[E]
    }) ||
    (star >> { (_) =>
      qed[E]
    }) || (firstIncl >> i1[E]) || (secondIncl >> i2[E])
  }

  def termToExpr[E: ExprLang](univ: Int => Option[E]) = {
    (universe >> univ) || termToExprRaw[E]
  }

  import TermLang.applyAll

  def buildRecDef(inds: Typ[Term] => Option[ConstructorSeqTL[_, _, _]] = (_) => None) : (Term, (Term, Vector[Term])) => Option[Term] =
     {
      case (pt : ProdTyp[u, v], (codom: Typ[w] , data) ) =>
        applyAll(Some(pt.rec(codom)), data)
      case (Zero, (codom: Typ[w] , data) ) =>
        applyAll(Some(Zero.rec(codom)), data)
      case (Unit, (codom: Typ[w] , data) ) =>
        applyAll(Some(Unit.rec(codom)), data)
      case (pt : PlusTyp[u, v], (codom: Typ[w] , data) ) =>
        applyAll(Some(pt.rec(codom)), data)
      case (pt : SigmaTyp[u, v], (codom: Typ[w] , data) ) =>
        applyAll(Some(pt.rec(codom)), data)
      case (dom: Typ[u], (codom : Typ[v], data)) =>
        inds(dom) flatMap (
          (cs) => applyAll(Some(cs.recE(codom)), data)
        )
      case _ => None
    }

    def buildIndRecDef(inds: Term => Option[IndexedConstructorSeqDom[_, Term, _,  _, _]] = (_) => None): (Vector[Term], (Term, (Term, Vector[Term]))) => Option[Term] =
      {
        case (Vector(start, finish), (idt: IdentityTyp[u], (codom: Typ[v], Vector(fn : Func[x, y])))) =>
          val rf = idt.rec(codom)
          applyAll(Some(rf), Vector(fn, start, finish))
        case (index, (dom, (codom : Typ[v], data))) =>
          inds(dom) flatMap (
            (cs) => applyAll(Some(cs.recE(codom)), data ++ index)
          )
      }

  def fm[U <: Term with Subs[U]](dom: Typ[U], fmly: Term) = {
    val x = dom.Var
    val tp = fold(fmly)(x)
    tp match {
      case t : Typ[_] => x :-> t
    }
  }


  def buildIndDef(inds: Typ[Term] => Option[ConstructorSeqTL[_, Term, _]] = (_) => None) : (Term, (Term, Vector[Term])) => Option[Term]=
     {
      case (pt : ProdTyp[u, v], (depcodom , data) ) =>
        val x = pt.first.Var
        val y = pt.second.Var
        val tp = fold(fold(depcodom)(x))(y).asInstanceOf[Typ[Term]]
        val fmly = x :-> (y :-> tp)
        applyAll(Some(pt.induc(fmly)), data)
      case (Zero, (depcodom , data) ) =>
        applyAll(Some(Zero.induc(fm(Zero, depcodom))), data)
      case (Unit, (depcodom , data) ) =>
        applyAll(Some(Unit.induc(fm(Unit, depcodom))), data)
      case (pt : PlusTyp[u, v], (depcodom , data) ) =>
        applyAll(Some(pt.induc(fm(pt, depcodom))), data)
      case (pt : SigmaTyp[u, v], (depcodom , data) ) =>
        val x = pt.fibers.dom.Var
        val y = pt.fibers(x).Var
        val tp = fold(fold(depcodom)(x))(y).asInstanceOf[Typ[Term]]
        val fmly = x :-> (y :-> tp)
        applyAll(Some(pt.induc(fmly)), data)
      case (dom: Typ[u], (depcodom, data)) =>
        inds(dom) flatMap (
          (cs) => applyAll(Some(cs.inducE(fm(cs.typ, depcodom))), data)
        )
      case _ => None
    }

    def buildIndIndDef(inds: Term => Option[IndexedConstructorSeqDom[_, Term, _,  _, _]] = (_) => None) : (Vector[Term], (Term, (Term, Vector[Term]))) => Option[Term]=
       {
        case (Vector(start, finish), (idt: IdentityTyp[u], (depcodom, Vector(fn : Func[x, y])))) =>
          val rf = idt.induc(depcodom.asInstanceOf[FuncLike[u, FuncLike[u, FuncLike[Term, Typ[Term]]]]])
          applyAll(Some(rf), Vector(fn, start, finish))
        case (index, (dom, (depcodom, data))) =>
          inds(dom) flatMap (
            (cs) => applyAll(Some(cs.inducE(depcodom)), data ++ index)
          )
      }



  // val blah: Term => Boolean = {case x : PlusTyp[u, v]#RecFn[w] => true}
}
