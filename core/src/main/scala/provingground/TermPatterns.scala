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

  val piTyp = Pattern.partial[Term, Id] {
    case PiTyp(fibre) => fibre
  }

  val piLam = Pattern.partial[Term, II] {
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

  val star = Pattern.filter[Term](_ == Star)

  val unit = Pattern.filter[Term](_ == Unit)

  val zero = Pattern.filter[Term](_ == Unit)

  val universe = Pattern.partial[Term, N] {
    case Universe(n) => n
  }

  val symbolic = Pattern.partial[Term, Named] {
    case sym: Symbolic with Term =>
      outerSym(sym).name match {
        case Name(name) => (name, sym.typ)
      }
  }

  def termToExprRaw[E: ExprLang] = {
    import ExprLang._
    (formalAppln >> appln[E]) || (lambdaAppln >> lambda[E]) ||
    (prodTyp >> pairTyp[E]) || (funcTyp >> func[E]) || (piLam >> pi[E]) ||
    (sigmaLam >> sigma[E]) || (equation >> equality[E]) ||
    (symbolic >> variable[E]) || (plusTyp >> or[E]) || (funcTyp >> func[E]) ||
    (prodTyp >> pairTyp[E]) || (absPair >> pair[E]) || (unit >> { (e: E) =>
          tt[E]
        }) || (zero >> { (e: E) =>
          ff[E]
        }) || (star >> { (e: E) =>
          qed[E]
        }) || (firstIncl >> i1[E]) || (secondIncl >> i2[E])
  }

  def termToExpr[E: ExprLang](univ: Int => Option[E]) = {
    (universe >> univ) || termToExprRaw[E]
  }

  // val blah: Term => Boolean = {case x : PlusTyp[u, v]#RecFn[w] => true}
}
