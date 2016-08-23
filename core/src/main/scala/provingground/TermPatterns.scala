package provingground

import Functor._

import HoTT._

import Translator._

object TermPatterns{
  val formalAppln = Pattern[Term, II](FormalAppln.unapply)

  val lambaAppln = Pattern.partial[Term, II]{
    case l : LambdaLike[u, v] => (l.variable, l.value)
  }

  val piTyp = Pattern.partial[Term, Id]{
    case PiTyp(fibre) => fibre
  }

  val sigmaTyp = Pattern.partial[Term, Id]{
    case SigmaTyp(fibre) => fibre
  }

  val plusTyp = Pattern.partial[Term, II]{
    case PlusTyp(first: Typ[u], second: Typ[v]) => (first, second)
  }

  val absPair = Pattern.partial[Term, II]{
    case p : AbsPair[u, v] => (p.first, p.second)
  }

  val funcTyp = Pattern.partial[Term, II]{
    case FuncTyp(dom: Typ[u], codom: Typ[v]) => (dom, codom)
  }

  val identityTyp = Pattern.partial[Term, III]{
    case IdentityTyp(dom: Typ[u], lhs: Term, rhs: Term) => (dom, lhs, rhs)
  }

  val firstIncl = Pattern.partial[Term, II]{
    case fi: PlusTyp.FirstIncl[u, v] => (fi.typ, fi.value)
  }

  val secondIncl = Pattern.partial[Term, II]{
    case fi: PlusTyp.ScndIncl[u, v] => (fi.typ, fi.value)
  }

  val star = Pattern.filter[Term](_ == Star)

  val unit = Pattern.filter[Term](_ == Unit)

  val zero = Pattern.filter[Term](_ == Unit)

  val universe = Pattern.filter[Term]{
    case Universe(n) => true
    case _ => false
  }
}
