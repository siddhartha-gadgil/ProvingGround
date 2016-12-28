package provingground

import provingground.HoTT._
import RecEnum._
import IntTypes._
import Math._
import scala.reflect.runtime.universe.{
  Try => UnivTry,
  Function => FunctionUniv,
  _
}
import scala.util._

object Norm {
//	def pairmaxopt(a: Option[Double], b: Option[Double]) = for (x <-a ; y <- b) yield max(x, y)

  def optop[U, V](op: (U, U) => U)(a: Option[U], b: Option[U]) = {
    for (x <- a; y <- b) yield op(x, y)
  }

  def foldopt[U](op: (U, U) => U)(l: List[Option[U]]) = {
    ((None: Option[U]) /: l)(optop(op))
  }

//	def maxopt(l: List[Option[Double]]) = ((Some(0.0) : Option[Double]) /: l)(pairmaxopt)

  def maxopt(l: List[Option[Double]]) = foldopt[Double](max)(l)

  def supnorm(term: Term): Option[Double] = (term, term.typ) match {
    case (_, tp: IntTyp) =>
      term match {
        case tp.rep(value) => Some(value.toDouble.abs)
        case _             => None
      }
    // case p: AbsPair[Term, Term] =>
    //   for (a <- supnorm(p.first); b <- supnorm(p.second)) yield max(a, b)
    case (fn: FuncLike[u, _], _) => {
      val domopt =
        Try(fn.dom.asInstanceOf[Typ[Term]]).toOption flatMap (recEnumList(_))
      domopt flatMap ((dom) => {
                        val normoptlist =
                          dom map ((t) =>
                                     Try(fn(t.asInstanceOf[u])).toOption flatMap (supnorm))
//	      val normlist = Try(normoptlist map (_.get)).toOption
                        maxopt(normoptlist)
                      })
    }
    case (PiDefn(x: Term, y: Typ[v]), _) => supnorm(x :-> y)
    case (PiTyp(section), _)             => supnorm(section)
    case (SigmaTyp(fn), _) => {
      val domopt = recEnumList(fn.dom.asInstanceOf[Typ[Term]])
      domopt flatMap ((dom) =>
                        foldopt[Double](min)(dom map ((t) => supnorm(fn(t)))))
    }

    case _ => None
  }

  def supWeightExp(fn: FuncLike[Term, Term])(x: Term) = {
    if (x.typ == fn.dom) supnorm(fn(x)) else None
  }

  def supWeight(fn: FuncLike[Term, Term])(x: Term) =
    supWeightExp(fn)(x) map ((t) => math.pow(2, -t)) getOrElse (0.0)
}
