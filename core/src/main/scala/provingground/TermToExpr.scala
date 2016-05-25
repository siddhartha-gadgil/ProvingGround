package provingground

import HoTT._

class TermToExpr[E](
    univ: Int => E, predef: Term => Option[E] = (t: Term) => None)(
    implicit l: ExprLang[E]) {
  def expr: Term => Option[E] = {
    case term if !predef(term).isEmpty => predef(term)
    case FormalAppln(func, arg) =>
      for (f <- expr(func); x <- expr(arg); fx <- l.appln(f, x)) yield fx
    case LambdaFixed(x: Term, y: Term) =>
      for (xe <- expr(x); ye <- expr(y); result <- l.lambda(xe, ye)) yield
        result
    case Lambda(x: Term, y: Term) =>
      for (xe <- expr(x); ye <- expr(y); result <- l.lambda(xe, ye)) yield
        result
    case pt: PiTyp[u, v] =>
      val x = pt.fibers.dom.Var
      for (xe <- expr(x); ye <- expr(pt.fibers(x)); result <- l.pi(xe, ye)) yield
        result
    case st: SigmaTyp[u, v] =>
      val x = st.fibers.dom.Var
      for (xe <- expr(x); ye <- expr(st.fibers(x)); result <- l.sigma(xe, ye)) yield
        result
    case PlusTyp(first: Typ[u], scnd: Typ[v]) =>
      for (xe <- expr(first.Var); ye <- expr(scnd); result <- l.or(xe, ye)) yield
        result
    case p: AbsPair[_, _] =>
      for (xe <- expr(p.first); ye <- expr(p.second); result <- l.pair(xe, ye)) yield
        result
    case fn: FuncTyp[_, _] =>
      for (xe <- expr(fn.dom.Var); ye <- expr(fn.codom); result <- l.pi(xe, ye)) yield
        result
    case sym: Symbolic with Term =>
      outerSym(sym).name match {
        case Name(name) =>
          for (typ <- expr(sym.typ); result <- l.variable(name, typ)) yield
            result
//        case inn: InnerSym[_] => expr(inn.variable)
        case _ => None
      }
    case IdentityTyp(dom, lhs: Term, rhs: Term) =>
      for (xe <- expr(lhs); ye <- expr(rhs); result <- l.equality(xe, ye)) yield
        result
    case Universe(n) =>
      Some(univ(n))
    case PlusTyp.FirstIncl(typ, value: Term) =>
      for (tp <- expr(typ); x <- expr(value); i <- l.incl1(tp);
           result <- l.appln(i, x)) yield result
    case PlusTyp.ScndIncl(typ, value: Term) =>
      for (tp <- expr(typ); x <- expr(value); i <- l.incl2(tp);
           result <- l.appln(i, x)) yield result
    case Unit => l.tt
    case Star => l.qed
    case Zero => l.ff
  }

  def apply(term: Term) = expr(term)
}

object TermToExpr {
  class NewNameFactory(prefix: String = "$") {

    var name: String = ""

    def get = {
      val newname = nextName(name)

      name = newname

      prefix + newname
    }

    val termNames: scala.collection.mutable.Map[Term, String] =
      scala.collection.mutable.Map()

    def getName(t: Term) =
      termNames.get(t) getOrElse {
        val name = get
        termNames += (t -> name)
        name
      }

    def getTerm(t: Term) = getName(t) :: (t.typ)
  }

  import TermLang._

  def isVar(t: Term) = t match {
    case sym: Symbolic if sym.name.toString.startsWith("$") => true
    case _ => false
  }

  def newTermOpt(term: Term, prefix: String = ".") = {
    val myNames = new NewNameFactory(prefix)
    def predefs(t: Term) =
      if (isVar(t)) Some(myNames.getTerm(t)) else None
    val rebuilder = new TermToExpr((n) => Universe(n), predefs)
    rebuilder(term)
  }

  def rebuild(t: Term, prefix: String = ".") = newTermOpt(t, prefix).get

  def rebuildList(ts: List[Term], prefix: String=".") = {
    val myNames = new NewNameFactory(prefix)
    def predefs(t: Term) =
      if (isVar(t)) Some(myNames.getTerm(t)) else None
    val rebuilder = new TermToExpr[Term]((n) => Universe(n), predefs)
    def recc: List[Term] => List[Term] = {
      case List() => List()
      case x :: ys =>
        val xx = rebuilder(x).get
        xx :: recc(ys)
    }
    recc(ts)
  }

  def rebuildMap[U <: Term with Subs[U]](m: Map[Term, Set[(U, Term)]], prefix: String =".") = {
    val list = m.toList map {
      case (x, s) =>
        val myNames = new NewNameFactory(prefix)
        def predefs(t: Term) =
          if (isVar(t)) Some(myNames.getTerm(t)) else None
        val rebuilder = new TermToExpr[Term]((n) => Universe(n), predefs)
        val xx = rebuilder(x).get
        val ss = s map {case (x, y) => (rebuilder(x).get.asInstanceOf[U], rebuilder(y).get)}
        (xx, ss)
    }
    list.toMap
  }
}
