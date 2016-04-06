package provingground

import HoTT._

class TermToExpr[E](univ : Int => E, predef : Term => Option[E] = Map())(implicit l: ExprLang[E]) {
  def expr: Term => Option[E] = {
    case FormalAppln(func, arg) => 
      for (f <- expr(func); x <- expr(arg); fx <- l.appln(f, x)) yield fx
    case LambdaFixed(x : Term, y: Term) => 
      for (xe <- expr(x); ye <- expr(y); result <- l.lambda(xe, ye)) yield result
    case Lambda(x: Term, y: Term) => 
      for (xe <- expr(x); ye <- expr(y); result <- l.lambda(xe, ye)) yield result
    case pt : PiTyp[u, v] => 
      val x = pt.fibers.dom.Var
      for (xe <- expr(x); ye <- expr(pt.fibers(x)); result <- l.pi(xe, ye)) yield result
    case st: SigmaTyp[u, v] => 
      val x = st.fibers.dom.Var
      for (xe <- expr(x); ye <- expr(st.fibers(x)); result <- l.sigma(xe, ye)) yield result
    case PlusTyp(first : Typ[u], scnd : Typ[v]) => 
      for (xe <- expr(first.Var); ye <- expr(scnd); result <- l.or(xe, ye)) yield result
    case p: AbsPair[_, _] => 
      for (xe <- expr(p.first); ye <- expr(p.second); result <- l.pair(xe, ye)) yield result
    case fn: FuncTyp[_, _] => 
      for (xe <- expr(fn.dom.Var); ye <- expr(fn.codom); result <- l.pi(xe, ye)) yield result
    case sym: Symbolic with Term => 
      sym.name match {
        case Name(name) => for (typ <- expr(sym.typ); result <- l.variable(name, typ)) yield result
        case _ => None
      }
    case IdentityTyp(dom, lhs: Term, rhs : Term) => 
      for (xe <- expr(lhs); ye <- expr(rhs); result <- l.equality(xe, ye)) yield result
    case Universe(n) => 
      Some(univ(n))
    case PlusTyp.FirstIncl(typ, value : Term) =>
      for (tp <- expr(typ); x <- expr(value); i <- l.incl1(tp); result <- l.appln(i, x)) yield result
    case PlusTyp.ScndIncl(typ, value: Term) =>
      for (tp <- expr(typ); x <- expr(value); i <- l.incl2(tp); result <- l.appln(i, x)) yield result 
    case Unit => l.tt
    case Star => l.qed
    case Zero => l.ff
  }
  
  def apply(term: Term) = predef(term) orElse expr(term)
}