package provingground

import HoTT._

import RefineTerms.{refine, refineTyp}

//import scala.language.existentials

import scala.util.Try

case object TermLang
    extends ExprLang[Term] with Domain[Term] with ExprPatterns[Term] {
  def variable[S](name: S, typ: Term): Option[Term] = (name, typ) match {
    case (s: String, t: Typ[u]) =>
      Some(t.symbObj(s))
    case _ => None
  }

  def typVariable[S](name: S, level: Int): Option[Term] = name match {
//    case t : Typ[u] => Some(t)
    case "Type" => Some(HoTT.Universe(level))
    case s: String =>
      Some(SymbTyp(s, level))
    case _ => None
  }

  /**
    * anonymous variable
    */
  def anonVar(typ: Term): Option[Term] = typ match {
    case t: Typ[_] => Some(t.Var)
    case _ => None
  }

  /**
    * meta-variable of a given type, i.e., whose value must be inferred
    * (elaborated in lean's terminology).
    */
  def metaVar(typ: Term): Option[Term] = None

  def lambda(variable: Term, value: Term): Option[Term] =
    Try(refine(HoTT.lambda(variable)(value))).toOption

  def pi(variable: Term, typ: Term): Option[Term] = typ match {
    case t: Typ[u] => Try(refineTyp(HoTT.pi(variable)(t))).toOption
    case _ => None
  }

  def appln(func: Term, arg: Term) = func match {
    case fn: FuncLike[u, v] if fn.dom == arg.typ =>
      Try(fn(arg.asInstanceOf[u])).toOption
    case _ => None
  }
  /*
  def appln(func: Term, arg: Term): Option[Term] ={
    def act(x: Term) = func match
  {
    case fn : FuncLike[u, v] if fn.dom == arg.typ => Try(fn(x.asInstanceOf[u])).toOption
    case _ => None
  }
    (conversions(arg) map (act)).flatten.toStream.headOption
//    act(arg)
  }
   */

  def equality(lhs: Term, rhs: Term): Option[Term] =
    if (lhs.typ == rhs.typ) Try(lhs =:= rhs).toOption else None

  def sigma(variable: Term, typ: Term): Option[Term] = typ match {
    case t: Typ[u] => Try(refineTyp(HoTT.sigma(variable)(t))).toOption
    case _ => None
  }

  def pair(x: Term, y: Term): Option[Term] =
    Some(mkPair(x, y))

  def proj1(xy: Term): Option[Term] = xy match {
    case p: AbsPair[u, v] => Some(p.first)
    case _ => None
  }

  def proj2(xy: Term): Option[Term] = xy match {
    case p: AbsPair[u, v] => Some(p.second)
    case _ => None
  }

  def or(first: Term, second: Term): Option[Term] = (first, second) match {
    case (f: Typ[u], s: Typ[v]) => Some(PlusTyp(f, s))
    case _ => None
  }

  def incl1(typ: Term): Option[Term] = typ match {
    case pt: PlusTyp[u, v] => Some(pt.incl1)
  }

  def incl2(typ: Term): Option[Term] = typ match {
    case pt: PlusTyp[u, v] => Some(pt.incl2)
  }

  /**
    * true type
    */
  def tt: Option[Term] = Some(Unit)

  /**
    * element of true type
    */
  def qed: Option[Term] = Some(Star)

  /**
    * false type
    */
  def ff: Option[Term] = Some(Zero)

  def orCases(first: Term, second: Term): Option[Term] =
    (first, second) match {
      case (fn1: Func[u, w], fn2raw: Func[v, ww])
          if (fn1.codom == fn2raw.codom) =>
        val tp = PlusTyp(fn1.dom, fn2raw.dom)
        val fn2 = fn2raw.asInstanceOf[Func[v, w]]
        Some(PlusTyp.RecFn(fn1.dom, fn2.dom, fn1.codom, fn1, fn2))
      case (fn1: FuncLike[u, w], fn2raw: FuncLike[v, ww])
          if (fn1.depcodom == fn2raw.depcodom) =>
        val tp = PlusTyp(fn1.dom, fn2raw.dom)
        val fn2 = fn2raw.asInstanceOf[FuncLike[v, w]]
        val x1 = fn1.dom.Var
        val x2 = fn2.dom.Var
        val fibre1 = lmbda(x1)(fn1(x1).typ.asInstanceOf[Typ[w]])
        val fibre2 = lmbda(x2)(fn2(x2).typ.asInstanceOf[Typ[w]])
        val fibre = PlusTyp.RecFn(fn1.dom, fn2.dom, Type, fibre1, fibre2)
        Some(tp.InducFn(fibre, fn1, fn2))
      case _ => None
    }

  def numeral(n: Int): Option[Term] =
    Try(NatRing.Literal(n)).toOption

  def isPair: Term => Option[(Term, Term)] = {
    case xy: AbsPair[u, v] => Some((xy.first, xy.second))
    case _ => None
  }

  def isSigma: Term => Option[(Term, Term)] = {
    case st: SigmaTyp[u, v] =>
      val x = st.fibers.dom.Var
      Some((x, st.fibers(x)))
    case st: ProdTyp[u, v] =>
      Some((st.first.Var, st.second))
    case _ => None
  }

  def isPi: Term => Option[(Term, Term)] = {
    case st: FuncTyp[u, v] =>
      Some((st.dom.Var, st.codom))
    case st: GenFuncTyp[u, v] =>
      val x = st.domain.Var
      Some((x, st.fib(x)))
    case _ => None
  }

  def domain: Term => Option[Term] = {
    case fn: FuncLike[u, v] => Some(fn.dom)
    case _ => None
  }

  implicit def termLang: ExprLang[Term] = this
}
