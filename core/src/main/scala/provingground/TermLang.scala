package provingground

import HoTT._

import RefineTerms.refine

import scala.language.existentials

import scala.util.Try

case class TermLang(
    dictionary : String => Option[Term] = (s) => None,
    formulas : String => Option[Term] = (f) => None,
    conversions: Term => Iterator[Term] = (t) => Seq(t).iterator) extends ExprLang[Term]{
   def variable[S](name: S, typ: Term): Option[Term] = (name, typ) match {
     case (s: String, t: Typ[u]) =>
       Some(t.symbObj(s))
     case  _ => None
   }

  /**
   * anonymous variable
   */
  def anonVar(typ: Term): Option[Term] = typ match {
    case t : Typ[_] => Some(t.Var)
    case _ => None
  }

  /**
   * meta-variable of a given type, i.e., whose value must be inferred 
   * (elaborated in lean's terminology). 
   */
  def metaVar(typ: Term): Option[Term] = None
  
  def lambda(variable: Term, value: Term) : Option[Term] =
    Try(refine(HoTT.lambda(variable)(value))).toOption

  def pi(variable: Term, typ: Term): Option[Term] = typ match {    
    case t: Typ[u] => Try(refine(HoTT.pi(variable)(t))).toOption
    case _ => None
  }

  def appln(func: Term, arg: Term): Option[Term] ={
    def act(x: Term) = func match 
  {
    case fn : FuncLike[u, v] => Try(fn(x.asInstanceOf[u])).toOption
    case _ => None
  }
    (conversions(arg) map (act)).flatten.toStream.headOption
//    act(arg)
  }
  def equality(lhs: Term, rhs: Term) : Option[Term] = 
    Try(lhs =:= rhs).toOption
  
  def sigma(variable: Term, typ: Term) : Option[Term] = typ match {    
    case t: Typ[u] => Try(refine(HoTT.sigma(variable)(t))).toOption
    case _ => None
  }
  
  def pair (x: Term, y: Term): Option[Term] = 
    Some(mkPair(x, y))

  def proj1(xy: Term): Option[Term] = xy match {
    case p : AbsPair[u, v] => Some(p.first)
    case  _ => None
  }

  def proj2(xy: Term): Option[Term] = xy match {
    case p : AbsPair[u, v] => Some(p.second)
    case  _ => None
  }

  def or(first: Term, second: Term):  Option[Term] = (first, second) match
  {
    case (f: Typ[u], s: Typ[v]) => Some(PlusTyp(f, s))
    case  _ => None
  }
  
  def incl1(typ : Term) : Option[Term] = typ match {
    case pt: PlusTyp[u, v] => Some(pt.ifn)
  }

  def incl2(typ: Term) :  Option[Term] = typ match {
    case pt: PlusTyp[u, v] => Some(pt.jfn)
  }

  /**
   * true type
   */
  def tt : Option[Term] = Some(Unit)

  /**
   * element of true type
   */
  def qed : Option[Term] = Some(Star)

  /**
   * false type
   */
  def ff : Option[Term] = Some(Zero)

  /**
   * optionally parse formula (such as latex) to expression.
   */
  def formula(fmla: String):  Option[Term] = formulas(fmla)

  /**
   * optionally parse token, such as function in language, to expression, depending on context;
   * note that this changes with more definitions,
   * so it may be best to refer to something external, which can be a mutable buffer, database etc.;
   * 
   */
  def vocab[C](name: String, context: C): Option[Term] = 
    dictionary(name)

  def numeral(n: Int): Option[Term] = 
    Try(NatRing.Literal(n)).toOption
   
}