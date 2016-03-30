package provingground
import scala.util.Try

/**
* language structure for the target of translation;
* includes:
* * the ways of forming expressions: function application, lambdas and (dependent) function types.
* * pre-defined objects:
*     (dependent) pair and union types and associated inclusions and projections, true and false types
* * variables with names and anonymous variables
*/
trait ExprLang[E]{
  def variable[S](name: S, typ: E): Option[E]

  /**
   * anonymous variable
   */
  def anonVar(typ: E): Option[E]

  /**
   * meta-variable of a given type, i.e., whose value must be inferred 
   * (elaborated in lean's terminology). 
   */
  def metaVar(typ: E): Option[E]
  
  def lambda(variable: E, value: E) : Option[E]

  def pi(variable: E, typ: E): Option[E]

  def appln(func: E, arg: E): Option[E]

  def equality(lhs: E, rhs: E) : Option[E]
  
  def sigma(variable: E, typFamily: E) : Option[E]  
  
  def pair (x: E, y: E): Option[E]

  def proj1(xy: E): Option[E]

  def proj2(xy: E): Option[E]

  def or(first: E, second: E):  Option[E]

  def incl1(typ : E) : Option[E]

  def incl2(typ: E) :  Option[E]

  /**
   * true type
   */
  def tt : Option[E]

  /**
   * element of true type
   */
  def qed : Option[E]

  /**
   * false type
   */
  def ff : Option[E]

  /**
   * optionally parse formula (such as latex) to expression.
   */
  def formula(fmla: String):  Option[E]

  /**
   * optionally parse token, such as function in language, to expression, depending on context;
   * note that this changes with more definitions,
   * so it may be best to refer to something external, which can be a mutable buffer, database etc.;
   * 
   */
  def vocab[C](name: String, context: C): Option[E]

  def numeral(n: Int): Option[E]
  
  /**
   * non-dependent pair built from abstract methods.
   */
  def pairTyp(first: E, second: E) =
    anonVar(first) flatMap ((x) =>
      lambda(x, second) flatMap (sigma(x, _)))
}

object ExprLang{
  case class Exists[E](term: E)
  
  case class All[E](term: E)
  
  def appln[E : ExprLang](func: E, arg: E): Option[E] = {
    val lang = implicitly[ExprLang[E]]
    (func, arg) match {
      case (_, All(x)) => 
        Try(appln(func, x.asInstanceOf[E]) flatMap (lang.lambda(x.asInstanceOf[E], _))).toOption.flatten
      case (_, Exists(x)) => 
        Try(appln(func, x.asInstanceOf[E]) flatMap (lang.sigma(x.asInstanceOf[E], _))).toOption.flatten
      case (All(x),_) =>
        Try(lang.lambda(x.asInstanceOf[E], arg)).toOption.flatten
      case (Exists(x),_) =>
        Try(lang.sigma(x.asInstanceOf[E], arg)).toOption.flatten
      case _ => lang.appln(func, arg)
    }
  }
}
