package provingGround

import scala.util.parsing.combinator._

import scala.util._

import provingGround.HoTT._

object AgdaExpressions{
  
  object AgdaParse extends JavaTokenParsers{
    override val skipWhitespace = false
    def sp: Parser[Unit] = whiteSpace ^^ ((_) => ())
    
    def token : Parser[Token] = "[^\\s]".r ^^ (Token(_))
    
    
    def colon : Parser[String] = ":"
      
    def To : Parser[String] = "->" | "\\to"
    
    def mapsTo : Parser[String] = ":->" |"\\mapsto" | "|->"
    
    
    def appl : Parser[Expression] = term~sp~expr ^^ {case f~_~x => Apply(f, x)}
    
    def arrow: Parser[Expression] = term~sp~To~sp~expr ^^{case a~_~_~_~b => Arrow(a, b)}
    
    def lambda : Parser[Expression] = typedvar~sp~mapsTo~sp~expr ^^{case x~_~_~_~y => Lambda(x, y)}
    
    def deparrow : Parser[Expression] = typedvar~sp~To~sp~expr ^^{case x~_~_~_~y => DepArrow(x, y)}
    
    def univ : Parser[Expression] = "_" ^^ {(_) => U}
    
    
    def typedvar : Parser[TypedVar] = token~sp~colon~sp~expr ^^ {case x~_~_~_~t => TypedVar(x.name, t)}

    def term : Parser[Expression] = token | "("~>expr<~")"
    
    def expr : Parser[Expression] = term | appl | arrow | lambda | deparrow | univ
  }
  
  
  /*
   * All Expressions, but not statements or blocks
   */
  trait Expression{
    def asTerm(names: String => Option[Term]): Option[Term]
  }
  
  trait TypExpression extends Expression{
    def asTerm(names: String => Option[Term]): Option[Typ[Term]]
  }
  
  def symbterm(name: String, tp: Term) : Option[Term] = tp match {
    case t : Typ[_] => Some(t.symbObj(name))
    case _ => None
  }
  
  case class TypedVar(name: String, typ: Expression) extends Expression{
    def asTerm(names: String => Option[Term]): Option[Term] = 
      for (tp <- typ.asTerm(names); x <- symbterm(name, tp)) yield x
  }
  
  case class Lambda(x : TypedVar, y: Expression) extends Expression{
    def asTerm(names: String => Option[Term]): Option[Term] = 
      for (a <- x.asTerm(names); b <- y.asTerm(names)) yield lambda(a)(b)
    }

  
  case class Token(name: String) extends Expression{
    def asTerm(names: String => Option[Term]): Option[Term] = names(name)
  }
  
  def applyterm(f : Term, arg: Term) : Option[Term] = f match {
    case f : FuncTerm[u, v] => Try(f(arg.asInstanceOf[u])).toOption
    case _ => None
  }
  
  
  case class Apply(func: Expression, arg: Expression) extends Expression{
    def asTerm(names: String => Option[Term]): Option[Term] = 
      for (a <- func.asTerm(names); b <- arg.asTerm(names); z <- applyterm(a, b)) yield z
  }
  

  
  case object U extends TypExpression{
    def asTerm(name: String => Option[Term]) = Some(__)
  }
  
  
  def arrowtyp(x : Term, y: Term) : Option[Typ[Term]] = (x, y) match {
    case (a : Typ[Term], b: Typ[Term]) => Some(a ->: b)
    case _ => None
  }
  
  case class Arrow(lhs: Expression, rhs: Expression) extends TypExpression{
    def asTerm(names: String => Option[Term]): Option[Typ[Term]] = 
      for (a <- lhs.asTerm(names); b <- rhs.asTerm(names); z <- arrowtyp(a, b)) yield z
  }
 
  def pityp(x: Term, y: Term) = y match {
    case tp : Typ[Term] =>
      	val fibre = (t : Term) => tp subs (x, t)
	    val family : FuncObj[Term, Typ[Term]] = LambdaFixed(x, tp)
	    Some(PiTyp(family))
    case _ => None
  }
  
  case class DepArrow(lhs: TypedVar, rhs: Expression) extends TypExpression{
    def asTerm(names: String => Option[Term]): Option[Typ[Term]] = 
      for (a <- lhs.asTerm(names); b <- rhs.asTerm(names); z <- pityp(a, b)) yield z
  }
 
  
}