package provingGround

import scala.util.parsing.combinator._

import scala.util._

import provingGround.HoTT._

object AgdaExpressions{
  
  class AgdaParse(patterns : List[List[String]]) extends JavaTokenParsers{
    override val skipWhitespace = false
    def spc: Parser[Unit] = "[ \\t]+".r ^^ ((_) => ())
    
    def token : Parser[Token] = "[^\\s]".r ^^ (Token(_))
    
    def wspc: Parser[Unit] = whiteSpace ^^ ((_) => ())
    
    def colon : Parser[String] = ":"
      
    def To : Parser[String] = "->" | "\\to"
    
    def mapsTo : Parser[String] = ":->" |"\\mapsto" | "|->"
    
    
    def appl(sp: Parser[Unit] = spc) : Parser[Expression] = term~sp~expr ^^ {case f~_~x => Apply(f, x)}
    
    def arrow(sp: Parser[Unit] = spc) : Parser[Expression] = term~sp~To~sp~expr ^^{case a~_~_~_~b => Arrow(a, b)}
    
    def lambda(sp: Parser[Unit] = spc) : Parser[Expression] = typedvar~sp~mapsTo~sp~expr ^^{case x~_~_~_~y => Lambda(x, y)}
    
    def deparrow(sp: Parser[Unit] = spc) : Parser[Expression] = typedvar~sp~To~sp~expr ^^{case x~_~_~_~y => DepArrow(x, y)}
    
    def univ : Parser[Expression] = "_" ^^ {(_) => U} // never reached
    
    
    def typedvar : Parser[TypedVar] = token~spc~colon~spc~expr ^^ {case x~_~_~_~t => TypedVar(x.name, t)}

    private def recptnmatch(ptn : List[String],sp: Parser[Unit]) : Parser[List[Expression]] = ptn match {
      case List("_") => expr ^^ {List(_)}
      case "_" :: word :: tail => expr~word~recptnmatch(tail, sp) ^^ {case x~_~ys => x :: ys}
      case word :: tail => word~>recptnmatch(tail, sp)
    } 
    
    private def ptnmatchlist(ptn : List[String],sp: Parser[Unit]) : Parser[List[Expression]] = ptn match {
      case List("_") => term ^^ {List(_)}
      case List(word) => word ^^ {(_) => List(Token(word))}
      case "_" :: tail => term~recptnmatch(tail, sp) ^^ {case x~ys => x :: ys}
      case ys => recptnmatch(ys, sp)
    } 
    
    def ptnmatch(ptn : List[String], sp: Parser[Unit]) ={
      val token: Expression = Token(("" /: ptn)(_+_))
      ptnmatchlist(ptn, sp) ^^ {(l) => (token /: l)(Apply(_,_))}
    }
      
    def term : Parser[Expression] = token | "("~opt(wspc)~>expr<~opt(wspc)~")" | "begin"~wspc~>expr<~wspc~"end"
    
    def expr : Parser[Expression] = ((term | appl() | arrow() | lambda() | 
    									deparrow() | univ | typedvar | eqlty()) /: patterns.map(ptnmatch(_, spc)))(_ | _) |
    									((arrow(wspc) | lambda(wspc) | deparrow(wspc) 
    									    | eqlty(wspc)) /: patterns.map(ptnmatch(_, wspc)))(_ | _)

    
    def eqlty(sp: Parser[Unit] = spc): Parser[Equality] = expr~sp~"="~sp~expr ^^ {case lhs~_~_~_~rhs => Equality(lhs, rhs)}
    									
    def data = "data"~>typedvar~"where"~rep(typedvar)<~"end" ^^ {case x~_~ls => (x, ls)}
    
    def crlf = opt(spc)~"\\n".r~opt(spc)
    
    def defn = typedvar~crlf~eqlty(wspc) ^^ {case x~_~y => (x, y)}
    
    def casedefn = typedvar~"where"~crlf~repsep(eqlty(wspc), crlf)<~opt("end")
  }
  
  
  object AgdaPatternParser extends JavaTokenParsers{
    override val skipWhitespace = false
    
    def word : Parser[String] = "^[ \t]+".r
    
    def blank : Parser[String] ="_"
      
    def prefixPtn: Parser[List[String]] = blank~mixfixPtn ^^ {case head~tail => head :: tail} | word ^^ {List(_)} 
    
    def mixfixPtn: Parser[List[String]] = word~prefixPtn ^^ {case head~tail => head :: tail} | word ^^ {List(_)}
  }
  
  
  trait Statement
  
  /*
   * All Expressions, but not statements or blocks
   */
  trait Expression{
    def asTerm(names: String => Option[Term]): Option[Term]
  }
  
  trait TypExpression extends Expression{
    def asTerm(names: String => Option[Term]): Option[Typ[Term]]
  }
  
  case class Equality(lhs: Expression, rhs: Expression) extends Statement with Expression{
    def asTerm(names: String => Option[Term]) = rhs.asTerm(names)
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