package provingground

import scala.util.parsing.combinator._

import scala.util._

import provingground.HoTT._
/**
 * Parsing an Agda-like language in two steps.
 * First there is a combinator parser that recognizes agda tokens.
 * At the next level, we note names and try to parse to terms.
 * 
 * There is also an object to recognize agda tokens.
 */
object AgdaExpressions{
  
  /**
   * Parsing an agda expression.
   * following scala, we first look for a valid expression without including line breaks, then include line breaks.
   * 
   * @param patterns agda patterns such as _and_ parsed to lists of strings, each '_' or a token.
   */
  class AgdaParse(patterns : List[List[String]] = List()) extends JavaTokenParsers{
    
    override val skipWhitespace = false // whitespaces not ignored.
    
    /**
     * spaces and tabs, but not newlines
     */
    def spc: Parser[Unit] = "[ \\t]+".r ^^ ((_) => ())
    
    /**
     * tokens - sequence of characters without whitespaces and not just one colon, equality or underscore (universe).
     */
    def token : Parser[Token] = """[^\s\(\)][^\s\(\)]+|[^\s:_\(\)=]""".r ^^ (Token(_))
    
    /**
     * whitespaces, including newlines.
     */
    def wspc: Parser[Unit] = whiteSpace ^^ ((_) => ())
    
    /**
     * colon
     */
    def colon : Parser[String] = ":"
      
    /**
     *   -> or its latex alternatives
     */  
    def To : Parser[String] = "->" | "\\to" | "\\rightarrow"
 
    /**
     * 'maps to' symbols
     */
    def mapsTo : Parser[String] = ":->" |"\\mapsto" | "|->"
    
    /**
     * a term applied to an expression
     * 
     * @param sp spaces, usually either a spaces/tabs or a whitespace.
     */
    def appl(sp: Parser[Unit] = spc) : Parser[Expression] = term~sp~expr ^^ {case f~_~x => Apply(f, x)}
    
    /**
     * expression A -> B
     * 
     * @param sp spaces, usually either a spaces/tabs or a whitespace.
     */
    def arrow(sp: Parser[Unit] = spc) : Parser[Expression] = term~sp~To~sp~expr ^^{case a~_~_~_~b => Arrow(a, b)}
    
    /**
     * expression "(a : A) :-> b" parsing to lambda.
     */
    def lambda(sp: Parser[Unit] = spc) : Parser[Expression] = typedvar~sp~mapsTo~sp~expr ^^{case x~_~_~_~y => LambdaExp(x, y)}
    
    /**
     * expression (a : A) -> B with B a function of a.
     */
    def deparrow(sp: Parser[Unit] = spc) : Parser[Expression] = typedvar~sp~To~sp~expr ^^{case x~_~_~_~y => DepArrow(x, y)}
    
    /**
     * the first universe
     */
    def univ : Parser[Expression] = "_" ^^ {(_) => U} 
    
    /**
     * expression (x : A) with A a general expression and x just a token.
     * 
     */
    def typedvar : Parser[TypedVar] = "("~opt(wspc)~>token~spc~colon~spc~expr<~opt(wspc)~")" ^^ {case x~_~_~_~t => TypedVar(x.name, t)}

    def typdefn : Parser[TypedVar] = token~spc~colon~spc~expr ^^ {case x~_~_~_~t => TypedVar(x.name, t)} | 
    									"("~opt(wspc)~>token~spc~colon~spc~expr<~opt(wspc)~")" ^^ {case x~_~_~_~t => TypedVar(x.name, t)}
    
    private def recptnmatch(ptn : List[String],sp: Parser[Unit]) : Parser[List[Expression]] = ptn match {
      case List("_") => expr ^^ {List(_)}
      case List("_", word) => expr<~sp~word ^^ {case x => List(x)}
      case "_" :: word :: tail => expr~sp~word~sp~recptnmatch(tail, sp) ^^ {case x~_~_~_~ys => x :: ys}
      case word :: tail => word~sp~>recptnmatch(tail, sp)
    } 
    
    private def ptnmatchlist(ptn : List[String],sp: Parser[Unit]) : Parser[List[Expression]] = ptn match {
      case List("_") => term ^^ {List(_)}
      case List(word) => word ^^ {(_) => List(Token(word))}
      case "_" :: tail => term~recptnmatch(tail, sp) ^^ {case x~ys => x :: ys}
      case ys => recptnmatch(ys, sp)
    } 
    
    /**
     * An agda pattern, parsed into a corresponding composition.
     */
    def ptnmatch(ptn : List[String], sp: Parser[Unit] = spc) ={
      val token: Expression = Token(("" /: ptn)(_+_))
      ptnmatchlist(ptn, sp) ^^ {(l) => (token /: l)(Apply(_,_))}
    }
     
    /**
     * a term - either a single token or expression enclosed in ( ) or begin...end
     */
    def term : Parser[Expression] = "("~opt(wspc)~>expr<~opt(wspc)~")" | "begin"~wspc~>expr<~wspc~"end" | token
    
    /**
     * An expression, to parse to an agda term.
     */
    def expr : Parser[Expression] = ((arrow()  | lambda() | 
    									deparrow() | univ | typedvar ) /: patterns.map(ptnmatch(_, spc)))(_ | _) |
    									((arrow(wspc) | lambda(wspc) | deparrow(wspc) 
    									    ) /: patterns.map(ptnmatch(_, wspc)))(_ | _) |  appl() | term

    def asTerm(e: String, names: String => Option[Term] = (_) => None) = {
      Try(parseAll(expr, e).get).toOption flatMap (_.asTerm(names))
    }
    /**
     * expression A = B
     */
    def eqlty(sp: Parser[Unit] = spc): Parser[Equality] = expr~sp~"="~sp~expr ^^ {case lhs~_~_~_~rhs => Equality(lhs, rhs)}
    
    /**
     * data definition:
     * data A : _ where
     * f : A -> A
     * ..
     * end
     * 
     */
    def data = "data"~>typdefn~"where"~rep(crlf~>typdefn)<~"end" ^^ {case x~_~ls => (x, ls)}
    
    /**
     * 
     * line breaks
     */
    def crlf = opt(spc)~""""\n|(\r\n)"""".r~opt(spc)
    
    /** 
     *  definition
     */
    def defn = typdefn~crlf~eqlty(wspc) ^^ {case x~_~y => (x, y)}
    
    /**
     * pattern matching definition
     */
    def casedefn = typdefn~"where"~crlf~repsep(eqlty(wspc), crlf)<~opt("end")
  }
  
  
  class AgdaPatternParser extends JavaTokenParsers{
    override val skipWhitespace = false
    
    def word : Parser[String] = "[^ \t_]+".r
    
    def blank : Parser[String] ="_"
      
    def prefixPtn: Parser[List[String]] = blank~mixfixPtn ^^ {case head~tail => head :: tail} | blank ^^ {List(_)} 
    
    def mixfixPtn: Parser[List[String]] = word~prefixPtn ^^ {case head~tail => head :: tail} | word ^^ {List(_)}
    
    def agdaPtn : Parser[List[String]] = mixfixPtn | prefixPtn
  }
  
  
  trait Statement
  
  /**
   * All Expressions, but not statements or blocks
   */
  trait Expression{
    def asTerm(names: String => Option[Term]): Option[Term]
    
    def asTyp(names: String => Option[Term]) = asTerm(names) flatMap {
      case tp : Typ[_] => Some(tp)
      case _ => None
    }
  }
  
  // Avoid
  trait TypExpression extends Expression{
    def asTerm(names: String => Option[Term]): Option[Typ[Term]]
  }
  
  /**
   * Expression a = b
   */
  case class Equality(lhs: Expression, rhs: Expression) extends Statement{
    def asTerm(names: String => Option[Term]) = rhs.asTerm(names)
  }
  
  /**
   * Symbolic term of given type if tp is a type.
   */
  def symbterm(name: String, tp: Term) : Option[Term] = tp match {
    case t : Typ[_] => Some(t.symbObj(name))
    case _ => None
  }
  
  /**
   * Symbolic variable with given type
   */
  case class TypedVar(name: String, typ: Expression) extends Expression{
    def asTerm(names: String => Option[Term]): Option[Term] = 
      for (tp <- typ.asTerm(names); x <- symbterm(name, tp)) yield x
  }
  
  /**
   * LambdaExpression maps to a lambda
   */
  case class LambdaExp(x : TypedVar, y: Expression) extends Expression{
    def asTerm(names: String => Option[Term]): Option[Term] = {
      val symb = x.name
      val newnames : String => Option[Term] = {
        case `symb` => x.typ.asTyp(names) map (_.symbObj(symb))
        case y => names(y)
      } 
      for (a <- x.asTerm(names); b <- y.asTerm(newnames)) yield lambda(a)(b)
    }
    }

  /**
   * token
   */
  case class Token(name: String) extends Expression{
    def asTerm(names: String => Option[Term]): Option[Term] = names(name)
  }
  
  /**
   * apply function to term if legal
   */
  def applyterm(f : Term, arg: Term) : Option[Term] = f match {
    case f : FuncTerm[u, v] => Try(f(arg.asInstanceOf[u])).toOption
    case _ => None
  }
  
  /**
   * expression func(arg)
   */
  case class Apply(func: Expression, arg: Expression) extends Expression{
    def asTerm(names: String => Option[Term]): Option[Term] = 
      for (a <- func.asTerm(names); b <- arg.asTerm(names); z <- applyterm(a, b)) yield z
  }
  

  /**
   * the first universe
   */
  case object U extends TypExpression{
    def asTerm(name: String => Option[Term]) = Some(__)
  }
  
  /**
   * try to interpret as A -> B
   */
  def arrowtyp(x : Term, y: Term) : Option[Typ[Term]] = (x, y) match {
    case (a : Typ[Term], b: Typ[Term]) => Some(a ->: b)
    case _ => None
  }
  
  /**
   * expression for A -> B
   */
  case class Arrow(lhs: Expression, rhs: Expression) extends TypExpression{
    def asTerm(names: String => Option[Term]): Option[Typ[Term]] = 
      for (a <- lhs.asTerm(names); b <- rhs.asTerm(names); z <- arrowtyp(a, b)) yield z
  }
 
  /**
   * try to interpret as pi-type
   */
  def pityp(x: Term, y: Term) = y match {
    case tp : Typ[Term] =>
      	val fibre = (t : Term) => tp subs (x, t)
	    val family : FuncObj[Term, Typ[Term]] = LambdaFixed(x, tp)
	    Some(PiTyp(family))
    case _ => None
  }
  
  /**
   * expression for pi-type
   */
  case class DepArrow(lhs: TypedVar, rhs: Expression) extends TypExpression{
    def asTerm(names: String => Option[Term]): Option[Typ[Term]] = 
      for (a <- lhs.asTerm(names); b <- rhs.asTerm(names); z <- pityp(a, b)) yield z
  }
 
  
}