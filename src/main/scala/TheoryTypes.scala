package provingGround
import provingGround.HoTT._
import scala.util.parsing.combinator._

object TheoryTypes{
  
  trait Expression
  
  trait Term extends Expression
 
  
  case class Colon(obj: Term, typ: Term) extends Expression
  
  case class TypedTerm(obj: Term, typ: Term) extends Term
  
  case class Prod(index: Expression, argument: Term) extends Term
  
  case class Equality(lhs: Term, rhs: Term) extends Term
  
  case class Apply(func: Term, argument: Term) extends Term
  
  case class FuncTerm(lhs: Term, rhs: Term) extends Term
  
  case class BinOpTerm(op: String, lhs: Term, rhs: Term) extends Term
  
  case class BigOpTerm(op: String, index: Term, argument: Term) extends Term
  
  case class Braces(index: Expression, argument: Term) extends Term
  
  case class SubScript(argument: Term, sub: Term) extends Term
  
  case class SupScript(argument: Term, sub: Term) extends Term
  
  case class TermSym(sym: String) extends Term
  
  case class LambdaTerm(arg: TypedTerm, value: Term) extends Term
  
  case object Underscore extends Term
  
  
  class ExpressionParser(binOps: List[String], binRels: List[String], bigOps: List[String]) extends JavaTokenParsers{
	  def sym: Parser[Term] = ("[a-zA-Z]".r | """\\(\w)+""".r) ^^ {case s: String => TermSym(s)}
	  
	  def underscore: Parser[Term] = "__" ^^ {case _ => Underscore}
	  
	  def equality: Parser[Term] = noRelTerm~"="~term ^^ {case lhs~"="~rhs => Equality(lhs, rhs)}
	  
	  def subScript: Parser[Term] = simpleTerm~"_"~simpleTerm ^^ {case lhs~"_"~rhs => SubScript(lhs, rhs)}
	  
	  def supScript: Parser[Term] = simpleTerm~"^"~simpleTerm ^^ {case lhs~"^"~rhs => SupScript(lhs, rhs)}
	  
	  def binOpTerm(op: String): Parser[Term] = noOpTerm~literal(op)~term ^^{case lhs~(_: String)~rhs => BinOpTerm(op, lhs, rhs)}
	  
	  def binRelTerm(op: String): Parser[Term] = noRelTerm~literal(op)~term ^^{case lhs~(_: String)~rhs => BinOpTerm(op, lhs, rhs)}
	  
	  def bigOpTerm(op: String): Parser[Term] = literal(op)~simpleTerm~term ^^{case (_: String)~(lhs: Expression)~(rhs: Expression) => BigOpTerm(op, lhs, rhs)}
	  
	  def application: Parser[Term] = simpleTerm~term ^^ {case f~a => Apply(f, a)}
	  
	  def func: Parser[Term] = noRelTerm~"->"~term ^^ {case lhs~"->"~rhs => FuncTerm(lhs, rhs)}
	  
	  
	  
	  
	  def binOpParse: Parser[Term] = binOps map (binOpTerm(_)) reduce (_ | _) 
	  
	  def bigOpParse: Parser[Term] = bigOps map (bigOpTerm(_)) reduce (_ | _)
	  
	  def binRelParse: Parser[Term] = binRels map (binRelTerm(_)) reduce (_ | _)
	  
	  
	  def braces: Parser[Term] = "{"~expression~"|"~term~"}" ^^ {case "{"~index~"|"~arg~"}" => Braces(index, arg)}
	  
	  def parenTerm: Parser[Term] = "("~term~")" ^^ {case "("~t~")" => t}
	  
	  def texBraces: Parser[Term] = """\{"""~expression~"|"~term~"""\}""" ^^ {case """\{"""~index~"|"~arg~"""\}""" => Braces(index, arg)}
	  
	  def term: Parser[Term] = noRelTerm | equality |binRelParse
	  
	  def simpleTerm: Parser[Term] = sym | typedTerm | parenTerm | braces | texBraces | underscore | subScript | supScript // without operations, relations, bigOps, 	  	  
	   
	  def noOpTerm: Parser[Term] = simpleTerm | bigOpParse
	  
	  def noRelTerm: Parser[Term] = noOpTerm | binOpParse
	  
	  def colon: Parser[Expression] = term~":"~term ^^ {case obj~":"~typ => Colon(obj, typ)}
	  
	  def typedTerm: Parser[TypedTerm] = "("~term~":"~term~")" ^^ {case "("~obj~":"~typ~")" => TypedTerm(obj, typ)}
	  
	  def lambda: Parser[Term] = typedTerm~"->"~term ^^ {case arg~"->"~value => LambdaTerm(arg, value)}
	  
 
	  
	  def expression: Parser[Expression] = colon | term
  }
  
  
  
  trait TheoryTyp extends FormalTyp
  
  case class TextTyp(text: String) extends TheoryTyp
  
  trait Claim extends TheoryTyp{
    val claim: FormalTyp
  }
  
  case class Using(using: FormalTyp, claim: FormalTyp) extends Claim
  
  trait Method
  
  case class Para(sentences: FormalTyp) extends TheoryTyp
  
}
