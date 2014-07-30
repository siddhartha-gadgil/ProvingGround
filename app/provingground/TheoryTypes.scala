package provingground
import provingground.HoTT._
import scala.util.parsing.combinator._
import scala.language.implicitConversions

object TheoryTypes{
  
  trait Expression{
    def -->(that: Expression) = Arrow(this, that)
    
    def \\(that: Expression) = ParaTerm(this, that)
    
    def --->(that: Expression) = LambdaTerm(this, that)
  }
  
  trait Term extends Expression{
    def -->(that: Term) = FuncTypTerm(this, that)
    
    def ::(that: Term) = Colon(that, this)
    
    def =:= (that: Term) = Equality(this, that)
    
  }
 
  case class And(first: Expression, second: Expression) extends Term
  
  case class Or(first: Expression, second: Expression) extends Term
 
  case class Is(first: Expression, second: Expression) extends Term
    
  case class Conjunct(conj: String, first: Expression, second: Expression) extends Term
  
  case class ParaTerm(first: Expression, rest: Expression) extends Term
  
  case class Colon(obj: Term, typ: Term) extends Expression
  
  case class TypedTerm(obj: Term, typ: Term) extends Term
  
  case class Prod(index: Expression, argument: Term) extends Term
  
  case class Equality(lhs: Term, rhs: Term) extends Term
  
  case class Apply(func: Term, argument: Term) extends Term
  
  case class MultiApply[L](func: Expression, args: Map[L, Expression]) extends Term
  
  case class FuncTypTerm(dom: Term, codom: Term) extends Term
  
  case class Arrow(dom:Expression, codom: Expression) extends Expression
  
  case class BinOpTerm(op: String, lhs: Term, rhs: Term) extends Term
  
  case class BigOpTerm(op: String, index: Term, argument: Term) extends Term
  
  case class Braces(index: Expression, argument: Term) extends Term
  
  case class SubScript(argument: Term, sub: Term) extends Term
  
  case class SupScript(argument: Term, sub: Term) extends Term
  
  case class TermSym[A](sym: A) extends Term
  
  case class LambdaTerm(arg: Expression, value: Expression) extends Term
  
  case object Underscore extends Term
  
  case class IntSym(value: Int) extends Term
  
  case class RealSym(value: Double) extends Term
  
   
  class ExpressionParser(binOps: List[String], binRels: List[String], bigOps: List[String]) extends JavaTokenParsers{
	  def varSym: Parser[Term] = ("[a-zA-Z]".r | """\\(\w)+""".r) ^^ {case s: String => TermSym(s)}
	  
	  def intSym: Parser[Term] = wholeNumber ^^ {case n => IntSym(n.toInt)}
	  
	  def realSym: Parser[Term] = floatingPointNumber ^^ {case x => RealSym(x.toDouble)}
	  
	  def sym: Parser[Term] = varSym | realSym | intSym
	  
	  def underscore: Parser[Term] = "__" ^^ {case _ => Underscore}
	  
	  def equality: Parser[Term] = noRelTerm~"="~term ^^ {case lhs~"="~rhs => Equality(lhs, rhs)}
	  
	  def subScript: Parser[Term] = sym~"_"~simpleTerm ^^ {case lhs~"_"~rhs => SubScript(lhs, rhs)}
	  
	  def supScript: Parser[Term] = sym~"^"~simpleTerm ^^ {case lhs~"^"~rhs => SupScript(lhs, rhs)}
	  
	  def binOpTerm(op: String): Parser[Term] = noOpTerm~literal(op)~noRelTerm ^^{case lhs~(_: String)~rhs => BinOpTerm(op, lhs, rhs)}
	  
	  def binRelTerm(op: String): Parser[Term] = noRelTerm~literal(op)~term ^^{case lhs~(_: String)~rhs => BinOpTerm(op, lhs, rhs)}
	  
	  def bigOpTerm(op: String): Parser[Term] = literal(op)~simpleTerm~noRelTerm ^^{case (_: String)~(lhs: Expression)~(rhs: Expression) => BigOpTerm(op, lhs, rhs)}
	  
	  def application: Parser[Term] = simpleTerm~term ^^ {case f~a => Apply(f, a)}
	  
	  def func: Parser[Term] = noRelTerm~"->"~term ^^ {case lhs~"->"~rhs => FuncTypTerm(lhs, rhs)}
	  
	  
	  def nullTerm: Parser[Term] = failure("") ^^ { _ =>Underscore}
	  
	  def binOpParse: Parser[Term] = (binOps map (binOpTerm(_))).foldLeft(nullTerm)((s, t) => s | t) 
	  
	  def bigOpParse: Parser[Term] = (bigOps map (bigOpTerm(_))).foldLeft(nullTerm)((s, t) => s | t) 
	  
	  def binRelParse: Parser[Term] = (binRels map (binRelTerm(_))).foldLeft(nullTerm)((s, t) => s | t) 
	  
	  
	  def braces: Parser[Term] = "{"~expression~"|"~term~"}" ^^ {case "{"~index~"|"~arg~"}" => Braces(index, arg)}
	  
	  def parenTerm: Parser[Term] = "("~term~")" ^^ {case "("~t~")" => t}
	  
	  def texBraces: Parser[Term] = "{"~term~"}" ^^ {case "{"~arg~"}" => arg}
	  
	  def term: Parser[Term] =  equality | binRelParse | lambda | noRelTerm 
	  
	  def simpleTerm: Parser[Term] = typedTerm | parenTerm | texBraces | braces | underscore | subScript | supScript | sym // without operations, relations, bigOps, 	  	  
	   
	  def noOpTerm: Parser[Term] = bigOpParse | simpleTerm 
	  
	  def noRelTerm: Parser[Term] = binOpParse | noOpTerm 
	  
	  def colon: Parser[Expression] = term~":"~term ^^ {case obj~":"~typ => Colon(obj, typ)}
	  
	  def typedTerm: Parser[TypedTerm] = "("~term~":"~term~")" ^^ {case "("~obj~":"~typ~")" => TypedTerm(obj, typ)}
	  
	  def lambda: Parser[Term] = typedTerm~"->"~term ^^ {case arg~"->"~value => LambdaTerm(arg, value)}
	  
 
	  
	  def expression: Parser[Expression] = colon | term
  }
  
  
  
  trait TheoryTyp extends SmallTyp
  
  case class TextTyp(text: String) extends TheoryTyp
  
  trait Claim extends TheoryTyp{
    val claim: LogicalTyp
  }
  
  case class Using(using: LogicalTyp, claim: LogicalTyp) extends Claim
  
  trait Method
  
  case class Para(sentences: LogicalTyp) extends TheoryTyp
  
}
