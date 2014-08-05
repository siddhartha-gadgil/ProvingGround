package worksheets
import provingground.AgdaExpressions._
import scala.util.parsing.combinator._

object AgdaParseTest {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val parser = new AgdaParse                      //> parser  : provingground.AgdaExpressions.AgdaParse = provingground.AgdaExpres
                                                  //| sions$AgdaParse@5a62a404

	import parser._
	
	parseAll(expr, "x")                       //> res0: worksheets.AgdaParseTest.parser.ParseResult[provingground.AgdaExpressi
                                                  //| ons.Expression] = [1.2] parsed: Token(x)
  parseAll(expr, "::")                            //> res1: worksheets.AgdaParseTest.parser.ParseResult[provingground.AgdaExpressi
                                                  //| ons.Expression] = [1.3] parsed: Token(::)
  
  parseAll(To, "->")                              //> res2: worksheets.AgdaParseTest.parser.ParseResult[String] = [1.3] parsed: ->
                                                  //| 
  parseAll(expr, "x -> y")                        //> res3: worksheets.AgdaParseTest.parser.ParseResult[provingground.AgdaExpressi
                                                  //| ons.Expression] = [1.7] parsed: Arrow(Token(x),Token(y))
  parseAll(term, "x")                             //> res4: worksheets.AgdaParseTest.parser.ParseResult[provingground.AgdaExpressi
                                                  //| ons.Expression] = [1.2] parsed: Token(x)
  arrow()                                         //> res5: worksheets.AgdaParseTest.parser.Parser[provingground.AgdaExpressions.E
                                                  //| xpression] = Parser (Parser (~)^^)
  parseAll(arrow(), "x -> y")                     //> res6: worksheets.AgdaParseTest.parser.ParseResult[provingground.AgdaExpressi
                                                  //| ons.Expression] = [1.7] parsed: Arrow(Token(x),Token(y))
  parseAll(expr, "(x -> y)")                      //> res7: worksheets.AgdaParseTest.parser.ParseResult[provingground.AgdaExpressi
                                                  //| ons.Expression] = [1.9] parsed: Arrow(Token(x),Token(y))
	parseAll(token, "::")                     //> res8: worksheets.AgdaParseTest.parser.ParseResult[provingground.AgdaExpressi
                                                  //| ons.Token] = [1.3] parsed: Token(::)
  parseAll(expr, "(x : A)")                       //> res9: worksheets.AgdaParseTest.parser.ParseResult[provingground.AgdaExpressi
                                                  //| ons.Expression] = [1.8] parsed: TypedVar(x,Token(A))
  parseAll(expr, "a -> b -> c")                   //> res10: worksheets.AgdaParseTest.parser.ParseResult[provingground.AgdaExpress
                                                  //| ions.Expression] = [1.12] parsed: Arrow(Token(a),Arrow(Token(b),Token(c)))
  parseAll(expr, "(a : A) :-> a")                 //> res11: worksheets.AgdaParseTest.parser.ParseResult[provingground.AgdaExpress
                                                  //| ions.Expression] = [1.14] parsed: LambdaExp(TypedVar(a,Token(A)),Token(a))
  parseAll(expr, "(a : A) -> a")                  //> res12: worksheets.AgdaParseTest.parser.ParseResult[provingground.AgdaExpress
                                                  //| ions.Expression] = [1.13] parsed: DepArrow(TypedVar(a,Token(A)),Token(a))
	parseAll(appl(), "a (b -> c)")            //> res13: worksheets.AgdaParseTest.parser.ParseResult[provingground.AgdaExpress
                                                  //| ions.Expression] = [1.11] parsed: Apply(Token(a),Arrow(Token(b),Token(c)))
	
	parseAll(expr, "a b")                     //> res14: worksheets.AgdaParseTest.parser.ParseResult[provingground.AgdaExpress
                                                  //| ions.Expression] = [1.4] parsed: Apply(Token(a),Token(b))
	parseAll(expr, "a b c")                   //> res15: worksheets.AgdaParseTest.parser.ParseResult[provingground.AgdaExpress
                                                  //| ions.Expression] = [1.6] parsed: Apply(Token(a),Apply(Token(b),Token(c)))
  asTerm("(x : _) :-> x")                         //> res16: Option[provingground.HoTT.Term] = Some((x?_)
  parseAll(expr, "(x : _) :-> x").get.asTerm((_) => None)
                                                  //> res17: Option[provingground.HoTT.Term] = Some((x?_)
  asTerm("(y : _) :-> (y -> y)")                  //> res18: Option[provingground.HoTT.Term] = Some((y?(_?_))
  
  parseAll(eqlty(), "x -> z = f y")               //> res19: worksheets.AgdaParseTest.parser.ParseResult[provingground.AgdaExpress
                                                  //| ions.Equality] = [1.13] parsed: Equality(Arrow(Token(x),Token(z)),Apply(Toke
                                                  //| n(f),Token(y)))
}