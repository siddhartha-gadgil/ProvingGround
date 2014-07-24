package provingground

import scala.util.parsing.combinator._
import provingground.Logic._ 

object LogicParser extends JavaTokenParsers{
    def sym: Parser[String] = "[a-zA-Z]".r | """\\(\w)+""".r
    def conj: Parser[String] = "&" | "|" | "=>" | "<=>"
    def baseFormula: Parser[Formula] = "("~>formula<~")" | atomFormula
    def atomFormula: Parser[Formula] = sym~"("~repsep(term, ",")~")" ^^ {case p~"("~ts~")" => PredSym(p, ts.length)(ts)}
    def negFormula: Parser[Formula] = "~"~formula ^^ {case "~"~p => !p}
    def conjFormula: Parser[Formula] = baseFormula~conj~formula ^^ {case p~cnj~q => ConjFormula(p, cnj, q)}
	def formula: Parser[Formula] = negFormula | conjFormula | baseFormula
	
	def variable: Parser[Var] = sym ^^ {case name => VarSym(name)}
	def baseTerm: Parser[Term] = intConst | variable | recTerm | "("~>term<~")"
	def binOpSym: Parser[String] ="+" | "-" | "*" | "/" | "|"
	def recTerm: Parser[Term] = sym~"("~repsep(term, ",")~")" ^^ {case p~"("~ts~")" => FuncSym(p, ts.length)(ts)}
	def term: Parser[Term] = baseTerm | baseTerm~binOpSym~term ^^ {case a~m~b => BinOp(m)(a,b)}
	def intConst: Parser[IntConst] = wholeNumber ^^ {case n => IntConst(n.toInt)}
	 
	
	def isWord(word: String): Parser[String] = literal(word)
	def isSomeWord(words: List[String]) = words map (literal(_)) reduce (_ | _)
	}