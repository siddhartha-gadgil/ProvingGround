package provingground

import scala.util.parsing.combinator._

import scala.util._

object LatexParser{
  /*
   * Parse latex expressions. 
   * Should include latex commands: ^ _ \frac \limits etc
   * Should also extend the various kinds of symbols.
   */
  class LateXTokenizer extends JavaTokenParsers{
    def letter: Parser[String] = "[a-zA-Z]".r
    def command: Parser[String] = """\[a-zA-Z]+""".r
    def symbol : Parser[String] =""""[!\^\+-=\*/\|:;,\.<>\()\[]]\{}"""".r
    def escapedSymbol: Parser[String] = """\"""~>"""[#@&\$]""".r
    def leftbracket: Parser[String]= """"[\(\[\{<\|]|\langle|\vert"""".r
    def rightbracket: Parser[String]= """"[)]\|]|\rangle"""".r
    def separator: Parser[String]= """[,;:\|]|\\""".r
    def bigop : Parser[String] = "\\Sigma" | "\\Pi" |"\\sum" | "\\prod"
    def binrel : Parser[String] = "=" | "<" | ">" | "\\leq" | "\\geq" | "\\le" | "\\ge" | "\\neq"
    def binop : Parser[String] = "+" | "-" | "*" |"/" | "\\" | "#" |"&" | "|"
    def unop : Parser[String] = "-" | "#"
  }
}
