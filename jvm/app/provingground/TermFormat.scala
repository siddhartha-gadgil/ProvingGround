package provingground

import HoTT._

object TermFormat {
  implicit class StringFormat(syms: TermSyms) extends TermRec[String] {
    val specialTerms: PartialFunction[Term, String] = Map()
    
    def fromString(str: String): String = str

    def appln(func: String, arg: String): String = func+ "(" + arg +")" 

    def arrow(dom: String, codom: String): String = dom + syms.Arrow + codom

    def lambda(variable: String, value: String): String = variable + syms.MapsTo + value

    def pi(fibre: String): String = syms.Pi + fibre

    def sigma(fibre: String): String = syms.Sigma + fibre

    def plus(first: String, scnd: String): String = first+"+" + scnd
    
    def pair(first: String, second: String) = "(" + first + " , "+ second + ")"

    def symbobj(term: SymbObj[Term]): String = term.name.toString +" : "+ this(term.typ)

    def symbtyp(typ: SymbTyp): String = typ.toString + " : _"
  }
}