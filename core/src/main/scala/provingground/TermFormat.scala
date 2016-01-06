package provingground

import HoTT._

import UnicodeSyms.UnivSym

object TermFormat {
  implicit class StringFormat(syms: TermSyms) extends TermRec[String] {
    val specialTerms: PartialFunction[Term, String] = Map()

    def fromString(str: String)(implicit typ: Typ[Term]): String = str

    def appln(func: String, arg: String): String = func+ "(" + arg +")"

    def arrow(dom: String, codom: String): String = dom + syms.Arrow + codom

    def lambda(variable: String, value: String): String = variable + syms.MapsTo + value

    def equality(dom: String, lhs: String, rhs: String) = s"$lhs = $rhs (in $dom)"

    def pi(fibre: String): String = syms.Pi + fibre

    def sigma(fibre: String): String = syms.Sigma + fibre

    def plus(first: String, scnd: String): String = first+"+" + scnd

    def pair(first: String, second: String) = "(" + first + " , "+ second + ")"

    def symbobj(term: SymbObj[Term]): String = term.name.toString +" : "+ this(term.typ)

    def symbtyp(typ: SymbTyp): String = typ.toString + " : _"

    def symbolic(name: AnySym, typ: Typ[Term]): String = s"$name : ${this(typ)}"

    def univ(n: Int) = s"${UnivSym}"
  }
}
