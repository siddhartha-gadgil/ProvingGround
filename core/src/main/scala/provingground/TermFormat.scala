package provingground

import HoTT._

import UnicodeSyms.UnivSym

object TermFormat {}

object PrintFormat extends TermRec[String] {
  val syms = UnicodeSyms

  val specialTerms: PartialFunction[Term, String] = Map()

  def fromString(str: String)(implicit typ: Typ[Term]): String = str

  def appln(func: String, arg: String): String = func + "(" + arg + ")"

  def arrow(dom: String, codom: String): String = dom + " " + syms.Arrow +" "+ codom

  def lambda(variable: String, typ: String, value: String): String =
    s"($variable : $typ) ${syms.MapsTo}  $value"

  def equality(dom: String, lhs: String, rhs: String) =
    s"$lhs = $rhs (in $dom)"

  def pi(fibre: String): String = s"${syms.Pi}($fibre)"

  def sigma(fibre: String): String = s"${syms.Sigma}($fibre)"

  def plus(first: String, scnd: String): String = first + " + " + scnd

  def pair(first: String, second: String) =
    "(" + first + " , " + second + ")"

  def symbobj(term: SymbObj[Term]): String =
    term.name.toString

  def symbtyp(typ: SymbTyp): String = typ.name.toString

  def symbolic(name: AnySym, typ: Typ[Term]): String =
    name.toString

  def univ(n: Int) = s"${UnivSym}"
}

object FansiFormat extends TermRec[fansi.Str]{
  import fansi.Str
  import fansi.Color._

  val syms = UnicodeSyms

  val specialTerms: PartialFunction[Term, Str] = Map()

  def fromString(str: String)(implicit typ: Typ[Term]): Str = Str(str)

  def appln(func: Str, arg: Str): Str = func ++ "(" ++ arg ++ ")"

  def arrow(dom: Str, codom: Str): Str = dom ++ " " ++ LightRed(syms.Arrow) ++" "++ codom

  def lambda(variable: Str, typ: Str, value: Str): Str =
    Str("(") ++ variable ++ Yellow(" : ") ++ typ ++") "++ LightRed(syms.MapsTo) ++ " " ++value

  def equality(dom: Str, lhs: Str, rhs: Str) =
    lhs ++ LightRed(" = ") ++ rhs++" (in "++dom++")"

  def pi(fibre: Str): Str = Cyan(Str(syms.Pi))++LightYellow("(")++fibre++LightYellow(")")

  def sigma(fibre: Str): Str = Cyan(Str(syms.Sigma))++LightYellow("(")++fibre++LightYellow(")")

  def plus(first: Str, scnd: Str): Str = first ++ LightRed(Str(" + ")) ++ scnd

  def pair(first: Str, second: Str) =
    Str("(") ++ first ++ " , " ++ second ++ ")"

  def symbobj(term: SymbObj[Term]): Str =
    Green(Str(term.name.toString))

  def symbtyp(typ: SymbTyp): Str = Green(Str(typ.name.toString))

  def symbolic(name: AnySym, typ: Typ[Term]): Str =
    Green(Str(name.toString))

  def univ(n: Int) = LightGreen(Str(UnivSym))


}
