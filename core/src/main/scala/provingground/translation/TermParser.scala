package provingground.translation

import scala.util.parsing.combinator._

import scala.util._

import provingground.HoTT._

/**
 * @author gadgil
 * Simple parser to reverse the toString operation.
 *
 */
class TermParser extends JavaTokenParsers {
  def arrow: Parser[Any] = UnicodeSyms.Arrow | SimpleSyms.Arrow
  def mapsto: Parser[Any] = UnicodeSyms.MapsTo | SimpleSyms.MapsTo
  def univ: Parser[Any] = UnicodeSyms.UnivSym | SimpleSyms.UnivSym
  def prod: Parser[Any] = UnicodeSyms.Pi | SimpleSyms.Pi
  def sigma: Parser[Any] = UnicodeSyms.Sigma | SimpleSyms.Sigma
  def colon: Parser[String] = ":"

  def makeSymbol(s: String): AnySym =
    if (s endsWith "_1") LeftProjSym(makeSymbol(s.dropRight(2)))
    else if (s endsWith "_2") RightProjSym(makeSymbol(s.dropRight(2)))
    else Name(s)

  def name: Parser[AnySym] = "[a-zA-Z0-9!@#$%^&*()_+-]+".r ^^ { makeSymbol(_) }

  def symbTyp: Parser[Typ[Term]] = name <~ colon ~ univ ^^ (SymbTyp(_, 0))

  def arrowTyp: Parser[Typ[Term]] =
    "(" ~> typ ~ ")" ~ arrow ~ "(" ~ typ <~ ")" ^^ {
      case dom ~ _ ~ _ ~ _ ~ codom => dom ->: codom
    }

  def piTyp: Parser[Typ[Term]] = prod ~ "(" ~> term <~ ")" ^^ {
    case fmly: Func[u, _] => PiDefn(fmly.asInstanceOf[Func[u, Typ[Term]]])
  }

  def sigmaTyp: Parser[Typ[Term]] = sigma ~ "(" ~> term <~ ")" ^^ {
    case fmly: Func[w, _] =>
      SigmaTyp(fmly.asInstanceOf[Func[w, Typ[Term]]])
  }

  def typ: Parser[Typ[Term]] = symbTyp | arrowTyp | uni

  def uni: Parser[Typ[Typ[Term]]] = univ ^^ { _ =>
    Type
  }

  def symbTerm: Parser[Term] = name ~ colon ~ "(" ~ typ <~ ")" ^^ {
    case nm ~ _ ~ _ ~ typ => typ.symbObj(nm)
  }

  def pair: Parser[Term] = "(" ~> term ~ ")" ~ "," ~ "(" ~ term <~ ")" ^^ {
    case a ~ _ ~ _ ~ _ ~ b => mkPair(a, b)
  }

  def term: Parser[Term] = typ | lambdaTerm | appln | symbTerm

  def lambdaTerm: Parser[Term] =
    "(" ~> term ~ ")" ~ mapsto ~ "(" ~ term <~ ")" <~ colon ~ typ ^^ {
      case variable ~ _ ~ _ ~ _ ~ value => lambda(variable)(value)
    }

  def appln: Parser[Term] = "(" ~> term ~ ")" ~ "(" ~ term <~ ")" ^^ {
    case (fn: FuncLike[u, v]) ~ _ ~ _ ~ arg => fn(arg.asInstanceOf[u])
  }
}
