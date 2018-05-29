package provingground.translation

import scala.util.parsing.combinator._

import scala.util._

import provingground.HoTT._

import HoTTParser._

object HoTTParser{
  sealed trait Stat

  case class Expr(term: Term) extends Stat

  case class Defn(name: String, value: Term) extends Stat

  case class Block(stats: Vector[Stat]){
    def +:(s: Stat) = Block(s +: stats)

    def valueOpt : Option[Term] =
      stats.lastOption.flatMap{
        case Expr(t) => Some(t)
        case _ => None
      }
  }
}

case class HoTTParser(names: Map[String, Term] = Map()){self =>
  import fastparse._
  val White = WhitespaceApi.Wrapper{
  import fastparse.all._
  NoTrace(" ".rep)
  }
  import fastparse.noApi._
  import White._

  def +(n: String, t: Term) = HoTTParser(names + (n -> t))

  def +(dfn: Defn) = HoTTParser(names + (dfn.name -> dfn.value))

  val predefs : P[Term] =
    P("Type").map((_) => Type : Term) |
    P("Star").map((_) => Star : Term) |
    P("Unit").map((_) => Unit : Term) |
    P("Zero").map((_) => Zero : Term) |
    P("Prop").map((_) => Prop : Term)

  val named : P[Term] =
    names.foldRight[P[Term]](predefs){
      case ((name, term), parser) => P(name).map((_) => term) | parser
    }

  val alphachar = ('a' to 'z') ++ ('A' to 'Z') ++ Seq('$', '@', '_')

  val alphanum = alphachar ++ ('0' to '9') ++ Seq('.')

  val str = (P(CharIn(alphachar).!)~P(CharsWhileIn(alphanum).?.!)).map{case (h, t) => h + t}

  val name : P[String] =
    P("\"" ~ str ~ "\"")

  val symbolic: P[Term] = P(name~P("::")~term).map{case (s, t) => s :: toTyp(t)}

  val parens : P[Term] = P("(" ~ term ~ ")")

  val lmbdaP : P[Term] =
    (P(
      "lmbda"~ "("~ term ~")" ~ "(" ~ term ~")"
    ) |
    P(
      simpleterm ~ ":->" ~ term
    )
    ).map{case (x, y) => lmbda(x)(y)}

  val lambdaP : P[Term] =
    (P(
      "lambda"~ "("~ term ~")" ~ "(" ~ term ~")"
    ) |
    P(
      simpleterm ~ ":~>" ~ term
    )
    ).map{case (x, y) => lmbda(x)(y)}

  val funcTyp = P(
    simpleterm ~ "->:" ~ term
  ).map{case (x, y) => toTyp(x) ->: toTyp(y)}

  val piTyp = P(
    simpleterm ~ "->:" ~ term
  ).map{case (x, y) => x ~>: toTyp(y)}

  val applnP = P(simpleterm ~ "(" ~term ~")").map{case (f, x) => applyFunc(f, x)}

  val simpleterm : P[Term] = P(parens | named)

  val term : P[Term] = P(symbolic | lmbdaP | lambdaP | applnP | funcTyp | piTyp | simpleterm)

  val break = P(spc ~ (End | CharIn("\n;")))

  val spc = CharIn(" \t").rep

  val defn = P(spc ~ "val"~ str ~ "=" ~ term ~ break).map{case (n, t) => Defn(n, t)}

  val expr = (spc ~ term~break).map(Expr(_))

  val stat: P[Stat] = defn | expr

  val block: P[Block] =
    (spc ~ End).map((_) => Block(Vector())) |
    defn.flatMap((dfn) =>
      (self + dfn).block.map{(tail) => dfn +: tail}
    ) |
    P(stat~block~End).map {case (s, v) => s +: v} |
    P(spc ~ "\n" ~ block)
}
/*
val leftTag = P( "<" ~ (!">" ~ AnyChar).rep(1).! ~ ">" )
def rightTag(s: String) = P( "</" ~ s.! ~ ">" )
val xml = P( leftTag.flatMap(rightTag) )

val Parsed.Success("a", _) = xml.parse("<a></a>")
val Parsed.Success("abcde", _) = xml.parse("<abcde></abcde>")

val failure = xml.parse("<abcde></edcba>").asInstanceOf[Parsed.Failure]
assert(
  failure.extra.traced.trace == """xml:1:1 / rightTag:1:8 / "abcde":1:10 ..."edcba>""""
)
*/

/*
Json
// Here is the parser
val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")

val space         = P( CharsWhileIn(" \r\n").? )
val digits        = P( CharsWhileIn("0123456789"))
val exponent      = P( CharIn("eE") ~ CharIn("+-").? ~ digits )
val fractional    = P( "." ~ digits )
val integral      = P( "0" | CharIn('1' to '9') ~ digits.? )

val number = P( CharIn("+-").? ~ integral ~ fractional.? ~ exponent.? ).!.map(
  x => Js.Num(x.toDouble)
)

val `null`        = P( "null" ).map(_ => Js.Null)
val `false`       = P( "false" ).map(_ => Js.False)
val `true`        = P( "true" ).map(_ => Js.True)

val hexDigit      = P( CharIn('0'to'9', 'a'to'f', 'A'to'F') )
val unicodeEscape = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
val escape        = P( "\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape) )

val strChars = P( CharsWhile(StringChars) )
val string =
  P( space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Js.Str)

val array =
  P( "[" ~/ jsonExpr.rep(sep=",".~/) ~ space ~ "]").map(Js.Arr(_:_*))

val pair = P( string.map(_.value) ~/ ":" ~/ jsonExpr )

val obj =
  P( "{" ~/ pair.rep(sep=",".~/) ~ space ~ "}").map(Js.Obj(_:_*))

val jsonExpr: P[Js.Val] = P(
  space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space
)

*/

/**
  * @author gadgil
  * Simple parser to reverse the toString operation.
  *
  */
class TermParser extends JavaTokenParsers {
  def arrow: Parser[Any]    = UnicodeSyms.Arrow | SimpleSyms.Arrow
  def mapsto: Parser[Any]   = UnicodeSyms.MapsTo | SimpleSyms.MapsTo
  def univ: Parser[Any]     = UnicodeSyms.UnivSym | SimpleSyms.UnivSym
  def prod: Parser[Any]     = UnicodeSyms.Pi | SimpleSyms.Pi
  def sigma: Parser[Any]    = UnicodeSyms.Sigma | SimpleSyms.Sigma
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
