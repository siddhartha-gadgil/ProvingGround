package provingground.translation

import provingground.HoTT._
import HoTTParser._

import scala.collection.immutable

object HoTTParser {
  sealed trait Stat

  case class Expr(term: Term) extends Stat

  case class Defn(name: String, value: Term) extends Stat

  case class Block(stats: Vector[Stat]) {
    def +:(s: Stat) = Block(s +: stats)

    def valueOpt: Option[Term] =
      stats.lastOption.map {
        case Expr(t)    => t
        case Defn(_, t) => t
      }
  }
}

case class HoTTParser(names: Map[String, Term] = Map()) { self =>
  import fastparse._
  val White: WhitespaceApi.Wrapper = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import fastparse.noApi._
  import White._

  def +(n: String, t: Term) = HoTTParser(names + (n -> t))

  def +(dfn: Defn) = HoTTParser(names + (dfn.name -> dfn.value))

  val predefs: P[Term] =
    P("Type").map((_) => Type: Term) |
      P("Star").map((_) => Star: Term) |
      P("Unit").map((_) => Unit: Term) |
      P("Zero").map((_) => Zero: Term) |
      P("Prop").map((_) => Prop: Term)

  val named: P[Term] =
    names.foldRight[P[Term]](predefs) {
      case ((name, term), parser) => P(name).map((_) => term) | parser
    }

  val alphachar
    : immutable.IndexedSeq[Char] = ('a' to 'z') ++ ('A' to 'Z') ++ Seq('$',
                                                                       '@',
                                                                       '_')

  val alphanum: immutable.IndexedSeq[Char] = alphachar ++ ('0' to '9') ++ Seq(
    '.')

  val str: core.Parser[String, Char, String] =
    (P(CharIn(alphachar).!) ~ P(CharIn(alphanum).rep.!)).map {
      case (h, t) => h + t.toString
    }

  val name: P[String] =
    P("\"" ~ str ~ "\"")

  val symbolic: P[Term] = P(name ~ P("::") ~ term).map {
    case (s, t) => s :: toTyp(t)
  }

  val parens: P[Term] = P("(" ~ term ~ ")")

  val lmbdaP: P[Term] =
    (P(
      "lmbda" ~ "(" ~ term ~ ")" ~ "(" ~ term ~ ")"
    ) |
      P(
        simpleterm ~ ":->" ~ term
      )).map { case (x, y) => lmbda(x)(y) }

  val lambdaP: P[Term] =
    (P(
      "lambda" ~ "(" ~ term ~ ")" ~ "(" ~ term ~ ")"
    ) |
      P(
        simpleterm ~ ":~>" ~ term
      )).map { case (x, y) => lmbda(x)(y) }

  val funcTyp = P(
    simpleterm ~ "->:" ~ term
  ).map { case (x, y) => toTyp(x) ->: toTyp(y) }

  val piTyp = P(
    simpleterm ~ "~>:" ~ term
  ).map { case (x, y) => x ~>: toTyp(y) }

  val applnP = P(simpleterm ~ "(" ~ term ~ ")").map {
    case (f, x) => applyFunc(f, x)
  }

  val simpleterm: P[Term] = P(parens | named)

  val term: P[Term] = P(
    symbolic | lmbdaP | lambdaP | applnP | funcTyp | piTyp | simpleterm)

  val break = P(spc ~ (End | CharIn("\n;"))) | P(
    "//" ~ CharPred(_ != '\n').rep ~ ("\n" | End))

  val spc = CharIn(" \t").rep

  val defn = P(spc ~ "val" ~ str ~ "=" ~ term ~ break).map {
    case (n, t) => Defn(n, t)
  }

  val expr = (spc ~ term ~ break).map(Expr)

  val stat: P[Stat] = defn | expr

  val block: P[Block] =
    P(spc ~ "//" ~ CharPred(_ != '\n').rep ~ "\n" ~ block) |
      (spc ~ "//" ~ CharPred(_ != '\n').rep ~ End).map((_) => Block(Vector())) |
      (spc ~ End).map((_) => Block(Vector())) |
      defn.flatMap((dfn) =>
        (self + dfn).block.map { (tail) =>
          dfn +: tail
      }) |
      P(stat ~ block ~ End).map { case (s, v) => s +: v } |
      P(spc ~ "\n" ~ block)
}
