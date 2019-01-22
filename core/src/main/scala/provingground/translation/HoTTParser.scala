package provingground.translation

import provingground._
import HoTT._
import HoTTParser._
import fastparse._
import monix.eval.Task
import provingground.scalahott.NatRing
import spire.math.SafeLong

import scala.collection.immutable

object HoTTParser {
  sealed trait Stat

  case class Expr(term: Term) extends Stat

  case class Defn(name: String, value: Term) extends Stat

  case class Import(name: String) extends Stat

  case class Block(stats: Vector[Stat]) {
    def +:(s: Stat) = Block(s +: stats)

    def valueOpt: Option[Term] =
      stats.lastOption.flatMap {
        case Expr(t)    => Some(t)
        case Defn(_, t) => Some(t)
        case Import(_)  => None
      }
  }
}

case class HoTTParser(ctx: Context = Context.Empty,
                      contextMap: Map[String, Context] = Map()) { self =>
  import ctx.namedTerms
  import fastparse._, SingleLineWhitespace._

  def +(n: String, t: Term) = HoTTParser(ctx.defineSym(Name(n), t), contextMap)

  def +(dfn: Defn) =
    HoTTParser(ctx.defineSym(Name(dfn.name), dfn.value), contextMap)

  def +(exp: Expr) = HoTTParser(ctx.introduce(exp.term), contextMap)

  def addImport(s: String) = HoTTParser(ctx ++ contextMap(s), contextMap)

  def predefs[_: P]: P[Term] =
    P("Type").map((_) => Type: Term) |
      P("Star").map((_) => Star: Term) |
      P("Unit").map((_) => Unit: Term) |
      P("Zero").map((_) => Zero: Term) |
      P("Prop").map((_) => Prop: Term)

  def named[_: P]: P[Term] =
    namedTerms.foldLeft[P[Term]](predefs) {
      case (parser, (name, term)) => P(parser | P(name).map((_) => term))
    }

  def alphachar[_: P] = CharIn("A-Z", "a-z", "$", "@", "_")

  def alphanum[_: P] = alphachar | CharIn("0-9")

  def str[_: P]: P[String] =
    (alphachar.! ~ P(alphanum.rep.!)).map {
      case (h, t) => h + t.toString
    }

  def name[_: P]: P[String] =
    P("\"" ~ str ~ "\"")

  def symbolic[_: P]: P[Term] = P(name ~ P("::") ~ term).map {
    case (s, t) => s :: toTyp(t)
  }

  def parens[_: P]: P[Term] = P("(" ~ term ~ ")")

  def simpleterm[_: P]: P[Term] = P(parens | named)

  def lmbdaP[_: P]: P[Term] =
    (P(
      "lmbda" ~ "(" ~ term ~ ")" ~ "(" ~ term ~ ")"
    ) |
      P(
        simpleterm ~ ":->" ~ term
      )).map { case (x, y) => lmbda(x)(y) }

  def lambdaP[_: P]: P[Term] =
    (P(
      "lambda" ~ "(" ~ term ~ ")" ~ "(" ~ term ~ ")"
    ) |
      P(
        simpleterm ~ ":~>" ~ term
      )).map { case (x, y) => lambda(x)(y) }

  def funcTyp[_: P] =
    P(
      simpleterm ~ "->:" ~ term
    ).map { case (x, y) => toTyp(x) ->: toTyp(y) }

  def plusTyp[_: P] =
    P(
      simpleterm ~ "||" ~ term
    ).map { case (x, y) => toTyp(x) || toTyp(y) }

  def prodTyp[_: P] =
    P(
      simpleterm ~ "&&" ~ term
    ).map { case (x, y) => toTyp(x) && toTyp(y) }

  def piTyp[_: P] =
    P(
      simpleterm ~ "~>:" ~ term
    ).map { case (x, y) => x ~>: toTyp(y) }

  def sigmaTyp[_: P] =
    P(
      simpleterm ~ "&:" ~ term
    ).map { case (x, y) => x &: toTyp(y) }

  def applnP[_: P]: P[Term] =
    P(simpleterm ~ "(" ~ term ~ ")").map {
      case (f, x) => applyFunc(f, x)
    }

  def polyApplnP[_: P] =
    P(simpleterm ~ ("(" ~ term ~ ")").rep(1)).map {
      case (f, xs) => xs.foldLeft(f)(applyFunc)
    }

  def recP[_: P]: P[Term] =
    P(simpleterm ~ ".rec" ~ "(" ~ term ~ ")").map {
      case (tp, x) => ctx.inducStruct.recOpt(tp, toTyp(x)).get
    }

  def inducP[_: P]: P[Term] =
    P(simpleterm ~ ".induc" ~ "(" ~ term ~ ")").map {
      case (tp, x) => ctx.inducStruct.inducOpt(tp, x).get
    }

  import spire.implicits._, spire.math._

  def num[_: P] = CharIn("0-9").rep(1).!.map {
    case n => NatRing.Literal(n.toInt: SafeLong)
  }

  def term[_: P]: P[Term] =
    P(symbolic | lmbdaP | lambdaP | polyApplnP | funcTyp | piTyp | prodTyp | plusTyp | sigmaTyp | recP | inducP | simpleterm | num)

  def break[_: P] =
    P(spc ~ (End | CharIn("\n;"))) | P(
      "//" ~ CharPred(_ != '\n').rep ~ ("\n" | End))

  def spc[_: P]: P[Unit] = CharIn(" \t").rep

  def defn[_: P] =
    P(spc ~ "val" ~ str ~ "=" ~ term ~ break).map {
      case (n, t) => Defn(n, t)
    }

  def imp[_: P] = P(spc ~ "import" ~ str ~ break).map(Import(_))

  def expr[_: P] = (spc ~ term ~ break).map(Expr)

  def stat[_: P]: P[Stat] = defn | expr | imp

  def block[_: P]: P[Block] =
    P(spc ~ "//" ~ CharPred(_ != '\n').rep ~ "\n" ~ block) |
      (spc ~ "//" ~ CharPred(_ != '\n').rep ~ End).map((_) => Block(Vector())) |
      (spc ~ End).map((_) => Block(Vector())) |
      defn.flatMap((dfn) =>
        (self + dfn).block.map { (tail) =>
          dfn +: tail
      }) |
      P(stat ~ block ~ End).map { case (s, v) => s +: v } |
      P(spc ~ "\n" ~ block)

  def context[_: P]: P[Context] =
    P(spc ~ "//" ~ CharPred(_ != '\n').rep ~ "\n" ~ context) |
      (spc ~ "//" ~ CharPred(_ != '\n').rep ~ End).map((_) => ctx) |
      (spc ~ End).map((_) => ctx) |
      defn.flatMap((dfn) => (self + dfn).context) |
      imp.flatMap((s) => addImport(s.name).context) |
      expr.flatMap((dfn) =>
        (self + dfn).context.map { (tail) =>
          tail //.defineSym(Name(dfn.name), dfn.value)
      }) |
//      P(defn ~ context ~ End).map { case (dfn, ct) => ct.defineSym(Name(dfn.name), dfn.value)} |
//      P(expr ~ context ~ End).map { case (exp, ct) => ct.introduce(exp.term)} |
      P(spc ~ "\n" ~ context)

  def parseContext(txt: String) = parse(txt, context(_))

  def parseBlock(txt: String) = parse(txt, block(_))

}
