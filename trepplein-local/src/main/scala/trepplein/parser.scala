package trepplein

import org.parboiled2._

import scala.collection.mutable
import scala.util.{Failure, Success}

sealed trait ExportFileCommand
case class ExportedModification(modification: Modification)
    extends ExportFileCommand
case class ExportedNotation(notation: Notation) extends ExportFileCommand

private class TextExportParser {
  val name: mutable.ArrayBuffer[Name]   = mutable.ArrayBuffer[Name]()
  val level: mutable.ArrayBuffer[Level] = mutable.ArrayBuffer[Level]()
  val expr: mutable.ArrayBuffer[Expr]   = mutable.ArrayBuffer[Expr]()

  name += Name.Anon
  level += Level.Zero

  def write[T](b: mutable.ArrayBuffer[T], i: Int, t: T, default: => T): Unit =
    b.size match {
      case `i` => b += t
      case s if s < i =>
        b += default
        write(b, i, t, default)
      case s if s > i =>
        b(i) = t
    }
}

private class LineParser(val textExportParser: TextExportParser,
                         val input: ParserInput)
    extends Parser {
  import textExportParser._

  def int: Rule1[Int] = rule {
    capture(oneOrMore(CharPredicate.Digit)) ~> ((x: String) => x.toInt)
  }
  def long: Rule1[Long] = rule {
    capture(oneOrMore(CharPredicate.Digit)) ~> ((x: String) => x.toLong)
  }

  def rest: Rule1[String] = rule { capture(zeroOrMore(ANY)) }

  def restNums: Rule1[Seq[Int]] = rule { zeroOrMore(" " ~ int) }

  def binderInfo: Rule1[BinderInfo] =
    rule {
      "#BD" ~ push(BinderInfo.Default) |
        "#BI" ~ push(BinderInfo.Implicit) |
        "#BC" ~ push(BinderInfo.InstImplicit) |
        "#BS" ~ push(BinderInfo.StrictImplicit)
    }

  def nameRef: Rule1[Name]   = rule { int ~> name }
  def levelRef: Rule1[Level] = rule { int ~> level }
  def exprRef: Rule1[Expr]   = rule { int ~> expr }

  def nameDef: Rule1[Name] =
    rule {
      "#NS " ~ nameRef ~ " " ~ rest ~> Name.Str |
        "#NI " ~ nameRef ~ " " ~ long ~> Name.Num
    }

  def levelDef: Rule1[Level] =
    rule {
      "#US " ~ levelRef ~> Level.Succ |
        "#UM " ~ levelRef ~ " " ~ levelRef ~> (Level.Max(_, _)) |
        "#UIM " ~ levelRef ~ " " ~ levelRef ~> Level.IMax |
        "#UP " ~ nameRef ~> Level.Param
    }

  def exprDef: Rule1[Expr] =
    rule {
      "#EV " ~ int ~> Var |
        "#ES " ~ levelRef ~> ((l: Level) => Sort(l)) |
        "#EC " ~ nameRef ~ restNums ~> ((n,
                                         ls) =>
                                          Const(n, ls.map(level).toVector)) |
        "#EA " ~ exprRef ~ " " ~ exprRef ~> App |
        "#EL " ~ binderInfo ~ " " ~ nameRef ~ " " ~ exprRef ~ " " ~ exprRef ~> (
            (b,
             n,
             t,
             e) => Lam(Binding(n, t, b), e)) |
        "#EP " ~ binderInfo ~ " " ~ nameRef ~ " " ~ exprRef ~ " " ~ exprRef ~> (
            (b,
             n,
             t,
             e) => Pi(Binding(n, t, b), e)) |
        "#EZ " ~ nameRef ~ " " ~ exprRef ~ " " ~ exprRef ~ " " ~ exprRef ~> (
            (n: Name,
             t: Expr,
             v: Expr,
             b: Expr) => Let(Binding(n, t, BinderInfo.Default), v, b))
    }

  def notationDef: Rule1[Notation] =
    rule {
      "#INFIX " ~ nameRef ~ " " ~ int ~ " " ~ rest ~> ((n: Name,
                                                        p: Int,
                                                        text: String) =>
                                                         Infix(n, p, text)) |
        "#POSTFIX " ~ nameRef ~ " " ~ int ~ " " ~ rest ~> (
            (n: Name,
             p: Int,
             text: String) => Postfix(n, p, text)) |
        "#PREFIX " ~ nameRef ~ " " ~ int ~ " " ~ rest ~> ((n: Name,
                                                           p: Int,
                                                           text: String) =>
                                                            Prefix(n, p, text))
    }

  def univParams: Rule1[Vector[Level.Param]] = rule {
    restNums ~> ((ps: Seq[Int]) => ps.view.map(name).map(Level.Param).toVector)
  }

  def modification: Rule1[Modification] =
    rule {
      "#AX " ~ nameRef ~ " " ~ exprRef ~ univParams ~> (
          (n,
           t,
           ps) => AxiomMod(Axiom(n, ps, t))) |
        "#DEF " ~ nameRef ~ " " ~ exprRef ~ " " ~ exprRef ~ univParams ~> (
            (n,
             t,
             v,
             ps) => DefMod(Definition(n, ps, t, v))) |
        "#QUOT" ~ push(QuotMod) |
        "#IND " ~ int ~ " " ~ nameRef ~ " " ~ exprRef ~ " " ~ int ~ restNums ~> parseInd _
    }

  def parseInd(numParams: Int,
               n: Name,
               t: Expr,
               numIntros: Int,
               rest: Seq[Int]): IndMod = {
    val (intros, ps) = rest.splitAt(2 * numIntros)
    IndMod(InductiveType(n, ps.view.map(name).map(Level.Param).toVector, t),
           numParams,
           intros
             .grouped(2)
             .map { case Seq(in, it) => (name(in), expr(it)) }
             .toVector)
  }

  def line: Rule1[Option[ExportFileCommand]] =
    rule {
      (int ~ " " ~ (nameDef ~> { (i: Int, n: Name) =>
        write(name, i, n, Name.Anon); None
      } |
        exprDef ~> { (i: Int, e: Expr) =>
          write(expr, i, e, Sort(0)); None
        } |
        levelDef ~> { (i: Int, l: Level) =>
          write(level, i, l, Level.Zero); None
        }) |
        notationDef ~> ((x: Notation) => Some(ExportedNotation(x))) |
        modification ~> ((x: Modification) => Some(ExportedModification(x)))) ~ EOI
    }
}

object TextExportParser {
  def parse(lines: Stream[String]): Stream[ExportFileCommand] = {
    val parser = new TextExportParser
    lines.flatMap { l =>
      val lineParser = new LineParser(parser, l)
      lineParser.line.run() match {
        case Success(mods) => mods
        case Failure(error: ParseError) =>
          throw new IllegalArgumentException(lineParser.formatError(error))
        case Failure(ex) => throw ex
      }
    }
  }

  def parseFile(fn: String): Stream[ExportFileCommand] =
    parse(io.Source.fromFile(fn).getLines().toStream)
}
