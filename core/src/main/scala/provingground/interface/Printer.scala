package provingground.interface

import provingground._
import HoTT._
import pprint.PPrinter
import provingground.translation.FansiShow._
import monix.execution.Scheduler.Implicits.global
import monix.eval._
import ujson._

import scala.collection.mutable.ArrayBuffer

trait Printer[A] {
  def viewLines(a: A, lines: Int): String
}

trait FallbackPrinter {
  implicit def fallbackPrinter[A]: Printer[A] =
    Printer.simple((a) => a.toString)
}

object Printer extends FallbackPrinter {
  def simple[A](fn: A => String): Printer[A] = new Printer[A] {
    override def viewLines(a: A, lines: Int): String = fn(a)
  }

  def apply[A](fn: (A, Int) => String): Printer[A] = new Printer[A] {
    override def viewLines(a: A, lines: Int): String = fn(a, lines)
  }

  def view[A](a: A, lines: Int = 20)(implicit printer: Printer[A]): String =
    printer.viewLines(a, lines)

  def display[A](a: A, lines: Int = 20)(implicit printer: Printer[A]): Unit =
    println(printer.viewLines(a, lines))

  val pp = pprint.PPrinter(additionalHandlers = fansiHandler)

  def pretty[A](a: A, lines: Int = -1)(implicit printer: Printer[A]): Unit =
    pp.pprintln(printer.viewLines(a, lines))

  implicit val strPrint: Printer[String] = Printer.simple(identity)

  implicit val doublePrint: Printer[Double] = Printer.simple(_.toString)

  implicit def pairPrint[A, B](
      implicit p1: Printer[A],
      p2: Printer[B]
  ): Printer[(A, B)] =
    Printer {
      case ((a, b), l) => s"(${p1.viewLines(a, l)}, ${p2.viewLines(b, l)})"
    }

  implicit def termPrint[U <: Term]: Printer[U] =
    Printer.simple(t => t.fansi)

  implicit def vecPrint[A](implicit p: Printer[A]): Printer[Vector[A]] =
    (a: Vector[A], lines: Int) => {
      val out =
        if (lines < 0) a.map(p.viewLines(_, lines))
        else a.take(lines).map(p.viewLines(_, lines))
      out.mkString("\n")
    }

  implicit def fdPrint[A](
      implicit p: Printer[A]
  ): Printer[FiniteDistribution[A]] =
    (a: FiniteDistribution[A], lines: Int) => view(a.pmfVec)

  import provingground.learning.GeneratorVariables._

  implicit def equationPrint: Printer[Equation] =
    Printer.simple(eq => s"${view(eq.lhs)} = ${view(eq.rhs)}")

  implicit def eqTermPrint: Printer[EquationTerm] =
    Printer.simple(eq => s"${view(eq.lhs)} = ${view(eq.rhs)} + ...")

  def exprView(expr: Expression): String = expr match {
    case value: VarVal[_] =>
      value match {
        case FinalVal(variable)   => s"p_f(${view(variable)})"
        case InitialVal(variable) => s"p_0(${view(variable)})"
      }
    case Log(exp)       => s"log(${exprView(exp)})"
    case Exp(exp)       => s"exp(${exprView(exp)})"
    case Sum(x, y)      => s"${exprView(x)} + ${exprView(y)}"
    case Product(x, y)  => s"${exprView(x)} * ${exprView(y)}"
    case Literal(value) => view(value)
    case Quotient(x, y) => s"${exprView(x)} / ${exprView(y)}"
    case Coeff(node, rv) =>
      s"coefficient(node = ${view(node)}, variable = ${view(rv)})"
    case IsleScale(boat, elem) =>
      s"isle_scale(boat = ${view(boat)}, element = ${view(elem)})"
  }

  implicit def jsPrinter[U <: ujson.Value]: Printer[U] = new Printer[U] {
    def viewValue(js: ujson.Value, lines: Int): String =
      js match {
        case Str(value) => value
        case Obj(value) =>
          value
            .map {
              case (title, v) =>
                s"$title:\n${viewValue(v, lines)}\n"
            }
            .mkString("\n")
        case Arr(value) =>
          val arr = if (lines < 0) value else value.take(lines)
          arr.map(viewValue(_, lines)).mkString("\n")
        case Num(value) => value.toString
        case bool: Bool => bool.toString()
        case Null       => ""
      }

    override def viewLines(a: U, lines: Int): String =
      viewValue(a, lines)
  }
}

trait Display[A] {
  def display(a: A, lines: Int): Unit

  def pretty(a: A, lines: Int): Unit
}

object Display extends FallbackDisplay {

  def display[A](a: A, lines: Int = 20)(implicit d: Display[A]): Unit = {
    d.display(a, lines)
    println()
  }

  def fansi[A](a: A, lines: Int = 20)(implicit d: Display[A]): Unit = {
    d.pretty(a, lines)
    println()
  }

  implicit def taskDisplay[A](implicit d: Display[A]): Display[Task[A]] =
    new Display[Task[A]] {
      override def display(ta: Task[A], lines: Int): Unit = {
        pprint.log("Running task and displaying result")
        ta.foreach(a => d.display(a, lines))
      }

      override def pretty(ta: Task[A], lines: Int): Unit = {
        pprint.log("Running task and displaying result")
        ta.foreach(a => d.pretty(a, lines))
      }
    }

}

trait FallbackDisplay {
  val pp: PPrinter = pprint.PPrinter(additionalHandlers = fansiHandler)

  implicit def fromPrinter[A](implicit printer: Printer[A]): Display[A] =
    new Display[A] {
      def display(a: A, lines: Int): Unit =
        println(printer.viewLines(a, lines))

      def pretty(a: A, lines: Int): Unit =
        pp.tokenize(
            printer.viewLines(a, lines),
            pp.defaultWidth,
            pp.defaultHeight,
            pp.defaultIndent,
            0
          )
          .foreach(print)
    }
}
