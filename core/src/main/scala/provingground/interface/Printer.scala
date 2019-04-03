package provingground.interface

import provingground._
import HoTT._
import pprint.PPrinter
import provingground.translation.FansiShow._
import monix.execution.Scheduler.Implicits.global
import monix.eval._

trait Printer[A]{
  def viewLines(a: A, lines: Int): String
}

trait FallbackPrinter{
  implicit def fallbackPrinter[A] : Printer[A] = Printer.simple((a) => a.toString)
}

object Printer extends FallbackPrinter{
  def simple[A](fn: A => String) : Printer[A] = new Printer[A] {
    override def viewLines(a: A, lines: Int): String = fn(a)
  }

  def apply[A](fn: (A, Int) => String): Printer[A] = new Printer[A]{
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

  implicit val doublePrint : Printer[Double] = Printer.simple(_.toString)

  implicit def pairPrint[A, B](implicit p1: Printer[A], p2: Printer[B]) : Printer[(A, B)] =
    Printer{case ((a, b), l) => s"(${p1.viewLines(a, l)}, ${p2.viewLines(b, l)})"}


  implicit def termPrint[U<: Term with Subs[U]] : Printer[U] = Printer.simple(t => t.fansi)

  implicit def vecPrint[A](implicit p : Printer[A]) : Printer[Vector[A]] = new Printer[Vector[A]] {
    override def viewLines(a: Vector[A], lines: Int): String = {
      val out = if (lines < 0) a.map(p.viewLines(_, lines)) else a.take(lines).map(p.viewLines(_, lines))
      out.mkString("\n")
    }
  }

  implicit def fdPrint[A](implicit p: Printer[A]): Printer[FiniteDistribution[A]] = new Printer[FiniteDistribution[A]] {
    override def viewLines(a: FiniteDistribution[A], lines: Int): String =
      view(a.entropyVec.map{case Weighted(x, p) => x -> p}, lines)
  }
}

trait Display[A]{
  def display(a: A, lines: Int): Unit

  def pretty(a: A, lines: Int): Unit
}

object Display extends FallbackDisplay {


  def display[A](a: A, lines: Int = 20)(implicit d: Display[A]): Unit =
    {
      d.display(a, lines)
      println()
    }

  def pretty[A](a: A, lines: Int = 20)(implicit d: Display[A]): Unit =
    {
      d.pretty(a, lines)
      println()
    }

  implicit def termDisplay[U<: Term]: Display[U] = new Display[U] {
    override def display(a: U, lines: Int): Unit = print(a)

    override def pretty(a: U, lines: Int): Unit = print(a.fansi)
  }

  implicit def vecDisplay[A](implicit d: Display[A]): Display[Vector[A]] = new Display[Vector[A]] {
    override def display(a: Vector[A], lines: Int): Unit =
      if (lines < 0) a.foreach(x => {d.display(x, lines); println()}) else a.take(lines).foreach(x => {d.display(x, lines); println()})

    override def pretty(a: Vector[A], lines: Int): Unit =
      if (lines < 0) a.foreach(x => {d.pretty(x, lines); println()}) else a.take(lines).foreach(x => {d.pretty(x, lines); println()})
  }

  implicit def pairDisplay[A, B](implicit dA: Display[A], dB: Display[B]): Display[(A, B)] = new Display[(A, B)] {
    override def display(a: (A, B), lines: Int): Unit = {
      dA.display(a._1, lines)
      print(" -> ")
      dB.display(a._2, lines)
    }

    override def pretty(a: (A, B), lines: Int): Unit = {
      dA.pretty(a._1, lines)
      print(" -> ")
      dB.pretty(a._2, lines)
    }
  }

  implicit def numDisplay[A: Numeric] : Display[A] = new Display[A] {
    override def display(a: A, lines: Int): Unit = print(a)

    override def pretty(a: A, lines: Int): Unit = print(a)
  }

  implicit def taskDisplay[A](implicit d: Display[A]) : Display[Task[A]] = new Display[Task[A]] {
    override def display(ta: Task[A], lines: Int): Unit =
      {
        pprint.log("Running task and displaying result")
        ta.foreach(a => d.display(a, lines))
      }

    override def pretty(ta: Task[A], lines: Int): Unit =
      {
        pprint.log("Running task and displaying result")
        ta.foreach(a => d.pretty(a, lines))
      }
  }

  implicit def fdDisplay[A](implicit d: Display[A]): Display[FiniteDistribution[A]] = new Display[FiniteDistribution[A]] {
    override def display(a: FiniteDistribution[A], lines: Int): Unit =
      Display.display(a.entropyVec.map{case Weighted(x, p) => x -> p}, lines)

    override def pretty(a: FiniteDistribution[A], lines: Int): Unit =
      Display.pretty(a.entropyVec.map{case Weighted(x, p) => x -> p}, lines)
  }
}

trait FallbackDisplay{
  val pp: PPrinter = pprint.PPrinter(additionalHandlers = fansiHandler)

  implicit def fallback[A]: Display[A] = new Display[A] {
    override def display(a: A, lines: Int): Unit = print(a)

    override def pretty(a: A, lines: Int): Unit =
      pp.tokenize(a.toString, pp.defaultWidth, pp.defaultHeight, pp.defaultIndent, 0).foreach(print)
  }

  implicit def fromPrinter[A](implicit printer: Printer[A]): Display[A] = new Display[A] {
    def display(a: A, lines: Int): Unit =
      println(printer.viewLines(a, lines))

    def pretty(a: A, lines: Int): Unit =
      pp.tokenize(printer.viewLines(a, lines), pp.defaultWidth, pp.defaultHeight, pp.defaultIndent, 0).foreach(print)
  }
}