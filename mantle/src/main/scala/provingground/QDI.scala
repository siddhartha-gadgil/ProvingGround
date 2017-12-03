package provingground.interface
import provingground._

import translation._

import java.io._
import java.awt.Desktop

import ammonite.ops

import scala.xml._
import scala.language.implicitConversions

import scala.concurrent._
import ExecutionContext.Implicits.global

import scala.annotation._

import StringParse._

import com.github.nscala_time.time.Imports._

object AmmRepl extends App {
  amm()
}

object amm {
  def apply(): Unit = {
    val initCommands =
      """import provingground._, HoTT._, induction._, ammonite.ops._, translation.FansiShow._; repl.pprinter.bind(fansiPrint)"""
    ammonite.Main(s"$initCommands").run()

  }
}

/**
  * @author gadgil
  */
object QDI {
  def writeFD(fd: FiniteDistribution[String],
              filename: String,
              dir: String = "data") = {
    val file = ops.pwd / dir / filename
    ops.rm(file)
    fd.pmf.foreach { case Weighted(s, p) => ops.write(file, s"$s\t$p\n") }
  }

  def readFD(filename: String, dir: String = "data") = {
    val file = ops.pwd / dir / filename
    val pmf =
      ops.read.lines(file) map
        ((l) => {
          val Array(s, p) = l.split("\t")
          Weighted(s, p.toDouble)
        })
    FiniteDistribution(pmf)
  }

  def runFor[A](f: A => A, init: A, duration: Long, save: (A, Int) => Unit) = {
    val start = System.currentTimeMillis()
    var steps = 0
    var state = init
    while (System.currentTimeMillis() < start + duration) {
      val next = f(state)
      steps += 1
      state = next
      save(state, steps)
    }
    state
  }

  def runFor[A](f: A => A, init: A, duration: Long) = {
    val start = System.currentTimeMillis()
    var steps = 0
    var state = init
    while (System.currentTimeMillis() < start + duration) {
      val next = f(state)
      steps += 1
      state = next
    }
    state
  }

  def runForFut[A](f: A => A, init: A, duration: Long, save: (A, Int) => Unit) =
    Future(runFor(f, init, duration, save))

  def runForFut[A](f: A => A, init: A, duration: Long) =
    Future(runFor(f, init, duration))

  lazy val runTime = java.lang.Runtime.getRuntime()

  def freeMem = runTime.freeMemory()

  def maxMem = runTime.maxMemory

  def totalMem = runTime.totalMemory

  def gc = runTime.gc

  def timed[A](result: => A) = {
    val start       = DateTime.now
    val computation = result
    println((start to (DateTime.now)).millis)
    computation
  }

  lazy val desktop = Desktop.getDesktop

  def datafile = DateTime.now.toString.replace(":", "_") + ".dat"

  def writeFile(text: String, fileName: String, append: Boolean = false) = {
    val writer = new FileWriter(fileName, append)
    writer.write(text)
    writer.close
  }

  def viewPage(body: Node, fileName: String = "tmp/qdi.html") = {
    val page = <html>{ head }<body>{ body }</body></html>
    writeFile(page.toString, fileName)
    val file = new File(fileName)
    desktop.browse(file.toURI)
  }

  def view(ps: Node*) = {
    val fileName = "tmp/qdi.html"
    val page     = <html>{ head }<body>{ NodeSeq.fromSeq(ps) }</body></html>
    writeFile(page.toString, fileName)
    val file = new File(fileName)
    desktop.browse(file.toURI)
  }

  def p(s: String): Node = <p> { s } </p>

  trait WebView[A] {
    def asXML(a: A): Node
  }

  implicit def toXML[A: WebView](a: A): Node = {
    implicitly[WebView[A]].asXML(a)
  }

  implicit def listView[A: WebView]: WebView[List[A]] = new WebView[List[A]] {
    def asXML(as: List[A]) = {
      val l = as map ((a) => implicitly[WebView[A]].asXML(a))
      <div>{ NodeSeq.fromSeq(l) }</div>
    }
  }

  import FiniteDistribution._

  implicit def fdDiv[A](fd: FiniteDistribution[A]): Node = {
    val lst      = fd.pmf.toList.sortBy((x) => -x.weight).zipWithIndex
    val title    = <div class="atom">
                  <span class="index"> index </span>
                  <span class="probability"> probability </span>
                  <span class="entropy"> entropy </span>
                  <span class="element"> element </span>
                </div>
    val nodeList = for ((Weighted(a, x), j) <- lst) yield (<div class="atom">
                                                             <span class="index"> { j } </span>
                                                             <span class="probability"> { x } </span>
                                                             <span class="entropy"> { -math.log(x) / math.log(2) } </span>
                                                             <span class="element"> { a } </span>
                                                           </div>)
    <div class="finite-distribution"> { NodeSeq.fromSeq(title +: nodeList) } </div>
  }

  implicit def fdListDiv[A](fds: List[FiniteDistribution[A]]): Node = {
    val lst = fds.last.pmf.toList.sortBy((x) => -x.weight).zipWithIndex
    def entropies(a: A) =
      for (fd <- fds)
        yield (<span class="entropy">{ -math.log(fd(a)) / math.log(2) }</span>)
    val nodeList = for ((Weighted(a, x), j) <- lst)
      yield (<div class="atom-list">
               <span class="index"> { j } </span>
               <span class="element"> { a } </span>
               NodeSeq.fromSeq(entropies(a))
             </div>)
    <div class="finite-distribution"> { NodeSeq.fromSeq(nodeList) } </div>
  }

  implicit def fdString[A](
      fd: FiniteDistribution[A]): _root_.scala.Predef.String =
    tableString(fdListList(fd))

  implicit def fdListList[A](
      fd: FiniteDistribution[A]): _root_.scala.collection.immutable.List[
    _root_.scala.collection.immutable.List[_root_.scala.Any]] = {
    for (Weighted(x, p) <- fd.pmf.toList) yield List(x, p)
  }

  def row(xs: List[String]) = {
    def cls(i: Int) = if (i % 2 == 0) "even" else "odd"
    val spans = {
      for ((x, i) <- xs.zipWithIndex) yield <span class={ cls(i) }>{ x }</span>
    }
    <div class="row">{ NodeSeq.fromSeq(spans) }</div>
  }

  implicit def tableDiv(xy: List[List[Any]]): _root_.scala.xml.Elem = {
    val rows = xy map ((r) => row(r map (_.toString)))
    <div class="table">{ NodeSeq.fromSeq(rows) }</div>
  }

  implicit def tableString(xy: List[List[Any]]): String = {
    val rows = xy map ((r) => (r map ((x) => s"""$x""")).mkString(","))
    rows.mkString(";")
  }

  trait Logger {
    def put(x: String): Unit

    def log[A: WriteString](a: A) = put(write(a))

    var closed: Boolean = false

    def get: Option[Iterator[String]]

    def close: Unit
  }

  class MemLog extends Logger {
    var mem: Vector[String] = Vector()

    def put(x: String) = (mem = mem :+ x)

    def get = if (closed) Some(mem.toIterator) else None

    def close = { closed = true }
  }

  class FileLog(filename: String, append: Boolean = false) extends Logger {
    val fl = new FileWriter(filename, append)

    val writer = new BufferedWriter(fl)

    def put(x: String) = { writer.write(x); writer.newLine }

    def close = { closed = true; writer.close }

    def get = if (closed) Some(readFile(filename)) else None
  }

  def readFile(filename: String) = scala.io.Source.fromFile(filename).getLines

  implicit def tableWrite[A]: WriteString[List[List[A]]] =
    new WriteString[List[List[A]]] {
      def writer(ll: List[List[A]]) = tableString(ll)
    }

  implicit def fdWrite[A]: WriteString[FiniteDistribution[A]] =
    new WriteString[FiniteDistribution[A]] {
      def writer(fd: FiniteDistribution[A]) = fdString(fd)
    }

  implicit def tableRead: ReadString[List[List[String]]] =
    new ReadString[List[List[String]]] {
      def reader(str: String) = {
        val tokens = str.split("\"").toList.tail
        def recread(ss: List[String],
                    accum: List[List[String]] = Nil): List[List[String]] = {
          if (ss.length == 0) accum
          else {
            val token :: sep :: tail = ss
            sep match {
              case "," => (token :: accum.head) :: accum.tail
              case ";" => List(token) :: accum
            }
          }
        }
        recread(tokens)
      }
    }

  implicit def fdRead: ReadString[List[String]] =
    new ReadString[List[String]] {
      def reader(str: String) = {
        val table = StringParse.read[List[List[String]]](str)
        def rec(t: List[List[String]]): List[String] = {
          if (table == List()) List()
          else t.head(0) :: t.head(1) :: rec(t.tail)
        }
        rec(table)
      }
    }

  @tailrec
  def iterLog[A](init: A, dyn: A => A, steps: Int, logger: Logger): A = {
    logger.log(init.toString)
    if (steps < 1) init
    else iterLog(dyn(init), dyn, steps - 1, logger)
  }

  def asyncIterLog[A](init: A, dyn: A => A, steps: Int, logger: Logger) =
    Future(iterLog[A](init: A, dyn: A => A, steps: Int, logger: Logger))

  def run[A](init: A, dyn: A => A, steps: Int, threads: Int = 6) = {
    (1 to threads) map { (j) =>
      val logger = new FileLog(s"data/$datafile-$j")
      asyncIterLog(init, dyn, steps, logger)
    }
  }

  val css = """
    .index {
        color: black;
        display: inline-block;
        width: 50px;
    }

    .element {
        color: red;
        display: inline-block;
        width: 400px;
    }
    .probability{
      color: green;
      display: inline-block;
      width: 250px;
      }
    .entropy{
      color: blue;
        display: inline-block;
        width: 250px;
      }
    .odd{
      color: red;
      display: inline-block;
      width: 200px;
      }
    .even{
      color: blue;
      display: inline-block;
      width: 200px;
      }
    """

  val head = <head>
               <style type="text/css">
                 { css }
               </style>
             </head>
}
