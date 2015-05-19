package provingground

import java.io._
import java.awt.Desktop

import scala.xml._
import scala.language.implicitConversions

import scala.concurrent._
import ExecutionContext.Implicits.global

import scala.annotation._

/**
 * @author gadgil
 */
object QDI {
  lazy val desktop = Desktop.getDesktop
  
  def writeFile(text: String, fileName: String, append: Boolean = false) ={
    val writer = new FileWriter(fileName, append)    
    writer.write(text)
    writer.close
  }
  
  def viewPage(body: Node, fileName: String = "qdi.html") ={
    val page = <html>{head}<body>{body}</body></html>
    writeFile(page.toString, fileName)
    val file = new File(fileName)
    desktop.browse(file.toURI)  
  }
  
  def view(ps : Node*) = {
    val fileName="qdi.html"
    val page = <html>{head}<body>{NodeSeq.fromSeq(ps)}</body></html>
    writeFile(page.toString, fileName)
    val file = new File(fileName)
    desktop.browse(file.toURI) 
  }
     
  
  def p(s: String) : Node = <p> {s} </p>
  
  trait WebView[A]{
    def asXML(a: A): Node
  }
  
  implicit def toXML[A: WebView](a: A) : Node = {
    implicitly[WebView[A]].asXML(a)
  }
  
  implicit def listView[A: WebView] : WebView[List[A]] = new WebView[List[A]]{
    def asXML(as: List[A]) = 
      {val l = as map ((a) => implicitly[WebView[A]].asXML(a))
      <div>{NodeSeq.fromSeq(l)}</div>      
      }
  }
  
  import Collections._
  
  implicit def fdDiv[A](fd: FiniteDistribution[A]) : Node = {
    val lst = fd.pmf.toList.sortBy((x) => -x.weight).zipWithIndex
    val title = <div class="atom">
        <span class="index"> index </span>
        <span class ="probability"> probability </span>
        <span class ="entropy"> entropy </span>
				<span class="element"> element </span>
        </div>
    val nodeList = for ((Weighted(a, x), j) <- lst)
      yield (<div class="atom">
				<span class="index"> {j} </span>
				<span class ="probability"> {x} </span>
				<span class ="entropy"> {-math.log(x)/math.log(2)} </span>
				<span class="element"> {a} </span>
				</div>)
   <div class="finite-distribution"> {NodeSeq.fromSeq(title +: nodeList)} </div>
  }
  
  implicit def fdListDiv[A](fds: List[FiniteDistribution[A]]) : Node = {
    val lst = fds.last.pmf.toList.sortBy((x) => -x.weight).zipWithIndex
    def entropies(a: A) = for (fd <- fds) 
      yield (<span class="entropy">{-math.log(fd(a))/math.log(2)}</span>)
    val nodeList = for ((Weighted(a, x), j) <- lst)
      yield (<div class="atom-list">
        <span class="index"> {j} </span>
        <span class="element"> {a} </span>
				NodeSeq.fromSeq(entropies(a))
        </div>)
   <div class="finite-distribution"> {NodeSeq.fromSeq(nodeList)} </div>
  }
  
  def row(xs: List[String]) = {
    def cls(i: Int) = if (i % 2 == 0) "even" else "odd" 
    val spans = {
      for ((x, i) <- xs.zipWithIndex) yield <span class={cls(i)}>{x}</span>
    }
    <div class="row">{NodeSeq.fromSeq(spans)}</div>
  }
  
  implicit def table(xy: List[List[Any]]) = {
    val rows = xy map ((r) => row(r map (_.toString)))
    <div class="table">{NodeSeq.fromSeq(rows)}</div>
  }
  
  trait Logger{
    def log(x: String): Unit
    
    var closed: Boolean = false
    
    def get: Option[Iterator[String]]
    
    def close: Unit
  }
  
  class MemLog extends Logger{
    var mem : Vector[String] = Vector()
    
    def log(x: String) = (mem = mem :+ x)
    
    def get = if (closed) Some(mem.toIterator) else None
    
    def close = {closed = true}
  }
  
  class FileLog(filename: String, append: Boolean = false) extends Logger{
    val fl = new FileWriter(filename, append)
    
    val writer = new BufferedWriter(fl)

    def log(x: String) = writer.write(x); writer.newLine
    
    def close = closed = true; fl.close
    
    def get = if (closed) Some(readFile(filename)) else None
  }
  
  def readFile(filename: String) = scala.io.Source.fromFile(filename).getLines
  
  @tailrec def iterLog[A](init: A, dyn: A => A, steps: Int, logger: Logger) : A ={
    logger.log(init.toString)
    if (steps <1) init
    else iterLog(dyn(init), dyn, steps, logger) 
  }
  
  def asyncIterLog[A](init: A, dyn: A => A, steps: Int, logger: Logger) = Future(iterLog[A](init: A, dyn: A => A, steps: Int, logger: Logger))
  
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
      color: red;
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
  
  val head= 
    <head>
			<style type="text/css">
      {css}
    	</style>
   </head>
    
}