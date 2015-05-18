package provingground

import java.io._
import java.awt.Desktop

import scala.xml._
import scala.language.implicitConversions


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
    val page = <html><body>{body}</body></html>
    writeFile(page.toString, fileName)
    val file = new File(fileName)
    desktop.browse(file.toURI)  
  }
  
  def view(ps : Node*) = 
     viewPage(<div>{NodeSeq.fromSeq(ps.toSeq)}</div>)
  
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
    val nodeList = for ((Weighted(a, x), j) <- lst)
      yield (<div class="atom">
				<span class="index"> {j} </span>
				<span class="element"> {a} </span>
				<span class ="probalility"> {x} </span>
				<span class ="entropy"> {-math.log(x)/math.log(2)} </span>
				</div>)
   <div class="finite-distribution"> {NodeSeq.fromSeq(nodeList)} </div>
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
}