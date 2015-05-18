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
        <span class="element"> element </span>
        <span class ="probability"> probability </span>
        <span class ="entropy"> entropy </span>
        </div>
    val nodeList = for ((Weighted(a, x), j) <- lst)
      yield (<div class="atom">
				<span class="index"> {j} </span>
				<span class="element"> {a} </span>
				<span class ="probability"> {x} </span>
				<span class ="entropy"> {-math.log(x)/math.log(2)} </span>
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
  
  val css = """
    .index {
        color: black;
        display: inline-block;
        width: 300px;
    }

    .element {
        color: red;
        display: inline-block;
        width: 300px;
    }
    .probability{
      color: green;
      display: inline-block;
      width: 300px;
      }
    .entropy{
      color: red;
        display: inline-block;
        width: 300px;
      }
    """
  
  val head= 
    <head>
			<style type="text/css">
      {css}
    	</style>
   </head>
    
}