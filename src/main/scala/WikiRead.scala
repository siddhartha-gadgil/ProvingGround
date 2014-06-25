package provingGround

import scala.xml._
import play.api.libs.ws._
import scala.concurrent.Future
import play.api.libs.json._

import play.api.libs.EventSource

import play.api.libs.concurrent.Execution.Implicits._




object WikiRead{
  def getXML(url: String) = WS.url(url).get().map{
    response =>
    	response.xml 
}
  
  val baseUrl = "http://en.wikipedia.org"
    
  case class MathPage(title: String, url: String){
	  val isList = title.containsSlice("List of") && title.containsSlice("topics")
  } 
    
  val masterlist = for (nodeseq <- getXML(baseUrl+"/wiki/Lists_of_mathematics_topics")) yield (nodeseq \\ "li")
  
  def mathPages(nodes: NodeSeq) = {
        for(node <- nodes; a <- (node \ "a"); h <- a \ "@href" if !(h.text.startsWith("#"))) yield MathPage(a.text,  h.text) 
    } 
  
  val toppagesFut = masterlist map(mathPages(_))
  
  val listListsFut = toppagesFut map (_ filter (_.isList)) /*flatMap {(pgs) => 
    Future.sequence(pgs map ((pg) => getXML(pg.url)))} */
  
  def savepages(save: NodeSeq => Unit) = toppagesFut onSuccess{case (pgs: Seq[MathPage]) =>
    for (pg <- pgs) yield (getXML(pg.url).onSuccess{case xml => save(xml)})
    
    listListsFut.onSuccess{
      case pgs => pgs map {(pg)=>
        getXML(pg.url).onSuccess{case xml => save(xml)}
        }
    }
  }
}