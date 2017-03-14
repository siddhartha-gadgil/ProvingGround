package provingground

import scala.xml._

//import play.api.libs.json._
import play.api.Play.current
import play.api.libs.ws._
import scala.concurrent.Future

import play.api.libs.EventSource

import play.api.libs.concurrent.Execution.Implicits._

object WikiRead {
  def getXML(url: String) = WS.url(url).get().map { response =>
    response.xml
  }

  val baseUrl = "http://en.wikipedia.org"

  case class MathPage(title: String, url: String) {
    val isList =
      title.containsSlice("List of") && title.containsSlice("topics")
  }

  def masterpage =
    MathPage("Lists of mathematics topics",
             baseUrl + "/wiki/Lists_of_mathematics_topics")

  def mathPages(nodes: NodeSeq) =
    for (node <- nodes; a <- (node \ "a"); h <- a \ "@href" if !(h.text
                                                 .startsWith("#"))) yield
      MathPage(a.text, h.text)

  def nextgen(pgsfut: Future[Seq[MathPage]]) =
    pgsfut map (_ filter (_.isList)) flatMap
    ((pgs) =>
          Future.sequence(pgs map
              ((pg) => getXML(pg.url) map ((xml) => mathPages(xml))))) map
    (_.flatten)

  def saveAll(pgsfut: Future[Seq[MathPage]])(implicit save: MathPage => Unit) = {
    pgsfut.onSuccess {
      case pgs => (pgs filter (!_.isList)).foreach(save)
    }
  }

  val topPagesFut = getXML(masterpage.url) map (mathPages(_))

  val otherPagesFut = nextgen(topPagesFut)

  val masterlist = for (nodeseq <- getXML(
      baseUrl + "/wiki/Lists_of_mathematics_topics")) yield (nodeseq \\ "li")

  def savePages(implicit save: MathPage => Unit) = {
    saveAll(topPagesFut)
    saveAll(otherPagesFut)
  }

  object test {
    implicit val save: MathPage => Unit = println(_)
  }
}
