package provingground

import scala.scalajs.js
import org.scalajs.dom
import dom.html.{Map => DomMap, _}
import scalajs.js.annotation.JSExport
import scalatags.JsDom.all._
import scala.util.Try

//import FansiShow._

import scala.concurrent._

import upickle.default._

import scala.concurrent.ExecutionContext.Implicits.global

import org.scalajs.dom.ext._

import HoTT._

//import FreeExprLang.{readTerm, readDist}


object ProvingGroundJS extends js.JSApp {
  def main(): Unit = {
    val page = Try(dom.document.getElementById("page"))
      .map(_.textContent)
      .getOrElse("default")

    page match {
      case "andrews-curtis" => AndrewsCurtisJS.andrewscurtisJS()
      case "default" => JsTest.jstest()
    }

//      JsTest.jstest()
  }

  lazy val jsDiv = dom.document.getElementById("jsdiv")

  def insertDiv(div: Div) =
    jsDiv.appendChild(div)

  def insertDiv(futDiv: Future[Div]) =
    futDiv.foreach(jsDiv.appendChild(_))

  def welcome = div("Dynamic view started")


  @JSExport
  def dummyUpdate() = {
    val jsDiv = dom.document.getElementById("dummy-space")
    jsDiv.appendChild(div("updating without query").render)
    Ajax.get("../../data/dummy").onSuccess{ case xhr =>
        jsDiv.appendChild(
        div(xhr.responseText).render
      )
    }
  }

  def fdView(fd: Vector[(String, String, Double)]) = {
    def row(wt: (String, String, Double)) =
      tr(td(wt._1), td(wt._2), td(-math.log(wt._3)))

    val rows = fd map (row)

    div(
        table(
            caption("Entropies of terms"),
            thead(
                tr(th("Term"), th("Type"), th("Entropy"))
                ),
            tbody(rows: _*)
                )
            ).render
  }



  @JSExport
  def showFD() = {
    val jsDiv = dom.document.getElementById("finite-distribution")
    Ajax.get("../terms-data").onSuccess{
      case xhr =>
        jsDiv.appendChild(
        fdView(read[Vector[(String, String, Double)]](xhr.responseText))
      )
      }
  }

  @JSExport
  def dynamic() : Unit = {

  val jsElems =
    dom.document.getElementsByClassName("js-element") map (_.asInstanceOf[
          Element])

    val script: Map[String, Element] = Map("welcome" -> welcome.render)

  jsElems foreach ((elem) => {
        elem.innerHTML = ""
        elem.appendChild(script(elem.getAttribute("data-script")))
        })
      }
}
