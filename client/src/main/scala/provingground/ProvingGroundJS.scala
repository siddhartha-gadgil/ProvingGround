package provingground

import scala.scalajs.js
import org.scalajs.dom
import dom.html.{Map => _, _}
import scalajs.js.annotation.JSExport
import scalatags.JsDom.all._
import scala.util.Try
import scalatags.JsDom.svgTags._
import scalatags.JsDom.svgAttrs._
import scalatags.JsDom.svgAttrs
import scalatags.JsDom.svgTags

import js.Dynamic.{ global => g }

//import FansiShow._

import scala.concurrent._

import upickle.default._

import scala.concurrent.ExecutionContext.Implicits.global

import org.scalajs.dom.ext._

//import HoTT._

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

  def katex(f : String) = {
    val d = span.render
    d.innerHTML = g.katex.renderToString(f.replace("$", "")).toString
    d
  }

  def fdView(fd: Vector[(String, String, Double)]) = {
    def row(wt: (String, String, Double)) = {
  //    val tdiv = div.render
  //    tdiv.innerHTML = g.katex.renderToString(wt._1.replace("$", "")).toString
      tr(td(katex(wt._1)), td(katex(wt._2)), td(-math.log(wt._3)))
}
    val rows = fd sortBy((t) => 1 - t._3)  take (100) map (row)

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


  def yc(y: Double) = (300 - (y * 30)).toInt

  def xc(x: Double) = (30 * x).toInt

  def svgLines(lines: List[(String, Vector[Double])]) =
    for (
      (label, points) <-lines;
      ((val1, val2), index) <- (points zip points.tail).zipWithIndex
    ) yield {
        val cons = dom.document.getElementById("time-series-console")
        line(onclick := {() => {
          cons.innerHTML = ""
          cons.appendChild(div("Element: ", katex(label), "Entropy: ", points.last).render)
        }})(onfocus := {() => {
          cons.innerHTML = ""
          cons.appendChild(katex(label))
        }})(
          x1 := xc(index), y1 := yc(val1), x2 := xc(index + 1), y2 := yc(val2),
          stroke := "red", strokeWidth := 4).render


    }
  def svgGroup(lines: List[(String, Vector[Double])]) = {
    val group = svgTags.g.render
    group.innerHTML = "<title> Hover </title>"
    svgLines(lines).foreach((elem) => group.appendChild(elem))
    group
  }

  def svgLabels(lines: List[(String, Vector[Double])]) =
    for (
      (label, points) <-lines
    )  yield text(x := xc(points.size), y := yc(points.last),
      label.replace("$", ".")).render

  @JSExport
  def showFD() = {
    val jsDiv = dom.document.getElementById("finite-distribution")
    val svg = dom.document.getElementById("time-series")
    svg.appendChild(
      rect(
        x:= 0, y :=0,
        svgAttrs.width := 1000, svgAttrs.height := 400,
        fill := "blue", fillOpacity := "0.1").render
    )
    Ajax.get("../terms-data").onSuccess{
      case xhr =>
        jsDiv.appendChild(
        fdView(read[Vector[(String, String, Double)]](xhr.responseText))
      )
      }
      Ajax.get("../terms-time-series").onSuccess{
        case xhr =>
          val tsList = read[List[(String, Vector[Double])]](xhr.responseText)
          svgLines(tsList).foreach((elem) => svg.appendChild(elem))
          val cons = dom.document.getElementById("time-series-console")
          tsList sortBy (_._2.last) foreach (
            (lv) =>
              cons.appendChild(
                span(katex(lv._1), ";").render
              ))
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
