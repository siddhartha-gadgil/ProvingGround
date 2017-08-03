package provingground

import scala.scalajs.js
import org.scalajs.dom
//import dom.html.{Map => _, _}
import scalajs.js.annotation.JSExport
import scalatags.JsDom.all._
import scalatags.JsDom.all
//import scala.util.Try
import scalatags.JsDom.svgTags._
import scalatags.JsDom.svgAttrs._
import scalatags.JsDom.svgAttrs
//import scalatags.JsDom.svgTags

import js.Dynamic.{global => g}

import org.scalajs.dom.ext._

//import scala.concurrent._

import upickle.default._

import scala.concurrent.ExecutionContext.Implicits.global

import org.scalajs.dom.ext._

@JSExport
object DeducerJS {
  def katex(f: String) = {
    val d = span.render
    d.innerHTML = g.katex.renderToString(f.replace("$", "")).toString
    d
  }

  def fdView(fd: Vector[(String, String, Double)]) = {
    def row(wt: (String, String, Double)) = {
      tr(td(katex(wt._1)), td(katex(wt._2)), td(-math.log(wt._3)))
    }
    val rows = fd sortBy ((t) => 1 - t._3) take (100) map (row)

    div(
      table(caption("Entropies of terms"),
            thead(tr(th("Term"), th("Type"), th("Entropy"))),
            tbody(rows: _*))).render
  }

  @JSExport
  def main() = {
    val jsDiv = dom.document.getElementById("jsdiv")

    val plotDiv = div(all.style := "float: left;").render

    val info = div.render

    val selected = div.render

    val refresh = input(all.`type` := "button", value := "Refresh").render

    //    val cons = div(refresh, info, selected).render

    val tableDiv = div(all.width := 1000,
                       all.height := 250,
                       all.style := "overflow-y: auto;").render

    var yScale = 15

    var xScale = 5

    def yc(y: Double) = (y * yScale).toInt // upside down plot

    def xc(x: Double) = (xScale * x).toInt

    val xcBox = input(all.`type` := "number", value := 15).render

    val ycBox = input(all.`type` := "number", value := 20).render

    val queryDiv =
      div(span("x-scale:"), xcBox, span("; y-scale:"), ycBox).render

    jsDiv.appendChild(
      div(h1("Dynamics and Learning for deduction"),
          plotDiv,
          selected,
          div(all.style := "clear: both;"),
          refresh,
          info,
          queryDiv,
          h2("Entropies of Terms"),
          tableDiv).render)

    def svgLines(lines: List[(String, Vector[Double])]) =
      for ((label, points)       <- lines;
           ((val1, val2), index) <- (points zip points.tail).zipWithIndex)
        yield {
          line(onclick := { () =>
            {
              info.innerHTML = ""
              info.appendChild(div("Element: ",
                                   katex(label),
                                   "Entropy: ",
                                   points.last).render)
            }
          })(onfocus := { () =>
            {
              info.innerHTML = ""
              info.appendChild(katex(label))
            }
          })(x1 := xc(index.toDouble),
             y1 := yc(val1),
             x2 := xc(index.toDouble + 1),
             y2 := yc(val2),
             stroke := "blue",
             strokeWidth := 3).render
        }

    def showFD() = {
      val plot = svg(svgAttrs.width := 1000, svgAttrs.height := 250).render

      plotDiv.innerHTML = ""

      plotDiv.appendChild(plot)

      plot.appendChild(
        rect(x := 0,
             y := 0,
             svgAttrs.width := 1000,
             svgAttrs.height := 400,
             fill := "grey",
             fillOpacity := "0.1").render)

      Ajax.get("../terms-data").onSuccess {
        case xhr =>
          tableDiv.appendChild(
            fdView(read[Vector[(String, String, Double)]](xhr.responseText)))
      }
      Ajax.get("../terms-time-series").onSuccess {
        case xhr =>
          val tsList = read[List[(String, Vector[Double])]](xhr.responseText)
          svgLines(tsList).foreach((elem) => plot.appendChild(elem))
          val items =
            tsList.sortBy(_._2.last).reverse map ((lv) => katex(lv._1)) map
              (li(_))
          val list = ol(items: _*).render
          // tsList sortBy (_._2.last) foreach (
          //   (lv) =>
          //     selected.appendChild(
          //       div(katex(lv._1)).render
          //     ))
          selected.innerHTML = ""
          selected.appendChild(list)
      }
    }
    showFD()

    refresh.onclick = (e: dom.Event) => showFD()

    xcBox.onchange = (ev: dom.Event) => {
      xScale = xcBox.value.toInt
      showFD()
    }

    ycBox.onchange = (ev: dom.Event) => {
      yScale = ycBox.value.toInt
      showFD()
    }
  }
}
