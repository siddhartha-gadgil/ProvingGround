package provingground

import scala.scalajs.js
import org.scalajs.dom
import dom.html
import scalajs.js.annotation.JSExport
import scalatags.JsDom.all._

import Collections._

import HoTT._

import upickle.default._

object AndrewsCurtisJS{
  import dom.ext._

  import scala.scalajs
              .concurrent
              .JSExecutionContext
              .Implicits
              .runNow

  val sse= new dom.EventSource("/acstream")

  def pmfDiv(fd: List[(String, Double)]) = {
    val lst = fd.sortBy((x) => -x._2).zipWithIndex
    val title = div(`class`:="atom")(
        span(`class`:="index")("index"),
        span(`class`:="probability")("probability"),
        span(`class`:="entropy")("entropy"),
        span(`class`:="element")("element")
        )

    val nodeList = for (((a, x), j) <- lst)
      yield (
          div(`class`:="atom")(
        span(`class`:="index")(j),
        span(`class`:="probability")(x),
        span(`class`:="entropy")(-math.log(x)/math.log(2)),
        span(`class`:="element")(a.toString)
        )
        )
   div(`class`:="finite-distribution")(title,
       div(nodeList : _*)).render

  }

  def getPMF(pickled: String) = read[List[(String, Double)]](pickled)

  def getFDtriple(pickled: String) =
    read[(List[(String, Double)], List[(String, Double)],
        List[(String, Double)])](pickled)

  def andrewscurtisJS() = {
    val jsdiv = dom.document.getElementById("jsdiv")
    jsdiv.appendChild(h2("Andrews-Curtis interface coming here").render)
  }
}
