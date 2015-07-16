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

  import Header._
  import scala.scalajs
              .concurrent
              .JSExecutionContext
              .Implicits
              .runNow

  val sse= new dom.EventSource("/acstream")

  sse.onmessage = (event: dom.MessageEvent) => {
      val (header, message) = read[(String, String)](event.data.toString)
      header match {
        case fdMVP => {
          val (pmfM, pmfV, pmfP) = read[
            (List[(String, Double)],
                List[(String, Double)],
                List[(String, Double)])](message)
          val output = pmfMVPdiv(pmfM, pmfV, pmfP)
          fdOut(output)
        }
      }
    }

  def pmfMVPdiv(
      pmfM : List[(String, Double)],
      pmfV : List[(String, Double)],
      pmfP : List[(String, Double)]) = {
    div(
        h3("Distribution on Moves"),
        pmfDiv(pmfM),
        h3("Distribution on Vertices"),
        pmfDiv(pmfV),
        h3("Distribution on Presentations"),
        pmfDiv(pmfP)
        ).render
  }

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

  lazy val dashboard = div(`class`:= "dashboard")(
      h3("Dashboard"),
      div(b("rank:"), rankBox),
      div(b("steps:"), stepsBox),
      div(evolveButton)).render

  lazy val output = div(`class` := "output")(h3("Ouput"), fdOutDiv).render

  lazy val fdOutDiv = div.render

  def fdOut(d: dom.html.Div) = {
    fdOutDiv.innerHTML = ""
    fdOutDiv.appendChild(d)
  }

  def andrewscurtisJS() = {
    val jsdiv = dom.document.getElementById("jsdiv")
    jsdiv.appendChild(h2("Andrews-Curtis interface coming here").render)
    jsdiv.appendChild(div(dashboard, output).render)
  }
  
  def postEvolve(rank: Int, steps: Int) = {
    val message = write((rank, steps))
    val post = write((Header.evolve, message))
    Ajax.post("/acquery", post)
  }
  
  val rankBox = input(
        `type`:="number",
        value := "2"
        ).render
        
  val stepsBox = input(
        `type`:="number",
        value := "2"
        ).render
  
  def getRank = rankBox.value.toInt
  
  def getSteps = stepsBox.value.toInt
  
  def runEvolve = postEvolve(getRank, getSteps)
  
  val evolveButton = input(`type` := "submit", value := "Start evolution").render
  
  evolveButton.onclick = (e: dom.Event) => {
    fdOut(div(p("output?")).render)
    runEvolve}
}
