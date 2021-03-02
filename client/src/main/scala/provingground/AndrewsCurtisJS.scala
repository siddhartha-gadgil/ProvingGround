package provingground

import org.scalajs.dom
import scalatags.JsDom.all._

import learning._



import andrewscurtis._

import SimpleAcEvolution._

import upickle.default._
import scala.math.Ordering.Double.TotalOrdering


object AndrewsCurtisJS {
  def readPath(s: String) = read[PickledPath](s).unpickle

  import dom.ext._

  // import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow

  val sse = new dom.EventSource("/acstream")

  val debugDiv = div.render

  def debug(mess: String) = debugDiv.appendChild(h4(mess).render)

  sse.onmessage = (event: dom.MessageEvent) => {
    //    debug(event.data.toString)
    val (header, message) = read[(String, String)](event.data.toString)
    //      debug(header)
    //    val header = fdMVP
    //    val message = event.data.toString
    header match {
      case fdMVP => {
        val (pmfM, pmfV, pmfP) =
          read[(List[(String, Double)],
                List[(String, Double)],
                List[(String, Double)])](message)
        val output = pmfMVPdiv(pmfM, pmfV, pmfP)
        fdOut(output)
      }
    }
  }

  def pmfMVPdiv(pmfM: List[(String, Double)],
                pmfV: List[(String, Double)],
                pmfP: List[(String, Double)]) = {
    div(h3("Distribution on Moves"),
        pmfDiv(pmfM),
        h3("Distribution on Vertices"),
        pmfDiv(pmfV),
        h3("Distribution on Presentations"),
        pmfDiv(pmfP)).render
  }

  def pmfDiv(fd: List[(String, Double)]) = {
    val lst = fd.sortBy((x) => -x._2).zipWithIndex
    val title = div(`class` := "atom")(
      span(`class` := "index")("index"),
      span(`class` := "probability")("probability"),
      span(`class` := "entropy")("entropy"),
      span(`class` := "element")("element"))

    val nodeList = for (((a, x), j) <- lst)
      yield
        (
          div(`class` := "atom")(
            span(`class` := "index")(j),
            span(`class` := "probability")(x),
            span(`class` := "entropy")(-math.log(x) / math.log(2)),
            span(`class` := "element")(a.toString)
          )
        )
    div(`class` := "finite-distribution")(title, div(nodeList: _*)).render
  }

  def getPMF(pickled: String) = read[List[(String, Double)]](pickled)

  def getFDtriple(pickled: String) =
    read[(List[(String, Double)],
          List[(String, Double)],
          List[(String, Double)])](pickled)

  lazy val dashboard = div(`class` := "dashboard")(h3("Dashboard"),
                                                   div(b("rank: "), rankBox),
                                                   p(""),
                                                   div(b("steps: "), stepsBox),
                                                   p(""),
                                                   div(evolveButton)).render

  lazy val output =
    div(`class` := "output")(h3("Ouput"), debugDiv, fdOutDiv).render

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
    val post    = write((interface.Header.evolve, message))
    Ajax.post("/acquery", post)
  }

  val rankBox = input(`type` := "number", size := 4, value := "2").render

  val stepsBox = input(`type` := "number", size := 4, value := "2").render

  def getRank = rankBox.value.toInt

  def getSteps = stepsBox.value.toInt

  def runEvolve = postEvolve(getRank, getSteps)

  val evolveButton =
    input(`type` := "submit", value := "Start evolution").render

  evolveButton.onclick = (e: dom.Event) => {
    fdOut(div(p("output?")).render)
    runEvolve
  }
}
