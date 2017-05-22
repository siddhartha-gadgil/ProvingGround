package provingground

import translation._

import akka.http.scaladsl._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
//import akka.stream.ActorMaterializer
//import scala.io.StdIn
//import akka.actor.ActorSystem
//import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model._

import scala.collection.mutable.{Map => MutMap, Set => MutSet}

import LatexFormat.latex

import HoTT._

import akka.stream.scaladsl._

import akka.stream._

import TermToExpr.encode

//import FansiShow._

import upickle.default._

//import scalatags.Text.all._

object WebServer {

//  implicit val system = Hub.system
//  implicit val materializer = ActorMaterializer()
  import Hub._

  // needed for the future flatMap/onComplete in the end
  implicit val executionContext = system.dispatcher

  private val views: MutMap[String, String] = MutMap()

  private val texts: MutMap[String, String] = MutMap()

  def dummyData = () => s"Dummy data at ${System.currentTimeMillis / 1000}"

  private val data: MutMap[String, () => String] = MutMap()

  def showView(name: String, data: String) = { views(name) = data }

  def showText(name: String, data: String) = { texts(name) = data }

  def showData(name: String, dataVal: () => String) = { data(name) = dataVal }

  def getData(name: String) = data.getOrElse(name, () => "no data")()

  def getView(name: String) = {
    val index = if (name.endsWith(".html")) name.dropRight(5) else name
    val div =
      views.get(index).getOrElse(s"""<h2>No view with index <i>$name</i></h2>
      <div class="js-element" data-script="welcome"></div>
      <script type="text/javascript">
        provingground.ProvingGroundJS().dynamic()
      </script>

      """)
    makePage(div)
  }

  val katex =
    """
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.6.0/katex.min.css">
    <script src="https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.6.0/katex.min.js"></script>
"""

  def makePage(divs: String) =
    s"""
      <!DOCTYPE html>
      <html>
      <head>
      <title>Proving-Ground : Automating theorem proving</title>
      $katex
      </head>
      <body>
      <script type="text/javascript" src="../resource/provingground-js-fastopt.js"></script>
      $divs
      </body>
      </html>
      """

  val fdView = makePage(
    """
    <div id ="jsdiv"></div>
    <script type="text/javascript">
    provingground.DeducerJS().main()
    </script>
    """
  )

  val termsView = {
    val divs =
      """
      <h1> Deduction viewer </h2>
      <h2> Time series for Deduction </h2>
        <svg version="1.1"
          baseProfile="full"
          width="1000" height="300"
          xmlns="http://www.w3.org/2000/svg"
          id="time-series">
          <div id="jsdiv"></div>
          <div id="time-series-console"></div>
          <br>
          <h2> Finite Distribution snapshot </h2>
        <div id="finite-distribution" style="overflow-y: auto; height: 300px;"></div>
        <script type="text/javascript">
        provingground.ProvingGroundJS().showFD()
        </script>
        """
    makePage(divs)
  }

  var fdVec: Vector[(String, String, Double)] = Vector()

  val timeSeries: MutMap[String, Vector[Double]] = MutMap()

  val typTimeSeries: MutMap[String, Vector[Double]] = MutMap()

  def showDist[U <: Term with Subs[U]](fd: FiniteDistribution[U],
                                       names: Vector[(Term, String)]) = {
    fdVec = fd.pmf map
        ((wt) =>
           (latex(encode(names)(wt.elem)),
            latex(encode(names)(wt.elem.typ)),
            wt.weight))
  }

  def showTimeSeries[U <: Term with Subs[U]](term: U,
                                             ts: Vector[Double],
                                             names: Vector[(Term, String)]) = {
    timeSeries(latex(encode(names)(term))) = ts
  }

  def showFDs[U <: Term with Subs[U]](fds: Vector[FiniteDistribution[U]],
                                      terms: Set[U],
                                      typs: Set[Typ[Term]],
                                      names: Vector[(Term, String)]) = {
    showDist(fds.last, names)
    timeSeries.clear
    val typFDs = fds map ((fd) => fd map (_.typ))
    for (x <- terms)
      showTimeSeries(x, fds map ((fd) => -math.log(fd(x))), names)
    for (x <- typs)
      showTimeSeries(x, typFDs map ((fd) => -math.log(fd(x))), names)
  }

  val viewTerms: MutSet[Term] = MutSet()

  val viewTypes: MutSet[Typ[Term]] = MutSet()

  def displayTS(fds: Vector[FiniteDistribution[Term]],
                names: Vector[(Term, String)] = Vector()) =
    showFDs(fds, viewTerms.toSet, viewTypes.toSet, names)

  def display(buf: Deducer#BufferedRun) = {
    displayTS(buf.getTimeSeries)
    buf.onChange((_) => displayTS(buf.getTimeSeries))
  }

  def getTimeSeries = timeSeries.toList

  val pingQueue = Source.queue[Unit](10, OverflowStrategy.dropHead)

//  def ping() = pingQueue.offer(())

//  import FreeExprLang.writeDist

  val fdRoute =
    path("terms") {
      get {
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, fdView))
      }
    } ~ path("trms") {
      get {
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, termsView))
      }
    } ~ path("terms-data") {
      get {
        complete(HttpEntity(ContentTypes.`text/plain(UTF-8)`, write(fdVec)))
      }
    } ~ path("terms-time-series") {
      get {
        complete(
          HttpEntity(ContentTypes.`text/plain(UTF-8)`, write(getTimeSeries)))
      }
    }

  val dummy = """
    <div id ="dummy-space">The dummy space</div>
    <script type="text/javascript">
      provingground.ProvingGroundJS().dummyUpdate()
    </script>
    </html>
    """

  def showDummy = {
    showView("dummy", dummy)
    showData("dummy", dummyData)
  }

  def getText(name: String) = {
    val index = if (name.endsWith(".html")) name.dropRight(5) else name
    texts.get(index).getOrElse(s"No view with index $name")
  }

  val htmlRoute = path("html" / Segment) { name =>
    get {
      complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, getView(name)))
    }
  }

  val textRoute = path("text" / Segment) { name =>
    get {
      complete(HttpEntity(ContentTypes.`text/plain(UTF-8)`, getText(name)))
    }
  }

  val dataRoute = path("data" / Segment) { name =>
    get {
      complete(HttpEntity(ContentTypes.`text/plain(UTF-8)`, getData(name)))
    }
  }

  val resourceRoute = path("resource" / Segment) { name =>
    getFromResource(name)
  }

  var otherRoutes: Option[Route] = None

  def addRoute(route: Route) =
    (otherRoutes = (otherRoutes map (_ ~ route)).orElse(Some(route)))

  def mixin(route: Route) = (otherRoutes map (route ~ _)) getOrElse (route)

  val route = mixin(
    htmlRoute ~ textRoute ~ dataRoute ~ resourceRoute ~ fdRoute)

  val helloRoute = path("hello") {
    get {
      complete(
        HttpEntity(ContentTypes.`text/html(UTF-8)`,
                   "<h1>Say hello to akka-http</h1>"))
    }
  }

  lazy val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

  def bind = bindingFuture.foreach((_) => ())

  def stop = {
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ ⇒ system.terminate()) // and shutdown when done
  }
}
