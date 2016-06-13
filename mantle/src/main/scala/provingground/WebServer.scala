package provingground

import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import scala.io.StdIn
import akka.actor.ActorSystem
//import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model._

import scala.collection.mutable.{Map => MutMap}

import HoTT._

//import scalatags.Text.all._

object WebServer {

  implicit val system = Hub.system
  implicit val materializer = ActorMaterializer()
  // needed for the future flatMap/onComplete in the end
  implicit val executionContext = system.dispatcher

  private val views: MutMap[String, String] = MutMap()

  private val texts: MutMap[String, String] = MutMap()

  def dummyData = () => s"Dummy data at ${System.currentTimeMillis / 1000}"

  private val data: MutMap[String, () => String] = MutMap()

  def showView(name: String, data: String) = { views(name) = data }

  def showText(name: String, data: String) = { texts(name) = data }

  def showData(name: String, dataVal: () => String) = {data(name) = dataVal}

  def getData(name: String) = data.getOrElse(name, () => "no data")()

  def getView(name: String) = {
    val index = if (name.endsWith(".html")) name.dropRight(5) else name
    val div = views.get(index).getOrElse(
      s"""<h2>No view with index <i>$name</i></h2>
      <div class="js-element" data-script="welcome"></div>
      <script type="text/javascript">
        provingground.ProvingGroundJS().dynamic()
      </script>

      """)
    makePage(div)
  }
  
  def makePage(divs: String) =
    s"""
      <!DOCTYPE html>
      <html>
      <head>
      <title>Proving-Ground : Automating theorem proving</title>
      </head>
      <body>
      <script type="text/javascript" src="../resource/provingground-js-fastopt.js"></script>
      $divs
      </body>
      </html>
      """

  val termsView = {
    val divs =
      """
        <div id="finite-distribution"></div>
        <script type="text/javascript">
        provingground.ProvingGroundJS().showFD()
        </script>
        """
    makePage(divs)
  }
  
  var fdTerms : FiniteDistribution[Term] = FiniteDistribution.empty[Term] 
  
  def showDist(fd: FiniteDistribution[Term]) = (fdTerms == fd)
  
  import FreeExprLang.writeDist
  
  val fdPath = 
    path("terms") {
      get {
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, termsView))
      }
    } ~ path("terms-data") {
      get{
        complete(HttpEntity(ContentTypes.`text/plain(UTF-8)`, writeDist(fdTerms)))
      }
    }
  
  val dummy =
    """
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

  val resourceRoute = path("resource" / Segment){ name =>
      getFromResource(name)}

  var otherRoutes: Option[Route] = None

  def addRoute(route: Route) =
    (otherRoutes = (otherRoutes map (_ ~ route)).orElse(Some(route)))

  def mixin(route: Route) = (otherRoutes map (route ~ _)) getOrElse (route)

  val route = mixin(htmlRoute ~ textRoute ~ dataRoute ~ resourceRoute)

  val helloRoute = path("hello") {
    get {
      complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
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
