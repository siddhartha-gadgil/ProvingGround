package provingground.interface

import provingground._
import translation._
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer

import scala.util.Try
import upickle.{Js, json}

import scala.util.Try
import scala.concurrent._

object MantleService{

  var keepAlive = true

  val baseRoute =
    get {
      path("resources" / Remaining) { path =>
        println("serving from resource: " + path)
        getFromResource(path)
      }
    } ~ get {
      path("docs" / Remaining) { path =>
        println("serving from resource: " + path)
        getFromFile(s"docs/$path")
      }
    } ~ path("halt") {
      keepAlive = false
      complete(HttpEntity(ContentTypes.`text/plain(UTF-8)`, "shutting down"))
    }

  import MantleServer.executionContext

  val buildRoute =
    get {
      path("build") {
        Future(
          Try(Site.mkSite())
            .getOrElse(pprint.log("Cannot build site, perhaps this is not run from the root of the repo"))
        )
        complete(HttpEntity(ContentTypes.`text/plain(UTF-8)`, "building site"))
      }
    }

  val indexHTML =
    """
      |
      |  <ul>
      |   <li> <a href="build" target="_blank">Build</a> the web page.</li>
      |   <li> <a href="prover.html">Prover</a> experiments. </li>
      |  </ul>
      |  <script type="text/javascript" src="resources/out.js"></script>
      |  <script>
      |   mantlemenu.add()
      |  </script>
      |
    """.stripMargin

  val proverHTML =
    """
      |
      |  <div id="prover-div"></div>
      |  <script type="text/javascript" src="resources/out.js"></script>
      |  <script>
      |   mantlemenu.add()
      |   prover.load()
      |  </script>
      |
    """.stripMargin

  val mantleRoute =
    get{
      (pathSingleSlash | path("index.html")) {
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
          Site.page(indexHTML, "resources/", "ProvingGround Server" , true)))
      }
    } ~ get{
      path("prover.html"){
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
          Site.page(proverHTML, "resources/", "Prover Experimentaion" , true)))
      }
    }

   val route = baseRoute ~ buildRoute ~ mantleRoute
}


object MantleServer extends  App {
  implicit val system: ActorSystem = ActorSystem("provingground")
  implicit val materializer = ActorMaterializer()

  // needed for the future flatMap/onComplete in the end
  implicit val executionContext: scala.concurrent.ExecutionContextExecutor =
    system.dispatcher

  import MantleService._

  val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)
  println(s"Server online at http://localhost:8080/\nExit by clicking Halt on the web page (or 'curl localhost:8080/halt' from the command line)")

  while (keepAlive) {
    Thread.sleep(10)
  }

  println("starting shutdown")

  bindingFuture
    .flatMap(_.unbind()) // trigger unbinding from the port
    .onComplete(_ ⇒ system.terminate()) // and shutdown when done
}
