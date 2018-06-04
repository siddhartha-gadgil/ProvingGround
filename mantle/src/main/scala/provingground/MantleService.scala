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
        Future(Site.mkSite())
        complete(HttpEntity(ContentTypes.`text/plain(UTF-8)`, "building site"))
      }
    }

  val indexHTML =
    """
      |
      |  <ul>
      |   <li> <a href="build">Build</a> the web page.</li>
      |   <li> Link for proving coming soon. </li>
      |  </ul>
      |
    """.stripMargin

  val homeRoute =
    get{
      (pathSingleSlash | path("index.html")) {
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
          Site.page(indexHTML, "docs/", "ProvingGround : Server" )))
      }
    }


}


object MantleServer extends  App {
  implicit val system: ActorSystem = ActorSystem("provingground")
  implicit val materializer = ActorMaterializer()

  // needed for the future flatMap/onComplete in the end
  implicit val executionContext: scala.concurrent.ExecutionContextExecutor =
    system.dispatcher


}