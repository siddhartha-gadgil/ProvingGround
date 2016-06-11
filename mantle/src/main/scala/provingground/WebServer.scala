package provingground

import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import scala.io.StdIn
import akka.actor.ActorSystem
//import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model._

import scala.collection.mutable.{Map => MutMap}

object WebServer {

  implicit val system = Hub.system
  implicit val materializer = ActorMaterializer()
  // needed for the future flatMap/onComplete in the end
  implicit val executionContext = system.dispatcher

  private val views: MutMap[String, String] = MutMap()

  private val texts: MutMap[String, String] = MutMap()

  def showView(name: String, data: String) = { views(name) = data }

  def showText(name: String, data: String) = { texts(name) = data }

  def getView(name: String) = {
    val index = if (name.endsWith(".html")) name.dropRight(5) else name
    views.get(index).getOrElse(s"<h2>No view with index <i>$name</i></h2>")
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

  val route = htmlRoute ~ textRoute

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
