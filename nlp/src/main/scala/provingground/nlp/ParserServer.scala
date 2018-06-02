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

import StanfordParser._
import TreeToMath._

import org.scalafmt.Scalafmt.format
import scala.io.StdIn

object ParserServer extends App{
  def parseResult(txt: String) = {
    val tree = texParse(txt)
    val expr = mathExprTree(tree)
    val code =
      format(s"object ConstituencyParsed {$expr}")
    Js.Obj("tree" -> tree.pennString, "expr" -> code.toString)
  }

  implicit val system: ActorSystem = ActorSystem("provingground-nlp")
  implicit val materializer = ActorMaterializer()


  // needed for the future flatMap/onComplete in the end
  implicit val executionContext : scala.concurrent.ExecutionContextExecutor =
    system.dispatcher

  val route =
    (pathSingleSlash | path("index.html")){
      get {
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, indexHTML))
      }
    } ~
    post {
      path("parse") {
        entity(as[String]) { txt =>
          println(s"parsing: $txt")

          complete(
            HttpEntity(ContentTypes.`application/json`, Js.Obj("tree" -> "tree", "expr" -> "expr").toString
          ))
          // val result =
          //   parseResult(txt)
          // println(s"Result:\n$result")
          // complete(HttpEntity(ContentTypes.`application/json`, result.toString))
        }
      }
    } ~ get {
          path("resources" / Remaining) { path =>
             println("serving from resource: " + path)
            getFromResource(path.toString)
          }
        }


val indexHTML =
    """
<!DOCTYPE html>

<html>
  <head>
    <title>ProvingGround: Constituency Parser</title>
    <link rel="stylesheet" href="/resources/css/bootstrap.min.css">
    <link rel="stylesheet" href="/resources/css/katex.min.css">
    <link rel="stylesheet" href="/resources/css/main.css">
    <script src="/resources/js/katex.min.js" type="text/javascript" charset="utf-8"></script>
    <script src="/resources/js/highlight.pack.js" type="text/javascript" charset="utf-8"></script>
    <script src="/resources/out.js" type="text/javascript" charset="utf-8"></script>

  </head>
  <body>

  <div class="container">
    <h2> ProvingGround: Constituency Parser </h2>

    <div id="constituency-parser"></div>

  </div>
  <script>
    parser.load()
  </script>
  </body>
</html>
"""

val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)
println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
def stop = {
  bindingFuture
    .flatMap(_.unbind()) // trigger unbinding from the port
    .onComplete(_ ⇒ system.terminate()) // and shutdown when done
}

stop

}
