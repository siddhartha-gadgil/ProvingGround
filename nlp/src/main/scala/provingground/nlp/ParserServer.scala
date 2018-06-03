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
import edu.stanford.nlp.trees.Tree
import org.scalafmt.Scalafmt.format
import scala.util.Try

import scala.io.StdIn

object ParserServer extends App {
  def parseResult(txt: String) = {
    val texParsed: TeXParsed          = TeXParsed(txt)
    val tree: Tree                    = texParsed.parsed
    val expr: MathExpr                = mathExprTree(tree).get
    val proseTree: NlpProse.ProseTree = texParsed.proseTree
    // println(proseTree.view)
    val code =
      Try(format(s"object ConstituencyParsed {$expr}").get)
        .getOrElse(s"\n//could not format:\n$expr\n\n//raw above\n\n")
    Js.Obj("tree"    -> tree.pennString,
           "expr"    -> code.toString,
           "deptree" -> proseTree.view.replace("\n", ""))
  }

  implicit val system: ActorSystem = ActorSystem("provingground-nlp")
  implicit val materializer        = ActorMaterializer()

  // needed for the future flatMap/onComplete in the end
  implicit val executionContext: scala.concurrent.ExecutionContextExecutor =
    system.dispatcher

  var keepAlive = true

  val route =
    (pathSingleSlash | path("index.html")) {
      get {
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, indexHTML))
      }
    } ~
      post {
        path("parse") {
          entity(as[String]) { txt =>
            println(s"parsing: $txt")

            val result =
              parseResult(txt)
            // println(s"Result:\n$result")
            println("result sent to  browser")
            complete(
              HttpEntity(ContentTypes.`application/json`, result.toString))
          }
        }
      } ~ get {
      path("resources" / Remaining) { path =>
        // println("serving from resource: " + path)
        getFromResource(path.toString)
      }
    } ~
      path("halt") {
        keepAlive = false
        complete(HttpEntity(ContentTypes.`text/plain(UTF-8)`, "shutting down"))
      }

  val indexHTML =
    """
      |<!DOCTYPE html>
      |
      |<html>
      |  <head>
      |    <title>ProvingGround: Natural language translation</title>
      |    <link rel="icon" href="resources/IIScLogo.jpg">
      |    <link rel="stylesheet" href="resources/css/bootstrap.min.css">
      |    <link rel="stylesheet" href="resources/css/katex.min.css">
      |    <link rel="stylesheet" href="resources/css/main.css">
      |    <script src="resources/js/katex.min.js" type="text/javascript" charset="utf-8"></script>
      |    <script src="resources/js/highlight.pack.js" type="text/javascript" charset="utf-8"></script>
      |
      |
      |  </head>
      |  <body>
      |
      |  <div class="container">
      |    <div id="halt"></div>
      |    <h2> ProvingGround: Natural language translation </h2>
      |
      |    <div id="constituency-parser"></div>
      |
      |  </div>
      |  <script src="resources/out.js" type="text/javascript" charset="utf-8"></script>
      |  <script>
      |    parser.load()
      |  </script>
      |  </body>
      |</html>
    """.stripMargin

  val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)
  println(s"Server online at http://localhost:8080/\nExit from the web page")

  while (keepAlive) {
    Thread.sleep(10)
  }

  println("starting shutdown")

  bindingFuture
    .flatMap(_.unbind()) // trigger unbinding from the port
    .onComplete(_ ⇒ system.terminate()) // and shutdown when done

}
