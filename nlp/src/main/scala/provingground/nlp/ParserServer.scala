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
import scala.concurrent._

import scala.io.StdIn

class ParserService(serverMode: Boolean)  {
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

  implicit val system: ActorSystem = ActorSystem("provingground")
  implicit val materializer = ActorMaterializer()

  // needed for the future flatMap/onComplete in the end
  implicit val executionContext: scala.concurrent.ExecutionContextExecutor =
    system.dispatcher

  val mantleService = new MantleService(serverMode)

  import mantleService._

  val parserRoute =
    (pathSingleSlash | path("index.html")) {
      get {
        complete(
          HttpEntity(
            ContentTypes.`text/html(UTF-8)`,
            Site.page(mainHTML, "resources/", "ProvingGround: Natural language translation"  ,!serverMode)
          )
        )
      }
    } ~
      post {
        path("parse") {
          entity(as[String]) { txt =>
            println(s"parsing: $txt")

            val resultFut =
              Future(parseResult(txt))
            val responseFut = resultFut.map { (result) =>
              pprint.log("result sent to  browser")
              HttpEntity(ContentTypes.`application/json`, result.toString)
            }
            complete(responseFut)
          }
        }
      } ~ get {
      path("resources" / Remaining) { path =>
        getFromResource(path.toString)
      }
    }

  val route = parserRoute ~ baseRoute

  val mainHTML =
    """
      |
      |    <div id="constituency-parser"></div>
      |
      |  <script src="resources/out.js" type="text/javascript" charset="utf-8"></script>
      |  <script>
      |    parser.load()
      |  </script>
    """.stripMargin


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
      |    <link rel="stylesheet" href="resources/css/nlp.css">
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

}
object ParserServer extends App {
  case class Config(
                   host: String = "localhost",
                   port: Int = 8080,
                  serverMode: Boolean = false)

// val config = Config()

val parser = new scopt.OptionParser[Config]("provingground-server") {
  head("ProvingGround Server", "0.1")

  opt[String]('i', "interface")
    .action((x, c) => c.copy(host = x))
    .text("server ip")
  opt[Int]('p', "port")
    .action((x, c) => c.copy(port = x))
    .text("server port")
  opt[Unit]("server")
  .action((x, c) => c.copy(serverMode = true))
  .text("running in server mode")
}


parser.parse(args, Config()) match {
  case Some(config) =>
    val parserService = new ParserService(config.serverMode)
    import parserService._,  mantleService.keepAlive

    // val server = new MantleService(config.serverMode)


    val bindingFuture =
      Http().bindAndHandle(route, config.host, config.port)

    val exitMessage =
      if (config.serverMode) "Kill process to exit"
      else "Exit by clicking Halt on the web page (or 'curl localhost:8080/halt' from the command line)"

    println(s"Server online at http://${config.host}:${config.port}/\n$exitMessage")

    while (keepAlive) {
      Thread.sleep(10)
    }

    println("starting shutdown")

    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done

  case None =>
    println("invalid options")
}


}
