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

class ParserService(serverMode: Boolean)(implicit ec: ExecutionContext, mat: ActorMaterializer) {
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


  val mantleService = new MantleService(serverMode)

  import mantleService._

  val parserRoute =
    (pathSingleSlash | path("index.html")) {
      get {
        complete(
          HttpEntity(
            ContentTypes.`text/html(UTF-8)`,
            Site.page(mainHTML,
                      "resources/",
                      "ProvingGround: Natural language translation",
                      !serverMode)
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

  val ammRoute =
    (pathSingleSlash | path("index.html")) {
      get {
        complete(
          HttpEntity(ContentTypes.`text/plain(UTF-8)`,
          "Fiddle does not work from the NLP server, use the HOTT server"
        )
      )
      }
    }


  val mainHTML =
    """
      |   <link rel="stylesheet" href="resources/css/nlp.css">
      |    <div id="constituency-parser"></div>
      |
      |  <script src="resources/out.js" type="text/javascript" charset="utf-8"></script>
      |  <script>
      |    parser.load()
      |  </script>
    """.stripMargin



}
object ParserServer extends App {

  implicit val system: ActorSystem = ActorSystem("provingground")
  implicit val materializer        = ActorMaterializer()

  // needed for the future flatMap/onComplete in the end
  implicit val executionContext: scala.concurrent.ExecutionContextExecutor =
    system.dispatcher

  import ammonite.ops._

  def path(s: String): Path =
    scala.util.Try(Path(s)).getOrElse(pwd / RelPath(s))

  implicit val pathRead: scopt.Read[Path] =
    scopt.Read.reads(path)

  case class Config(
      scriptsDir: Path = pwd / "repl-scripts",
      objectsDir: Path = pwd / "core" / "src" / "main" / "scala" / "provingground" / "scripts",
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
    opt[Path]('s', "scripts")
      .action((x, c) => c.copy(scriptsDir = x))
      .text("scripts directory")
    opt[Path]('o', "objects")
      .action((x, c) => c.copy(objectsDir = x))
      .text("created objects directory")
    opt[Unit]("server")
      .action((x, c) => c.copy(serverMode = true))
      .text("running in server mode")
  }

  parser.parse(args, Config()) match {
    case Some(config) =>
      val parserService = new ParserService(config.serverMode)
      import parserService._, mantleService.keepAlive




      val bindingFuture =
        Http().bindAndHandle(
          route ~
            pathPrefix("hott"){
              mantleService.route ~ pathPrefix("scripts")(ammRoute)
            },
            config.host, config.port)

      val exitMessage =
        if (config.serverMode) "Kill process to exit"
        else
          "Exit by clicking Halt on the web page (or 'curl localhost:8080/halt' from the command line)"

      println(
        s"Server online at http://${config.host}:${config.port}/\n$exitMessage")

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
