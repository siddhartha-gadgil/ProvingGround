package provingground.interface

import provingground._
import translation._
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.Materializer

import scala.util.Try
import ujson.Js
import StanfordParser._
import TreeToMath._
import edu.stanford.nlp.trees.Tree
// import org.scalafmt.Scalafmt.format
import scala.util.Try
import scala.concurrent._

import scala.io.StdIn

class AkkaParserService(serverMode: Boolean)(implicit ec: ExecutionContext,
                                         mat: Materializer) {
  def parseResult(txt: String) = {
    val texParsed: TeXParsed          = TeXParsed(txt)
    val tree: Tree                    = texParsed.parsed
    val expr: MathExpr                = mathExprTree(tree).get
    val proseTree: NlpProse.ProseTree = texParsed.proseTree
    // println(proseTree.view)
    val code = pprint.PPrinter.BlackWhite(expr, height=500)
      // Try(format(s"object ConstituencyParsed {$expr}").get)
      //   .getOrElse(s"\n//could not format:\n$expr\n\n//raw above\n\n")
    ujson.Obj("tree"    -> tree.pennString,
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
      path("resources" / Remaining) { path => getFromResource(path.toString)
      }
    }

  val route = parserRoute ~ baseRoute

  val ammRoute =
    (pathSingleSlash | path("index.html")) {
      get {
        complete(
          HttpEntity(
            ContentTypes.`text/plain(UTF-8)`,
            "Fiddle does not work from the NLP server, use the HOTT server")
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

