package provingground.interface

import provingground._
import translation._

import scala.util.Try
import upickle.{Js, json}
import StanfordParser._
import TreeToMath._
import edu.stanford.nlp.trees.Tree
// import org.scalafmt.Scalafmt.format
import scala.util.Try
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

object ParserRoutes extends cask.Routes {
  def parseResult(txt: String) = {
    val texParsed: TeXParsed          = TeXParsed(txt)
    val tree: Tree                    = texParsed.parsed
    val baseExpr: MathExpr                = mathExprTree(tree).get
    val strictParsed                  = mathExpr(tree).nonEmpty
    def polyExprOpt = texParsed.polyParsed.map(mathExpr(_)).flatten.headOption
    val expr =
      if (strictParsed) baseExpr
      else {
        texParsed.polyParsed.foreach(t => println(t))
        polyExprOpt.getOrElse(baseExpr)
      }
    def parsed = strictParsed || polyExprOpt.nonEmpty
    pprint.log(strictParsed)
    pprint.log(parsed)
    val proseTree: NlpProse.ProseTree = texParsed.proseTree
    // println(proseTree.view)
    val code = {
      // println(pprint.PPrinter.BlackWhite(expr))
      pprint.PPrinter.BlackWhite(expr, height=500)
    }
    // Try(format(s"object ConstituencyParsed {$expr}").get)
    //   .getOrElse(s"\n//could not format:\n$expr\n\n//raw above\n\n")
    Js.Obj(
      "tree" -> (tree.pennString + "\n\n" + pprint.PPrinter
        .BlackWhite(FormalExpr.translator(tree), height = 500)),
      "expr"    -> code.toString,
      "parsed"  -> ujson.Js.Bool(parsed),
      "deptree" -> proseTree.view.replace("\n", "")
    )
  }

  @cask.get("/nlp.html")
  def nlp(): String = {
    Future(parseResult("Hello World")) // waking up the stanford pipeline
    Site.page(mainHTML,
              "resources/",
              "ProvingGround: Natural language translation",
              false)
  }

  @cask.post("/parse")
  def parse(request: cask.Request): String = {
    val txt = new String(request.readAllBytes())
    println(s"parsing: $txt")
    ujson.write(parseResult(txt))
  }

  val mainHTML =
    """
      |   <link rel="stylesheet" href="resources/css/nlp.css">
      |    <div id="constituency-parser"></div>
      |
      |  <script src="resources/out.js" type="text/javascript" charset="utf-8"></script>
      |  <script>
      |    parser.load()
      |    mantlemenu.add()
      |    leanlib.load()
      |  </script>
    """.stripMargin

  initialize()

}

object ParserCask extends cask.Main(ParserRoutes, MantleRoutes, LeanRoutes) {
  override def port = Try(sys.env("PROVINGGROUND_PORT").toInt).getOrElse(8080)
  override def host = Try(sys.env("IP")).getOrElse("localhost")
}
