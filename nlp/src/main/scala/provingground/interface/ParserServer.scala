package provingground.interface

import provingground._
import translation._

//import scala.util.Try
import ujson.Obj
import StanfordParser._
import TreeToMath._
import edu.stanford.nlp.trees.Tree
// import org.scalafmt.Scalafmt.format
import scala.util.Try
import scala.concurrent._
// import scala.concurrent.ExecutionContext.Implicits.global
import cask.main.Routes
import cask.util.Logger

object NLPParser{
  def parseResult(txt: String): Obj = {
    val texParsed: TeXParsed          = TeXParsed(txt)
    val tree: Tree = texParsed.parsed
    val baseExpr: MathExpr = mathExprTree(tree).get
    val strictParsed = mathExpr(tree).nonEmpty
    def polyExprOpt : Option[MathExpr] =
    // None
      texParsed.polyParsed.flatMap(mathExpr(_)).headOption
    val expr =
      if (strictParsed) baseExpr
      else {
        // texParsed.polyParsed.foreach(t => println(t))
        polyExprOpt.getOrElse(baseExpr)
      }
    def parsed = strictParsed || polyExprOpt.nonEmpty
    pprint.log(strictParsed)
    pprint.log(parsed)
    val proseTree: NlpProse.ProseTree = texParsed.proseTree
    // println(proseTree.view)
    val code = {
      pprint.PPrinter.BlackWhite(expr, height=500)
    }
    // Try(format(s"object ConstituencyParsed {$expr}").get)
    //   .getOrElse(s"\n//could not format:\n$expr\n\n//raw above\n\n")
    ujson.Obj(
      "tree" -> (tree.pennString + "\n\n" + pprint.PPrinter
        .BlackWhite(FormalExpr.translator(tree), height = 500)),
      "expr"    -> code.toString,
      "parsed"  -> ujson.Bool(parsed),
      "deptree" -> proseTree.view.replace("\n", "")
    )
  }

  def parseView(txt: String): Obj = {
    val obj = parseResult(txt)
    Obj("tree" -> obj("tree"), "expr" -> obj("expr"), "parsed" -> obj("parsed"))
  }
}

import NLPParser._

case class ParserRoutes()(implicit cc: castor.Context,
                           log: cask.Logger) extends cask.Routes {
  def log: Logger = new Logger.Console()

  @cask.get("/nlp.html")
  def nlp() = {
    Future(parseResult("Hello World")) // waking up the stanford pipeline
    cask.Response(Site.page(mainHTML,
              "resources/",
              "ProvingGround: Natural language translation",
              false),
              headers = Seq("Content-Type" -> "text/html")
    )
  }

  @cask.post("/parse")
  def parse(request: cask.Request): String = {
    val txt = new String(request.readAllBytes())
    println(s"parsing: $txt")
    ujson.write(parseResult(txt))
  }

  val mainHTML: String =
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

object ParserCask extends cask.Main {
  def allRoutes: Seq[Routes] = Seq(ParserRoutes(), MantleRoutes(), LeanRoutes())
  override def port: Int = Try(sys.env("PROVINGGROUND_PORT").toInt).getOrElse(8080)
  override def host: String = Try(sys.env("IP")).getOrElse("localhost")
}
