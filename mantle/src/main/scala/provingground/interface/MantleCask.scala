package provingground.interface

import provingground._

// import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
// import MantleService._
import io.undertow.websockets.WebSocketConnectionCallback
import io.undertow.websockets.core.{AbstractReceiveListener, BufferedTextMessage, WebSocketChannel, WebSockets}
import io.undertow.websockets.spi.WebSocketHttpExchange
import monix.execution.CancelableFuture

import scala.util.Try
import cask.main.Routes
import cask.util.Logger

object MantleRoutes extends cask.Routes {
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  def log: Logger = new Logger.Console
  
  val indexHTML =
  """
    |
    | <p> This is a server to experiment with a few aspects of the ProvingGround project, as well as help with development. The natural
    | language processing is on a separate server as it has a large additional dependency.</p>
    |  <ul>
    |   <li> <a href="build" target="_blank">Build</a> the documentation page.</li>
    |   <li> <a href="prover.html">Prover experiments</a>: currently one illustration of autonomous proving. </li>
    |   <li> <a href="scripts/index.html" target="_blank">Fiddle</a>: an interpreter
    | with much of the code of the ProvingGround project in the class path.</li>
    |  </ul>
    |  <script type="text/javascript" src="resources/out.js"></script>
    |  <script>
    |   mantlemenu.add()
    |  </script>
    |
  """.stripMargin

  val fiddleHTML =
  """
  |<!DOCTYPE html>
  |
  |<html>
  |  <head>
  |    <title>ProvingGround Fiddle</title>
  |    <link rel="stylesheet" href="../resources/css/bootstrap.min.css">
  |    <link rel="icon" href="/resources/IIScLogo.jpg">
  |    <script src="../resources/js/ace.js" type="text/javascript" charset="utf-8"></script>
  |    <link rel="stylesheet" href="../resources/css/katex.min.css">
  |    <script src="../resources/js/katex.min.js" type="text/javascript" charset="utf-8"></script>
  |   <script src="../resources/js/auto-render.min.js" type="text/javascript" charset="utf-8"></script>
  |    <link rel="stylesheet" href="../resources/css/github-gist.css">
  |    <script src="../resources/js/highlight.pack.js" type="text/javascript" charset="utf-8"></script>
  |    <script src="../resources/out.js" type="text/javascript" charset="utf-8"></script>
  |    <style type="text/css" media="screen">
  |        .editor {
  |            height: 300px;
  |            font-size: 14px;
  |
  |        }
  |        .view {
  |          overflow-y: auto;
  |          height: 300px;
  |        }
  |        .btn-space {
  |    margin-right: 5px;
  |}
  |    </style>
  |  </head>
  |  <body>
  |
  |  <div class="container">
  |    <h2 class="text-center"> ProvingGround Fiddle </h2>
  |    <p> Any commands entered below are run in an interpreter, with the code of
  |    the ProvingGround project, <em>excluding</em> the natural language processing component, in the class path.</p>
  |
  |
  |    <div id="edit-div"></div>
  |  </div>
  |
  |  <script>
  |    CodeEditorJS.main()
  |    </script>
  |
  |  </body>
  |</html>
  |
""".stripMargin

val proverHTML =
  """
    |
    |  <div id="prover-div"></div>
    |  <script type="text/javascript" src="../resources/out.js"></script>
    |  <script>
    |   mantlemenu.add()
    |   prover.load()
    |  </script>
    |
  """.stripMargin

  val leanlibHTML =
    """
      |
      |  <div id="leanlib-div"></div>
      |  <script type="text/javascript" src="../resources/out.js"></script>
      |  <script>
      |   mantlemenu.add()
      |   leanlib.load()
      |  </script>
      |
    """.stripMargin

  val interactiveProverHTML =
  """
    |<div id="interactive-prover-div"></div>
    |<script type="text/javascript" src="../resources/out.js"></script>
    |  <script>
    |   mantlemenu.add()
    |   interactiveProver.load()
    |  </script>
  """.stripMargin


  def trySite() =
  Try(Site.mkHome())
    .map { (_) =>
      Future(
        Try(Site.mkSite())
          .getOrElse(pprint.log(
            "Cannot build site, perhaps this is not run from the root of the repo"))
      )
      "Building site"
    }
    .getOrElse(
      "Cannot build site, perhaps this is not run from the root of the repo")



  @cask.staticFiles("/docs")
  def docsRoute() = "docs"

  def getResource(segs: Seq[String]) = {
    val path = segs.foldLeft[os.ResourcePath](os.resource)(_ / _)
    val txt = os.read(path)
    txt
  }

  @cask.get("/resources", subpath = true)
  def public(request: cask.Request) = {
    val segs = request.remainingPathSegments
    // pprint.log(segs)
    getResource(segs)
  }

  @cask.get("/")
  def root() =
    Site.page(indexHTML, "resources/", "ProvingGround HoTT Server", false)

  @cask.get("/index.html")
  def index() =
    Site.page(indexHTML, "resources/", "ProvingGround HoTT Server", false)

  @cask.get("/prover.html")
  def prover() =
    Site.page(proverHTML, "resources/", "ProvingGround HoTT Server", false)

  @cask.get("/leanlib.html")
  def leanlib() =
    Site.page(leanlibHTML, "resources/", "ProvingGround Lean Export", false)

  @cask.get("/interactive-prover.html")
  def interactiveProver() : String =
    Site.page(interactiveProverHTML, "resources/", "ProvingGround: Interactive prover", false)

  @cask.post("/monoid-proof")
  def seek() = {
    pprint.log("seeking proof")
    val pfFut = MonoidServer.seekResultFut
    pfFut.foreach((pf) => pprint.log(pf))
    "seeking proof"
  }

  @cask.route("build", methods = Seq("get", "post"))
  def site(request: cask.Request) =
    trySite()

  @cask.get("/scripts/index.html")
  def fiddle() = fiddleHTML

  @cask.post("/scripts/kernel")
  def repl(request: cask.Request) = {
    val d = new String(request.readAllBytes())
    println(s"post received:\n$d")

    val result =
      MDocService.replResult(d) match {
        case Right(z) => "--RESULT--\n" + z
        case Left(z)  => "--ERROR--\n" + z
      }

    result
  }

  def sendProof(channel: WebSocketChannel): Unit = {
    val pfFut = MonoidServer.seekResultFut
    pfFut.foreach((pf) => WebSockets.sendTextBlocking(pf.toString, channel))
  }

  @cask.websocket("/monoid-websock")
  def showUserProfile(): cask.WebsocketResult = {
    new WebSocketConnectionCallback() {
      override def onConnect(exchange: WebSocketHttpExchange,
                             channel: WebSocketChannel): Unit = {
        channel.getReceiveSetter.set(
          new AbstractReceiveListener() {
            override def onFullTextMessage(
                channel: WebSocketChannel,
                message: BufferedTextMessage): Unit = {
              message.getData match {
                case "" => channel.close()
                case data =>
                  pprint.log(s"received $data over websocket")
                  sendProof(channel)
                  WebSockets.sendTextBlocking(data, channel)
              }
            }
          }
        )
        channel.resumeReceives()
      }
    }
  }

  import learning._

  @cask.websocket("/prover-websock")
  def proverSocket() =
    new TaskSocket(TermGenJson.all)

  initialize()

}

object MantleCask extends cask.Main {
  override def allRoutes: Seq[Routes] = Seq(MantleRoutes, LeanRoutes)
  override def port = Try(sys.env("PROVINGGROUND_PORT").toInt).getOrElse(8080)
  override def host = Try(sys.env("IP")).getOrElse("localhost")
}

object ReplCask{
  import MantleCask._
  import io.undertow.Undertow
  lazy val server = Undertow.builder
      .addHttpListener(port, host)
      .setHandler(defaultHandler)
      .build
}

import monix.eval._


class TaskSocket(task: String => Task[String])
    extends WebSocketConnectionCallback() {

  import monix.execution.Scheduler.Implicits.global

  def respond(t: Task[String], channel: WebSocketChannel): Unit =
    t.foreach((output) => WebSockets.sendTextBlocking(output, channel))

  override def onConnect(exchange: WebSocketHttpExchange,
                         channel: WebSocketChannel): Unit = {
    channel.getReceiveSetter.set(
      new AbstractReceiveListener() {
        override def onFullTextMessage(channel: WebSocketChannel,
                                       message: BufferedTextMessage): Unit = {
          message.getData match {
            case ""   => channel.close()
            case data =>
              respond(task(data), channel)
          }
        }
      }
    )
    channel.resumeReceives()
  }
}
