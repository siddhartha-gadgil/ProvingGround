package provingground.interface

import scala.concurrent.ExecutionContext.Implicits.global
import MantleService._
import io.undertow.websockets.WebSocketConnectionCallback
import io.undertow.websockets.core.{AbstractReceiveListener, BufferedTextMessage, WebSocketChannel, WebSockets}
import io.undertow.websockets.spi.WebSocketHttpExchange
import monix.execution.CancelableFuture

import scala.util.Try

object MantleRoutes extends cask.Routes {
  @cask.staticFiles("docs")
  def docsRoute() = "docs"

  @cask.staticResources("resources")
  def public() = "."

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
  def fiddle() = AmmService.indexHTML

  @cask.post("/scripts/kernel")
  def repl(request: cask.Request) = {
    val d = new String(request.readAllBytes())
    println(s"post received:\n$d")

    val result =
      AmmService.replResult(d) match {
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

  initialize()

}

object MantleCask extends cask.Main(MantleRoutes, LeanRoutes) {
  override def port = Try(sys.env("PROVINGGROUND_PORT").toInt).getOrElse(8080)
  override def host = Try(sys.env("PROVINGGROUND_HOST")).getOrElse("localhost")
}

import monix.eval._


class TaskSocket(task: String => Task[String])
    extends WebSocketConnectionCallback() {

  import monix.execution.Scheduler.Implicits.global

  def respond(t: Task[String], channel: WebSocketChannel): CancelableFuture[Unit] =
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
