package provingground.interface

import provingground._
import library.LeanMemo
import ammonite.ops._
import monix.eval._
import monix.execution.Scheduler.Implicits.global

import LeanInterface._
import LeanParser._
import provingground.HoTT._
import trepplein.{Modification, Name}
import ujson.Js


import io.undertow.websockets.WebSocketConnectionCallback
import io.undertow.websockets.core.{AbstractReceiveListener, BufferedTextMessage, WebSocketChannel, WebSockets}
import io.undertow.websockets.spi.WebSocketHttpExchange
import scala.util.Try

import scala.collection.mutable.{ArrayBuffer, Map => mMap, Set => mSet}

object LeanResources{
  val index: IndexedSeq[String] = read.lines(resource / "index.txt")

  val modTasks: Map[String, Task[Vector[Modification]]] = index.map { (name) =>
      val path = resource / name
      val in = new java.io.ByteArrayInputStream(read.bytes(path))
      name -> Task(getModsFromStream(in)).memoize
    }.toMap

  lazy val baseParser: LeanParser =
    new LeanParser(Seq(), library.LeanMemo.defTaskMap, library.LeanMemo.indTaskMap)


  def loadTask(f: String): Task[Unit] = modTasks(f).map((m) => baseParser.addMods(m))

  val defnMap: mMap[Name, Term] = mMap()

  val termIndModMap: mMap[Name, TermIndMod] = mMap()

  val mods: ArrayBuffer[Modification] = ArrayBuffer.empty[Modification]

  def logUpdate: Logger = new Logger{
    def apply(l: Log) = l match {
      case LeanParser.Defined(name, term)    => defnMap += name -> term
      case LeanParser.DefinedInduc(name) => ???
      case LeanParser.ParseWork(expr)    => ()
      case LeanParser.Parsed(expr)       => ()
    }
  }

}

object LeanRoutes extends cask.Routes{
  @cask.get("codegen-defns")
  def codeGenDefs(): Vector[String] = LeanMemo.defTaskMap.keys.map(_.toString).toVector

  @cask.get("codegen-induc-defns")
  def codeGenInducDefs(): Vector[String] = LeanMemo.indTaskMap.keys.map(_.toString).toVector

  var channelOpt : Option[WebSocketChannel] = None

  @cask.websocket("/monoid-websock")
  def showUserProfile(): cask.WebsocketResult = {
    new WebSocketConnectionCallback() {
      override def onConnect(exchange: WebSocketHttpExchange, channel: WebSocketChannel): Unit = {
        channelOpt = Some(channel)
        channel.resumeReceives()
      }
    }
  }

  import LeanResources._

  def send(s: String): Unit = channelOpt.foreach((channel: WebSocketChannel) => WebSockets.sendTextBlocking(s, channel))

  def sendLog(s: String): Unit =
    send(
      Js.Obj("type" -> Js.Str("log"), "message" -> Js.Str(s)).toString()
    )

  val sendLogger = logger.dispatch(sendLog)

  def messenger: Logger =
    logUpdate && sendLogger

  def parser = new LeanParser(mods, library.LeanMemo.defTaskMap, library.LeanMemo.indTaskMap, messenger)

  @cask.post("/loadFile")
  def loadFile(request: cask.Request): String = {
    val name = new String(request.readAllBytes())
    loadTask(name).foreach((_) => sendLog(s"loaded $name"))
    s"loading $name"
  }

  initialize()
}
