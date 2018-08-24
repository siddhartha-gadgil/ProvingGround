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
import io.undertow.websockets.core.{
  AbstractReceiveListener,
  BufferedTextMessage,
  WebSocketChannel,
  WebSockets
}
import io.undertow.websockets.spi.WebSocketHttpExchange
import provingground.translation.TeXTranslate

import scala.collection.mutable.{ArrayBuffer, Map => mMap, Set => mSet}

import upickle.default.{read => _, write => uwrite, _}

object LeanResources {
  def index: IndexedSeq[String] = read.lines(resource / "index.txt").filter(_.endsWith(".lean.export"))

  val modTasks: Map[String, Task[Vector[Modification]]] = index.map { (name) =>
    val path = resource / name
    val in   = new java.io.ByteArrayInputStream(read.bytes(path))
    name -> Task(getModsFromStream(in)).memoize
  }.toMap

  lazy val baseParser: LeanParser =
    new LeanParser(Seq(),
                   library.LeanMemo.defTaskMap,
                   library.LeanMemo.indTaskMap)

  def loadTask(f: String): Task[Unit] =
    modTasks(f).map((m) => baseParser.addMods(m))

  val defnMap: mMap[Name, Term] = mMap()

  val termIndModMap: mMap[Name, TermIndMod] = mMap()

  val mods: ArrayBuffer[Modification] = ArrayBuffer.empty[Modification]

  val loadedFiles: ArrayBuffer[String] = ArrayBuffer()

  def logUpdate: Logger = new Logger {
    def apply(l: Log): Unit = l match {
      case LeanParser.Defined(name, term) => defnMap += name -> term
      case LeanParser.DefinedInduc(name, termIndMod) =>
        termIndModMap += name -> termIndMod
      case LeanParser.ParseWork(expr) => ()
      case LeanParser.Parsed(expr)    => ()
    }
  }

}

object LeanRoutes extends cask.Routes {
  import LeanResources._
  @cask.get("/files")
  def files(): String = {
    pprint.log(index.mkString(","))
    uwrite(index.toVector)
  }

  @cask.get("/codegen-defns")
  def codeGenDefs(): Vector[String] =
    LeanMemo.defTaskMap.keys.map(_.toString).toVector

  @cask.get("/codegen-induc-defns")
  def codeGenInducDefs(): Vector[String] =
    LeanMemo.indTaskMap.keys.map(_.toString).toVector

  @cask.get("/mem-defns")
  def memDefs(): Vector[String] = defnMap.keys.map(_.toString).toVector

  @cask.get("/mem-induc-defns")
  def memInducDefs(): Vector[String] =
    termIndModMap.keys.map(_.toString).toVector

  @cask.get("mods/:file")
  def getMods(file: String): Vector[Js.Value] = {
    val path = resource / file
    val in   = new java.io.ByteArrayInputStream(read.bytes(path))
    getModsFromStream(in).map{(m) =>
      val tp = m match {
        case _ : trepplein.DefMod => "definition"
        case _ : trepplein.IndMod => "inductive type"
      }
    Js.Obj("type" -> tp, "name" -> m.name.toString)
    }
  }

  var channelOpt: Option[WebSocketChannel] = None

  @cask.websocket("/leanlib-websock")
  def showUserProfile(): cask.WebsocketResult = {
    new WebSocketConnectionCallback() {
      override def onConnect(exchange: WebSocketHttpExchange,
                             channel: WebSocketChannel): Unit = {
        channelOpt = Some(channel)
        channel.resumeReceives()
      }
    }
  }

  def send(s: String): Unit =
    channelOpt.foreach((channel: WebSocketChannel) =>
      WebSockets.sendTextBlocking(s, channel))

  def sendLog(s: String): Unit =
    send(
      Js.Obj("type" -> Js.Str("log"), "message" -> Js.Str(s)).toString()
    )

  val sendLogger: Logger = Logger.dispatch(send)

  def messenger: Logger =
    logUpdate && sendLogger

  def parser =
    new LeanParser(mods,
                   library.LeanMemo.defTaskMap,
                   library.LeanMemo.indTaskMap,
                   messenger)

  @cask.post("/loadFile")
  def loadFile(request: cask.Request): String = {
    val name = new String(request.readAllBytes())
    if (loadedFiles.contains(name))
      sendLog(s"file $name already loaded")
    else
      loadTask(name).foreach { (_) =>
        loadedFiles += name
        sendLog(s"loaded file $name")
      }
    s"loading $name"
  }

  @cask.post("/parse")
  def parse(request: cask.Request): String = {
    def result(name: String, t: Term): Unit = send(
      Js.Obj("type" -> "parse-result", "name" -> name, "tex" -> TeXTranslate(t))
        .toString
    )
    val name = new String(request.readAllBytes())
    defnMap
      .get(name)
      .map { (t) =>
        result(name, t)
        s"previously parsed $name"
      }
      .getOrElse {
        parser.get(name).foreach { (t) =>
          result(name, t)
        }
        s"parsing $name"
      }

  }

  @cask.post("/save-code")
  def parse() = {
    val lc = LeanCodeGen(parser)
    lc.save()
    lc.memo()
    "Generated code for all definitions"
  }

  initialize()
}
