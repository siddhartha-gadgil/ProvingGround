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
import ujson.Js.Value

import scala.collection.mutable.{ArrayBuffer, Map => mMap, Set => mSet}
import upickle.default.{write => uwrite, read => _, _}

import scala.util.Try

object LeanResources {
  def index: IndexedSeq[String] =
    read.lines(resource / "index.txt").filter(_.endsWith(".lean.export"))

  val modTasks: Map[String, Task[Vector[Modification]]] = index.map { (name) =>
    val path = resource / name
    val in   = new java.io.ByteArrayInputStream(read.bytes(path))
    name -> Task(getModsFromStream(in)).memoize
  }.toMap

  val loadedNames: mSet[Name] = mSet()

  lazy val baseParser: LeanParser =
    new LeanParser(Seq(),
                   library.LeanMemo.defTaskMap,
                   library.LeanMemo.indTaskMap)

  val defnMap: mMap[Name, Term] = mMap()

  val termIndModMap: mMap[Name, TermIndMod] = mMap()

  val mods: ArrayBuffer[Modification] = ArrayBuffer.empty[Modification]

  def loadTask(f: String): Task[Unit] =
    if (loadedFiles.contains(f)) Task.pure(())
    else
      modTasks(f).map { (m) =>
        mods ++= m
        loadedNames ++= m.map(_.name)
        loadedFiles += f
      }

  val loadedFiles: ArrayBuffer[String] = ArrayBuffer()

  def logUpdate: Logger = {
    case LeanParser.Defined(name, term) => defnMap += name -> term
    case LeanParser.DefinedInduc(name, termIndMod) =>
      termIndModMap += name -> termIndMod
    case LeanParser.ParseWork(expr) => ()
    case LeanParser.Parsed(expr)    => ()
  }

  def indModView(ind: TermIndMod): Js.Value = {
    def introJs(t: Term) =
      Js.Obj("name"  -> Js.Str(t.toString),
             "tex"   -> Js.Str(TeXTranslate(t.typ)),
             "plain" -> Js.Str(t.typ.toString))
    Js.Obj(
      "type"   -> Js.Str("inductive-definition"),
      "name"   -> Js.Str(ind.name.toString),
      "tex"    -> Js.Str(TeXTranslate(ind.typF)),
      "plain"  -> Js.Str(ind.typF.toString),
      "intros" -> Js.Arr(ind.intros.map(introJs): _*)
    )
  }
}

object LeanRoutes extends cask.Routes {
  import LeanResources._
  @cask.get("/files")
  def files(): String = {
//    pprint.log(index.mkString(","))
    sendLog("sending files")
    uwrite(index.toVector)
  }

  @cask.get("/codegen-defns")
  def codeGenDefs(): String =
    uwrite(LeanMemo.defTaskMap.keys.map(_.toString).toVector)

  @cask.get("/codegen-induc-defns")
  def codeGenInducDefs(): String =
    uwrite(LeanMemo.indTaskMap.keys.map(_.toString).toVector)

  @cask.get("/mem-defns") // TODO send definitions
  def memDefs(): String =
    uwrite[Vector[(String, String)]](defnMap.map {
      case (name, term) => name.toString -> TeXTranslate(term)
    }.toVector)

  @cask.get("/mem-induc-defns")
  def memInducDefs(): String =
    uwrite(Js.Arr(termIndModMap.values.toSeq.map(indModView): _*))

  @cask.get("mods/:file")
  def getMods(file: String): String = {
    val path    = resource / file
    val in      = new java.io.ByteArrayInputStream(read.bytes(path))
    val newMods = getModsFromStream(in)
    mods ++= newMods
    val res: Js.Value = Js.Arr(newMods.map { (m: Modification) =>
      val tp = m match {
        case _: trepplein.DefMod   => "definition"
        case _: trepplein.IndMod   => "inductive type"
        case _: trepplein.AxiomMod => "axiom"
        case _                     => m.toString
      }
      Js.Obj("type" -> tp, "name" -> Js.Str(m.name.toString))
    }: _*)
//    pprint.log(res)
    uwrite(res)
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
      uwrite[Js.Value](Js.Obj("type" -> Js.Str("log"), "message" -> Js.Str(s)))
    )

  val sendLogger: Logger = Logger.dispatch(sendLog)

  def messenger: Logger =
    logUpdate && sendLogger

  def parser =
    new LeanParser(mods,
                   library.LeanMemo.defTaskMap,
                   library.LeanMemo.indTaskMap,
                   messenger)

  @cask.post("/loadFile")
  def loadFileReq(request: cask.Request): String = {
    val name = new String(request.readAllBytes())
    loadFile(name)
    s"loading $name"
  }

  def loadFile(name: String): Task[Unit] = {
    if (loadedFiles.contains(name))
      Task(sendLog(s"file $name already loaded"))
    else
      loadTask(name).map { (_) =>
        loadedFiles += name
        sendLog(s"loaded file $name")
      }

  }
  @cask.post("/parse")
  def parse(request: cask.Request): String = {
    def result(name: String, t: Term): Unit = send(
      uwrite[Js.Value](
        Js.Obj("type" -> "parse-result",
               "name" -> name,
               "tex" -> TeXTranslate(t)
                 .replace("'", "\\check "),
               "plain" -> t.toString))
    )
    val name = new String(request.readAllBytes())
    defnMap
      .get(trepplein.Name(name.split("\\."): _*))
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

  @cask.post("/inductive-definition")
  def inducDefn(request: cask.Request) = {
    val name                   = new String(request.readAllBytes())
    val task: Task[TermIndMod] = parser.getIndTask(name)
    task.foreach { (indMod) =>
      send(uwrite[Js.Value](indModView(indMod)))
    }
    s"seeking inductive definition for $name"
  }

  @cask.post("/save-code")
  def saveCode(): String = {
    Task(Try {
      val lc = LeanCodeGen(parser)
      lc.save()
      lc.memo()
    }).foreach((r) => sendLog(s"Result of code generation: $r"))

    pprint.log("generating code")
    "Generating code for all definitions"
  }

  initialize()
}
