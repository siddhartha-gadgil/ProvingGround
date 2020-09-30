package provingground.interface


import provingground._
import library.LeanMemo
import os._
import monix.eval._

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
import monix.execution.CancelableFuture
import provingground.translation.TeXTranslate
import ujson.Value

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import upickle.default.{write => uwrite, read => _, _}

import scala.util.Try

case class  LeanRoutes()(implicit cc: castor.Context,
                           log: cask.Logger) extends cask.Routes {
  import LeanResources._

  def log: cask.util.Logger = new cask.util.Logger.Console

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

  @cask.get("/mem-defns")
  def memDefs(): String =
    uwrite[Vector[(String, String)]](defnMap.map {
      case (name, term) => name.toString -> TeXTranslate(term, true)
    }.toVector)

  @cask.get("/mem-induc-defns")
  def memInducDefs(): String =
    uwrite(ujson.Arr(termIndModMap.values.toSeq.map(indModView): _*))

  def getModsJs(file: String) : ujson.Value = {
    val path    = resource / file
    val in      = new java.io.ByteArrayInputStream(read.bytes(path))
    val newMods = getModsFromStream(in)
    mods ++= newMods
    val res: ujson.Value = ujson.Arr(newMods.map { (m: Modification) =>
      val tp = m match {
        case _: trepplein.DefMod   => "definition"
        case _: trepplein.IndMod   => "inductive type"
        case _: trepplein.AxiomMod => "axiom"
        case _                     => m.toString
      }
      ujson.Obj("type" -> tp, "name" -> ujson.Str(m.name.toString))
    }: _*)
    res
  }

  @cask.get("mods/:file")
  def getMods(file: String): String = {
    uwrite(getModsJs(file))
  }

  var channels: ArrayBuffer[WebSocketChannel] = ArrayBuffer()

  @cask.websocket("/leanlib-websock")
  def showUserProfile(): cask.WebsocketResult = {
    new WebSocketConnectionCallback() {
      override def onConnect(exchange: WebSocketHttpExchange,
                             channel: WebSocketChannel): Unit = {
        channels += channel
        channel.resumeReceives()
      }
    }
  }

  def send(s: String): Unit =
    channels.foreach((channel: WebSocketChannel) =>
      WebSockets.sendTextBlocking(s, channel))

  def sendLog(s: String): Unit =
    send(
      uwrite[ujson.Value](ujson.Obj("type" -> ujson.Str("log"), "message" -> ujson.Str(s)))
    )

  def sendErr(s: String): Unit =
    send(
      uwrite[ujson.Value](
        ujson.Obj("type" -> ujson.Str("error"), "message" -> ujson.Str(s)))
    )

  val sendLogger: Logger = Logger.dispatch(sendLog)

  def messenger: Logger =
    logUpdate && sendLogger

  def parser =
    new LeanParser(mods.toVector,
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
  @cask.post("/lean-parse")
  def parse(request: cask.Request): String = {
    def result(name: String, t: Term): Unit = send(
      uwrite[ujson.Value](
        ujson.Obj("type" -> "parse-result",
               "name" -> name,
               "tex" -> TeXTranslate(t, true)
                 .replace("'", "\\check "),
               "plain" -> t.toString))
    )
    val name = new String(request.readAllBytes())
    defnMap
      .get(trepplein.Name(name.split("\\.").toIndexedSeq : _*))
      .map { (t) =>
        result(name, t)
        s"previously parsed $name"
      }
      .getOrElse {
        val p  = parser
        
        val cf = {
            import monix.execution.Scheduler.Implicits.global
            p.getTask(name).materialize.runToFuture
        }
        parseCanc += name -> cf
        cf.foreach { (tt) =>
          tt.fold(
            { (err) =>
              val message =
                s"while parsing $name, got $err\n Modifier: \n ${p.findDefMod(
                  trepplein.Name(name.split("\\.").toIndexedSeq: _*)).map(_.value)}"
              pprint.log(message)
              pprint.log(p.findDefMod(
                trepplein.Name(name.split("\\.").toIndexedSeq: _*)).map(_.value), height = 1000)
              sendErr(message)
            }, { (t) =>
              result(name, t)
              defnMap ++= p.defnMap
              termIndModMap ++= p.termIndModMap
              pprint.log(p.defnMap.keys)
              pprint.log(defnMap.keys)
              pprint.log(termIndModMap.keys)
            }
          )

        }
        s"parsing $name"
      }

  }

  @cask.post("/cancel")
  def cancelParse(request: cask.Request): String = {
    val name    = new String(request.readAllBytes())
    val toPurge = parseCanc.filter(_._1 == name)
    pprint.log(s"cancel request for $name, cancelling ${toPurge.size}")
    toPurge.foreach(_._2.cancel())
    parseCanc --= toPurge
    s"cancelling parsing of $name"
  }

  @cask.post("/inductive-definition")
  def inducDefn(request: cask.Request): String = {
    val name                   = new String(request.readAllBytes())
    import monix.execution.Scheduler.Implicits.global
    val task: Task[TermIndMod] = parser.getIndTask(name)
    task.foreach { (indMod) =>
      send(uwrite[ujson.Value](indModView(indMod)))
    }
    s"seeking inductive definition for $name"
  }

  @cask.post("/save-code")
  def saveCode(): String = {
    pprint.log("generating code")
    val r = Try {
      val p = parser
      p.defnMap ++= defnMap
      p.termIndModMap ++= termIndModMap
      val lc = LeanCodeGen(p)
      lc.save()
      lc.memo()
    }
    pprint.log(r)
    sendLog(s"Result of code generation: $r")

    "Generating code for all definitions"
  }

  initialize()
}
