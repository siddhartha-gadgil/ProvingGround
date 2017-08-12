package provingground
// import example._

import translation._

import akka.http.scaladsl.server.Directives
// import shared.SharedMessages
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._

import scala.util.Try

import upickle.{Js, json}

object BaseServer {

  val route =
    pathPrefix("assets" / Remaining) { file =>
      // println(s"asset requested: assets/$file")
      getFromResource("public/" + file)

    } ~
      pathPrefix("static" / Remaining) { file =>
        // optionally compresses the response with Gzip or Deflate
        // if the client accepts compressed responses
        val f = new java.io.File("static/" + file)
        // println("serving from file: " + f)
        getFromFile(f)

      } ~ path("resources" / Remaining) { path =>
      // println("serving from resource: " + path)
      getFromResource(path.toString)
    }

}

import ammonite.ops._

class AmmService(
    val scriptsDir: Path = pwd / "repl-scripts",
    val objectsDir: Path = pwd / "core" / "src" / "main" / "scala" / "provingground" / "scripts") {
  import ammonite.kernel._

  val initCommands =
    "import provingground._\nimport HoTT._\nimport induction.TLImplicits._\nimport shapeless._\n; repl.pprinter.bind(translation.FansiShow.simplePrint)"

  def listScripts =
    ls(scriptsDir)
      .filter(_.ext == "sc")
      .map(_.name.drop(scriptsDir.name.length).dropRight(3))

  def script(name: String) = read(scriptsDir / s"${name}.sc")

  def saveScript(name: String, body: String) = {
    write.over(scriptsDir / s"${name}.sc", body)
  }

  def makeObject(name: String,
                 body: String,
                 header: String = "package provingground.scripts") =
    s"""$header

object $name{
$body
}
"""

  def clean(body: String) =
    body
      .split("\n")
      .filter((l) => !l.startsWith("//result:"))
      .map("  " + _)
      .mkString("\n")

  def saveObject(name: String, body: String) =
    write.over(objectsDir / s"${name}.scala", makeObject(name, clean(body)))

  // implicit var kernel = ReplKernel()

  // import scala.collection.mutable.ArrayBuffer
  var prevCode = ""

  def initKernel() = {

    // kernel.process(initCommands)

    prevCode = initCommands

    // println("initialized kernel")

  }

  // initKernel()

  // def newCode(s: String) =
  //   if (s.startsWith(prevCode)) Some(s.drop(prevCode.length)) else None
  //
  // def useCode(s: String) =
  //   newCode(s).getOrElse {
  //     kernel = ReplKernel()
  //     initKernel()
  //     s
  //   }

  def kernelRes(inp: String)(
      implicit ker: ReplKernel): Option[Either[String, Any]] =
    ker.process(inp) map { resp =>
      resp.toEither match {
        case Right(evaluation) =>
          Right(evaluation.value)
        case Left(nel) =>
          Left(nel.list.toList.map(_.msg).mkString(";"))
      }
    }

  def kernelJS(res: Option[Either[String, Any]]) = {
    import HoTT._
    // val res = kernelRes(inp)
    val base = res match {
      case Some(Right(evaluation)) =>
        evaluation match {
          case t: Term =>
            Js.Obj("result" -> Js.Str(evaluation.toString),
                   "tex" ->
                     Js.Str(TeXTranslate(t)))
          case _ => Js.Obj("result" -> Js.Str(evaluation.toString))
        }
      case Some(Left(log)) => Js.Obj("log" -> Js.Str(log.toString))
      case None            => Js.Obj("log" -> Js.Str("No response from kernel"))
    }
    base
  }

  import java.io.OutputStream

  class StringOutputStream extends OutputStream {
    private val bytes = collection.mutable.ArrayBuffer[Byte]()

    def isEmpty = bytes.isEmpty

    def reset = bytes.clear()

    override def write(b: Int): Unit = {
      bytes += b.toByte
    }

    def string: String = new String(bytes.toArray.map(_.toChar))
  }

  def replResult(code: String) = {
    import java.io._
    val outputS = new ByteArrayOutputStream()
    val errLog  = new ByteArrayOutputStream()
    val infoLog = new ByteArrayOutputStream()

    import java.nio.charset.StandardCharsets

    val ammMain =
      ammonite.Main(
        predefCode =
          "interp.colors() = ammonite.util.Colors.BlackWhite\n @ repl.frontEnd() = ammonite.repl.FrontEnd.JLineUnix\n @ repl.prompt() = \"\\nscala> \" ",
        inputStream = new ByteArrayInputStream(
          (code + "\nexit\n").getBytes(StandardCharsets.UTF_8)),
        outputStream = outputS,
        errorStream = errLog,
        verboseOutput = false,
        remoteLogging = false
        // // infoStream = infoLog
      )

    println(code)

    ammMain.run()

    val output = new String(outputS.toByteArray, "UTF-8")

    val err = new String(errLog.toByteArray, "UTF-8")

    val info = new String(infoLog.toByteArray, "UTF-8")

    println(
      s"output (is this okay?) :\n${output}\n log: ${info}\n errors: ${err}")

    if (err == "") Right(output)
    else Left("--INFO--\n" + info + err + "--OUTPUT--\n" + output)
  }

  def replResJS(code: String) = replResult(code) match {
    case Right(res) =>
      println(s"\n\nraw:\n$res")
      println(s"\n\nJs.Str:\n${Js.Str(res)}")
      Js.Obj("result" -> Js.Str(res))
    case Left(log) => Js.Obj("log" -> Js.Str(log))
  }

  val route =
    post {
      path("kernel") {
        entity(as[String]) { d =>
          println(s"post received:\n$d")
          // val code = useCode(d)
          // println(s"processing code: \n$code")
          // val res = kernelRes(code)
          // println(res)
          // res.foreach { e =>
          //   e.foreach((_) => prevCode = d)
          // }
          val result =
            replResult(d) match {
              case Right(z) => "--RESULT--\n" + z
              case Left(z)  => "--ERROR--\n" + z
            }
          complete(HttpEntity(ContentTypes.`text/plain(UTF-8)`, result))
        // val js = kernelJS(res)
        // val js = replResJS(d)
        // println(js)
        //Js.Obj("result" -> Js.Str("not implemented"))
        //Js.Obj("response" -> Js.Str(res.toString))
        // println(res)
        // println(kernelJS(res).toString)
        // TimeServer.buf = TimeServer.buf :+ res.toString
        // println(s"Buffer: ${TimeServer.buf}")
        // complete(HttpEntity(ContentTypes.`application/json`, json.write(js)))
        }
      }
    } ~
      get {
        path("list-scripts") {
          complete(HttpEntity(ContentTypes.`text/plain(UTF-8)`,
                              listScripts.mkString("\n")))
        }
      } ~
      post {
        path("save-script" / Segment) { name =>
          entity(as[String]) { body =>
            val res = Try(saveScript(name, body))
              .map(_ => s"saved script $name")
              .toString
            // println(res)
            complete(res)
          }
        }
      } ~
      get {
        path("script" / Segment) { name =>
          complete(
            HttpEntity(ContentTypes.`text/plain(UTF-8)`,
                       Try(script(name)).toString))
        }
      } ~
      post {
        path("create-object" / Segment) { name =>
          entity(as[String]) { body =>
            val res = Try(saveObject(name, body))
              .map(_ => s"created object $name")
              .toString
            complete(res)
          }
        }
      } ~
      get {
        path("load-object" / Segment) { name =>
          val objTry = Try(makeObject(name, clean(script(name)), ""))
          complete(
            HttpEntity(ContentTypes.`text/plain(UTF-8)`, objTry.toString))
        }
      }

}

// object AmmServer extends AmmService()

class AmmScriptServer(
    val scriptsDir: Path = pwd / "repl-scripts",
    val objectsDir: Path = pwd / "core" / "src" / "main" / "scala" / "provingground" / "scripts") {
  val htmlRoute = {
    pathSingleSlash {
      get {
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, indexHTML))
      }
    }
  }

  val indexHTML =
    """
<!DOCTYPE html>

<html>
  <head>
    <title>Script Editor</title>
    <link rel="stylesheet" href="/resources/bootstrap.min.css">
    <script src="/resources/src-min/ace.js" type="text/javascript" charset="utf-8"></script>
    <link rel="stylesheet" href="/resources/katex.min.css">
    <script src="/resources/katex.min.js" type="text/javascript" charset="utf-8"></script>
   <script src="/resources/auto-render.min.js" type="text/javascript" charset="utf-8"></script>
    <link rel="stylesheet" href="/resources/github-gist.css">
    <script src="/resources/highlight.pack.js" type="text/javascript" charset="utf-8"></script>
    <script src="/resources/provingground-js-fastopt.js" type="text/javascript" charset="utf-8"></script>
    <style type="text/css" media="screen">
        .editor {
            height: 300px;
            font-size: 14px;

        }
        .view {
          overflow-y: auto;
          height: 300px;
        }
        .btn-space {
    margin-right: 5px;
}
    </style>
  </head>
  <body>

  <div class="container">
    <h2> Proving Ground script editor </h2>


    <div id="edit-div"></div>
  </div>

  <script>
    provingground.CodeEditorJS().main()
    </script>

  </body>
</html>
"""

  val AmmServer = new AmmService(scriptsDir, objectsDir)

  val route = htmlRoute ~ BaseServer.route ~ AmmServer.route // ~ TimeServer.route

}

// import akka.NotUsed
// import akka.actor.ActorSystem
// import akka.http.scaladsl.Http
// import akka.http.scaladsl.model.StatusCodes.PermanentRedirect
// import akka.http.scaladsl.server.Directives
// import akka.stream.ActorMaterializer
// import akka.stream.scaladsl.Source
// import de.heikoseeberger.akkasse.{ EventStreamMarshalling, ServerSentEvent }
// import java.time.LocalTime
// import java.time.format.DateTimeFormatter
// import scala.concurrent.duration.DurationInt
//
//
//
// object TimeServer {
//
//   var buf = Vector[String]()
//"Success:" +
//
//   def route = {
//     import Directives._
//     import EventStreamMarshalling._
//
//
//
//   val src = Source
//   .tick(100.millis, 100.millis, NotUsed)
//   .mapConcat(_ => {
//     val b = buf
//     buf = Vector()
//     b
//   }).map (ServerSentEvent(_))
//   .keepAlive(1.second, () => ServerSentEvent.heartbeat)
//             //
//             // .map(_ => LocalTime.now())
//             // .map(_ => ServerSentEvent("this"))
//             // .keepAlive(1.second, () => ServerSentEvent.heartbeat)
//
//
//     def events =
//       path("events") {
//         get {
//           complete {
//             src
//           }
//         }
//       }
//
//     events
//   }
//
//   private def timeToServerSentEvent(time: LocalTime) =
//     ServerSentEvent(DateTimeFormatter.ISO_LOCAL_TIME.format(time))
// }
