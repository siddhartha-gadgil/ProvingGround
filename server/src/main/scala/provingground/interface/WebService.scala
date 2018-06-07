package provingground.interface
// import example._

import java.nio.charset.StandardCharsets

import provingground._
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
  // import ammonite.kernel._

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

  var prevCode = ""

  def initKernel() = {
    prevCode = initCommands
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

  import java.nio.charset.StandardCharsets

  def replResult(code: String) = {
    import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
    val outputS = new ByteArrayOutputStream()
    val errLog  = new ByteArrayOutputStream()
    val infoLog = new ByteArrayOutputStream()
    val inpStream = new ByteArrayInputStream(
      (code + "\nexit\n").getBytes(StandardCharsets.UTF_8))

    println(code)

    pprint.log("running ammonite")

    val ammMain =
      Console.withIn(inpStream){
        Console.withErr(errLog){
          Console.withOut(outputS){
            ammonite.Main.main0(
              args = List("--predef-code",
                "interp.colors() = ammonite.util.Colors.BlackWhite\n"),
              stdIn = inpStream,
              stdOut = outputS,
              stdErr = errLog
            )

          }
        }
      }

     println("ran ammonite")

    val silly = """\[[0-9]+[A-Z]""".r


    val output =
      silly.replaceAllIn((new String(outputS.toByteArray, "UTF-8")).replace("\u001b", "") , "")


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

          val result =
            replResult(d) match {
              case Right(z) => "--RESULT--\n" + z
              case Left(z)  => "--ERROR--\n" + z
            }
          complete(HttpEntity(ContentTypes.`text/plain(UTF-8)`, result))
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
      } ~
        get {
          path("resources" / Remaining) { path =>
             println("serving from resource: " + path)
            getFromResource(path.toString)
          }
        }

}

// object AmmServer extends AmmService()

class AmmScriptServer(
    val scriptsDir: Path = pwd / "repl-scripts",
    val objectsDir: Path = pwd / "core" / "src" / "main" / "scala" / "provingground" / "scripts") {
  val htmlRoute = {
    (pathSingleSlash | path("index.html")) {
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
    CodeEditorJS.main()
    </script>

  </body>
</html>
"""

  val AmmServer = new AmmService(scriptsDir, objectsDir)

  val route = htmlRoute ~ BaseServer.route ~ AmmServer.route // ~ TimeServer.route

}
