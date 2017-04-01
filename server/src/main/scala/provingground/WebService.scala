package provingground
// import example._

import akka.http.scaladsl.server.Directives
// import shared.SharedMessages
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._

object  BaseServer {

  val route =
      pathPrefix("assets" / Remaining) { file =>
        // optionally compresses the response with Gzip or Deflate
        // if the client accepts compressed responses
        encodeResponse {
          getFromResource("public/" + file)
        }
      } ~
        pathPrefix("static" / Remaining) { file =>
          // optionally compresses the response with Gzip or Deflate
          // if the client accepts compressed responses
            val f = new java.io.File("static/"+ file)
            println("serving from file: " + f)
            getFromFile(f)

        }

}

class AmmService{
  import ammonite.kernel._

  import ammonite.ops._

  val initCommands = "import provingground._\nimport HoTT._\nimport TLImplicits._\nimport shapeless._\n"

  val scriptsDir = pwd / "repl-scripts"

  def listScripts = ls(scriptsDir).filter(_.ext == "sc").map(_.name.drop(scriptsDir.name.length).dropRight(3))

  def script(name: String) = read(scriptsDir / s"${name}.sc")

  def saveScript(name: String, body: String) = {
    write.over(scriptsDir / s"${name}.sc", body)
  }

  def makeObject(name: String, body: String) =
    s"""package provingground.scripts

object $name{
  $body
}
"""

  val objectsDir = pwd / "core" / "src" / "main" / "scala" / "provingground" / "scripts"

  def clean(body: String) =
    body.split("\n").filter((l) => !l.startsWith("//result:")).mkString("\n")

  def saveObject(name: String, body: String) =
    write.over(objectsDir / s"${name}.sc", makeObject(name, clean(body)))

  implicit var kernel = ReplKernel()

  // import scala.collection.mutable.ArrayBuffer
  var prevCode = ""

  def initKernel() = {

    kernel.process(initCommands)

    prevCode = initCommands

    println("initialized kernel")

  }

  initKernel()

  def newCode(s: String) = if (s.startsWith(prevCode)) Some(s.drop(prevCode.length)) else None

  def useCode(s: String) =
    newCode(s).getOrElse{
      kernel = ReplKernel()
      initKernel()
      s
  }


  def kernelRes(inp: String)(implicit ker: ReplKernel): Option[Either[String, Any]] =
    ker.process(inp) map {resp =>
      resp.toEither match {
        case Right(evaluation) =>
          Right(evaluation.value)
        case Left(nel) =>
          Left(nel.list.toList.map(_.msg).mkString(";"))
      }
    }



  val route =
    post {
      path("kernel") {
        entity(as[String]) { d =>
          println(s"post received:\n$d")
          val code = useCode(d)
          println(s"processing code: \n$code")
          val res = kernelRes(code)
          println(res)
          res.foreach{e =>
            e.foreach((_) => prevCode = d)
          }
          // TimeServer.buf = TimeServer.buf :+ res.toString
          // println(s"Buffer: ${TimeServer.buf}")
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, res.toString))
        }
      }
    } ~
    get {
      path("list-scripts") {
        complete(HttpEntity(ContentTypes.`text/plain(UTF-8)`, listScripts.mkString("\n")))
      }
    } ~
    post {
      path("save-script" / Segment) {name =>
        entity(as[String]) { body =>
          saveScript(name, body)
          complete("Saved script")
        }}
    } ~
    get {
      path("script" / Segment) {name =>
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, script(name)))}
    }

}

object AmmServer extends AmmService

object TestServer{
  val testRoute = {
    pathSingleSlash {
      get {
        complete {
          provingground.html.index.render()
        }
      }
    }
  }

  val route = testRoute ~ BaseServer.route ~ AmmServer.route // ~ TimeServer.route
}

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes.PermanentRedirect
import akka.http.scaladsl.server.Directives
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import de.heikoseeberger.akkasse.{ EventStreamMarshalling, ServerSentEvent }
import java.time.LocalTime
import java.time.format.DateTimeFormatter
import scala.concurrent.duration.DurationInt



object TimeServer {

  var buf = Vector[String]()


  def route = {
    import Directives._
    import EventStreamMarshalling._



  val src = Source
  .tick(100.millis, 100.millis, NotUsed)
  .mapConcat(_ => {
    val b = buf
    buf = Vector()
    b
  }).map (ServerSentEvent(_))
  .keepAlive(1.second, () => ServerSentEvent.heartbeat)
            //
            // .map(_ => LocalTime.now())
            // .map(_ => ServerSentEvent("this"))
            // .keepAlive(1.second, () => ServerSentEvent.heartbeat)


    def events =
      path("events") {
        get {
          complete {
            src
          }
        }
      }

    events
  }

  private def timeToServerSentEvent(time: LocalTime) =
    ServerSentEvent(DateTimeFormatter.ISO_LOCAL_TIME.format(time))
}
