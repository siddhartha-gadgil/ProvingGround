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

  implicit var kernel = ReplKernel()

  // import scala.collection.mutable.ArrayBuffer
  var prevCode = ""

  def initKernel() = {

    val initCommands = "import provingground._\nimport HoTT._\nimport TLImplicits._\nimport shapeless._\n"

    kernel.process(initCommands)

    prevCode = initCommands

    println("initialized kernel")

  }

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
      path("ammker") {
        entity(as[String]) { d =>
          println(s"post received:\n$d")
          val code = useCode(d)
          println(s"processing code: \n$code")
          val res = kernelRes(code)
          println(res)
          res.foreach{e =>
            e.foreach((_) => prevCode = d)
          }
          TimeServer.buf = TimeServer.buf :+ res.toString
          // println(s"Buffer: ${TimeServer.buf}")
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, res.toString))
        }
      }
    }

}

object AmmServer extends AmmService

object TestServer{
  val testRoute = {
    pathSingleSlash {
      get {
        complete {
          provingground.html.test.render(provingground.HoTT.Type.toString)
        }
      }
    }
  }

  val route = testRoute ~ BaseServer.route ~ AmmServer.route ~ TimeServer.route
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
