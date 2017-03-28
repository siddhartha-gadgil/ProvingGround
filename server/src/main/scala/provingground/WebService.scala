package provingground
// import example._

import akka.http.scaladsl.server.Directives
// import shared.SharedMessages
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._

object  BaseRoute {

  val route = {
    pathSingleSlash {
      get {
        complete {
          provingground.html.index.render(provingground.HoTT.Type.toString)
        }
      }
    } ~
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
}

object CoreServer{
  import ammonite.kernel._

  val kernel = ReplKernel()

  kernel.process("import provingground._; import HoTT._")

  val ammRoute =
    post {
      path("ammker") {
        entity(as[String]) { d =>
          println(s"post received $d")
          val res = kernel.process(d)
          println(res)
          TimeServer.buf = TimeServer.buf :+ res.toString
          println(s"Buffer: ${TimeServer.buf}")
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, res.toString))
        }
      }
    }

  val route = BaseRoute.route ~ ammRoute ~ TimeServer.route
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
