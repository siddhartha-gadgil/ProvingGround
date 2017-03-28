package provingground
// import example._

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import scala.io.StdIn

object WebServer {
  def main(args: Array[String]) {
    implicit val system = ActorSystem("server-system")
    implicit val materializer = ActorMaterializer()

    implicit val executionContext = system.dispatcher

    // val config = ConfigFactory.load()
    val interface = "localhost" //config.getString("http.interface")
    val port = 8080//config.getInt("http.port")

    // val service = new WebService()

    val kernel = ammonite.kernel.ReplKernel()

    println(kernel.process("provingground.HoTT.Type"))

    val bindingFuture = Http().bindAndHandle(CoreServer.route, interface, port)

    println(s"Server online at http://$interface:$port\n Press RETURN to stop")

    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}
