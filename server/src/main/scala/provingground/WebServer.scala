package provingground
// import example._

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import scala.io.StdIn

object ScriptServer  extends App {
    implicit val system = ActorSystem("server-system")
    implicit val materializer = ActorMaterializer()

    implicit val executionContext = system.dispatcher

    import ammonite.ops._

    case class Config(
      scriptsDir : Path = pwd / "repl-scripts",
      objectsDir : Path = pwd / "core" / "src" / "main" / "scala" / "provingground" / "scripts",
      host: String = "localhost",
      port: Int = 8080
    )

    val config = Config()

    val interface = "localhost"
    val port = 8080

    val server = new AmmScriptServer(config.scriptsDir, config.objectsDir)

    val bindingFuture = Http().bindAndHandle(server.route, config.host, config.port)

    println(s"Server online at http://${config.host}:${config.port}\n Press RETURN to stop")

    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done

}
