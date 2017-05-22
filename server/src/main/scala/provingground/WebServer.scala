package provingground
// import example._

import translation._

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import scala.io.StdIn

object ScriptServer extends App {
  implicit val system       = ActorSystem("server-system")
  implicit val materializer = ActorMaterializer()

  implicit val executionContext = system.dispatcher

  import ammonite.ops._

  def path(s: String) = scala.util.Try(Path(s)).getOrElse(pwd / RelPath(s))

  implicit val pathRead: scopt.Read[Path] =
    scopt.Read.reads(path)

  case class Config(
      scriptsDir: Path = pwd / "repl-scripts",
      objectsDir: Path = pwd / "core" / "src" / "main" / "scala" / "provingground" / "scripts",
      host: String = "localhost",
      port: Int = 8080
  )

  val config = Config()

  val parser = new scopt.OptionParser[Config]("provingground-server") {
    head("ProvingGround Server", "0.1")

    opt[String]('i', "interface")
      .action((x, c) => c.copy(host = x))
      .text("server ip")
    opt[Int]('p', "port")
      .action((x, c) => c.copy(port = x))
      .text("server port")
    opt[Path]('s', "scripts")
      .action((x, c) => c.copy(scriptsDir = x))
      .text("scripts directory")
    opt[Path]('o', "objects")
      .action((x, c) => c.copy(objectsDir = x))
      .text("created objects directory")

  }

  parser.parse(args, Config()) match {
    case Some(config) =>
      // println(s"Scopt config: $config")
      val server = new AmmScriptServer(config.scriptsDir, config.objectsDir)
      val bindingFuture =
        Http().bindAndHandle(server.route, config.host, config.port)
      println(
        s"Server online at http://${config.host}:${config.port}\n Press RETURN to stop")
      StdIn.readLine() // let it run until user presses return
      bindingFuture
        .flatMap(_.unbind()) // trigger unbinding from the port
        .onComplete(_ => system.terminate()) // and shutdown when done

    case None =>
      system.terminate()
  }

}
