package provingground.interface
import provingground._, library._
import scala.util._, Properties.envOrNone
import Utils._

object CzSlRun extends App {
  Utils.logger = {
    import scribe._, writer._, Utils._
    logger
      .withHandler(writer = FileWriter().path(file.LogPath.daily()))
      .withHandler(
        writer = FileWriter().path(file.LogPath.daily("errors")),
        minimumLevel = Some(Level.Error)
      )
      .withHandler(
        writer = FileWriter().path(file.LogPath.daily("debug")),
        minimumLevel = Some(Level.Debug)
      )
      .replace()
  }

  logger.info(s"Number of threads: $threadNum")

  envOrNone("JAVA_OPTS").foreach(w => logger.info(s"JAVA_OPTS: $w") )

  envOrNone("JAVA_HOME").foreach(w => logger.info(s"JAVA_HOME: $w") )

  CzSlOly.sessF
  
  while (Utils.running) {
    Thread.sleep(100)
  }

}
