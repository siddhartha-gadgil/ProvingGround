package provingground.interface
import provingground._, library._

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

  CzSlOly.sessF
  
  while (Utils.running) {
    Thread.sleep(100)
  }

}
