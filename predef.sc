import provingground._ , learning._, interface._, translation._, HoTT._
Utils.logger = {
    import scribe._, writer._, Utils._
    logger.withHandler(writer = FileWriter().path(file.LogPath.daily())).replace()
}