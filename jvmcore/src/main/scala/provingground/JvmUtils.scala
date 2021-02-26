package provingground

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable._
import scala.concurrent._

object JvmUtils {
  @annotation.tailrec
  def gatherMapSet[A, B](
      l: Vector[Vector[A]],
      accum: Set[B],
      fn: A => B,
      limitOpt: Option[Long] = None
  ): Set[B] =
    if (limitOpt
          .map(limit => System.currentTimeMillis() > limit)
          .getOrElse(false)) {
      JvmUtils.logger.debug(
        s"out of time with ${l.size} batches remaining, returning accumulated stuff."
      )
      accum
    } else
      l match {
        case head +: tail =>
          JvmUtils.logger.debug(s"processing ${l.size} batches")
          limitOpt.foreach(
            limit =>
              JvmUtils.logger.debug(
                s"time remaining ${(limit - System.currentTimeMillis()) / 1000} seconds"
              )
          )
          val result = head.par.map(fn)
          JvmUtils.logger.debug(s"mapped batch of size ${head.size}")
          gatherMapSet(tail, result.seq.toSet union (accum), fn, limitOpt)
        case Vector() =>
          JvmUtils.logger.debug(
            s"All batches mapped and gathered, got set of size ${accum.size}"
          )
          accum
      }

    import scribe._, writer._
  var logger = Logger()
  // .setModifiers(List(modify.LevelFilter.>(Level.Debug)))
  // .replace()

  def logDebug() =
    logger = logger.withHandler(
      writer = ConsoleWriter,
      minimumLevel = Some(Level.Debug)
    )

  def logTrace() =
    logger = logger.withHandler(
      writer = ConsoleWriter,
      minimumLevel = Some(Level.Trace)
    )

  def logBrief =
    logger = logger
      .setModifiers(List(modify.LevelFilter.>(Level.Debug)))
      .replace()

}