package provingground.andrewscurtis

import provingground._

import com.github.nscala_time.time.Imports._
import provingground.andrewscurtis.SimpleAcEvolution.Path
import scala.annotation.tailrec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import upickle.default._

object SimpleAcRun {
  def getId(thread: Int = 0) =
    s"""SimpleAcEvolution#$thread@${DateTime.now}"""

  def mongoUpdate(p: Path) = ???

  def mongoRead: List[Path] = ???

  @tailrec
  def iterLog(ps: List[Path], loops: Int, initial: Boolean = false) : List[Path] = {
    if (!initial) ps map (mongoUpdate)
    if (loops < 1) ps else {
      val newPaths = ps map (_.next)
      iterLog(newPaths, loops-1)
    }
  }

  def continue(ps: List[Path], loops: Int) = Future(iterLog(ps, loops, true))

  def resume(loops: Int) = continue(mongoRead, loops)

  def start(rank: Int, steps: Int, loops: Int, threads: Int = 6,
        wordCntn: Double = 0.5, size: Double = 1000,
        scale: Double = 0.1) = {
    val ps = (1 to threads) map ((t: Int) => Path.init(rank, steps, wordCntn, size, scale, getId(t)))
    continue(ps.toList, loops: Int)
  }


}
