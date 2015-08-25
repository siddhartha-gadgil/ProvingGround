package provingground.andrewscurtis

import provingground._

import com.github.nscala_time.time.Imports._
import provingground.andrewscurtis.SimpleAcEvolution._
import scala.annotation.tailrec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import upickle.default._

import reactivemongo.bson.BSONDocument
import reactivemongo.api.collections.bson.BSONCollection

import reactivemongo.api._

import reactivemongo.bson._

//import Hub._ // should import this for a database, unless play plugin database is used.

object SimpleAcRun {
  def getId(thread: Int = 0) =
    s"""SimpleAcEvolution#$thread@${DateTime.now}"""

  def coll(implicit database: DefaultDB) = database("simpleACpaths")

  implicit def mongoUpdate(implicit database: DefaultDB): Path => Unit = (p) => {
    val query = BSONDocument("id" -> p.id)
    val entry = BSONDocument("id" -> p.id, "path" -> write(p.pickle))
    coll(database).update(query, entry)
  }

  implicit def mongoRead(implicit database: DefaultDB): Future[List[Path]] = {
    val futureList = coll(database).find(BSONDocument()).cursor[BSONDocument]().collect[List]()
    val ps = futureList map ((doclist) =>
      for (bd <- doclist; p <- bd.getAs[String]("path")) yield read[PickledPath](p).unpickle)
    ps
  }

  @tailrec
  def iterLog(ps: List[Path], loops: Int, initial: Boolean = false)(implicit update: Path => Unit): List[Path] = {
    if (!initial) ps map (update)
    if (loops < 1) ps else {
      val newPaths = ps map (_.next)
      iterLog(newPaths, loops - 1)(update)
    }
  }

  def continue(ps: List[Path], loops: Int)(implicit update: Path => Unit) = Future(iterLog(ps, loops, true)(update))

  def resume(loops: Int)(implicit dbread: Future[List[Path]], update: Path => Unit) = dbread flatMap ((ps: List[Path]) => continue(ps, loops)(update))

  def start(rank: Int, steps: Int, loops: Int, threads: Int = 6,
    wordCntn: Double = 0.5, size: Double = 1000,
    scale: Double = 0.1)(implicit update: Path => Unit) = {
    val ps = (1 to threads) map ((t: Int) => Path.init(rank, steps, wordCntn, size, scale, getId(t)))
    continue(ps.toList, loops: Int)
  }

}
