package provingground.andrewscurtis

import provingground.Collections._
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

import com.mongodb.casbah.Imports._

import FiniteDistribution._

import LinearStructure._

import FreeGroups._

import Moves._

import Hub._ // should import this for a database, unless play plugin database (or something else) is used.

object SimpleAcRun {
  def getId(thread: Int = 0) =
    s"""SimpleAcEvolution#$thread@${DateTime.now}"""

  object Casbah {
    import Hub.Casbah._
    val collection = db("simpleACpaths")

    implicit def mongoRead: Future[List[Path]] =
      Future {
        val doclist = collection.find().toList
        for (bd <- doclist; p <- bd.getAs[String]("path")) yield read[PickledPath](p).unpickle
      }

    implicit def mongoUpdate: Path => Unit = (p) => {
      val query = MongoDBObject("id" -> p.id)
      val entry = MongoDBObject("id" -> p.id, "path" -> write(p.pickle))
      collection.update(query, entry)
    }
  }

  object Reactive {
    import Hub.ReactiveMongo._
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

  /**
   *  methods for viewing generated paths
   *  create an instance of this and import for easy use.
   */
  class PathView(implicit dbread: Future[List[Path]]) {
    lazy val paths = scala.concurrent.Await.result(dbread, scala.concurrent.duration.Duration.Inf)

    lazy val finalproofs = for (p <- paths) yield (p.current.fdV)

    lazy val finalthms = for (p <- paths) yield (p.current.fdP)

    lazy val proofs = vBigSum(finalproofs)

    lazy val thms = vBigSum(finalthms)

    lazy val thmsView = thms.entropyView

    def proofsOf(thm: Presentation) =
      for (p <- proofs.support  if actOnTriv(thm.rank)(p) == thm) yield Weighted(p, proofs(p))

  }

}
