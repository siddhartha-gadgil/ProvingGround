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

import java.io._

import scala.io.Source

object SimpleAcRun {
  def getId(thread: Int = 0) =
    s"""SimpleAcEvolution#$thread@${DateTime.now}"""

  object Casbah {
    import Hub.Casbah._
    val collection = db("simpleACpaths")

    implicit def mongoRead: Future[List[Path]] =
      Future {
        val doclist = collection.find().toList
        for (bd <- doclist; p <- bd.getAs[String]("path"))
          yield read[PickledPath](p).unpickle
      }

    implicit def mongoUpdate: Path => Unit =
      (p) => {
        val query = MongoDBObject("id" -> p.id)
        val entry = MongoDBObject("id" -> p.id, "path" -> write(p.pickle))
        collection.update(query, entry, upsert = true)
      }
  }

  object Reactive {
    import Hub.ReactiveMongo._
    def coll(implicit database: DefaultDB) = database("simpleACpaths")

    implicit def mongoUpdate(implicit database: DefaultDB): Path => Unit =
      (p) => {
        val query = BSONDocument("id" -> p.id)
        val entry = BSONDocument("id" -> p.id, "path" -> write(p.pickle))
        coll(database).update(query, entry, upsert = true)
      }

    implicit def mongoRead(implicit database: DefaultDB): Future[List[Path]] = {
      val futureList = coll(database)
        .find(BSONDocument())
        .cursor[BSONDocument]()
        .collect[List]()
      val ps =
        futureList map
          ((doclist) =>
             for (bd <- doclist; p <- bd.getAs[String]("path"))
               yield read[PickledPath](p).unpickle)
      ps
    }
  }

  class InMem {
    var dict: Map[String, String] = Map.empty

    def save(filename: String) = {
      val pr = new PrintWriter(filename)

      val d = dict

      for ((x, y) <- d) pr.println(write((x, y)))

      pr.close
    }

    def load(filename: String) = {
      val list =
        Source.fromFile(filename).getLines.toList map
          ((x) => read[(String, String)](x))

      dict = list.toMap
    }

    implicit def memUpdate: Path => Unit =
      (p) => {
        dict = dict + (p.id -> write(p.pickle))
//      println(s"$p.id -> $p")
      }

    implicit def memRead: Future[List[Path]] =
      Future(for ((_, p) <- dict.toList) yield read[PickledPath](p).unpickle)
  }

  object MemDB extends InMem

  @tailrec
  def iter(p: Path, loops: Int, initial: Boolean = false)(
      implicit update: Path => Unit): Path = {
    if (!initial) update(p)
    if (loops < 1) p
    else {
      val newPath = p.next
      println(s"looped; remaining loops: ${loops - 1}")
      iter(newPath, loops - 1)(update)
    }
  }

  def continue(ps: List[Path], loops: Int)(implicit update: Path => Unit) =
    ps map ((p) => Future(iter(p, loops, true)(update)))

  def resume(loops: Int)(implicit dbread: Future[List[Path]],
                         update: Path => Unit) =
    dbread flatMap
      ((ps: List[Path]) => Future.sequence(continue(ps, loops)(update)))

  def restart(rank: Int,
              steps: Int,
              loops: Int,
              threads: Int = 6,
              wordCntn: Double = 0.5,
              size: Double = 1000,
              scale: Double = 1.0)(implicit dbread: Future[List[Path]],
                                   update: Path => Unit) = {
    val p = new PathView()(dbread)

    val ps = for (j <- 0 to threads - 1)
      yield
        Path(rank,
             steps,
             wordCntn,
             size,
             scale,
             List(State(rank, p.Mdist, p.mkFDV(threads, rank)(j))),
             List(),
             getId(j))

    continue(ps.toList, loops: Int)
  }

  def start(rank: Int,
            steps: Int,
            loops: Int,
            threads: Int = 6,
            wordCntn: Double = 0.5,
            size: Double = 1000,
            scale: Double = 1.0)(implicit update: Path => Unit) = {
    val ps =
      (1 to threads) map
        ((t: Int) => Path.init(rank, steps, wordCntn, size, scale, getId(t)))
    continue(ps.toList, loops: Int)
  }

  /**
    *  methods for viewing generated paths
    *  create an instance of this and import for easy use.
    */
  class PathView(implicit dbread: Future[List[Path]]) {
    lazy val paths = scala.concurrent.Await
      .result(dbread, scala.concurrent.duration.Duration.Inf)

    lazy val finalproofs = for (p <- paths) yield (p.current.fdV)

    lazy val finalthms = for (p <- paths) yield (p.current.fdP)

    lazy val finalMs = for (p <- paths) yield (p.current.fdM)

    lazy val Mdist = vBigSum(finalMs).flatten.normalized()

    lazy val proofs = vBigSum(finalproofs).flatten.normalized()

    lazy val thms = vBigSum(finalthms).flatten.normalized()

    lazy val thmsView = thms.entropyView

    def proofsOf(thm: Presentation) =
      proofs filter ((pf: Moves) => actOnTriv(thm.rank)(pf) == thm)

    def pickProof(thm: Presentation) = proofsOf(thm).next

    def mkFDV(groups: Int, rank: Int) =
      thms filter ((thm: Presentation) => thm.rank == rank) normalized () split
        (groups) mapValues {
        _ map (pickProof(_))
      }
  }
}
