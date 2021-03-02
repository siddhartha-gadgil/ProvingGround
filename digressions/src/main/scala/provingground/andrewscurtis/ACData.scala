package provingground.andrewscurtis

import ammonite.ops._

import provingground._

import learning._

import Collections._

import upickle.default.{write => uwrite, read => uread, _}

import LinearStructure._

import FiniteDistribution._

import ACLooper._

import scala.io.Source

import akka.actor._

import FreeGroups._

import ACElem._

import ACData._

import akka.stream._

import akka.stream.scaladsl.{Source => Src, _}

// import com.mongodb.casbah.Imports._

import com.github.nscala_time.time.Imports._

import ACFlow._

case class ACData(paths: Map[String,
                             LazyList[(FiniteDistribution[AtomicMove],
                                     FiniteDistribution[Moves])]],
                  dir: String)
    extends ACresults(paths) {

  def last = ACStateData(states, dir)

  def take(n: Int) = ACData(paths.view.mapValues (_ take (n)).toMap, dir)

  def resetFiles() = {
    for ((name, data) <- paths) yield {
      write.over(wd / dir / name, pickle(data.toVector.last))
    }
  }

  def thmCSV(name: String, rank: Int = 2) = {
    val supp = thmSupp(name, rank)
    val file = wd / dir / s"$name.csv"
    rm(file)
    val tVec = thmVec(name, rank)
    def pVec(p: FreeGroups.Presentation) =
      tVec map ((fd) => fd(p))
    supp.foreach((p) => {
      write.append(file, s""""$p",${pVec(p).mkString(",")}\n""")
    })
  }
}

case class ACStateData(
    states: Map[String,
                (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])],
    dir: String)
    extends ACStates {
  def revive(name: String, p: Param = Param())(implicit hub: ActorRef) = {
    import p._
    val state = states(name)
    val ref   = ???
    // ACLooper.spawn(name,
    //                        rank,
    //                        size,
    //                        wrdCntn,
    //                        state,
    //                        ACData.srcRef(dir, rank),
    //                        p)
//    FDHub.start(ref)
    ref
  }

  def reviveAll(p: Param = Param())(implicit hub: ActorRef) = {
    val refs = for (name <- names) yield revive(name, p)
    refs
  }

  def spawn(name: String, p: Param = Param()) = {
    import p._
    // ACLooper.spawn(name,
    //                rank,
    //                size,
    //                wrdCntn,
    //                blended,
    //                ACData.srcRef(dir, rank),
    //                p)
  }

  def spawns(name: String, mult: Int = 4, p: Param = Param()) = {
    for (j <- 1 to mult) yield spawn(name + "." + j.toString, p)
  }
}

import FDLooper._

class ACFileSaver(dir: String = "acDev", rank: Int = 2)
    extends FDSrc[(FiniteDistribution[AtomicMove], FiniteDistribution[Moves]),
                  Param] {
  def save =
    (snap: Snap) => fileSave(snap.name, dir, rank)(snap.state._1, snap.state._2)
}

object ACFileSaver {
  def props(dir: String = "acDev", rank: Int) =
    Props(new ACFileSaver(dir, rank))

  def actorRef(dir: String = "acDev", rank: Int) =
    Hub.system.actorOf(props(dir, rank))
}

object ACFlowSaver {

  /**
    * Saving to files.
    */
  def fileSaver(dir: String = "acDev", rank: Int = 2) =
    Sink.foreach { (snap: Snap) =>
      fileSave(snap.name, dir, rank)(snap.state._1, snap.state._2)
    }

  /**
    * ActorRef for messages to start a flow, with file saving.
    * Obtained by materializing source with file saver sink.
    */
  def actorRef(dir: String = "acDev", rank: Int = 2) = {
    val sink = fileSaver(dir, rank)
    sink.runWith(src)
  }

  val snapd = wd / "ACSnaps"

  import Snap._

  def snapFile(batch: Int) =
    snapd / (LocalDate.now().toString()) / batch.toString

  def snapSave =
    (snap: Snap) => write.append(snapFile(snap.loops / 100), uwrite(snap))

  val snapSink = Sink.foreach(snapSave)

  def snapSource(date: String, batch: String) =
    Src(read.lines(snapd / date / batch.toString).to(LazyList))

  def snapStream(date: String) =
    (ls(wd / date).to(LazyList) map (_.last))
      .sortBy(_.toInt)
      .flatMap((filename: String) =>
        (read.lines(snapd / date / filename)).to(LazyList))
      .map((s) => uread[Snap](s))

  def snapSource(date: String) = Src(snapStream(date))
  //  import Hub.Casbah._
  //
  // /**
  //   * Database for elements
  //   */
  // val elems = db("ACElems")
  //
  // /**
  //   * Database for actors:
  //   * on creation additional parameter data can be added; only number of loops is updated.
  //   */
  // val actorData = db("ACActorData")
  //
  // /**
  //   * Data base for weights on atomic moves.
  //   */
  // val fdMdb = db("AC-FDM")
  //
  // /**
  //   * Database for presentations (theorems).
  //   */
  // val thmsdb = db("AC-THMS")
  //
  // /**
  //   * add element to database
  //   */
  // def addElem(elem: ACElem) = {
  //   import elem._
  //   val obj = MongoDBObject("name" -> name,
  //                           "moves"        -> uwrite(moves),
  //                           "presentation" -> uwrite(pres),
  //                           "loops"        -> loops,
  //                           "weight"       -> weight)
  //   elems.insert(obj)
  // }
  //
  // /**
  //   * add presentation (theorem) to database
  //   */
  // def addThm(thm: ACThm) = {
  //   import thm._
  //   val obj = MongoDBObject("name" -> name,
  //                           "presentation" -> uwrite(pres),
  //                           "loops"        -> loops,
  //                           "weight"       -> weight)
  //   thmsdb.insert(obj)
  // }
  //
  // /**
  //   * Sink for adding all elements from snapshot
  //   */
  // def elemsCashbahAdd =
  //   elemsFlow to Sink.foreach(addElem)
  //
  // /**
  //   * sink for adding all theorems from snapshot.
  //   */
  // def thmsCashbahAdd =
  //   thmsFlow to Sink.foreach(addThm)
  //
  // /**
  //   * update number of loops from snapshot
  //   */
  // def updateLoops(name: String, loops: Int) = {
  //   val query  = MongoDBObject("name" -> name)
  //   val update = $set("loops" -> loops)
  //   val exists = !(actorData.findOne(query).isEmpty)
  //   if (exists)
  //     actorData.update(query, update) // updat number of loops, leaving other actor data unchanged.
  //   else
  //     actorData.insert(MongoDBObject("name" -> name, "loops" -> loops)) // if no actor data, add number of loops.
  // }
  //
  // /**
  //   * Update the weights of atomic moves given name and distribution
  //   */
  // def updateFDM(name: String, fdM: FiniteDistribution[AtomicMove]) = {
  //   val query = MongoDBObject("name" -> name)
  //   val entry = MongoDBObject("name" -> name, "fdM" -> uwrite(fdM))
  //   fdMdb.update(query, entry, upsert = true)
  // }
  //
  // /**
  //   * Sink for updating loops in database from snapshot
  //   */
  // val loopsCasbahUpdate =
  //   loopsFlow to Sink.foreach {
  //     case (name, loops) => updateLoops(name, loops)
  //   }
  //
  // /**
  //   * sink for updating weights of atomic moves in databse from snapshot
  //   */
  // val fdmCasbahUpdate =
  //   fdMFlow to Sink.foreach { case (name, fdm) => updateFDM(name, fdm) }
  //
  // /**
  //   *  ActorRef from materialized flow saving various things in Casbah mongo database
  //   */
  // def mongoSaveRef[M](
  //     interface: Sink[Snap, M] = Sink.foreach((x: Snap) => {})) = {
  //   // Note: loops saved last so they should reflect the correct number.
  //   val sink =
  //     fl alsoTo fdmCasbahUpdate alsoTo thmsCashbahAdd alsoTo elemsCashbahAdd alsoTo loopsCasbahUpdate to interface
  //   sink.runWith(src)
  // }
  //
  // /**
  //   * returns iterator of weights of a given presentation for a given actor at all stages, ordered by loops/
  //   */
  // def presWeights(name: String, pres: Presentation) = {
  //   val query  = MongoDBObject("presentation" -> uwrite(pres), "name" -> name)
  //   val sorter = MongoDBObject("loops" -> 1)
  //   val cursor = thmsdb.find(query)
  //   for (e <- cursor) yield e.as[Double]("weight")
  // }
  //
  // /**
  //   * returns finite distribution on Moves given stage and number of loops.
  //   */
  // def FDV(name: String, loops: Int) = {
  //   val query =
  //     MongoDBObject("loops" -> loops, "name" -> name) // query by given name, stage
  //   val cursor = elems.find(query)
  //   val pmf = (for (c <- cursor) yield {
  //     for (mvs <- c.getAs[String]("moves");  // pickled Moves
  //          wt  <- c.getAs[Double]("weight")) // associate weight
  //       yield Weighted(uread[Moves](mvs), wt) // Weighted Moves
  //   }).toVector.flatten // vector, flattened as options are returned.
  //   FiniteDistribution(pmf)
  // }
  //
  // def getElems(name: String, loops: Int) = {
  //   val query =
  //     MongoDBObject("loops" -> loops, "name" -> name) // query by given name, stage
  //   val cursor = elems.find(query)
  //   val list = (for (c <- cursor) yield {
  //     for (mvs  <- c.getAs[String]("moves"); // pickled Moves
  //          wt   <- c.getAs[Double]("weight"); // associate weight
  //          pres <- c.getAs[String]("presentation");
  //          rank <- c.getAs[Int]("rank"))
  //       yield
  //         ACElem(name,
  //                uread[Moves](mvs),
  //                rank,
  //                uread[Presentation](pres),
  //                wt,
  //                loops) // Weighted Moves
  //   }).toVector.flatten // vector, flattened as options are returned.
  //   list
  // }
  //
  // def getCurrentElems(name: String) = getElems(name, actorLoops(name).get)
  //
  // /**
  //   * Names of actors in database.
  //   */
  // def actorNames = {
  //   val cursor = actorData.find()
  //   (for (c <- cursor) yield c.getAs[String]("name")).toVector.flatten
  // }
  //
  // /**
  //   * (optional) number of loops of an actor in database
  //   */
  // def actorLoops(name: String) = {
  //   val query  = MongoDBObject("name" -> name)
  //   val cursor = actorData.findOne(query)
  //   (for (c <- cursor) yield c.getAs[Int]("loops")).flatten
  // }
  //
  // /**
  //   * (optional) Finite distribution on moves given actor name, for the latest stage of the actor
  //   */
  // def currentFDV(name: String) =
  //   actorLoops(name) map (FDV(name, _))
  //
  // /**
  //   * Weights on atomic moves for the actor.
  //   */
  // def FDM(name: String) = {
  //   val query = MongoDBObject("name" -> name)
  //   (fdMdb.findOne(query).flatMap(_.getAs[String]("fdM"))) map
  //     (uread[FiniteDistribution[AtomicMove]])
  // }
  //
  // def getState(name: String) = {
  //   for (fdV <- currentFDV(name); fdM <- FDM(name)) yield (fdM, fdV)
  // }
}

object ACData {
  // def srcRef(batch: String = "acDev", rank: Int) = ACFlowSaver.mongoSaveRef()
  //ACFlowSaver.actorRef(batch, rank)

  def thmFileCSV(dir: String = "acDev",
                 file: String,
                 rank: Int = 2,
                 lines: Option[Int] = None) = {
    val source = wd / dir / s"${file}.acthms"
    val target = wd / dir / s"${file}.acthms.csv"
    val l = (lines map ((n) => ((read.lines(source)).take(n)).toVector))
      .getOrElse(read.lines(source))
    val pmfs = l map ((x) => uread[Vector[(String, Double)]](x))
    val tVec =
      pmfs map
        ((pmf => FiniteDistribution(pmf map ((xp) => Weighted(xp._1, xp._2)))))
    val supp = (tVec map (_.supp.toSet) reduce (_ union _)).toVector
    def pVec(p: String) =
      tVec map ((fd) => fd(p))
    supp.foreach((p) => {
      write.append(target, s""""$p",${pVec(p).mkString(",")}\n""")
    })
  }

  def thmFiles(dir: String = "acDev", s: String => Boolean = (x) => true) = {
    ls(wd / dir) filter
      ((file) => file.ext == "acthms" && s(file.last.dropRight(7)))
  }

  def pickle(
      state: (FiniteDistribution[AtomicMove], FiniteDistribution[Moves])) = {
    val fdM = state._1
    val fdV = state._2
    val pmfM = for (Weighted(m, p) <- fdM.pmf)
      yield (PickledWeighted(uwrite(m), p))
    val pmfV = for (Weighted(v, p) <- fdV.pmf)
      yield (PickledWeighted(uwrite(v), p))
    val s = uwrite((pmfM, pmfV))
//    println(unpickle(s)) // a test
    s
  }

  def unpickle(str: String) = {
    val fdStrings =
      uread[(Vector[PickledWeighted], Vector[PickledWeighted])](str)
    val pmfM =
      fdStrings._1 map
        ((w: PickledWeighted) => w map ((x) => uread[AtomicMove](x)))
    val pmfV =
      fdStrings._2 map ((w: PickledWeighted) => w map ((x) => uread[Moves](x)))
    (FiniteDistribution(pmfM), FiniteDistribution(pmfV))
  }

  val wd = pwd / "data"
  def fileSave(name: String, dir: String = "acDev", rank: Int = 2)(
      fdM: FiniteDistribution[AtomicMove],
      fdV: FiniteDistribution[Moves]) = {
    val file      = wd / dir / (name + ".acrun")
    val statefile = wd / dir / (name + ".acstate")
    val thmfile   = wd / dir / (name + ".acthms")
    write.append(file, s"${pickle((fdM, fdV))}\n")
    write.over(statefile, s"${pickle((fdM, fdV))}\n")
    def writethms = {
      val thms =
        (toPresentation(2, fdV) map (_.toString)).flatten.pmf map {
          case Weighted(a, p) => (a, p)
        }
      write.append(thmfile, s"${uwrite(thms)}\n")
    }
    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global
    Future(writethms)
  }

  def load(name: String, dir: String = "acDev") = {
    val file  = wd / dir / (name + ".acrun")
    val lines = (read.lines(file)).to(LazyList)
    lines map (unpickle)
  }

  def loadFinal(name: String, dir: String = "acDev") = {
    val file = wd / dir / (name + ".acrun")
    val line = (read.lines(file)).last
    unpickle(line)
  }

  def loadAllFinal(name: String, dir: String = "acDev") = {
    val fileNames =
      ls(wd / dir) filter (_.ext == "acstate") map (_.toString.dropRight(8))
    val states =
      (for (name <- fileNames) yield (name, loadFinal(name, dir))).toMap
    ACStateData(states, dir)
  }

  def loadState(file: ammonite.ops.Path) = {
    unpickle(read(file))
  }

  def loadAll(dir: String = "acDev") = {
    val fileNames =
      ls(wd / dir) filter (_.ext == "acrun") map (_.last.dropRight(6))
    (for (name <- fileNames) yield (name, load(name, dir))).toMap
  }

  def loadData(dir: String = "acDev") = ACData(loadAll(dir), dir)

  def saveFD[T](file: String,
                dir: String = "0.5-output",
                fd: FiniteDistribution[T]) = {
    for (Weighted(x, p) <- fd.pmf) write.append(wd / file, s"$x, $p\n")
  }

  def saveEntropy(file: String,
                  dir: String = "0.5-output",
                  ent: List[Weighted[String]]) = {
    for (Weighted(x, p) <- ent) write.append(wd / file, s"$x, $p\n")
  }

  import FDHub._

  import Hub.system

  import scala.concurrent.ExecutionContext.Implicits.global

  def resetFile(file: ammonite.ops.Path) = {
    val lastline = read.lines(file).last
    write.over(file, s"$lastline\n")
  }

  def lastLine(source: ammonite.ops.Path, target: ammonite.ops.Path) = {
    val lastline = read.lines(source).last
    write.over(target, s"$lastline\n")
  }

  def succFile(source: ammonite.ops.Path) = {
    val target = source / up / s"succ-${source.last}"
    lastLine(source, target)
  }
}
